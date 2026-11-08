ap_read <- function(file,sheet=1) {
  ext<-tolower(tools::file_ext(file$name))
  if(ext=='xlsx') return(as.data.frame(readxl::read_excel(file$datapath,sheet=sheet)))
  if(ext=='csv') return(read.csv(file$datapath,check.names=FALSE,stringsAsFactors=FALSE))
  stop('Upload an .xlsx or comma-separated .csv file.')
}
ap_defaults <- function() data.frame(Period=c('Jan','Feb','Mar','Apr','May','Jun'),Demand=c(900,1200,1600,1000,1500,1100),Days=c(20,20,23,21,22,20))
aggregate_plan_ui <- function(id,method,title) {
  ns<-NS(id)
  sidebarLayout(sidebarPanel(width=4,
    h4('Demand and working days'),fileInput(ns('file'),'Upload Excel or CSV',accept=c('.xlsx','.csv')),
    uiOutput(ns('sheet_ui')),helpText('A six-period example is used until a file is uploaded. CSV files must use commas as separators.'),
    selectInput(ns('period_col'),'Period column',choices=NULL),selectInput(ns('demand_col'),'Demand column',choices=NULL),
    selectInput(ns('days_col'),'Working-days column',choices=NULL),
    textInput(ns('period_names'),'Override period names (comma-separated)',''),
    h4('Plant'),radioButtons(ns('k_mode'),'Productivity',c('Enter K directly'='direct','Compute from history'='history')),
    conditionalPanel("input.k_mode == 'direct'",ns=ns,numericInput(ns('K'),'K: units per worker-day',5,min=.0001)),
    conditionalPanel("input.k_mode == 'history'",ns=ns,numericInput(ns('past_units'),'Past production (units)',1000,min=0),
      numericInput(ns('past_workers'),'Past workers',10,min=.001),numericInput(ns('past_days'),'Past working days',20,min=.001)),
    numericInput(ns('workforce'),'Starting workforce',10,min=0),
    numericInput(ns('start_inventory'),'Starting inventory (units)',100,min=0),numericInput(ns('end_inventory'),'Minimum ending inventory (units)',100,min=0),
    h4('Costs'),numericInput(ns('hire'),'Hiring cost per worker',300,min=0),numericInput(ns('fire'),'Firing cost per worker',500,min=0),
    numericInput(ns('hold'),'Holding cost per unit per period',2,min=0),numericInput(ns('regular'),'Regular wage per worker-day',100,min=0),
    numericInput(ns('overtime'),'Overtime cost per unit',30,min=0),numericInput(ns('subcontract'),'Subcontract cost per unit',40,min=0),
    if(method %in% c('lp','transport')) tagList(h4('Capacity options'),numericInput(ns('ot_limit'),'Overtime capacity / regular capacity',.25,min=0,max=3,step=.05),
      numericInput(ns('sub_limit'),'Subcontract limit per period (units)',500,min=0)),
    if(method=='lp') tagList(checkboxInput(ns('allow_ot'),'Allow overtime',TRUE),checkboxInput(ns('allow_idle'),'Allow idle time',TRUE),checkboxInput(ns('allow_sub'),'Allow subcontracting',TRUE)),
    if(method=='mixed') selectizeInput(ns('change_periods'),'Workforce may change at start of these periods',choices=1:6,selected=c(1,4),multiple=TRUE),
    if(method=='transport') selectInput(ns('transport_method'),'Transportation solution',choices=c('North-west corner (feasibility checked)'='northwest','Cheapest cell first (feasibility checked)'='cheapest','Optimal (linear programming)'='optimal')),
    helpText('No backorders. Regular wages are paid for all worker-days, including idle time. Ending inventory is a minimum target. Holding cost is charged on each period’s ending stock.'),
    actionButton(ns('run'),'Calculate plan',class='btn-primary')
  ),mainPanel(width=8,h3(title),
    if(method=='level') helpText('The smallest constant workforce that covers every cumulative demand requirement is used. This may exceed the horizon-average workforce.'),
    if(method=='mixed') helpText('Period 1 always starts a workforce block. Within each block, the workforce is constant and covers all cumulative requirements.'),
    if(method=='transport') helpText('Workforce is fixed at the entered starting workforce. Future production cannot supply earlier demand. Starting solutions reserve capacity when needed to keep later assignments feasible. Cheapest-cell-first is a heuristic, not a guarantee of minimum cost.'),
    tableOutput(ns('input_table')),textOutput(ns('summary')),
    tabsetPanel(tabPanel('Original plan',tableOutput(ns('plan')),plotOutput(ns('plot')),tableOutput(ns('costs'))),
      tabPanel('Whole workers',textOutput(ns('round_note')),tableOutput(ns('round_plan')),plotOutput(ns('round_plot')),tableOutput(ns('round_costs'))),
      if(method=='transport') tabPanel('Transportation tableau',h4('Allocation'),tableOutput(ns('allocation')),h4('Incremental unit costs (regular payroll added separately)'),tableOutput(ns('unit_cost')))),
    downloadButton(ns('download'),'Download original plan CSV')
  ))
}
aggregate_plan_server <- function(id,method) moduleServer(id,function(input,output,session){
  output$sheet_ui<-renderUI({req(input$file);if(tolower(tools::file_ext(input$file$name))=='xlsx') selectInput(session$ns('sheet'),'Excel sheet',readxl::excel_sheets(input$file$datapath))})
  raw<-reactive({if(is.null(input$file)) return(ap_defaults());if(tolower(tools::file_ext(input$file$name))=='xlsx') req(input$sheet);ap_read(input$file,if(is.null(input$sheet)) 1 else input$sheet)})
  observeEvent(raw(),{
    cols<-names(raw()); for(pair in list(c('period_col','Period'),c('demand_col','Demand'),c('days_col','Days'))) {
      selected<-if(pair[2] %in% cols) pair[2] else cols[min(match(pair[1],c('period_col','demand_col','days_col')),length(cols))]
      updateSelectInput(session,pair[1],choices=cols,selected=selected)
    }
    updateSelectizeInput(session,'change_periods',choices=seq_len(nrow(raw())),selected=unique(c(1,min(4,nrow(raw())))))
  })
  data<-reactive({req(input$period_col,input$demand_col,input$days_col);x<-raw()
    d<-data.frame(Period=as.character(x[[input$period_col]]),Demand=suppressWarnings(as.numeric(x[[input$demand_col]])),Days=suppressWarnings(as.numeric(x[[input$days_col]])))
    if(nzchar(trimws(input$period_names))) { names<-trimws(strsplit(input$period_names,',',fixed=TRUE)[[1]]);validate(need(length(names)==nrow(d),'Provide one period name per row.'));d$Period<-names };d
  })
  output$input_table<-renderTable(data())
  result<-eventReactive(input$run,{
    tryCatch({
      d<-data();K<-if(input$k_mode=='history') input$past_units/(input$past_workers*input$past_days) else input$K
      p<-list(K=K,workforce=input$workforce,start_inventory=input$start_inventory,end_inventory=input$end_inventory,
        hire=input$hire,fire=input$fire,hold=input$hold,regular=input$regular,overtime=input$overtime,subcontract=input$subcontract,
        ot_limit=if(is.null(input$ot_limit)) 0 else input$ot_limit,sub_limit=if(is.null(input$sub_limit)) 0 else input$sub_limit)
      ap_check(d,p)
      if(method=='lp') {
        original<-ap_lp(d,p,input$allow_ot,input$allow_idle,input$allow_sub)
        rounded<-tryCatch(ap_lp(d,p,input$allow_ot,TRUE,input$allow_sub,ceiling(original$plan$Workers-1e-7)),error=function(e)e)
        note<-'Workers are rounded up, then production is re-optimized with those fixed workers. Idle time is allowed in this alternative to avoid forced excess production.'
      } else if(method=='transport') {
        original<-ap_transport(d,p,input$transport_method);rounded<-ap_transport(d,p,input$transport_method,TRUE)
        note<-'Fixed workforce is rounded up; capacities and the transportation plan are recalculated.'
      } else {
        original<-ap_heuristic(d,p,method,input$change_periods);rounded<-ap_heuristic(d,p,method,input$change_periods,TRUE)
        note<-'Workers are rounded up at each decision point; production and inventory are recalculated. Chase uses carried inventory to calculate the next requirement.'
      }
      list(original=original,rounded=rounded,note=note,K=K)
    },error=function(e){validate(need(FALSE,conditionMessage(e)))})
  },ignoreInit=TRUE)
  output$summary<-renderText({r<-result();paste('K =',round(r$K,4),'| Original cost:',round(r$original$total,2),'| Whole-worker cost:',if(inherits(r$rounded,'error')) 'No feasible rounded plan' else round(r$rounded$total,2))})
  output$plan<-renderTable(result()$original$plan,digits=3)
  output$costs<-renderTable(result()$original$costs,digits=2)
  rounded<-reactive({r<-result()$rounded;validate(need(!inherits(r,'error'),if(inherits(r,'error')) conditionMessage(r) else ''));r})
  output$round_plan<-renderTable(rounded()$plan,digits=3);output$round_costs<-renderTable(rounded()$costs,digits=2)
  output$round_note<-renderText(result()$note)
  draw<-function(r){a<-cumsum(r$plan$Production);b<-r$net_demand;plot(seq_along(a),a,type='o',col='#176B87',ylim=range(c(0,a,b)),xaxt='n',xlab='Period',ylab='Aggregate units',main='Cumulative production vs. cumulative net demand');axis(1,seq_along(a),r$plan$Period);lines(seq_along(b),b,type='o',col='#D55E00',lty=2);legend('topleft',c('Production','Net demand incl. final stock target'),col=c('#176B87','#D55E00'),lty=c(1,2),bty='n')}
  output$plot<-renderPlot(draw(result()$original));output$round_plot<-renderPlot(draw(rounded()))
  output$allocation<-renderTable(result()$original$allocation,rownames=TRUE,digits=2)
  output$unit_cost<-renderTable(result()$original$unit_cost,rownames=TRUE,digits=2)
  output$download<-downloadHandler(filename=function() paste0(method,'_plan.csv'),content=function(file) write.csv(result()$original$plan,file,row.names=FALSE))
})
