# Square-root allocation of a single selected aggregate-plan period.
# Minimize sum_i (SetupCost_i * Demand_i / Q_i), with sum_i Q_i = P
# and Q_i >= max(Demand_i - OpeningInventory_i, 0).
# All quantities must use the same aggregate-equivalent unit.
disaggregate_square_root <- function(families,total) {
  required<-c('Family','Demand','SetupCost','OpeningInventory')
  if(!all(required %in% names(families))) stop(paste('Required columns:',paste(required,collapse=', ')))
  if(!nrow(families)||anyNA(families$Family)||any(!nzchar(trimws(families$Family)))||anyDuplicated(families$Family)) stop('Use unique, nonempty family names.')
  for(n in required[-1]) {
    families[[n]]<-suppressWarnings(as.numeric(families[[n]]))
    if(any(!is.finite(families[[n]]))||any(families[[n]]<0)) stop('Demand, setup costs and opening inventories must be finite and nonnegative.')
  }
  if(length(total)!=1||!is.finite(total)||total<0)stop('Aggregate production must be finite and nonnegative.')
  lower<-pmax(families$Demand-families$OpeningInventory,0)
  if(sum(lower)>total+1e-7)stop(paste('Aggregate production cannot cover family net demand. Minimum required:',round(sum(lower),4)))
  a<-families$SetupCost*families$Demand;weights<-sqrt(a)
  if(!is.finite(sum(a)))stop('Inputs are too large.')
  if(total==0) q<-rep(0,nrow(families)) else if(sum(weights)==0) {
    q<-lower+(total-sum(lower))/length(lower)
  } else if(abs(sum(lower)-total)<1e-8) q<-lower else {
    # Active lower bounds: Q_i = max(lower_i, lambda * sqrt(S_i D_i)).
    hi<-total/min(weights[weights>0])+1
    lambda<-uniroot(function(v)sum(pmax(lower,v*weights))-total,c(0,hi),tol=1e-10)$root
    q<-pmax(lower,lambda*weights)
  }
  out<-data.frame(Family=families$Family,Demand=families$Demand,SetupCost=families$SetupCost,
    OpeningInventory=families$OpeningInventory,MinimumProduction=lower,SquareRootWeight=weights,
    Production=q,EndingInventory=families$OpeningInventory+q-families$Demand,
    Share=if(total>0)q/total else 0)
  stopifnot(abs(sum(q)-total)<1e-5,all(out$EndingInventory>=-1e-6))
  out
}
disaggregation_ui <- function(id) {
  ns<-NS(id)
  sidebarLayout(sidebarPanel(width=4,
    fileInput(ns('plan_file'),'Upload aggregate plan CSV or Excel',accept=c('.csv','.xlsx')),
    helpText('Use Download original plan CSV in any planning module, then upload it here. Excel uses the first sheet.'),
    uiOutput(ns('period_ui')),numericInput(ns('total'),'Production to allocate (aggregate units)',1200,min=0),
    fileInput(ns('family_file'),'Upload family data CSV or Excel',accept=c('.csv','.xlsx')),
    helpText('Required columns: Family, Demand, SetupCost, OpeningInventory. All quantities must use the same aggregate-equivalent unit.'),
    textAreaInput(ns('families'),'Or edit family CSV directly',
      'Family,Demand,SetupCost,OpeningInventory\nA,500,100,100\nB,300,200,50\nC,200,150,0',rows=9),
    actionButton(ns('run'),'Allocate production',class='btn-primary')
  ),mainPanel(width=8,h3('Disaggregation — Square-Root Rule'),
    p('For one selected period: Q_i = max(net demand_i, lambda × sqrt(SetupCost_i × Demand_i)); lambda is chosen so total family production equals the aggregate plan.'),
    p('With inactive net-demand bounds, Q_i = P × sqrt(S_i D_i) / sum(sqrt(S_j D_j)). Equal setup costs give demand-only square-root weights.'),
    helpText('This rule minimizes the setup-frequency surrogate sum(S_i D_i / Q_i) under a fixed total. It is a family allocation heuristic. Opening inventory and demand must refer to the selected period. When processing the next period, carry forward ending inventories and enter its family demands.'),
    textOutput(ns('summary')),tableOutput(ns('allocation')),plotOutput(ns('plot')),
    downloadButton(ns('download'),'Download family allocation CSV')))
}
disaggregation_server <- function(id) moduleServer(id,function(input,output,session){
  plan<-reactive({req(input$plan_file);x<-ap_read(input$plan_file);validate(need(all(c('Period','Production')%in%names(x)),'Aggregate plan must contain Period and Production columns.'));x})
  output$period_ui<-renderUI({req(input$plan_file);selectInput(session$ns('period'),'Aggregate-plan period',choices=setNames(seq_len(nrow(plan())),plan()$Period))})
  observeEvent(input$period,{req(input$period);updateNumericInput(session,'total',value=as.numeric(plan()$Production[as.integer(input$period)]))})
  result<-eventReactive(input$run,{
    tryCatch({
      f<-if(!is.null(input$family_file))ap_read(input$family_file) else read.csv(text=input$families,check.names=FALSE)
      disaggregate_square_root(f,input$total)
    },error=function(e)validate(need(FALSE,conditionMessage(e))))
  },ignoreInit=TRUE)
  output$allocation<-renderTable(result(),digits=4)
  output$summary<-renderText({x<-result();paste('Allocated:',round(sum(x$Production),4),'| Total ending inventory:',round(sum(x$EndingInventory),4))})
  output$plot<-renderPlot({x<-result();barplot(rbind(x$Demand,x$Production),beside=TRUE,names.arg=x$Family,col=c('#D55E00','#176B87'),ylab='Aggregate-equivalent units',legend.text=c('Demand','Production'))})
  output$download<-downloadHandler(filename=function()'family_allocation.csv',content=function(file)write.csv(result(),file,row.names=FALSE))
})
