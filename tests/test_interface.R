library(shiny)
for(f in list.files('R',pattern='[.]R$',full.names=TRUE)) source(f)
for(m in c('chase','level','mixed','lp','transport')) {
 testServer(aggregate_plan_server,args=list(method=m),{
   session$setInputs(period_col='Period',demand_col='Demand',days_col='Days',period_names='',k_mode='direct',K=5,
      workforce=10,start_inventory=100,end_inventory=100,hire=300,fire=500,hold=2,regular=100,overtime=30,subcontract=40,
      ot_limit=.25,sub_limit=500,allow_ot=TRUE,allow_idle=TRUE,allow_sub=TRUE,change_periods=c('1','4'),transport_method='optimal')
   session$setInputs(run=1)
   stopifnot(result()$original$feasible,nrow(result()$original$plan)==6)
   stopifnot(nzchar(output$summary),nzchar(output$plan),nzchar(output$costs),nzchar(output$round_plan))
   session$setInputs(period_names='A,B,C,D,E,F',k_mode='history',past_units=1000,past_workers=10,past_days=20,run=2)
   stopifnot(result()$K==5,result()$original$plan$Period[1]=='A')
 });cat(m,'interface PASS\n')
}
