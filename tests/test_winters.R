library(shiny)
source('R/winters_method.R')
x<-data.frame(Demand=100+2*(1:48)+rep(c(-15,0,10,5),12))
f<-tempfile(fileext='.csv');write.csv(x,f,row.names=FALSE)
for(mode in c('regression','estimate','manual')) {
 testServer(winters_method_server,{
  session$setInputs(file=list(name='series.csv',datapath=f),column='Demand',season_length=4,horizon=8,model='additive',
    initial_mode=mode,initial_level=108,initial_trend=2,parameter_mode='manual',alpha=.3,beta=.2,gamma=.2)
  r<-result();stopifnot(length(r$future)==8,all(is.finite(r$future)),all(is.finite(r$metrics)))
  if(mode=='regression')stopifnot(abs(r$initial$trend-2)<1e-8)
 });cat('Winters',mode,'PASS\n')
}
