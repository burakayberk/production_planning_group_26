library(shiny)
for(f in list.files('R',pattern='[.]R$',full.names=TRUE)) source(f)
p<-list(K=5,workforce=10,start_inventory=100,end_inventory=100,hire=300,fire=500,hold=2,regular=100,overtime=30,subcontract=40,ot_limit=.25,sub_limit=500)
d<-ap_defaults()
check<-function(x){stopifnot(x$feasible,abs(x$total-sum(x$costs$Total))<1e-5,all(x$plan$Inventory>=-1e-6));stopifnot(max(abs(x$plan$Inventory-(p$start_inventory+cumsum(x$plan$Production-d$Demand))))<1e-5)}
for(m in c('chase','level','mixed'))for(r in c(FALSE,TRUE)){z<-ap_heuristic(d,p,m,c(1,4),r);check(z);if(r)stopifnot(all(abs(z$plan$Workers-round(z$plan$Workers))<1e-8));cat(m,r,z$total,'PASS\n')}
z<-ap_heuristic(d,p,'chase');stopifnot(max(abs(z$plan$Inventory-c(0,0,0,0,0,100)))<1e-6)
lp<-ap_lp(d,p);check(lp);stopifnot(abs(lp$total-lp$objective)<1e-5)
rounded<-ap_lp(d,p,TRUE,TRUE,TRUE,ceiling(lp$plan$Workers-1e-7));check(rounded)
trans<-list()
for(m in c('northwest','cheapest','optimal')) {z<-ap_transport(d,p,m);check(z);stopifnot(abs(z$total-z$transport_objective)<1e-4);trans[[m]]<-z;cat(m,z$total,'PASS\n')}
stopifnot(trans$optimal$total<=trans$northwest$total+1e-5,trans$optimal$total<=trans$cheapest$total+1e-5)
fixed<-ap_lp(d,p,TRUE,TRUE,TRUE,rep(p$workforce,nrow(d)));stopifnot(abs(fixed$total-trans$optimal$total)<1e-4)
# Hand-computable one-period case: 10 workers, 20 days, 1000 units.
one<-data.frame(Period='P1',Demand=1000,Days=20);q<-p;q$start_inventory<-q$end_inventory<-0
for(m in c('chase','level','mixed'))stopifnot(ap_heuristic(one,q,m)$total==20000)
# Early peak cannot be fulfilled using later capacities.
bad<-data.frame(Period=c('A','B'),Demand=c(10000,0),Days=c(1,100));q$ot_limit<-q$sub_limit<-0
stopifnot(inherits(try(ap_transport(bad,q),silent=TRUE),'try-error'))
# Opening stock above total demand remains in inventory and incurs holding cost.
q<-p;q$start_inventory<-10000;q$end_inventory<-0
z<-ap_transport(d,q);stopifnot(abs(z$total-z$transport_objective)<1e-4)
cat('All planning checks passed.\n')
