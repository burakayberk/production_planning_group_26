# Transportation tableau: workforce fixed at the entered starting workforce.
# Regular payroll is fixed and added to the transportation objective.
ap_transport <- function(d,p,method='optimal',rounded=FALSE) {
  ap_check(d,p);n<-nrow(d);w<-p$workforce;if(rounded) w<-ceiling(w-1e-9)
  src<-data.frame(Name='Opening inventory',Period=0,Type='Initial',Capacity=p$start_inventory)
  for(t in 1:n) src<-rbind(src,data.frame(Name=paste(d$Period[t],c('Regular','Overtime','Subcontract')),
     Period=t,Type=c('Regular','Overtime','Subcontract'),Capacity=c(w*p$K*d$Days[t],w*p$K*d$Days[t]*p$ot_limit,p$sub_limit)))
  needs<-c(d$Demand,p$end_inventory)
  if(sum(src$Capacity)<sum(needs)-1e-7) stop('Total transportation capacity is insufficient.')
  needs<-c(needs,max(0,sum(src$Capacity)-sum(needs)))
  nr<-nrow(src);nc<-length(needs);cost<-matrix(Inf,nr,nc)
  for(i in 1:nr) {
    t<-src$Period[i];base<-switch(src$Type[i],Initial=0,Regular=0,Overtime=p$overtime,Subcontract=p$subcontract)
    for(j in 1:n) if(t<=j) cost[i,j]<-base+(j-max(1,t))*p$hold
    cost[i,n+1]<-base+(n-max(1,t)+1)*p$hold
    cost[i,n+2]<-if(t==0) n*p$hold else 0
  }
  cells<-which(is.finite(cost),arr.ind=TRUE);nv<-nrow(cells)
  A<-matrix(0,nr+nc,nv)
  for(k in 1:nv){A[cells[k,1],k]<-1;A[nr+cells[k,2],k]<-1}
  obj<-cost[cells]; b<-c(src$Capacity,needs)
  fit<-lpSolve::lp('min',obj,A,rep('=',nr+nc),b,timeout=20)
  if(fit$status!=0) stop('Capacity arrives too late to cover demand. Increase earlier capacity; backorders are not allowed.')
  if(method=='optimal') x<-fit$solution else {
    # Process real demand first and unused capacity last. At each allocation,
    # maximize the selected cell subject to feasibility of the residual tableau.
    order_cells<-if(method=='northwest') order(cells[,2],cells[,1]) else
      order(cells[,2]==nc,obj,cells[,2],cells[,1])
    x<-numeric(nv);remaining<-b;active<-seq_len(nv)
    for(k in order_cells) {
      if(remaining[cells[k,1]]>1e-8 && remaining[nr+cells[k,2]]>1e-8) {
        objective<-numeric(length(active));objective[match(k,active)]<--1
        f<-lpSolve::lp('min',objective,A[,active,drop=FALSE],rep('=',nr+nc),pmax(remaining,0),timeout=20)
        if(f$status!=0) stop('Could not construct a feasible starting solution.')
        amount<-max(0,f$solution[match(k,active)]);x[k]<-amount
        remaining<-remaining-A[,k]*amount
      }
      active<-setdiff(active,k)
    }
  }
  allocation<-matrix(0,nr,nc);allocation[cells]<-pmax(x,0)
  production<-rowSums(allocation[,seq_len(n+1),drop=FALSE])
  getprod<-function(type) vapply(1:n,function(t) sum(production[src$Period==t & src$Type==type]),numeric(1))
  out<-ap_finish(d,p,rep(w,n),getprod('Regular'),getprod('Overtime'),getprod('Subcontract'))
  colnames(allocation)<-c(d$Period,'Target ending stock','Unused / excess opening stock')
  rownames(allocation)<-src$Name;dimnames(cost)<-dimnames(allocation)
  out$allocation<-allocation;out$unit_cost<-cost;out$sources<-src
  out$transport_objective<-sum(obj*x)+sum(w*d$Days*p$regular)+max(w-p$workforce,0)*p$hire
  out
}
