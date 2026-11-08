# Aggregate planning: quantities in aggregate units; no backorders.
# Regular cost is paid per worker-day, including unused regular capacity.
ap_check <- function(d, p) {
  stopifnot(all(c('Period','Demand','Days') %in% names(d)))
  if (!nrow(d) || nrow(d)>60 || any(!is.finite(d$Demand)) || any(d$Demand<0) ||
      any(!is.finite(d$Days)) || any(d$Days<=0)) stop('Use 1–60 periods, nonnegative demand and positive working days.')
  if (anyNA(d$Period) || any(!nzchar(trimws(d$Period))) || anyDuplicated(d$Period)) stop('Period names must be nonempty and unique.')
  vals <- unlist(p[c('K','workforce','start_inventory','end_inventory','hire','fire','hold','regular','overtime','subcontract','ot_limit','sub_limit')])
  if (any(!is.finite(vals)) || any(vals<0) || p$K<=0) stop('Enter finite nonnegative inputs; productivity must be positive.')
  invisible(TRUE)
}
ap_finish <- function(d,p,w,r,ot=rep(0,nrow(d)),sub=rep(0,nrow(d))) {
  capacity <- w*p$K*d$Days
  inventory <- p$start_inventory+cumsum(r+ot+sub-d$Demand)
  changes <- w-c(p$workforce,head(w,-1)); hired <- pmax(changes,0); fired <- pmax(-changes,0)
  idle <- pmax(capacity-r,0)
  tab <- data.frame(Period=d$Period,Demand=d$Demand,Days=d$Days,Workers=w,Hired=hired,Fired=fired,
    Regular=r,Overtime=ot,Subcontract=sub,Production=r+ot+sub,Inventory=inventory,Idle_Units=idle)
  costs <- data.frame(Period=d$Period,Hiring=hired*p$hire,Firing=fired*p$fire,
    Holding=pmax(inventory,0)*p$hold,Regular=w*d$Days*p$regular,
    Overtime=ot*p$overtime,Subcontract=sub*p$subcontract)
  costs$Total <- rowSums(costs[-1])
  list(plan=tab,costs=costs,total=sum(costs$Total),
       feasible=all(inventory>=-1e-6)&&tail(inventory,1)>=p$end_inventory-1e-6,
       net_demand=cumsum(d$Demand)-p$start_inventory+c(rep(0,nrow(d)-1),p$end_inventory))
}
ap_heuristic <- function(d,p,method='chase',changes=1,rounded=FALSE) {
  ap_check(d,p); n <- nrow(d); w <- r <- numeric(n); inv <- p$start_inventory
  if(method=='chase') {
    for(t in seq_len(n)) {
      need <- max(0,d$Demand[t]+(if(t==n) p$end_inventory else 0)-inv)
      w[t] <- need/(p$K*d$Days[t]); if(rounded) w[t] <- ceiling(w[t]-1e-9)
      r[t] <- w[t]*p$K*d$Days[t]; inv <- inv+r[t]-d$Demand[t]
    }
  } else {
    starts <- if(method=='level') 1L else sort(unique(c(1L,as.integer(changes))))
    if(anyNA(starts)||any(starts<1|starts>n)) stop('Invalid workforce change period.')
    ends <- c(starts[-1]-1L,n)
    for(j in seq_along(starts)) {
      ix <- starts[j]:ends[j]
      required <- cumsum(d$Demand[ix])-inv
      if(tail(ix,1)==n) required[length(ix)] <- required[length(ix)]+p$end_inventory
      workers <- max(0,required/(p$K*cumsum(d$Days[ix])))
      if(rounded) workers <- ceiling(workers-1e-9)
      w[ix] <- workers; r[ix] <- workers*p$K*d$Days[ix]
      inv <- inv+sum(r[ix])-sum(d$Demand[ix])
    }
  }
  ap_finish(d,p,w,r)
}
# LP variables per period: workforce, hires, fires, regular production,
# overtime production, subcontracting, ending inventory, idle capacity.
ap_lp <- function(d,p,allow_ot=TRUE,allow_idle=TRUE,allow_sub=TRUE,fixed_workers=NULL) {
  ap_check(d,p); n<-nrow(d); nv<-8*n
  idx<-function(k,t) (k-1)*n+t
  obj<-numeric(nv); obj[idx(1,1:n)]<-p$regular*d$Days
  obj[idx(2,1:n)]<-p$hire; obj[idx(3,1:n)]<-p$fire
  obj[idx(5,1:n)]<-p$overtime;obj[idx(6,1:n)]<-p$subcontract;obj[idx(7,1:n)]<-p$hold
  A<-list(); dirs<-character(); rhs<-numeric()
  add<-function(ids,coefs,dir,value) { row<-numeric(nv);row[ids]<-coefs;A[[length(A)+1]]<<-row;dirs<<-c(dirs,dir);rhs<<-c(rhs,value) }
  for(t in 1:n) {
    add(c(idx(1,t),idx(2,t),idx(3,t),if(t>1) idx(1,t-1)),c(1,-1,1,if(t>1) -1),'=',if(t==1) p$workforce else 0)
    add(c(idx(4,t),idx(5,t),idx(6,t),idx(7,t),if(t>1) idx(7,t-1)),c(1,1,1,-1,if(t>1) 1),'=',d$Demand[t]-(if(t==1) p$start_inventory else 0))
    add(c(idx(4,t),idx(8,t),idx(1,t)),c(1,1,-p$K*d$Days[t]),'=',0)
    add(c(idx(5,t),idx(1,t)),c(1,-(if(allow_ot) p$ot_limit else 0)*p$K*d$Days[t]),'<=',0)
    add(idx(6,t),1,'<=',if(allow_sub) p$sub_limit else 0)
    if(!allow_idle) add(idx(8,t),1,'=',0)
    if(!is.null(fixed_workers)) add(idx(1,t),1,'=',fixed_workers[t])
  }
  add(idx(7,n),1,'>=',p$end_inventory)
  fit<-lpSolve::lp('min',obj,do.call(rbind,A),dirs,rhs,timeout=20)
  if(fit$status!=0) stop('No optimal feasible plan found. Check capacities, inventory targets and enabled options.')
  x<-pmax(fit$solution,0)
  out<-ap_finish(d,p,x[idx(1,1:n)],x[idx(4,1:n)],x[idx(5,1:n)],x[idx(6,1:n)])
  out$objective<-fit$objval;out
}
