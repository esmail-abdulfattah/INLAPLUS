rm(list = ls(all.names = TRUE))

library(INLA)
library(devtools)
#remotes::install_github("esmail-abdulfattah/INLAPLUS")
library(INLAPLUS)

n = 10
m = 100

2*n+2*m+m*n+1
m*n
n*m - (n-2)*(m-1) + 2 + 1

n*m - (n-2)*(m-1)
2*n+2*m+m*n+1

type = "type4"
orderRW = 2

mtau = 1e-3
mn = n*m
idx.time = rep(1:n, each=m)
idx.space = rep(1:m, n)
idx.inter = 1:mn
x_size = 2*n+2*m+mn+1
x_size

Qx1_FIXED <- INLA:::inla.rw(n, order=orderRW, scale.model=TRUE, sparse=FALSE)
Qx2_FIXED <-  exp(runif(1,5,6))*diag(n)
Qx3_FIXED <- get_random_besag_graph(m)
constr.inter <- list(A = t(eigen(Qx3_FIXED)$vectors[,m]), e = rep(0, 1))
Qx3_FIXED = as.matrix(inla.scale.model(Qx3_FIXED,constr.inter))
Qx4_FIXED <-  exp(runif(1,5,6))*diag(m)

r1 = dim(Qx1_FIXED)[1]-rankMatrix(Qx1_FIXED)[1]
r2 = dim(Qx3_FIXED)[1]-rankMatrix(Qx3_FIXED)[1]

ginvQx1 = INLA:::inla.ginv(Qx1_FIXED,rankdef = r1)
ginvQx3 = INLA:::inla.ginv(Qx3_FIXED,rankdef = r2)

constr.inter = NULL
STZ = FALSE

sQx5_FIXED <- Qx1_FIXED %x%  Qx3_FIXED
Qx5_FIXED <- Qx1_FIXED %x%  Qx3_FIXED
ginvQx5 <- ginvQx1 %x%  ginvQx3

r3 = dim(Qx5_FIXED)[1]-rankMatrix(Qx5_FIXED)[1]


interact.constr <- matrix(0, n+m-1, mn)
for(i in 1:m){
  interact.constr[i, seq(1, n * m, by=m) + (i-1)] = 1
}
for(i in 1:(n-1)){
  interact.constr[i+m, 1:m + (i-1)*m] = 1
}

if(orderRW==2)
{
  v1 = rep(1:n,each=m)
  for(i in 1:(m-1)) interact.constr = rbind(interact.constr,v1*interact.constr[i,])
  constr.inter <- list(A = interact.constr, e = rep(0, n+m-1+m-1))

}else{
  constr.inter <- list(A = interact.constr, e = rep(0, n+m-1))
}
STZ = FALSE


r3 =n*m - (n-r1)*(m-r2)

eff1 <- drop(get_sample_from(1,Qx1_FIXED,r1))
eff2 <- drop(get_sample_from(1,Qx2_FIXED,0))
eff3 <-  drop(get_sample_from(1,Qx3_FIXED,r2))
eff4 <- drop(get_sample_from(1,Qx4_FIXED,0))
eff5 <- drop(get_sample_from(1,sQx5_FIXED,r3))

eff1 = eff1/max(eff1)
eff2 = eff2/max(eff2)
eff3 = eff3/max(eff3)
eff4 = eff4/max(eff4)
eff5 = eff5/max(eff5)

ccc = rep(eff1,each=m) + rep(eff2,each=m) + rep(eff3,n) + rep(eff4,n) + eff5
eta <- 2 + ccc#/max(ccc)
ysim <- rpois(n*m,lambda = exp(eta)) + 2

summary(ysim)

if(orderRW==2) {rw2constr.inter <- list(A = matrix(1:n, 1, n), e = rep(0, 1))
}else {rw2constr.inter <- list(A = t(as.matrix(rep(0,n))), e = rep(0, 1))}

ef1 = idx.time
ef2 = idx.time
ef3 = idx.space
ef4 = idx.space
ef5 = idx.inter

prior1 = list(type1 = "loggama.prec", par1 = c(1,1))
#prior1 = list(type1 = "pc.prec", par1 = c(1,0.01))

offset = runif(n*m,0.5,10)

my.data <- data.frame(y = ysim,idx.time=idx.time,idx.space=idx.space,id1 = idx.time,id2 = idx.space)
formula1 = y ~ 1 +
  f(idx.time, model="generic0", Cmatrix = Qx1_FIXED,constr=TRUE,rankdef=r1,extraconstr = rw2constr.inter,diagonal = 1.015113e-3,
    hyper = list(theta = list(prior = "loggamma", param = c(1.0, 1.0),
                              initial = 4, fixed = FALSE))) +
  f(id1, model="iid", hyper = list(theta = list(prior = "loggamma", param = c(1.0, 1.0),
                                                initial = 4, fixed = FALSE))) +
  f(idx.space, model="generic0", Cmatrix = Qx3_FIXED,rankdef=r2,constr=TRUE, diagonal = 1.015113e-03,
    hyper = list(theta = list(prior = "loggamma", param = c(1.0, 1.0),
                              initial = 4, fixed = FALSE))) +
  f(id2, model="iid", hyper = list(theta = list(prior = "loggamma", param = c(1.0, 1.0),
                                                initial = 4, fixed = FALSE))) +
  f(idx.inter, model="generic0",
    Cmatrix = Qx5_FIXED,
    constr=STZ, extraconstr = constr.inter,
    diagonal = 1e-5,
    rankdef=r3,
    hyper = list(theta = list(prior = "loggamma", param = c(1.0, 1.0),initial = 4, fixed = FALSE)))

formula2 = y ~ 1 +
  f(idx.time, model="generic0", Cmatrix = Qx1_FIXED,constr=TRUE,rankdef=r1,extraconstr = rw2constr.inter,diagonal = 1.015113e-3,
    hyper = list(prec=list(prior="pc.prec", param=c(1,0.01)))) +
  f(id1, model="iid",hyper = list(prec=list(prior="pc.prec", param=c(1,0.01)))) +
  f(idx.space, model="generic0", Cmatrix = Qx3_FIXED,rankdef=r2,constr=TRUE, diagonal = 1.015113e-03,
    hyper = list(prec=list(prior="pc.prec", param=c(1,0.01)))) +
  f(id2, model="iid",
    hyper = list(prec=list(prior="pc.prec", param=c(1,0.01)))) +
  f(idx.inter, model="generic0",
    Cmatrix = Qx5_FIXED,
    constr=FALSE, extraconstr = constr.inter,
    diagonal = 1e-4,
    rankdef=r3,
    hyper = list(prec=list(prior="pc.prec", param=c(1,0.01))))

control_strategy = list(Strategy = "GA")
mydata = list(y_response = ysim)
myModel = list(like="Poisson",  offset = offset)
control_prec = list(prec_mu=mtau)#, prec_beta = 0.001*diag(nbeta))

gf = y ~ block(type="intercept", Q = matrix(mtau)) + #block(type="covariate", Z = matrix(4,10,2)) +
  block(type="RW1", rankdef=r1, Q = Qx1_FIXED, prior = prior1) +
  block(type="iid_time", rankdef=0, Q = diag(n), prior = prior1) +
  block(type="besag", rankdef=r2, Q = Qx3_FIXED, prior = prior1) +
  block(type="iid_space", rankdef=0, Q = diag(m), prior = prior1) +
  block(type="type4", rankdef=r3, prior = prior1)

res <- INLAPLUS::inla1234(formula = gf,
                          Model = myModel,
                          control_strategy = list(Strategy = "GA", vbc = FALSE),
                          Qx_type = list(type="generic_Qx"),
                          data = mydata,
                          control_opt = list(safemode = FALSE),
                          control_parallel = list(num_omp=1,num_proc=1,sarus_resource="export PATH=/opt/sarus/1.5.0-Release/bin:${PATH} &&"))



#print(res)
res = inla(formula1,data = my.data,
           verbose = TRUE,
           #num.threads = "8:6",
           control.inla=list(strategy="gaussian",
                             control.vb=list(enable=FALSE, f.enable.limit=999999),
                             h = 5E-3,use.directions = FALSE,num.hessian="central"),
           #num.hessian="central",use.directions = FALSE),
           family = "poisson",E=offset,
           control.fixed = list(prec.intercept = mtau),
           control.compute=list(dic=TRUE),
           inla.mode = "experimental")

unname(res$mode$theta)
