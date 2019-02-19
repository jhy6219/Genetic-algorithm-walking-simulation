adjwalk.distance<-function(T,s,H,h,bias){
  a=0.4
  assign("T",T,envir = .GlobalEnv)
  assign("s",s,envir = .GlobalEnv)
  assign("H",H,envir = .GlobalEnv)
  assign("h",h,envir = .GlobalEnv)
  assign("bias",bias,envir = .GlobalEnv)
  assign("tend",5,envir = .GlobalEnv)
  assign("dt",0.01,envir = .GlobalEnv)
  source("constants.R") # 상수불러오기
  source("solver.R")
  return(a*max(body.mass[1,])+(1-a)*(-sqrt(mean(X[3,]^2)))) # 무게중심의 x값 불러오기
}


system.time(ga62<-ga(type="real-valued",fitness=function(x)+adjwalk.distance(x[1],x[2],x[3],x[4],x[5]),lower=lower,
        upper=upper,keepBest=TRUE, maxiter=30 ,monitor=TRUE))