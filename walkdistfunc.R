walk.distance<-function(T,s,H,h,bias){
  assign("T",T,envir = .GlobalEnv)
  assign("s",s,envir = .GlobalEnv)
  assign("H",H,envir = .GlobalEnv)
  assign("h",h,envir = .GlobalEnv)
  assign("bias",bias,envir = .GlobalEnv)
  assign("tend",5,envir = .GlobalEnv)
  assign("dt",0.01,envir = .GlobalEnv)
  source("constants.R") # 상수불러오기
  source("solver.R")
  return(max(body.mass[1,])) # 무게중심의 x값 불러오기
}

system.time(walk.distance(2, 0.05, 0.03 ,0.8 ,-0.02))


library(GA)
lower<-c(0.1,0.05,0.001,0.2,-.01)
upper<-c(4  ,0.3,0.08,0.3,+.01)
ga1<-ga(type="real-valued",fitness=function(x)+walk.distance(x[1],x[2],x[3],x[4],x[5]),lower=lower,
        upper=upper,keepBest=TRUE, maxiter=10 ,monitor=TRUE)

library(GenSA)
walk.distance2<-function(T,s,H,h,bias){
  assign("T",T,envir = .GlobalEnv)
  assign("s",s,envir = .GlobalEnv)
  assign("H",H,envir = .GlobalEnv)
  assign("h",h,envir = .GlobalEnv)
  assign("bias",bias,envir = .GlobalEnv)
  assign("tend",5,envir = .GlobalEnv)
  assign("dt",0.01,envir = .GlobalEnv)
  source("constants.R") # 상수불러오기
  source("solver.R")
  return(-max(body.mass[1,])) # 무게중심의 x값 불러오기
}
system.time(gensa1<-GenSA(lower=lower,upper=upper,fn=function(x){walk.distance2(x[1],x[2],x[3],x[4],x[5])},control=list(verbose=TRUE,max.time=10)))

print(gensa1[c("value","par","counts")])

optim
