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
  return(-max(body.mass[1,])) # 무게중심의 x값 불러오기  # 최솟값을 불러주는 것으로 return에 음수를 넣어주는게 핵심 
}
lower<-c(0.1,0.05,0.001,0.2,-.01)
upper<-c(4  ,0.3,0.08,0.3,+.01)
system.time(gensa1<-GenSA(lower=lower,upper=upper,fn=function(x){walk.distance2(x[1],x[2],x[3],x[4],x[5])},control=list(verbose=TRUE)))
print(gensa1[c("value","par","counts")])
system.time(optim1<-optim(par=c(2,0.1,0.03,0.15,-0.01),fn=function(x){walk.distance2(x[1],x[2],x[3],x[4],x[5])},lower=lower,upper=upper,method="L-BFGS-B"))