source("ggen_pulse.R")

gait<-list()
rvrs=0 #µÞ¹ß
gait[[-rvrs+2]]<-ggen(t,T,s,h+hw,H,bias,rvrs=0)
rvrs=1 #¾Õ¹ß
gait[[c(-rvrs+2)]]<-ggen(t,T,s,h+hw,H,bias,rvrs=1)
source("rk.R")

ftx=0
ftch0=c(0,0,0,0)
info1=c(0,0,0,0)
for (i in 1:(N-1)){
  source("position calculator.R")
  source("ground touch.R")
  X[,i+1]=RK(X[,i],F[,i]+Fg[,i],F[,i]+Fg[,i],dt,Amat,Bmat)
  if (X[2,i+1]<h/2 || abs(X[3,i+1])>pi/4||X[1,i+1]<(-2))
  {break}
}