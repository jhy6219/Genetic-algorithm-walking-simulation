#install.packages("numbers")
library(numbers)
u<-c()
pulse<-function(t,T,delay){ 
  #t:현재시간,T:gait cycle period,delay:몇초 늦게 시작하는(왼발 오랜발 시간 차이),nf=0or1)
 u=t*c(0)
         u[which((t-delay)-(T*floor((t-delay)/T))>=T/2)]=1
         #원래는 mod(t-delay,T)>=T/2 인데 T=1임으로 생략
  return(u)
}

ggen<-function(t,T,s,h,H,bias,rvrs){ #t:(0,tend까지 dt간격), T=cycle periods, h=골반, H=진폭,bias = 골반과 발목중심
  gait.mat<-matrix()
  del = rvrs*pi#왼발과 오른발이 궤적 차이
  p = pulse(t,T,T/4+T/2*rvrs)*(-1)^rvrs
  wx = 2*pi/T # x에다 쓸 각속도 (상수)
  wy = pi/s #y에다 쓸 각속도
  x = s/2*sin(wx*t +del) # x축으로의 위치
  xv= s/2*wx*cos(wx*t +del) #x축으로의 속도
  xa= (-s)/2*wx^2*sin(wx*t+del) #x축으로의 가속도
  y = H*cos(wy*x +del)*p-h #y축으로의 위치
  yv= (-H)*wy*xv*sin(wy*x + del)*p #y축으로의 속도
  ya= (-H)*(wy*xv)^2*cos(wy*x + del)-H*wy*xa*sin(wy*x + del)*p # y축으로의 가속도
  th = (0*pi)/180*matrix(rep(1,length(t)),nrow=1) # theta의 위치
  thv= t*0 # theta의 속도
  tha= t*0 # theta의 가속도
  gait.mat= matrix(c(x+bias,y,th,xv,yv,thv,xa,ya,tha),nrow=9,byrow=T)# 9행 매트릭스
  return(gait.mat)
}


#Rmat
Rmat<-function(X){
  th = X[3]
  R = matrix(c(cos(th),-sin(th),sin(th),cos(th)),byrow=T,nrow=2) # 회전행렬
  dR= matrix(c(-sin(th),-cos(th),cos(th),-sin(th)),byrow=T,nrow=2) #회전행렬 미분
  return(rbind(R,dR))
}



