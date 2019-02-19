RK<-function(X0,F0,F1,h,A,B){
  k1<-A%*%X0+B%*%F0
  k2<-A%*%(X0+(h/2)*k1)+B%*%(F0+F1)/2
  k3<-A%*%(X0-k1*h+2*k2*h)+B%*%F1
  X1<-X0+(h/6)*(k1+4*k2+k3)
  return(X1)
}