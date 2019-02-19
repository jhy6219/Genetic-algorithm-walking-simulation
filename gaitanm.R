T=2#sol[1]#gait cycle period
s= 0.2# sol[2]# gait width
H=  0.05#  sol[3]#gait height
h=  0.25 #sol[4]# pelvis height
bias= -0.02# sol[5]#gait x-bias

#simulation times
dt=0.01
tend=2

source("constants.R")
X[2,1]<-h+hw+0.1
source("solver.R")

library(animation)
ani.options(convert = 'C:/PROGRA~1/ImageMagick-7.0.8-Q8/magick.exe ')

source("legkin.R")
LP.1<-legkin(body.plvs,ftc[1:2,],l)
LP.2<-legkin(body.plvs,ftc[3:4,],l)

k<-seq(1,N-1,by=5)
saveGIF({
  for (i in k){
      plot(X[1,i],X[2,i],type="o",asp=1,xlim=c(-.2,.2),ylim=c(0,.6),xlab=c(""),ylab=c(""),lwd=1.5,cex=2)
      
      #symbols(X[1,i],X[2,i],circles=1, inches=1/6,add=F, bg="blue1", fg=NULL,ylim=c(-0.2,2),asp=1)
      lines(c(body.plvs[1,i],X[1,i],body.sght[1,i]),c(body.plvs[2,i],X[2,i],body.sght[2,i]),type="l",lwd=1.5)
      lines(c(body.plvs[1,i],LP.1[1,i]),c(body.plvs[2,i],LP.1[2,i]),type="l",col=1,lwd=1.5)
      lines(c(body.plvs[1,i],LP.2[1,i]),c(body.plvs[2,i],LP.2[2,i]),type="l",col=2,lwd=1.5)
      lines(c(LP.1[1,i],ftc[1,i]),c(LP.1[2,i],ftc[2,i]),type="l",col=1,lwd=1.5)
      lines(c(LP.2[1,i],ftc[3,i]),c(LP.2[2,i],ftc[4,i]),type="l",col=2,lwd=1.5)
      lines(c(ftip[3,i],ftip[1,i]),c(ftip[4,i],ftip[2,i]),type="l",col=1,lwd=1.5) # ¾Õ¹ß
      lines(c(ftip[5,i],ftip[7,i]),c(ftip[6,i],ftip[8,i]),type="l",col=2,lwd=1.5) # µÞ¹ß
      }
}, interval = dt,  ani.width = 500, ani.height = 500,movie.name = "gait.gif")