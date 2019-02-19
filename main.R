#rm(list=ls())
#load("~/최후의 날/gadata2.RData")
#sol=ga1@bestSol[[80]]
sol=c(0.9,0.15,0.02,0.23,-0.03)
##gait parameter: 보행분석 모수
T= sol[1]#gait cycle period
s=  sol[2]# gait width
H=    sol[3]#gait height
h=  sol[4]# pelvis height
bias=  sol[5]#gait x-bias

#simulation times
dt=0.01
tend=5

source("constants.R")
source("solver.R")


library(animation)
ani.options(convert = 'C:/PROGRA~1/ImageMagick-7.0.8-Q8/magick.exe ')

source("legkin.R")
LP.1<-legkin(body.plvs,ftc[1:2,],l)
LP.2<-legkin(body.plvs,ftc[3:4,],l)

  
k<-seq(1,N-1,by=5)
anm.break=0
ii<-c()
saveGIF({
  for (i in k){
    #print(c(b.t,i,ii))
    if((X[2,i+1]<h/2 || abs(X[3,i+1])>pi/2||X[1,i+1]<(-2))==FALSE){
    plot(X[1,i],X[2,i],type="o",asp=1,xlim=c(-.2,2),ylim=c(-.2,.6),xlab=c(""),ylab=c(""),lwd=1.5,cex=2)
    rect(-5, -0.5, 20, 0,col = "brown")
    #symbols(X[1,i],X[2,i],circles=1, inches=1/6,add=F, bg="blue1", fg=NULL,ylim=c(-0.2,2),asp=1)
    lines(c(body.plvs[1,i],X[1,i],body.sght[1,i]),c(body.plvs[2,i],X[2,i],body.sght[2,i]),type="l",lwd=1.5)
    lines(c(body.plvs[1,i],LP.1[1,i]),c(body.plvs[2,i],LP.1[2,i]),type="l",col=1,lwd=1.5)
    lines(c(body.plvs[1,i],LP.2[1,i]),c(body.plvs[2,i],LP.2[2,i]),type="l",col=2,lwd=1.5)
    lines(c(LP.1[1,i],ftc[1,i]),c(LP.1[2,i],ftc[2,i]),type="l",col=1,lwd=1.5)
    lines(c(LP.2[1,i],ftc[3,i]),c(LP.2[2,i],ftc[4,i]),type="l",col=2,lwd=1.5)
    lines(c(ftip[3,i],ftip[1,i]),c(ftip[4,i],ftip[2,i]),type="l",col=1,lwd=1.5) # 앞발
    lines(c(ftip[5,i],ftip[7,i]),c(ftip[6,i],ftip[8,i]),type="l",col=2,lwd=1.5) # 뒷발
    points(rcd[1,i],0,col=3,lwd=1.5,cex=2)
    clock<-as.character(formatC(c(i*dt), digits = 2, format = "f"))
    legend("topleft",legend=c(clock,"sec"))
    ii<-c(i)
    }
    else {
    #symbols(X[1,i],X[2,i],circles=1, inches=1/6,add=F, bg="blue1", fg=NULL,ylim=c(-0.2,2),asp=1)
    plot(X[1,ii],X[2,ii],type="o",asp=1,xlim=c(-.2,2),ylim=c(-.2,.6),xlab=c(""),ylab=c(""),lwd=1.5,cex=2)
    rect(-5, -0.5, 20, 0,col = "brown")
    lines(c(body.plvs[1,ii],X[1,ii],body.sght[1,ii]),c(body.plvs[2,ii],X[2,ii],body.sght[2,ii]),type="l",lwd=1.5)
    lines(c(body.plvs[1,ii],LP.1[1,ii]),c(body.plvs[2,ii],LP.1[2,ii]),type="l",col=1,lwd=1.5)
    lines(c(body.plvs[1,ii],LP.2[1,ii]),c(body.plvs[2,ii],LP.2[2,ii]),type="l",col=2,lwd=1.5)
    lines(c(LP.1[1,ii],ftc[1,ii]),c(LP.1[2,ii],ftc[2,ii]),type="l",col=1,lwd=1.5)
    lines(c(LP.2[1,ii],ftc[3,ii]),c(LP.2[2,ii],ftc[4,ii]),type="l",col=2,lwd=1.5)
    lines(c(ftip[3,ii],ftip[1,ii]),c(ftip[4,ii],ftip[2,ii]),type="l",col=1,lwd=1.5) # 앞발
    lines(c(ftip[5,ii],ftip[7,ii]),c(ftip[6,ii],ftip[8,ii]),type="l",col=2,lwd=1.5) # 뒷발
    points(rcd[1,ii],0,col=3,cex=2,lwd=1.5)
    clock<-as.character(formatC(c(i*dt), digits = 2, format = "f"))
    stopclock<-as.character(formatC(c(ii*dt), digits = 2, format = "f"))
    legend("topleft",legend=c(clock,"sec"))
    legend("topright",legend=c("stop time",stopclock,"sec"))
    anm.break=anm.break+1
    if (anm.break>10){break}
    }}
  
  }, interval = dt,  ani.width = 1000, ani.height = 500,movie.name = "walking.gif")
