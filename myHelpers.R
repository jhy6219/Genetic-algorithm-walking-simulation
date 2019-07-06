#function to find coordinate of knee

legkin<-function(P,Foot,l){ #P: pelvis, Foot: left(ftc[1:2,]) or right(ftc[3:4,])
                            #I: leg length
  N=length(P[1,])
  th=c()#angle of sight
  LP=matrix(rep(0,2*N),nrow=2)#leg coordinate matrix
  l1=l[1] # upper leg
  l2=l[2] # lower leg
  for (i in 1:N){
    D=(Foot[,i])-(P[,i]) # center of mass - pelvis coordinate
    d=sqrt(D[1]^2+D[2]^2) # distance
    a = atan2(D[2],D[1]) # c+theta
    b = acos((l1^2 + l2^2 - d^2)/(2*l1*l2)) # angle of upper and lower leg
    c = acos((l1^2 + d^2 - l2^2)/(2*l1*d)) # angle of upper leg and d
    th[i] = c(a+c) #a is negative, so we use +
    LP[,i]= matrix(c(l1*cos(th[i])+P[1,i], #coordinate of knee in i iteration
                     l1*sin(th[i])+P[2,i]),nrow=2)
  }
  return(LP)
}

# foot can not dig the ground. To create a semi-circular trajectory, I made pulse function.
# A matrix consisting of binary methods is derived.

pulse<-function(t,T,delay){ # t: t(th) iteration, T:gait cycle period
                            # time difference between left and right foot
  u=t*c(0)
  u[which((t-delay)-(T*floor((t-delay)/T))>=T/2)]=1 # (mod(t-delay,T)>=T/2)
  return(u)
}

ggen<-function(t,T,s,h,H,bias,rvrs){ #The first five parameters are gait parameters.
                                     # rvrs=0(right foot),rvrs=1(left foot)
  gait.mat<-matrix()
  del = rvrs*pi
  p = pulse(t,T,T*(5/4)+T/2*rvrs)*(-1)^rvrs #I put 5/4, if i put 1/4, right foot starts points is not in the air. 
                                            #(-1)^rvrs= -1(right foot),+1(left foot)
  wx = 2*pi/T #velocity of x-angle
  wy = pi/s  #velocity of y-angle
  x = s/2*sin(wx*t +del) # x position
  xv= s/2*wx*cos(wx*t +del) # x velocity
  xa= (-s)/2*wx^2*sin(wx*t+del) # x acceralation
  y = H*cos(wy*x +del)*p-h # y position
  yv= (-H)*wy*xv*sin(wy*x + del)*p # y velocity
  ya=(-H)*(wy*xv)^2*cos(wy*x + del)-H*wy*xa*sin(wy*x + del)*p # y acceralation
  th = (0*pi)/180*matrix(rep(1,length(t)),nrow=1) #angle
  thv= t*0 #angle velocity
  tha= t*0 #angle acceralation
  gait.mat=matrix(c(x+bias,y,th,xv,yv,thv,xa,ya,tha),nrow=9,byrow=T)
  return(gait.mat)
}

# rotational matrix
# Rmat
Rmat<-function(X){
  th = X[3]
  R = matrix(c(cos(th),-sin(th),sin(th),cos(th)),byrow=T,nrow=2) #rotational matrix
  dR= matrix(c(-sin(th),-cos(th),cos(th),-sin(th)),byrow=T,nrow=2)#differentiating rotational matrix 
                                  # to find velocity of angle
  return(rbind(R,dR))
}
#3rd rungekutta method: kind of numerical integral
RK<-function(X0,F0,F1,h,A,B){
  k1<-A%*%X0+B%*%F0 #f()
  k2<-A%*%(X0+(h/2)*k1)+B%*%(F0+F1)/2
  k3<-A%*%(X0-k1*h+2*k2*h)+B%*%F1
  X1<-X0+(h/6)*(k1+4*k2+k3)
  return(X1)
}

#making walking simulation animation using animation packages
WalkingAnimation<-function(file.name="walking.gif",
                           body.plvs,body.sght,ftc,ftip,l,N,X,h,dt,rcd){
  
  LP.1<-legkin(body.plvs,ftc[1:2,],l)#left knee coordinate
  LP.2<-legkin(body.plvs,ftc[3:4,],l)#right knee coordinate
  
  # I repeat the algorithm 500 times, but since 500 images are too many,
  # I actually attach them one at every 5 times.
  k<-seq(1,N+100-1,by = 5) 
  anm.break = 0
  ii<-c()# to use if robot falls down
  saveGIF({
    for (i in k){
      if(i>500){i=499}# after 5 second stop for 1 second.
      # if robot falls
      if((X[2,i+1]<h/2 || abs(X[3,i+1])>pi/2||X[1,i+1]<(-2)) == FALSE){
        plot(X[1,i],X[2,i],type = "o",asp = 1,xlim = c(-.1,2.7),
             ylim = c(-.2,.6),xlab = c(""),ylab = c(""),lwd = 1.5,cex = 2)#head(center of mass)
        rect(-5, -0.5, 20, 0,col  =  "brown")#ground
        lines(c(body.plvs[1,i],X[1,i],body.sght[1,i]),
              c(body.plvs[2,i],X[2,i],body.sght[2,i]),type = "l",lwd = 1.5)
        lines(c(body.plvs[1,i],LP.1[1,i]),c(body.plvs[2,i],LP.1[2,i]),
              type = "l",col = 1,lwd = 1.5)
        lines(c(body.plvs[1,i],LP.2[1,i]),c(body.plvs[2,i],LP.2[2,i]),
              type = "l",col = 2,lwd = 1.5)
        lines(c(LP.1[1,i],ftc[1,i]),c(LP.1[2,i],ftc[2,i]),
              type = "l",col = 1,lwd = 1.5)
        lines(c(LP.2[1,i],ftc[3,i]),c(LP.2[2,i],ftc[4,i]),
              type = "l",col = 2,lwd = 1.5)
        lines(c(ftip[3,i],ftip[1,i]),c(ftip[4,i],ftip[2,i]),
              type = "l",col = 1,lwd = 1.5) 
        lines(c(ftip[5,i],ftip[7,i]),c(ftip[6,i],ftip[8,i]),
              type = "l",col = 2,lwd = 1.5) 
        points(rcd[1,i],0,col = 3,lwd = 1.5,cex = 2)
        if(i>498){i=500} # Stop after 5 seconds.
        clock<-as.character(formatC(c(i*dt), digits  =  2, format  =  "f"))
        legend("topleft",legend = c(clock,"sec"))
        ii<-c(i)
      }
      else{ #If the robot falls, it keeps stopped in the applicable state.
        plot(X[1,ii],X[2,ii],type = "o",asp = 1,xlim = c(-.1,2.7),
             ylim = c(-.2,.6),xlab = c(""),ylab = c(""),lwd = 1.5,cex = 2)
        rect(-5, -0.5, 20, 0,col  =  "brown")
        lines(c(body.plvs[1,ii],X[1,ii],body.sght[1,ii]),
              c(body.plvs[2,ii],X[2,ii],body.sght[2,ii]),type = "l",lwd = 1.5)
        lines(c(body.plvs[1,ii],LP.1[1,ii]),c(body.plvs[2,ii],LP.1[2,ii]),
              type = "l",col = 1,lwd = 1.5)
        lines(c(body.plvs[1,ii],LP.2[1,ii]),c(body.plvs[2,ii],LP.2[2,ii]),
              type = "l",col = 2,lwd = 1.5)
        lines(c(LP.1[1,ii],ftc[1,ii]),c(LP.1[2,ii],ftc[2,ii]),type = "l",
              col = 1,lwd = 1.5)
        lines(c(LP.2[1,ii],ftc[3,ii]),c(LP.2[2,ii],ftc[4,ii]),type = "l",
              col = 2,lwd = 1.5)
        lines(c(ftip[3,ii],ftip[1,ii]),c(ftip[4,ii],ftip[2,ii]),type = "l",
              col = 1,lwd = 1.5) 
        lines(c(ftip[5,ii],ftip[7,ii]),c(ftip[6,ii],ftip[8,ii]),type = "l",
              col = 2,lwd = 1.5) 
        points(rcd[1,ii],0,col = 3,cex = 2,lwd = 1.5)
        clock<-as.character(formatC(c(i*dt), digits  =  2, format  =  "f"))
        stopclock<-as.character(formatC(c(ii*dt), digits  =  2, format  =  "f"))
        legend("topleft",legend = c(clock,"sec"))
        legend("topright",legend = c("stop time",stopclock,"sec"))#stop time
        anm.break = anm.break+1
        #if(anm.break>10){break} # if robots falls, just stopped for just 9 iterations.
      }}
  }, interval  =  dt,  ani.width  =  1100, ani.height  =  500,
  movie.name  =  file.name)
}

# if you input the parameters and setting of robots, this function show the coordinate of 
# all body parts over time. 
Walking<-function(T,s,H,h,bias,dt=0.01,tend=5,m=1,I=0.02,
                  sight=0.05,hw=0.1,
                  Upper.leg=0.19,Lower.leg=0.15,
                  foot.size=0.1,foot.ratio=0.7){
  g<-9.80665 #gravitational acceleration
  l<-c(Upper.leg,Lower.leg) # setting of robots
  lf<-c(foot.ratio,foot.ratio-1)*foot.size
  
  #simulation parameter&matrix
  t = seq(0,tend,by = dt)
  N = length(t)
  Om = diag(rep(0,3)) # 3*3 zero matrix 
  Im = diag(rep(1,3)) # 3*3 unit matrix
  Amat = rbind(cbind(Om,Im),cbind(Om,Om))
  Bmat = rbind(Om,Im*c(1/m, 1/m, 1/I))
  X = matrix(rep(0,6*N),nrow = 6)
  X[2,1]<-h+hw # initial y
  Fg = matrix(rep(0,3*N),nrow = 3) # gravitational force
  F = matrix(rep(0,3*N),nrow = 3) # other force
  zm = matrix(rep(0,2*N),nrow = 2) # 2*N zero matrix
  
  ftip = rbind(zm,zm,zm,zm)#foot tip point/ 8*N matrix
  ftc = rbind(zm,zm) # foot center point
  body.mass = zm # center of mass
  body.sght = zm # for sight drawing
  body.plvs = zm #pelvis point
  rcd = matrix(rep(0,4*N),nrow = 4)#to record which points touched
  
  gait<-list()
  rvrs=0 #right foot
  gait[[-rvrs+2]]<-ggen(t,T,s,h+hw,H,bias,rvrs=0)
  rvrs=1 #left foot
  gait[[c(-rvrs+2)]]<-ggen(t,T,s,h+hw,H,bias,rvrs=1)
  
  ftx=0
  ftch0=c(0,0,0,0)
  info1=c(0,0,0,0)
  for (i in 1:(N-1)){
    s
    R=Rmat(X[,i])[c(1,2),]
    dR=Rmat(X[,i])[c(3,4),]
    
    body.mass[,i]=X[c(1,2),i]#center of mass
    body.sght[,i]=X[c(1,2),i]+R%*%matrix(c(sight,0),nrow=2)#view (according to angle of sight)
    body.plvs[,i]=X[c(1,2),i]+R%*%matrix(c(0,-hw),nrow=2)#pelvis
    
    ftc[c(1,2),i] = X[c(1,2),i]+R%*%gait[[1]][c(1,2),i] #left foot center point
    ftc[c(3,4),i] = X[c(1,2),i]+R%*%gait[[2]][c(1,2),i] #right foot center point
    
    ftip[c(1,2),i]=ftc[c(1,2),i]+R%*%matrix(c(lf[1],0),nrow=2) #left foot toe
    ftip[c(3,4),i]=ftc[c(1,2),i]+R%*%matrix(c(lf[2],0),nrow=2) #left foot heel
    ftip[c(5,6),i]=ftc[c(3,4),i]+R%*%matrix(c(lf[1],0),nrow=2) #right foot toe
    ftip[c(7,8),i]=ftc[c(3,4),i]+R%*%matrix(c(lf[2],0),nrow=2) #right foot heel
    
    ##array of foot position(4 * N matrix, positions of each 4 point)
    ftpos=cbind(ftip[c(1,2),i],ftip[c(3,4),i],ftip[c(5,6),i],ftip[c(7,8),i]) 
    
    ftch=(ftpos[2,]<=0) # set untouched zero(foot check)
    
    ftpos=ftpos%*%(diag(4)*ftch) # get touched ones only(2*4 matrix)
    tmp=ftpos[1,which(ftpos[1,]!=0)] # get non-zero ones only(only x value) 
    
    switch(as.character(sum(ftch)),
           "0" = {Fg[2,i]=-m*g;ftnum=0;tpnum=0}, # if foot don't touch the ground
           "1" = {  # 1 point touched
             cmod=1 #set 1 point touched
             switch(as.character(which(ftch==1)),
                    "1"={ftnum=1 #left foot toe touched
                    tpnum=1 
                    Temp<-cnstnfrc(cmod,ftch0,ftch,ftip,ftnum,tpnum,ftx,
                                   X,R,gait,dR,ftc,F,m,body.mass, 
                                   info1,i,g,tmp)
                    ftx=Temp$ftx
                    X=Temp$X
                    F=Temp$F
                    RG=Temp$RG
                    zmp=Temp$zmp
                    info1=Temp$info1},
                    "2"={ftnum=1 #left foot heel touched
                    tpnum=2 
                    Temp<-cnstnfrc(cmod,ftch0,ftch,ftip,ftnum,tpnum,ftx,
                                   X,R,gait,dR,ftc,F,m,body.mass, 
                                   info1,i,g,tmp)
                    ftx=Temp$ftx
                    X=Temp$X
                    F=Temp$F
                    RG=Temp$RG
                    zmp=Temp$zmp
                    info1=Temp$info1 },
                    "3"={ftnum=2 #right foot toe touched
                    tpnum=1 
                    Temp<-cnstnfrc(cmod,ftch0,ftch,ftip,ftnum,tpnum,ftx,
                                   X,R,gait,dR,ftc,F,m,body.mass, 
                                   info1,i,g,tmp)
                    ftx=Temp$ftx
                    X=Temp$X
                    F=Temp$F
                    RG=Temp$RG
                    zmp=Temp$zmp
                    info1=Temp$info1 },
                    "4"={ftnum=2 #right foot heel touched
                    tpnum=2 
                    Temp<-cnstnfrc(cmod,ftch0,ftch,ftip,ftnum,tpnum,ftx,
                                   X,R,gait,dR,ftc,F,m,body.mass, 
                                   info1,i,g,tmp)
                    ftx=Temp$ftx
                    X=Temp$X
                    F=Temp$F
                    RG=Temp$RG
                    zmp=Temp$zmp
                    info1=Temp$info1})},
           "4" = {cmod=2 # 4 points touched
           #assumed 2 points of same foot have high x-value touched
           if(ftc[1,i]>ftc[3,i]){
             ftnum=1 ;tpnum=0
             Temp<-cnstnfrc(cmod,ftch0,ftch,ftip,ftnum,tpnum,ftx,
                            X,R,gait,dR,ftc,F,m,body.mass, 
                            info1,i,g,tmp)
             ftx=Temp$ftx
             X=Temp$X
             F=Temp$F
             RG=Temp$RG
             zmp=Temp$zmp
             info1=Temp$info1 
           }else{ 
             ftnum=2;tpnum=0
             Temp<-cnstnfrc(cmod,ftch0,ftch,ftip,ftnum,tpnum,ftx,
                            X,R,gait,dR,ftc,F,m,body.mass, 
                            info1,i,g,tmp)
             ftx=Temp$ftx
             X=Temp$X
             F=Temp$F
             RG=Temp$RG
             zmp=Temp$zmp
             info1=Temp$info1}},
           #other wise 2 points of 3 points touched
           {cmod=2
           if(sum(ftch[1:2])==2){ #left foot's toe,heel touched
             ftnum=1;tpnum=0
             Temp<-cnstnfrc(cmod,ftch0,ftch,ftip,ftnum,tpnum,ftx,
                            X,R,gait,dR,ftc,F,m,body.mass, 
                            info1,i,g,tmp)
             ftx=Temp$ftx
             X=Temp$X
             F=Temp$F
             RG=Temp$RG
             zmp=Temp$zmp
             info1=Temp$info1
           }else if(sum(ftch[3:4])==2){ #right foot's toe,heel touched
             ftnum=2;tpnum=0
             Temp<-cnstnfrc(cmod,ftch0,ftch,ftip,ftnum,tpnum,ftx,
                            X,R,gait,dR,ftc,F,m,body.mass, 
                            info1,i,g,tmp)
             ftx=Temp$ftx
             X=Temp$X
             F=Temp$F
             RG=Temp$RG
             zmp=Temp$zmp
             info1=Temp$info1
           }else{ # two points touched by different feet
             cmod=1
             pp=which(ftpos[1,]==max(tmp)) #find point who has high x-value
             ftnum=1+as.numeric(pp>2.5) #Which foot has that point?
             tpnum=2-mod(pp,2) #heel or toe?
             Temp<-cnstnfrc(cmod,ftch0,ftch,ftip,ftnum,tpnum,ftx,
                            X,R,gait,dR,ftc,F,m,body.mass, 
                            info1,i,g,tmp)
             ftx=Temp$ftx
             X=Temp$X
             F=Temp$F
             RG=Temp$RG
             zmp=Temp$zmp
             info1=Temp$info1}})
    
    ftch0=ftch # old ftpos (check which points touched)
    rcd[,i]=info1 # ftx for animation(foot touch location) 
    
    X[,i+1]=RK(X[,i],F[,i]+Fg[,i],F[,i]+Fg[,i],dt,Amat,Bmat)
    # if robot leans heavily, assume that it fall and stopped the simulation
    if(X[2,i+1]<h/2 || abs(X[3,i+1])>pi/4||X[1,i+1]<(-2)){ 
      break}
  }
  return(list(body.plvs=body.plvs,
              body.sght=body.sght,
              ftc=ftc,
              ftip=ftip,
              l=l,N=N,X=X,h=h,dt=dt,rcd=rcd))
}  

# if foot point digs the ground, reposition the center of mass and velocity
# by differentiating repositioned center of mass

#this fucntion solve the force according the cases that robot can have
#and constraint the robot position and velocity.
cnstnfrc<-function(cmod,ftch0,ftch,ftip,ftnum,tpnum,ftx,
                   X,R,gait,dR,ftc,F,m,body.mass, info1,i,g,tmp,...){
  #constraint
  switch ( as.character(cmod), 
           "1"={if(identical(ftch0,ftch)==FALSE) #1 point touched
             ftx=ftip[(ftnum-1)*4+2*(tpnum-1)+1,i] 
           #position constraint
           X[1:2,i]=X[1:2,i]-ftip[c( (ftnum-1)*4+2*(tpnum-1)+1, 
                                     (ftnum-1)*4+2*(tpnum-1)+2 ),i]+ 
             matrix(c(ftx,0),nrow=2,byrow=T) 
           #velocity constraint
           X[4:5,i]=-R%*%gait[[ftnum]][4:5,i]-
             dR%*%gait[[ftnum]][1:2,i]*X[6,i]}, 
           "2"={if(identical(ftch0,ftch)==FALSE) #2 points touched
             ftx=ftc[(ftnum-1)*2+1,i]
           #position constraint
           X[c(1,2),i]=X[c(1,2),i]-ftc[c((ftnum-1)*2+1,(ftnum-1)*2+2),i]+
             matrix(c(ftx,0),nrow=2,byrow=T)
           X[3,i]=0
           #velocity constraint
           X[4:5,i]=-R%*%gait[[ftnum]][4:5,i]-
             dR%*%gait[[ftnum]][1:2,i]*X[6,i]})
  
  #if two points touched in same foot, normal force are not exactly same
  #so, for the convenience of physical calculation we will use ZMP
  ##forces
  F[1:2,i]=-m*R%*%gait[[ftnum]][7:8,i]
  RG=body.mass[,i]-ftc[c((ftnum-1)*2+1,(ftnum-1)*2+2),i]#ankle to mass
  zmp=RG[1]-F[1,i]/(F[2,i]+m*g)*RG[2]+ftc[(ftnum-1)*2+1,i] #Zero Moment Point
  
  info1[2:4]<-c(0,0,1)  # you can remove it 
  
  #solve the force according to the cases that robot can have
  switch(as.character(cmod),
         "1"={d=body.mass[1:2,i]-ftip[c((ftnum-1)*4+2*(tpnum-1)+1,
                                        (ftnum-1)*4+2*(tpnum-1)+2),i]},
         "2"={if(zmp<min(tmp)){
           d=body.mass[1:2,i]-ftc[c((ftnum-1)*2+1,(ftnum-1)*2+2),i] 
         }else if(max(tmp)<zmp){ 
           d=body.mass[1:2,i]-ftc[c((ftnum-1)*2+1,(ftnum-1)*2+2),i]
         }else{
           d=matrix(c(0,0),nrow=2)
           info1[2:4]=c(0,1,0)}})
  ##total torque
  F[3,i]=d[1]*(F[2,i]-m*g)-d[2]*F[1,i]
  info1[1]=zmp # zmp for animation
  return(list(ftx=ftx,X=X,F=F,RG=RG,zmp=zmp,info1=info1))
}






