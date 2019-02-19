#robot parameters
m=1 #mass(무게)
I=0.02 # mass moment of inertia(질량관성모멘트)
g=9.81 #gravitational acceleration(중력 가속도)

sight=0.05# sight direction indicator
hw=0.1 # waist hight
l=c(0.19,0.15)#leg length
lf=c(0.07,-0.03)# foot size

#simulation parameter&matrix
t=seq(0,tend,by=dt)
N=length(t)
Om=matrix(rep(0,9),nrow=3) # 3*3 zero matrix
Im=matrix(c(1,0,0,0,1,0,0,0,1),byrow=T,nrow=3)# 3*3 unit matrix
Amat=rbind(cbind(Om,Im),cbind(Om,Om))
Bmat=rbind(Om,Im*c(1/m, 1/m, 1/I))
X=matrix(rep(0,6*N),nrow=6)
X[2,1]<-h+hw # initial y
Fg=matrix(rep(0,3*N),nrow=3) # gravitational force
F=matrix(rep(0,3*N),nrow=3) # other force
zm=matrix(rep(0,2*N),nrow=2) # 2*N zero matrix

ftip=rbind(zm,zm,zm,zm)#foot tip point/ 8*N matrix
ftc=rbind(zm,zm) # foot center point
body.mass=zm # center of mass
body.sght=zm # for sight drawing
body.plvs=zm #pelvis point
rcd=matrix(rep(0,4*N),nrow=4)#앞,뒷발앞꿈치,뒷꿈치중에 어디가 닿았는지




