library(GA)

lower<-c(0.1,0.05,0.001,0.2,-.01)
upper<-c(3  ,0.3,0.08,0.3,+.01)
ga2<-ga(type="real-valued",
        fitness=function(x)+(max(Walking(x[1],x[2],x[3],x[4],x[5])$X[1,])),
          #walk.distance(x[1],x[2],x[3],x[4],x[5]),
        lower=lower,
        upper=upper,keepBest=TRUE,popSize=20,
        maxiter=100 ,monitor=TRUE)


solution<-ga2@bestSol

##generation animation
#race the first place winner by generation
sol1<-as.numeric(solution[[1]][1,1:5])
B<-Walking(sol1[1],sol1[2],sol1[3],sol1[4],sol1[5])
max(as.vector(B$X[1,]))
WalkingAnimation(file.name="walkingg1.gif",
                 B$body.plvs,B$body.sght,B$ftc,B$ftip,B$l,B$N,B$X,B$h,B$dt,B$rcd)

sol5<-as.numeric(solution[[5]][1,1:5])
B<-Walking(sol5[1],sol5[2],sol5[3],sol5[4],sol5[5])
max(as.vector(B$X[1,]))
WalkingAnimation(file.name="newwalkingg5.gif",
                 B$body.plvs,B$body.sght,B$ftc,B$ftip,B$l,B$N,B$X,B$h,B$dt,B$rcd)

sol20<-as.numeric(solution[[20]][1,1:5])
B<-Walking(sol20[1],sol20[2],sol20[3],sol20[4],sol20[5])
max(as.vector(B$X[1,]))
WalkingAnimation(file.name="newwalkingg20.gif",
                 B$body.plvs,B$body.sght,B$ftc,B$ftip,B$l,B$N,B$X,B$h,B$dt,B$rcd)
  
  sol100<-as.numeric(solution[[100]][1,1:5])
  B<-Walking(sol100[1],sol100[2],sol100[3],sol100[4],sol100[5])
  max(as.vector(B$X[1,]))
  WalkingAnimation(file.name="newwalkingg100.gif",
                   B$body.plvs,B$body.sght,B$ftc,B$ftip,B$l,B$N,B$X,B$h,B$dt,B$rcd)

