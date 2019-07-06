library(numbers)
library(animation)
library(GA)

source("myHelpers.R")

A<-Walking(T=2,s=0.1,H=0.02,h=0.25,bias=-0.02)
max(as.vector(A$X[1,]))

WalkingAnimation(file.name="walkingA1.gif",
                 A$body.plvs,A$body.sght,A$ftc,A$ftip,A$l,A$N,A$X,A$h,A$dt,A$rcd)

B<-Walking(T=1.220259,s=0.2829116,H=0.04778423,h=0.282916,bias=-0.006126497)

WalkingAnimation(file.name="walkingB.gif",
                 B$body.plvs,B$body.sght,B$ftc,B$ftip,B$l,
                 B$N,B$X,B$h,B$dt,B$rcd)

