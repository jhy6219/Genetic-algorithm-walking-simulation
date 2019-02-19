
R=Rmat(X[,i])[c(1,2),]
dR=Rmat(X[,i])[c(3,4),]

body.mass[,i]=X[c(1,2),i]#¹«°ÔÁß½É
body.sght[,i]=X[c(1,2),i]+R%*%matrix(c(sight,0),nrow=2)# ·Îº¿ÀÇ½Ã¼±
body.plvs[,i]=X[c(1,2),i]+R%*%matrix(c(0,-hw),nrow=2)#·Îº¿ÀÇ °ñ¹Ý

 
ftc[c(1,2),i] = X[c(1,2),i]+R%*%gait[[1]][c(1,2),i] # ¾Õ¹ß FOOT CENTER POINT
ftc[c(3,4),i] = X[c(1,2),i]+R%*%gait[[2]][c(1,2),i] # µÞ¹ß FOOT CENTER POINT

ftip[c(1,2),i]=ftc[c(1,2),i]+R%*%matrix(c(lf[1],0),nrow=2)# ¾Õ¹ß ¾Õ²ÞÄ¡
ftip[c(3,4),i]=ftc[c(1,2),i]+R%*%matrix(c(lf[2],0),nrow=2)# ¾Õ¹ß µÞ²ÞÄ¡
ftip[c(5,6),i]=ftc[c(3,4),i]+R%*%matrix(c(lf[1],0),nrow=2)# µÞ¹ß ¾Õ²ÞÄ¡
ftip[c(7,8),i]=ftc[c(3,4),i]+R%*%matrix(c(lf[2],0),nrow=2)# µÞ¹ß µÞ²ÞÄ¡