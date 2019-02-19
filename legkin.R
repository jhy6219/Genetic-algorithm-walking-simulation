legkin<-function(P,Foot,l){ #P: 골반 ,Foot:발의 중심 # Leg kinematic 함수
  N=length(P[1,])
  th=c()
  LP=matrix(rep(0,2*N),nrow=2)
    l1=l[1]
    l2=l[2]
    #P<-as.matrix(P)
   # Foot<-as.matrix(Foot)
    for (i in 1:N){
    D=(Foot[,i])-(P[,i]) # 발복좌표-골반좌표
    d=sqrt(D[1]^2+D[2]^2) # 발목과 골반사이의 거리
    a = atan2(D[2],D[1])#c+theta
    b = acos((l1^2 + l2^2 - d^2)/(2*l1*l2))#l1,l2의 각도
    c = acos((l1^2 + d^2 - l2^2)/(2*l1*d))#l1,d의 각도
    th[i] = c(a+c) # a는 -각도고 c는 +각도라서 a+c함
    #LP[[i]][[,c(1:3)]] = matrix(rep(0,6),nrow=2)
    #LP[[i]][[,1]] = matrix(P[1,i],P[2,i],nrow=2) # 골반위치
    #print(LP[,i])
    LP[,i]= matrix(c(l1*cos(th[i])+P[1,i], l1*sin(th[i])+P[2,i]),nrow=2) # 무릎위치
    #LP[[i]][[,3]]= matrix(l2*cos(th[2,i]+th[1,i]), l2*sin(th[2,i]+th[1,i]),nrow=2) +LP[[i]][[,2]]#발 위치

  }
    return(LP)
}

