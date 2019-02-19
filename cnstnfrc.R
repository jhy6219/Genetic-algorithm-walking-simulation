##constriant and force

#constraint
switch ( as.character(cmod), # cmod: 몇개 닿았냐
         "1"={ #onepoint constraints
           if (identical(ftch0,ftch)==FALSE) # 업데이트 할 필요가 있는지 체크한다.
            {ftx=ftip[(ftnum-1)*4+2*(tpnum-1)+1,i] #ftx 꿈치 좌표 , ftnum과 tpnum은 언제 0이하로 떨어지는지 체크
           }
           #position constraints(위치)
         X[1:2,i]=X[1:2,i]-ftip[c( (ftnum-1)*4+2*(tpnum-1)+1, (ftnum-1)*4+2*(tpnum-1)+2 ),i]+ matrix(c(ftx,0),nrow=2,byrow=T) #Or-(Or+R(G+lb))+[ftx;0]
          #velocity constraints(속도)
         X[4:5,i]=-R%*%gait[[ftnum]][4:5,i]-dR%*%gait[[ftnum]][1:2,i]*X[6,i] # *X[6,i] 는 세타 속도
         },#미분
         "2"={ #같은발 two point constraints
           #if its new type of touch, update ftx
           if (identical(ftch0,ftch)==FALSE){ # if its new type of touch update fix
             ftx=ftc[(ftnum-1)*2+1,i]} # 땅이랑 바닥에 닿은 발목 알고자 함 
           #position constraints
           X[c(1,2),i]=X[c(1,2),i]-ftc[c((ftnum-1)*2+1,(ftnum-1)*2+2),i]+matrix(c(ftx,0),nrow=2,byrow=T)
           X[3,i]=0# 발이랑 시야는 평행
           #velocity constraints
           X[4:5,i]=-R%*%gait[[ftnum]][4:5,i]-dR%*%gait[[ftnum]][1:2,i]*X[6,i]
         }
)
##forces
F[1:2,i]=-m*R%*%gait[[ftnum]][7:8,i]
RG=body.mass[,i]-ftc[c((ftnum-1)*2+1,(ftnum-1)*2+2),i]#ankle to mass
zmp=RG[1]-F[1,i]/(F[2,i]+m*g)*RG[2]+ftc[(ftnum-1)*2+1,i] # ZMP

info1[2:4]<-c(0,0,1) # zmp color on animation

switch(as.character(cmod),
       "1"={#one point case
         d=body.mass[1:2,i]-ftip[c((ftnum-1)*4+2*(tpnum-1)+1,(ftnum-1)*4+2*(tpnum-1)+2),i]
         },
      "2"={ # two point case
        if(zmp<min(tmp))
        {d=body.mass[1:2,i]-ftc[c((ftnum-1)*2+1,(ftnum-1)*2+2),i] # 한발이 닿았을땐 꿈치를 두발이 닿았을때는 발목을
        }
        else if (max(tmp)<zmp)
        { d=body.mass[1:2,i]-ftc[c((ftnum-1)*2+1,(ftnum-1)*2+2),i]
        }
        else {d=matrix(c(0,0),nrow=2)
        info1[2:4]=c(0,1,0) #zmp color on animation
        }
      }
)
##total torque
F[3,i]=d[1]*(F[2,i]-m*g)-d[2]*F[1,i]
info1[1]=zmp # zmp on animation

         
  
  
