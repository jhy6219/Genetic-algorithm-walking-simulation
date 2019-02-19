##array of foot position
ftpos=cbind(ftip[c(1,2),i],ftip[c(3,4),i],ftip[c(5,6),i],ftip[c(7,8),i]) # 4*N matrix 각 꿈치의 포지션
ftch=(ftpos[2,]<=0) # set untouched zero(foot check)

ftpos=ftpos%*%(diag(4)*ftch) # get touched ones only
tmp=ftpos[1,which(ftpos[1,]!=0)] # get non-zero ones only (x값 위치)

switch(as.character(sum(ftch)),
       "0" = {Fg[2,i]=-m*g;ftnum=0;tpnum=0},
       "1" = { #one point(꿈치) touch
        cmod=1 
        switch(as.character(which(ftch==1)),
              "1"={
                ftnum=1 #foot number=1 앞발
                tpnum=1 #앞발 앞꿈치
                source("cnstnfrc.R")
              },
              "2"={
                ftnum=1 #foot number=1 앞발
                tpnum=2 #앞발 뒷꿈치
                source("cnstnfrc.R")
              },
              "3"={
                ftnum=2 #foot number=2 뒷발
                tpnum=1 #뒷발 앞꿈치
                source("cnstnfrc.R")
              },
              "4"={
                ftnum=2 #foot number=2 뒷발
                tpnum=2 #뒷발 뒷꿈치
                source("cnstnfrc.R")
              })
       },
       "4"={
         cmod=2
         if (ftc[1,i]>ftc[3,i]){
           ftnum=1 ;tpnum=0
           source("cnstnfrc.R")
         }  # 앞발이 더 x값이 클때는 앞발만 두점 닿았다고 가정한다.
         else{ ftnum=2;tpnum=0
         source("cnstnfrc.R")} # 뒷발의 x값이 더 큰 경우
       },
       {# otherwise (two point or three  point touch)
         cmod=2
         if (sum(ftch[1:2])==2) # 앞발의 두 포인트가 닿았을때
         {ftnum=1;tpnum=0
         source("cnstnfrc.R")}
         else if (sum(ftch[3:4])==2)# 뒷발의 두 포인트가 닿았을때
         {ftnum=2;tpnum=0
         source("cnstnfrc.R")}
         else # both each
         {cmod=1
         pp=which(ftpos[1,]==max(tmp)) # 각 발의 땅과 닾은 포인트중에 x값이 큰 포인트 찾기
         ftnum=1+as.numeric(pp>2.5) # 위에서 찾은 포인트의 발을 입력
         tpnum=2-mod(pp,2) # 앞꿈치인지 뒷꿈치 인지 입력
         source("cnstnfrc.R")}
         }
)
#print(c(ftch,ftx))
ftch0=ftch # old ftpos
rcd[,i]=info1 # ftx for animation(foot touch location), cnstnfrc 에서 나옴 

 