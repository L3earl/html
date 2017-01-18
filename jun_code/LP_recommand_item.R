# 사용되는 패키지들, 한번만 설치하면 됨
install.packages('doParallel')
install.packages('foreach')
install.packages('foreign')     # sav 파일 import 가능하게 하는 foreign 라이브러리 존재
install.packages('itertools')
install.packages('data.table')

# 사용되는 라이브러리들
library(doParallel)
library(foreach)
library(foreign)
library(itertools)
library(data.table)

# 작업폴더 지정
setwd("F:/googledrive/temp")

#####################################
# 추천 아이템- 사용자 기반 #
#####################################

# 사용자 기반 선호도 매트릭스를 불러옴
User.based.dist <- read.spss('Userbaseddist.sav', to.data.frame = TRUE)

max.order <- function(x, max.number = 5){  
  # 함수 내에서 쓸 dataframe을 선언해 놓음
  axis.dtfm <- data.frame(matrix(0, ncol = max.number, nrow = 1))
  value.dtfm <- data.frame(matrix(0, ncol = max.number, nrow = 1))  
  
  # 병렬처리 foreach문, 내부의 실행 결과를 data.table 형태로 final에 rbind 시킨다.
  final<-foreach(i = 1:nrow(x),.combine="rbind",.packages=c("foreach")) %dopar% {
    foreach(j = 1:max.number) %do% {
      temp.axis <- which.max(x[i,])
      temp.value <- max(x[i,])      

      x[i,temp.axis] <- 0
      
      axis.dtfm[1,j] <- temp.axis      # max 좌표와 값을 dataframe에 넣음
      value.dtfm[1,j] <- temp.value 
    }
    # max 좌표max.number개와 값 max.number개를 cbind 한다. 
    data.frame(
      cbind(axis.dtfm[1,],value.dtfm[1,])
    )
  }

  # 반환할 final 컬럼명 변경  
  setnames(final, c("a1","a2","a3","a4","a5","v1","v2","v3","v4","v5") )
  
  return(final)
}

# 함수 실행
system.time({

# 병렬 처리 cluster 4개 생성 및 등록
cl <- makeCluster(4)
registerDoParallel(cl)
getDoParWorkers()

# row별로 max의 위치와 값을 max.number 개수만큼 뽑아냄
recommand.user.based <- max.order(User.based.dist, 5)
str(recommand.user.based)

# cluster stop
stopCluster(cl)
})

# sav 파일로 export
export(recommand.user.based, "F:/googledrive/temp/recommandUserBased.sav")

#####################################s
# 추천 아이템- 아이템 기반 #
#####################################

# 사용자 기반 선호도 매트릭스를 불러옴
item.based.dist <- read.spss('ItemBasedDist.sav', to.data.frame = TRUE)

# 함수 실행
recommand.item.based <- max.order(item.based.dist, 5)
str(recommand.item.based)

# sav 파일로 export
export(recommand.item.based, "F:/googledrive/temp/recommandItemBased.sav")

