# 사용되는 패키지들, 한번만 설치하면 됨
install.packages("foreign")     # sav 파일 import 가능하게 하는 foreign 라이브러리 존재
install.packages('rio')         # sav 파일 export 가능하게 해주는 rio 라이브러리 존재

# 사용되는 라이브러리들
library(foreign)
library(rio)

# 작업폴더 지정
setwd("F:/googledrive/temp")
getwd()


#####################################
# 추천 아이템- 사용자 기반 #
#####################################


# 사용자 기반 선호도 매트릭스를 불러옴
User.based.dist <- read.spss('Userbaseddist.sav', to.data.frame = TRUE)

# row별로 max의 위치와 값을 max.number 개수만큼 뽑아냄
max.order <- function(x, max.number = 5){
  axis.dtfm <- data.frame(1:max.number)     # 함수 내에서 쓸 dataframe을 선언해 놓음
  value.dtfm <- data.frame(1:max.number)
  temp.axis.dtfm <- data.frame(1:max.number)
  temp.value.dtfm <- data.frame(1:max.number)
  
  for(i in 1:nrow(x)){      # 한개의 row를 뽑아서, max값을 찾고, 해당 위치에 0을 넣는 것을 max.number 만큼 반복, 모든 row에 반복
    for(j in 1:max.number){
      col.axis <- which.max(x[i,])
      col.value <- max(x[i,])
      
      x[i,col.axis] <- 0
      
      temp.axis.dtfm[j,] <- col.axis      # max 좌표와 값을 dataframe에 넣음
      temp.value.dtfm[j,] <- col.value
    }
    # dataframe을 합침
    axis.dtfm <- cbind(axis.dtfm, temp.axis.dtfm)
    value.dtfm <- cbind(value.dtfm, temp.value.dtfm)
  }
  
  # 좌표, 값 dataframe을 위아래로 합쳐서 반환함
  final <- rbind(axis.dtfm[,-1], value.dtfm[,-1])
  return(final)
}

# 함수 실행
recommand.user.based <- max.order(User.based.dist, 5)
str(recommand.user.based)

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
s