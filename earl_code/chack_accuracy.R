# 사용되는 패키지들, 한번만 설치하면 됨
install.packages('doParallel')
install.packages('foreach')
install.packages('foreign')     # sas에서 만든 sav 파일 import 가능, foreign 라이브러리
install.packages('itertools')
install.packages('data.table')
install.packages('rio')         # sav 파일로 export 가능, rio 라이브러리
install.packages("dplyr")

# 사용되는 라이브러리들
library(doParallel)
library(foreach)
library(foreign)
library(itertools)
library(data.table)
library(rio)
library(dplyr)

# 작업폴더 지정
setwd("F:/googledrive/L.point 빅데이터/scenario")

# 선호도 matrix를 import하여 list로 반환
read.prefer.matrix <- function(scenarioNum){
  dir.location <- sprintf("s_%03d/data/", scenarioNum)
  clust.names <- dir(dir.location, pattern = "UserDistpurchaseSparse")
  clust.list <- lapply(paste(dir.location, clust.names, sep = ""), read.spss
                       , to.data.frame = TRUE)
  return(clust.list)
}

preference <- read.prefer.matrix(1)


# 선호도 매트릭스에서 user별 max좌표 뽑아서, 추천 아이템 좌표 매트릭스를 만듬
max.search <- function(x, max.number = 5){  
  # 함수 내에서 쓸 dataframe을 선언해 놓음
  axis.dtfm <- data.frame(matrix(0, ncol = max.number, nrow = 1))
  value.dtfm <- data.frame(matrix(0, ncol = max.number, nrow = 1))  
  
  # 병렬처리 foreach문, 내부의 실행 결과를 data.table 형태로 final에 rbind 시킨다.
  fit.item.list <- foreach(k = 1:length(x),.packages = c("foreach")) %dopar% {
    foreach(i = 1:nrow(x[[k]]),.combine="rbind") %do% {
        foreach(j = 1:max.number) %do% {
          temp.axis <- which.max(x[[k]][i,])
          temp.value <- max(x[[k]][i,])      
          
          x[[k]][i,temp.axis] <- 0
          
          axis.dtfm[1,j] <- temp.axis      # max 좌표와 값을 dataframe에 넣음
          value.dtfm[1,j] <- temp.value 
        }
      # max 좌표max.number개와 값 max.number개를 cbind 한다. 
      fusion.dtfm <- data.frame(cbind(axis.dtfm[1,],value.dtfm[1,]))
      # setnames(fusion.dtfm, c(sprintf("item%01d", 1:5),sprintf("max.value%01d", 1:5)))
      return(fusion.dtfm) #오류 가능성 있음
    }
  }
  # 반환할 final 컬럼명 변경  
  # names(fit.item.list) <- c(1:length(x))
  return(fit.item.list)
}

# 함수 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(4)
  registerDoParallel(cl)
  getDoParWorkers()
  
  # row별로 max의 위치와 값을 max.number 개수만큼 뽑아냄
  recommend.item.axis <- max.search(preference, 5)
  
  # cluster stop
  stopCluster(cl)
})


# sav 파일로 export
export(recommend.item.axis[[1]], "F:/googledrive/L.point 빅데이터/scenario/s_001/data/recommendItemAxis1.sav")
export(recommend.item.axis[[2]], "F:/googledrive/L.point 빅데이터/scenario/s_001/data/recommendItemAxis2.sav")
export(recommend.item.axis[[3]], "F:/googledrive/L.point 빅데이터/scenario/s_001/data/recommendItemAxis3.sav")

# 구매 희소 행렬을 import하여 list로 반환
read.test.data <- function(scenarioNum){
  dir.location <- sprintf("s_%03d/data/", scenarioNum)
  purchase.name <- dir(dir.location, pattern = "purchaseSparse3")
  purchase.list <- lapply(paste(dir.location, purchase.name, sep = ""), import)
  return(purchase.list)
}

purchase <- read.test.data(1)


check.accuracy <- function(recommend, purchase, max.number = 3){
  temp.dtfm <- data.frame(matrix(NA, ncol = 1, nrow = 1))
  x <- 0
  
  accuracy <- foreach(i = 1:length(recommend),.packages = c("foreach")) %dopar% {
    foreach(j = 1:nrow(recommend[[i]]),.combine="rbind",.packages=c("foreach")) %do% {
      foreach(k = 1:max.number) %do% {
        if(purchase[[i]][j,recommend[[i]][j,k]] == 1){
          x <- x + 1
        }
      }
      temp.dtfm[1,1] <- x
      x <- 0
      return(temp.dtfm)
    }
  }
  return(accuracy)
}

# 함수 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(4)
  registerDoParallel(cl)
  getDoParWorkers()
  
  # row별로 max의 위치와 값을 max.number 개수만큼 뽑아냄
  final <- check.accuracy(recommend.item.axis, purchase)
  
  # cluster stop
  stopCluster(cl)
})

# 검증
for(i in 1:length(final)){
  na.number <- 0
  na.number <- sum(is.na(final[[i]])) + na.number
  print(na.number)
}

for(i in 1:length(final)){
  sum.mean <- 0
  accuracy <- 0
  
  sum.mean <- mean(final[[i]][,1])/3*100
  print(sum.mean)
}

temp <- sum(final[[1]],final[[2]],final[[3]])
accuracy <- temp/sum(nrow(final[[1]]),nrow(final[[2]]),nrow(final[[3]]))/3*100
print(accuracy)


export(final[[1]], "F:/googledrive/L.point 빅데이터/scenario/s_001/data/result1.sav")
export(final[[2]], "F:/googledrive/L.point 빅데이터/scenario/s_001/data/result2.sav")
export(final[[3]], "F:/googledrive/L.point 빅데이터/scenario/s_001/data/result3.sav")
