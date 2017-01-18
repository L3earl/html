
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

# 시나리오 넘버와 폴더 지정해 놓음
scenarioNum <- 1
dir.location <- sprintf("s_%03d", scenarioNum)

# 선호도 matrix를 import하여 list로 반환
read.prefer.matrix <- function(scenarioNum){
  clust.names <- dir(dir.location, pattern = "UserDistpurchaseSparse")
  clust.list <- lapply(paste0(dir.location, "/", clust.names), read.spss
                       , to.data.frame = TRUE)
  return(clust.list)
}

preference <- read.prefer.matrix(scenarioNum)


# 선호도 매트릭스에서 user별 max좌표 뽑아서, 추천 아이템 좌표 매트릭스를 만듬
max.search <- function(x, max.number = 3){  
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
      return(fusion.dtfm) 
    }
  }
  return(fit.item.list)
}

# 함수 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(4)
  registerDoParallel(cl)
  getDoParWorkers()
  
  # row별로 max의 위치와 값을 max.number 개수만큼 뽑아냄
  recommend.item.axis <- max.search(preference, 3)
  
  # cluster stop
  stopCluster(cl)
})


# sav 파일로 export
for(i in 1:length(recommend.item.axis)){
  paste0('export(recommend.item.axis[[', i, ']],', '"F:/googledrive/L.point 빅데이터/scenario/'
         , dir.location, '/data/recommendItemAxis', i, '.sav")')
  eval(parse(text=textOrder))
}

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

purchase <- read.test.data(scenarioNum)

# Test기간동안의 추천 아이템의 좌표를 구매 희소 행렬에서 찾아서 1의 개수를 반환하는 루틴을 모든 유저만큼 반복
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

temp.dtfm <- c(0:100)
unique(0)
for(i in 1:3){
  temp.dtfm[1,1] <- i
  print(temp.dtfm)
}

# 함수 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(4)
  registerDoParallel(cl)
  getDoParWorkers()
  
  # 
  final <- check.accuracy(recommend.item.axis, purchase)
  
  # cluster stop
  stopCluster(cl)
})

# 검증1
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

# sav 파일로 export
for(i in 1:length(final)){
  paste0('export(final[[', i, ']],', '"F:/googledrive/L.point 빅데이터/scenario/'
         , dir.location, '/data/result', i, '.sav")')
  eval(parse(text=textOrder))
}

export(final[[1]], "F:/googledrive/L.point 빅데이터/scenario/s_001/data/result1.sav")
export(final[[2]], "F:/googledrive/L.point 빅데이터/scenario/s_001/data/result2.sav")
export(final[[3]], "F:/googledrive/L.point 빅데이터/scenario/s_001/data/result3.sav")
