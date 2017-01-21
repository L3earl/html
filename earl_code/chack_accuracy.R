
# 사용되는 라이브러리들
library(doParallel)
library(foreach)
library(foreign)
library(itertools)
library(data.table)
library(rio)
library(gdata)
library(scales)
library(dplyr)

# 작업폴더 지정
setwd("F:/googledrive/L.point 빅데이터/scenario")

# 시나리오 넘버 지정 놓음
scenarioNum <- 6
dir.location <- sprintf("s_%03d", scenarioNum)

# 일할 cpu worker개수 입력
cpu.Num <- 4

# 뽑을 추천 아이템 개수 입력
max.number <- 50

# 선호도 matrix를 import하여 list로 반환
read.prefer.matrix <- function(scenarioNum){
  clust.names <- dir(paste0(dir.location, "/data/"), pattern = "UserDist")
  clust.list <- lapply(paste0(dir.location, "/data/", clust.names), read.spss
                       , to.data.frame = TRUE)
  return(clust.list)
}

preference <- read.prefer.matrix(scenarioNum)

# import product category3 name and code
product <- fread("F:/googledrive/L.point 빅데이터/제3회 Big Data Competition-개인화상품추천/제3회 Big Data Competition-분석용데이터-03.상품분류.txt") 
product.category3 <- filter(product[,c(4,6)])
remove(product)

# 선호도 매트릭스에서 user별 max좌표 뽑아서, 추천 아이템 좌표 매트릭스를 만듬
max.search <- function(x, product, max.number){  

  # 병렬처리 foreach문, 내부의 실행 결과를 data.table 형태로 final에 rbind 시킨다.
  result <- foreach(i = 1:length(x),.packages = c("foreach")) %do% {
    foreach(j = 1:nrow(x[[i]]),.combine="rbind",.packages = c("foreach")) %dopar% {
      temp.dtfm <- data.frame(matrix(0, ncol = max.number*4, nrow = 1))
      
      foreach(k = 1:max.number,.packages = c("foreach")) %do% {
        temp.axis <- which.max(x[[i]][j,])      # get max 좌표 
          
        temp.dtfm[1,k] <- product[temp.axis,2]    #set max name
        temp.dtfm[1,k+max.number*1] <- product[temp.axis,1]   #set max code
        temp.dtfm[1,k+max.number*2] <- temp.axis     # set max 좌표
        temp.dtfm[1,k+max.number*3] <- x[[i]][j,temp.axis] # set max 값

        x[[i]][j,temp.axis] <- 0    # change max value to 0
      }
      return(temp.dtfm)
    }
  }
  return(result)
}

# 함수 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(cpu.Num)
  registerDoParallel(cl)
  
  # row별로 max의 위치와 값을 max.number 개수만큼 뽑아냄
  recommend <- max.search(preference, product.category3, max.number)
  
  # cluster stop
  stopCluster(cl)
})

# import user ID clust
read.clust <- function(scenarioNum){
  clust.names <- dir(dir.location, pattern = "clust")
  clust.list <- lapply(paste0(dir.location, "/", clust.names), import)
  return(clust.list)
}

clust.userID.list <- read.clust(senarioNum)  

# set userID as rownames 
for(i in 1:length(recommend)){
  temp.text <- paste0("rownames(recommend[[", i, "]]) <- clust.userID.list[[", i, "]][,1]")
  eval(parse(text = temp.text))
  temp.text <- paste0("colnames(recommend[[", i, "]]) <- c(1:", length(recommend[[i]]), ")")
  eval(parse(text = temp.text))
}

# divide data
recommend.item.axis <- lapply(recommend, function(x) x[,(1+max.number*2):(max.number*4)])
suggest.item <- lapply(recommend, function(x) x[,1:(max.number*2)])

# ItemAxis sav 파일로 export
for(i in 1:length(recommend.item.axis)){
  temp.text <- paste0('export(recommend.item.axis[[', i, ']],', '"F:/googledrive/L.point 빅데이터/scenario/'
                      , dir.location, '/result/recommendItemAxis', i, '.sav")')
  eval(parse(text=temp.text))
}

# Item name,code excel 파일로 export
for(i in 1:length(suggest.item)){
  temp.text <- paste0('export(suggest.item[[', i, ']],', '"F:/googledrive/L.point 빅데이터/scenario/'
                      , dir.location, '/result/suggestItem', i, '.csv", col.names=TRUE, row.names=TRUE)')
  eval(parse(text=temp.text))
}

# 구매 희소 행렬을 import하여 list로 반환
read.test.data <- function(scenarioNum){
  dir.location <- sprintf("s_%03d/data/", scenarioNum)
  purchase.name <- dir(dir.location, pattern = "purchaseSparse3")
  purchase.list <- lapply(paste(dir.location, purchase.name, sep = ""), import)
  return(purchase.list)
}

purchase <- read.test.data(scenarioNum)

# Test기간동안의 추천 아이템의 좌표를 구매 희소 행렬에서 찾아서 1의 개수를 반환하는 루틴을 모든 유저만큼 반복
check.accuracy <- function(recommend, purchase, max.number){
  temp.dtfm <- data.frame(matrix(NA, ncol = 1, nrow = 1))
  x <- FALSE
  
  accuracy <- foreach(i = 1:length(recommend),.packages = c("foreach")) %dopar% {
    foreach(j = 1:nrow(recommend[[i]]),.combine="rbind",.packages=c("foreach")) %do% {
      foreach(k = 1:max.number,.combine = "cbind") %do% {
        if(purchase[[i]][j,recommend[[i]][j,k]] == 1){
          x <- TRUE
        }else{
          x <- FALSE
        }
        temp.dtfm[1,1] <- x
        return(temp.dtfm)
      }
    }
  }
  return(accuracy)
}

# 함수 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(cpu.Num)
  registerDoParallel(cl)
  getDoParWorkers()
  
  # 
  True.False <- check.accuracy(recommend.item.axis, purchase, max.number)
  
  # cluster stop
  stopCluster(cl)
})

# set userID as rownames 
for(i in 1:length(True.False)){
  temp.text <- paste0("rownames(True.False[[", i, "]]) <- clust.userID.list[[", i, "]][,1]")
  eval(parse(text = temp.text))
  temp.text <- paste0("colnames(True.False[[", i, "]]) <- c(1:", length(True.False[[i]]), ")")
  eval(parse(text = temp.text))
}

# sav 파일로 export
for(i in 1:length(True.False)){
  temp.text <- paste0('export(True.False[[', i, ']],', '"F:/googledrive/L.point 빅데이터/scenario/'
                      , dir.location, '/result/ResultTrueFalse', i, '.csv", col.names=TRUE, row.names=TRUE)')
  eval(parse(text=temp.text))
}

# check recommend is right or wrong('x' is added in front of recommend item name)
mark.recommend <- function(suggest, TorF, max.number){
  temp.dtfm <- data.frame(matrix(NA, ncol = 1, nrow = 1))
  
  result <- foreach(i = 1:length(suggest),.packages = c("foreach")) %dopar% {
    foreach(j = 1:nrow(suggest[[i]]),.combine="rbind",.packages=c("foreach")) %do% {
      foreach(k = 1:max.number,.combine = "cbind") %do% {
        if(TorF[[i]][j,k] == FALSE){
          temp.dtfm[1,1] <- paste0('x', suggest[[i]][j,k])
        }else{
          temp.dtfm[1,1] <- suggest[[i]][j,k]
        }
        return(temp.dtfm)
      }
    }
  }
  return(result)
}


# 함수 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(cpu.Num)
  registerDoParallel(cl)
  getDoParWorkers()
  
  # 
  marking.recommend <- mark.recommend(suggest.item, True.False, max.number)
  
  # cluster stop
  stopCluster(cl)
})

# sav 파일로 export
for(i in 1:length(marking.recommend)){
  temp.text <- paste0('export(marking.recommend[[', i, ']],', '"F:/googledrive/L.point 빅데이터/scenario/'
                      , dir.location, '/result/ResultMarkingRecommend', i, '.csv", col.names=TRUE, row.names=TRUE)')
  eval(parse(text=temp.text))
}

# accuracy per recommended sequence by clust
clust.accuracy <- function(TorF, max.number){
  result <- foreach(i = 1:length(TorF),.combine="rbind",.packages = c("foreach")) %dopar% {
    temp.dtfm <- data.frame(matrix(NA, ncol = max.number+1, nrow = 3))
    temp.dtfm[1,1] <- userNum <- nrow(TorF[[i]])
    sum.trueNum <- 0
    
    for(j in 1:max.number){
      temp.dtfm[1,j+1] <- trueNum <- table(TorF[[i]][,j])["TRUE"]
      temp.dtfm[2,j+1] <- round(trueNum/userNum,4)*100
      sum.trueNum <- trueNum + sum.trueNum
      temp.dtfm[3,j+1] <- round(sum.trueNum/(userNum*j),4)*100
    }
    return(temp.dtfm)
  }
  return(result)
}

# 함수 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(cpu.Num)
  registerDoParallel(cl)
  getDoParWorkers()
  
  # 
  clust.accuracy <- clust.accuracy(True.False, max.number)
  
  # cluster stop
  stopCluster(cl)
})


# 전체에 대한 정확도를 판별

for(i in 1:length(True.False)){
  True.False[[i]]$userID <- clust.userID.list[[i]][,1]
}


overall <- arrange(rbindlist(True.False), userID)

overall.accuracy <- function(overall, max.number){
  temp.dtfm <- data.frame(matrix(NA, ncol = max.number+1, nrow = 3))
  temp.dtfm[1,1] <- userNum <- nrow(overall)
  sum.trueNum <- 0
  
  for(i in 1:max.number){
    temp.dtfm[1,i+1] <- trueNum <- table(overall[,i])["TRUE"]
    temp.dtfm[2,i+1] <- round(trueNum/userNum,4)*100
    sum.trueNum <- trueNum + sum.trueNum
    temp.dtfm[3,i+1] <- round(sum.trueNum/(userNum*i),4)*100
  }
  return(temp.dtfm)
}

overall.accuracy <- overall.accuracy(overall, max.number)

# clust 와 overall 정보를 합쳐서 scv로 저장
clust.overall.accuracy <- rbind(overall.accuracy, clust.accuracy)

temp <- paste0('export(clust.overall.accuracy, "F:/googledrive/L.point 빅데이터/scenario/'
                , dir.location, '/result/overallAccuracy.csv", col.names=TRUE, row.names=FALSE)')
eval(parse(text=temp))


# 유저 전체에 대한 정확도를 판별
user.accuracy <- function(overall, max.number){
  userNum <- nrow(overall)
  temp.dtfm <- data.frame(matrix(NA, ncol = max.number+1, nrow = userNum))
  temp.dtfm[,1] <- select(overall, userID)
  
  for(i in 1:userNum){
    trueNum <- 0
    
    for(j in 1:max.number){
      if(overall[i,j] == TRUE){
        trueNum <- trueNum + 1
      }
      temp.dtfm[i,j+1] <- round(trueNum/j,4)*100
    }
  }
  return(temp.dtfm)
}

system.time(user.accuracy <- user.accuracy(overall, max.number))

# 저장
temp <- paste0('export(user.accuracy, "F:/googledrive/L.point 빅데이터/scenario/'
               , dir.location, '/result/userAccuracy.csv", col.names=TRUE, row.names=FALSE)')
eval(parse(text=temp))
