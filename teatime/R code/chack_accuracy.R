
# 뽑을 추천 아이템 개수 입력
max.number <- 100

# 시나리오 넘버를 입력하세요
scenarioNum <- 60

# 작업 중간에 나오는 데이터가 저장된 폴더의 주소를 입력하세요
dir.data <- 'F:/temp/Lpoint/scenario'

# scenario 폴더의 주소를 입력하세요. (이미 존재하는 걸 폴더의 주소를 입력해야 함)
dir.lpoint <- 'F:/googledrive/L.point 빅데이터/scenario'

# 일할 cpu worker개수 입력
cpu.Num <- 4

# 필요한 폴더를 생성합니다. (이미 있으면 경고만 나오고 끝남)
dir.scenario <- sprintf("/s_%03d", scenarioNum)
dir.create(paste0(dir.data, dir.scenario))
dir.create(paste0(dir.lpoint, dir.scenario, '/result'))
dir.data <- paste0(dir.data, dir.scenario)
dir.result <- paste0(dir.lpoint, dir.scenario, '/result')

# 유저 군집 아이디를 불러옴
read.clust <- function(scenarioNum){
  clust.names <- mixedsort(dir(paste0(dir.lpoint, dir.scenario), pattern = "clu"))
  clust.list <- lapply(paste0(dir.lpoint, dir.scenario, "/", clust.names), import)
  return(clust.list)
}

clust.userID.list <- read.clust(senarioNum)  

# SAS에서 나온 선호도 행렬을 불러와 list로 반환
read.prefer.matrix <- function(scenarioNum){
  clust.names <- mixedsort(dir(dir.data, pattern = "UserDist"))
  clust.list <- lapply(paste0(dir.data, '/', clust.names), read.spss, to.data.frame = TRUE)
  return(clust.list)
}

preference <- read.prefer.matrix(scenarioNum)

# 클러스터 개수 저장해 놓음
user.clust.num <- length(preference)

# 선호도 매트릭스에서 user별 max좌표 뽑아서, 추천 아이템 좌표 매트릭스를 만듬
max.axis.search <- function(prefer, max.number){  
  
  # 병렬처리 foreach문, 내부의 실행 결과를 data.table 형태로 rbind 시킨다.
  result <- foreach(i = 1:length(prefer),.packages = c("foreach", 'dplyr')) %do% {
    temp <- prefer[[i]]
    temp$productAxis <- c(1:nrow(prefer[[i]]))
    
    temp.dtfm <- foreach(j = 1:ncol(prefer[[i]]),.combine="rbind",.packages = c("dplyr")) %dopar% {
      temp2 <- select(temp, j, ncol(temp))
      temp3 <- arrange(temp2, desc(temp2[,1]))
      temp4 <- temp3[c(1:max.number),2]
      return(temp4)
    }
    
    temp5 <- apply(temp.dtfm, 2, Modef)
    
    for(k in 1:nrow(temp.dtfm)){
      if(sum(temp.dtfm[k,]) == sum(c(1:max.number))){
        temp.dtfm[k,] <- temp5
      }
    }
    
    return(temp.dtfm)
  }
  return(result)
}

# mode function 최빈값 함수
Modef <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 함수 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(cpu.Num)
  registerDoParallel(cl)
  
  # row별로 max의 좌표 위치를 max.number 개수만큼 뽑아냄
  recommend.item.axis <- max.axis.search(preference, max.number)
  
  # cluster stop
  stopCluster(cl)
})

# 다 쓴 것 메모리에서 삭제
remove(preference)

# import product 소분류코드 name and code 
product <- fread("F:/googledrive/L.point 빅데이터/제3회 Big Data Competition-개인화상품추천/제3회 Big Data Competition-분석용데이터-03.상품분류.txt") 
product.name.code <- filter(product[,c(4,6)])
colnames(product.name.code) <- c('productCode', 'productName')
remove(product)

# 줄어든 상품의 소분류코드를 불러들여, name and code 부분에 적용함
temp <- import(paste0(dir.lpoint, dir.scenario, '/product_1.csv'))
colnames(temp) <- 'productCode'
product.name.code <- merge(product.name.code, temp, by = "productCode")

# 추천 아이템 좌표에 해당하는 소분류코드 이름과 코드를 찾음
max.name.code.search <- function(itemAxis, product, max.number){  
  
  result <- foreach(i = 1:length(itemAxis),.packages = c("foreach")) %do% {
    foreach(j = 1:nrow(itemAxis[[i]]),.combine="rbind",.packages = c("foreach")) %dopar% {
      temp.dtfm <- data.frame(matrix(0, ncol = max.number*2, nrow = 1))
      
      foreach(k = 1:max.number,.packages = c("foreach")) %do% {
        temp.dtfm[1,k] <- product[itemAxis[[i]][j,k],2]    #set max name
        temp.dtfm[1,k+max.number*1] <- product[itemAxis[[i]][j,k],1]   #set max code
      }
      return(temp.dtfm)
    }
  }
  return(result)
}

# 병렬 처리 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(cpu.Num)
  registerDoParallel(cl)
  
  # 함수 실행
  suggest.item <- max.name.code.search(recommend.item.axis, product.name.code, max.number)
  
  # cluster stop
  stopCluster(cl)
})

# 다 쓴 것 메모리에서 삭제
remove(product.name.code)

# 추천 상품 이름, 코드를 행렬로 내보냄
for(i in 1:length(suggest.item)){
  suggest.item[[i]]$userID <- clust.userID.list[[i]][,1]
}

suggest.item.matrix <- arrange(rbindlist(suggest.item), userID)
export(suggest.item.matrix[,c(201, 1:100)], paste0(dir.result, '/suggest.item.name.csv'))
export(suggest.item.matrix[,c(201, 101:200)], paste0(dir.result, '/suggest.item.code.csv'))

# TEST기간 구매 희소 행렬을 불러와 list로 반환
read.test.data <- function(scenarioNum){
  purchase.name <- mixedsort(dir(dir.data, pattern = "purchaseSparse3"))
  purchase.list <- lapply(paste0(dir.data, '/' ,purchase.name), import)
  return(purchase.list)
}

purchase <- read.test.data(scenarioNum)

# Test기간동안의 추천 아이템의 좌표를 구매 희소 행렬에서 찾아서 구매를 했으면 TURE를 해당 자리에 넣는 함수
check.accuracy <- function(recommend, purchase, max.number){
  temp.dtfm <- data.frame(matrix(FALSE, ncol = 1, nrow = 1))
  
  accuracy <- foreach(i = 1:length(recommend),.packages = c("foreach")) %dopar% {
    foreach(j = 1:nrow(recommend[[i]]),.combine="rbind",.packages=c("foreach")) %do% {
      foreach(k = 1:max.number,.combine = "cbind") %do% {
        temp.dtfm[1,1] <- FALSE
        if(purchase[[i]][j,recommend[[i]][j,k]] == 1){
          temp.dtfm[1,1] <- TRUE
        }
        return(temp.dtfm)
      }
    }
  }
  return(accuracy)
}

# 병렬 처리 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(cpu.Num)
  registerDoParallel(cl)
  getDoParWorkers()
  
  # 함수 실행
  True.False <- check.accuracy(recommend.item.axis, purchase, max.number)
  
  # cluster stop
  stopCluster(cl)
})


# set userID as rownames
for(i in 1:length(True.False)){
  temp.text <- paste0("rownames(True.False[[", i, "]]) <- clust.userID.list[[", i, "]][,1]")
  eval(parse(text = temp.text))
  temp.text <- paste0('colnames(True.False[[', i, ']]) <- c(1:', length(True.False[[i]]), ')')
  eval(parse(text = temp.text))
}

# 다 쓴 것 메모리에서 삭제
remove(recommend.item.axis)
remove(purchase)

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

# 병렬 처리 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(cpu.Num)
  registerDoParallel(cl)
  getDoParWorkers()
  
  # 함수 실행
  marking.recommend <- mark.recommend(suggest.item, True.False, max.number)
  
  # cluster stop
  stopCluster(cl)
})

# set userID as rownames
for(i in 1:length(marking.recommend)){
  temp.text <- paste0("rownames(marking.recommend[[", i, "]]) <- clust.userID.list[[", i, "]][,1]")
  eval(parse(text = temp.text))
  temp.text <- paste0('colnames(marking.recommend[[', i, ']]) <- c(1:', length(marking.recommend[[i]]), ')')
  eval(parse(text = temp.text))
}

# 유저별로 실제로 산 아이템과 안 산 아이템을 구별해주는 marking.recommend를 excel 파일로 export
system.time({
  for(i in 1:length(marking.recommend)){
    temp.text <- paste0('export(marking.recommend[[', i, ']], "', dir.result, '/markingRecommend', scenarioNum, '_', i, '.csv"
                        , col.names=TRUE, row.names=TRUE)')
    eval(parse(text=temp.text))
  }
})

# 다 쓴 것 메모리에서 삭제
remove(suggest.item)
remove(marking.recommend)

# TRUE의 개수로 군집의 정확도,누적 정확도를 반환하는 함수
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

# 병렬 처리 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(cpu.Num)
  registerDoParallel(cl)
  getDoParWorkers()
  
  # 함수 실행
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

# 군집과 전체 정보를 합침
clust.overall.accuracy <- rbind(overall.accuracy, clust.accuracy)

# 다 쓴 것 메모리에서 삭제
remove(True.False)
remove(clust.userID.list)
remove(clust.accuracy)
remove(overall.accuracy)

# set rownames & colnames
temp.row.names <- c('전체-정답 수', '정확도', '누적 정확도')
for(i in 1:user.clust.num){
  temp <- c(paste0('군집', i, '-정답 수'), paste0('군집', i, '-정확도'), paste0('군집', i, '누적 정확도'))
  temp.row.names <- append(temp.row.names, temp)
}
rownames(clust.overall.accuracy) <- temp.row.names
remove(temp.row.names)

temp.text <- paste0('colnames(clust.overall.accuracy) <- c( "user수" , c(1:', length(clust.overall.accuracy)-1, '))')
eval(parse(text = temp.text))
remove(temp.text)

# excel 파일로 export
temp.text <- paste0('write.xlsx(clust.overall.accuracy, file="', dir.result, '/accuracy', scenarioNum, '.xlsx"'
                      , ', sheetName= "overall", col.names=TRUE, row.names=TRUE, append=FALSE)')
eval(parse(text=temp.text))
remove(temp.text)

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

system.time(
  user.accuracy <- user.accuracy(overall, max.number)
)

# set rownames & colnames 
temp.text <- paste0('rownames(user.accuracy) <- c(1:', nrow(user.accuracy), ')')
eval(parse(text = temp.text))
remove(temp.text)
temp.text <- paste0('colnames(user.accuracy) <- c( "userID" , c(1:', length(user.accuracy)-1, '))')
eval(parse(text = temp.text))
remove(temp.text)

# excel 파일로 export
temp.text <- paste0('export(user.accuracy,"', dir.result, '/userAccuracy', scenarioNum, '.csv", col.names=TRUE, row.names=TRUE)')
eval(parse(text=temp.text))
remove(temp.text)

# 다 쓴 것 메모리에서 삭제
remove(user.accuracy)
rm(list = ls(all=TRUE))
gc()
