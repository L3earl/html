
# 라이브러리
library(rio) 
library(gdata) 
library(dplyr) 
library(data.table)
library(foreach)
library(doParallel)

# 시나리오 넘버와 폴더 지정해 놓음
scenarioNum <- 2
dir.location <- sprintf("s_%03d", scenarioNum)

# 기간 개수 입력
periodNum <- 3

# 일할 cpu worker개수 입력
cpu.Num <- 4

# import multi user Id matrix by scenario
setwd("F:/googledrive/L.point 빅데이터/scenario")

read.clust <- function(scenarioNum){
  clust.names <- dir(dir.location, pattern = "clust")
  clust.list <- lapply(paste0(dir.location, "/", clust.names), import)
  return(clust.list)
}

clust.userID.list <- read.clust(senarioNum)  
user.clust.num <- length(clust.userID.list)

# import product id, make character vector (아이템 개수나 순서가 바뀌면 코드가 바뀌어야 함)
product.category3 <- import("F:/googledrive/L.point 빅데이터/scenario/common/productCategory3.sav")
product <- as.character(unmatrix(product.category3, byrow = TRUE))

# make frame of purchase matrix (list type)
make.frame <- function(u_id, p_id) {
    result <-  matrix(0, nrow = nrow(u_id), ncol = length(p_id)
                      , dimnames = list(u_id[,1],p_id))
    return(result)
}

purchase.matrix.frame <- lapply(clust.userID.list, make.frame, p_id = product)

# 다 쓴 내용 정리
remove(product.category3)
remove(product)

# import purchase data
sorted.receipt <- import("F:/googledrive/L.point 빅데이터/scenario/common/sortedreceipt.sav")

# saperate purchase data as each period (기간이 바뀔때마다 변경되는 부분)
receipt.period.1 <- distinct(filter(sorted.receipt[,c(1,2)]))
receipt.period.2 <- distinct(filter(sorted.receipt[,c(1,2)]
                                    , sorted.receipt[,3] > 20150131 | sorted.receipt[,3] < 20150101))
receipt.period.3 <- distinct(filter(sorted.receipt[,c(1,2)]
                                    , sorted.receipt[,3] > 20141231 & sorted.receipt[,3] < 20150201))

# 다 쓴 내용 정리
remove(sorted.receipt)

# merge clust UserId amd receipt each period
merge.receipt.clust <- function(clust, receipt){
  temp <- merge(clust, receipt, by = "userID")
  return(temp)
}

for(i in 1:periodNum){
  assign(paste0("receipt.clust.", i), lapply(clust.userID.list, merge.receipt.clust
                                                     , receipt = eval(parse(text = paste0("receipt.period.", i)))))
  remove(list = paste0("receipt.period.",i))
}

# make sparse matrix
make.sparse <- function(p_frame, r_clust) { 
  result <- foreach(i = 1:length(p_frame)) %dopar% {
    for(j in 1:nrow(r_clust[[i]])){
      p_frame[[i]][as.character(r_clust[[i]][j,1]),r_clust[[i]][j,2]] <- 1
    }
    return(p_frame[[i]])
  }
  return(result)
}

# 함수 실행
system.time({
  
  # 병렬 처리 cluster 4개 생성 및 등록
  cl <- makeCluster(cpu.Num)
  registerDoParallel(cl)
  getDoParWorkers()
  
  # make sparse list
  for(i in 1:periodNum){
    assign(paste0("purchase.sparse.", i)
           , make.sparse(purchase.matrix.frame, eval(parse(text = paste0("receipt.clust.", i)))))
    remove(list = paste0("receipt.clust.",i))
  }
  
  # cluster stop
  stopCluster(cl)
})

# SAS에서 자카드 계산을 하기 위해 list인 purchase.sparse를 매트릭스로 분할하여 내보내며, colname도 변경
temp.colname <- sprintf("COL%1d", 1:4386)

for(i in 1:periodNum){ 
  for(j in 1:user.clust.num){
    temp.text <- paste0("colnames(purchase.sparse.", i, "[[", j, "]]) <- temp.colname")
    eval(parse(text=temp.text))
    temp.text <- paste0('export(purchase.sparse.', i, '[[', j, ']],', '"F:/googledrive/L.point 빅데이터/scenario/'
                        , dir.location, '/data/purchaseSparse', i, '_', j, '.sav")')
    eval(parse(text=temp.text))
  }
  # remove(list = paste0("purchase.sparse.",i))
}
