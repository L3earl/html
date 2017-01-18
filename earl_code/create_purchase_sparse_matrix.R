
# 라이브러리
library(rio) #import, export
library(gdata) #unmatrix
library(dplyr) #filter
library(data.table)
library(foreach)
library(doParallel)

# import multi user Id matrix by scenario
setwd("F:/googledrive/L.point 빅데이터/scenario")

read.clust <- function(scenarioNum){
  dir.location <- sprintf("s_%03d/", scenarioNum)
  clust.names <- dir(dir.location, pattern = "clust")
  clust.list <- lapply(paste(dir.location, clust.names, sep = ""), import)
  return(clust.list)
}

clust.userID.list <- read.clust(1)

# import product id, make character vector
product.category3 <- import("F:/googledrive/L.point 빅데이터/scenario/common/productCategory3.sav")
product <- as.character(unmatrix(product.category3, byrow = TRUE))

# make frame of purchase matrix (list type)
make.frame <- function(u_id, p_id) {
    result <-  matrix(0, nrow = nrow(u_id), ncol = length(p_id)
                      , dimnames = list(u_id[,1],p_id))
    return(result)
}

purchase.matrix.frame <- lapply(clust.userID.list, make.frame, p_id = product)

# afrter make frame, remove productID from RAM
remove(product.category3)
remove(product)

# import purchase data
sorted.receipt <- import("F:/googledrive/L.point 빅데이터/scenario/common/sortedreceipt.sav")

# saperate purchase data as each period
receipt.period.1 <- distinct(filter(sorted.receipt[,c(1,2)]))
receipt.period.2 <- distinct(filter(sorted.receipt[,c(1,2)]
                                    , sorted.receipt[,3] > 20150131 | sorted.receipt[,3] < 20150101))
receipt.period.3 <- distinct(filter(sorted.receipt[,c(1,2)]
                                    , sorted.receipt[,3] > 20141231 & sorted.receipt[,3] < 20150201))
remove(sorted.receipt)

# merge clust UserId amd receipt each period
merge.receipt.clust <- function(clust, receipt){
  temp <- merge(clust, receipt, by = 'userID')
  return(temp)
}

receipt.clust.1 <- lapply(clust.userID.list, merge.receipt.clust
                          , receipt = receipt.period.1)

receipt.clust.2 <- lapply(clust.userID.list, merge.receipt.clust
                          , receipt = receipt.period.2)

receipt.clust.3 <- lapply(clust.userID.list, merge.receipt.clust
                          , receipt = receipt.period.3)

remove(receipt.period.1)
remove(receipt.period.2)
remove(receipt.period.3)

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
  cl <- makeCluster(4)
  registerDoParallel(cl)
  getDoParWorkers()
  
  # make sparse list
  purchase.sparse.1 <- make.sparse(purchase.matrix.frame, receipt.clust.1)
  purchase.sparse.2 <- make.sparse(purchase.matrix.frame, receipt.clust.2)
  purchase.sparse.3 <- make.sparse(purchase.matrix.frame, receipt.clust.3)
  
  # cluster stop
  stopCluster(cl)
})

remove(receipt.clust.1)
remove(receipt.clust.2)
remove(receipt.clust.3)

# export sparse matrix of purchase.sparse
temp1.1 <- purchase.sparse.1[[1]]
temp1.2 <- purchase.sparse.1[[2]]
temp1.3 <- purchase.sparse.1[[3]]
temp2.1 <- purchase.sparse.2[[1]]
temp2.2 <- purchase.sparse.2[[2]]
temp2.3 <- purchase.sparse.2[[3]]
temp3.1 <- purchase.sparse.3[[1]]
temp3.2 <- purchase.sparse.3[[2]]
temp3.3 <- purchase.sparse.3[[3]]

temp.name <- sprintf("COL%1d", 1:4386)
colnames(temp1.1) <- temp.name
colnames(temp1.2) <- temp.name
colnames(temp1.3) <- temp.name
colnames(temp2.1) <- temp.name
colnames(temp2.2) <- temp.name
colnames(temp2.3) <- temp.name
colnames(temp3.1) <- temp.name
colnames(temp3.2) <- temp.name
colnames(temp3.3) <- temp.name

export(temp1.1, "F:/googledrive/L.point 빅데이터/scenario/s_001/data/purchaseSparse1_1.sav")
export(temp1.2, "F:/googledrive/L.point 빅데이터/scenario/s_001/data/purchaseSparse1_2.sav")
export(temp1.3, "F:/googledrive/L.point 빅데이터/scenario/s_001/data/purchaseSparse1_3.sav")
export(temp2.1, "F:/googledrive/L.point 빅데이터/scenario/s_001/data/purchaseSparse2_1.sav")
export(temp2.2, "F:/googledrive/L.point 빅데이터/scenario/s_001/data/purchaseSparse2_2.sav")
export(temp2.3, "F:/googledrive/L.point 빅데이터/scenario/s_001/data/purchaseSparse2_3.sav")
export(temp3.1, "F:/googledrive/L.point 빅데이터/scenario/s_001/data/purchaseSparse3_1.sav")
export(temp3.2, "F:/googledrive/L.point 빅데이터/scenario/s_001/data/purchaseSparse3_2.sav")
export(temp3.3, "F:/googledrive/L.point 빅데이터/scenario/s_001/data/purchaseSparse3_3.sav")
