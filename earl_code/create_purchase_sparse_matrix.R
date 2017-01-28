
# 라이브러리
library(rio) 
library(gdata) 
library(dplyr) 
library(data.table)
library(foreach)
library(doParallel)
library(gtools)
library(foreach)
library(foreign)

# 시나리오 넘버를 입력하세요
scenarioNum <- 29

# 기간 개수 입력
periodNum <- 3

# 작업 중간에 나오는 데이터를 저장할 폴더의 주소를 입력하세요
dir.data <- 'F:/temp/Lpoint/scenario'

# scenario 폴더의 주소를 입력하세요. (이미 존재하는 걸 폴더의 주소를 입력해야 함)
dir.lpoint <- 'F:/googledrive/L.point 빅데이터/scenario'

# 일할 cpu worker개수 입력
cpu.Num <- 4

# 필요한 폴더를 생성합니다. (이미 있으면 경고만 나오고 지나감)
dir.scenario <- sprintf("/s_%03d", scenarioNum)
dir.create(paste0(dir.data, dir.scenario))
dir.data <- paste0(dir.data, dir.scenario)

# import multi user Id matrix by scenario
read.clust <- function(scenarioNum){
  clust.names <- mixedsort(dir(paste0(dir.lpoint, dir.scenario), pattern = "clu"))
  clust.list <- lapply(paste0(dir.lpoint, dir.scenario, "/", clust.names), import)
  return(clust.list)
}

clust.userID.list <- read.clust(senarioNum)  

# set userID as col1's name
for(i in 1:length(clust.userID.list)){
  temp.text <- paste0('colnames(clust.userID.list[[', i, ']]) <- "userID"')
  eval(parse(text = temp.text))
}

# 클러스터 개수 저장해 놓음
user.clust.num <- length(clust.userID.list)

# import product id, make character vector 
product.category3 <- import(paste0(dir.lpoint, '/common/productCategory3code.sav'))
product <- as.character(unmatrix(product.category3, byrow = TRUE))

### import 아이템 개수나 순서가 바뀌면 코드가 바뀌어야 함
product.category3 <- import(paste0(dir.lpoint, dir.scenario, '/product_1.csv'))
product <- as.character(unmatrix(product.category3, byrow = TRUE))
colnames(product) <- 'category3code'

# make frame of purchase matrix (list type)
make.frame <- function(u_id, p_id) {
    result <-  matrix(0, nrow = nrow(u_id), ncol = length(p_id)
                      , dimnames = list(u_id[,1],p_id))
    return(result)
}

purchase.matrix.frame <- lapply(clust.userID.list, make.frame, p_id = product)

# 다 쓴 내용 정리
remove(product.category3)
# remove(product)

# import purchase data +++++++(아이템 개수나 순서가 바뀌면 코드가 바뀌어야 함)
t.purchase <- fread("F:/googledrive/L.point 빅데이터/제3회 Big Data Competition-개인화상품추천/제3회 Big Data Competition-분석용데이터-02.구매상품TR.txt")
# purchase <- merge(t.purchase, product, by = "productID") #arrange is needed?
period.1 <- filter(purchase[,c(6,5)])
period.2 <- filter(purchase[,c(6,5)], purchase[,8] > 20150131 | purchase[,8] < 20150101)
period.3 <- filter(purchase[,c(6,5)], purchase[,8] > 20141231 & purchase[,8] < 20150201)
remove(purchase)
gc()

period.1$num <- 1
period.2$num <- 1
period.3$num <- 1

colnames(period.1) <- c('userID', 'category3code', 'num')
colnames(period.2) <- c('userID', 'category3code', 'num')
colnames(period.3) <- c('userID', 'category3code', 'num')

t_period.1 <- aggregate(num ~ userID+category3code, period.1, sum)
t_period.2 <- aggregate(num ~ userID+category3code, period.2, sum)
t_period.3 <- aggregate(num ~ userID+category3code, period.3, sum)

receipt.period.1 <- distinct(filter(t_period.1[,c(1,2)], t_period.1[,3] >= 3))
receipt.period.2 <- distinct(filter(t_period.2[,c(1,2)], t_period.2[,3] >= 3))
receipt.period.3 <- distinct(filter(t_period.3[,c(1,2)], t_period.3[,3] >= 1))
#View(receipt.period.1)
remove(period.1)
remove(period.2)
remove(period.3)
remove(t_period.1)
remove(t_period.2)
remove(t_period.3)
gc()

##
t.sorted.receipt <- import(paste0(dir.lpoint, '/common/sortedreceipt.sav'))
sorted.receipt <- merge(t.sorted.receipt, product, by = "category3code")

# saperate purchase data as each period (기간이 바뀔때마다 변경되는 부분)
receipt.period.1 <- distinct(filter(sorted.receipt[,c(1,2)]))
receipt.period.2 <- distinct(filter(sorted.receipt[,c(1,2)]
                                    , sorted.receipt[,3] > 20150131 | sorted.receipt[,3] < 20150101))
receipt.period.3 <- distinct(filter(sorted.receipt[,c(1,2)]
                                    , sorted.receipt[,3] > 20141231 & sorted.receipt[,3] < 20150201))


temp.filter1 <- filter(sorted.receipt[,c(1,2)], sorted.receipt[,3] < 20140301)
temp.filter2 <- filter(sorted.receipt[,c(1,2)], sorted.receipt[,3] > 20141131 & sorted.receipt[,3] < 20150101)
temp.filter3 <- filter(sorted.receipt[,c(1,2)], sorted.receipt[,3] > 20150131 & sorted.receipt[,3] < 20150301)
temp.filter4 <- filter(sorted.receipt[,c(1,2)], sorted.receipt[,3] > 20151131)
receipt.period.1 <- rbind(temp.filter1,temp.filter2,temp.filter3,temp.filter4)
receipt.period.2 <- rbind(temp.filter1,temp.filter2,temp.filter3,temp.filter4)

remove(temp.filter1)
remove(temp.filter2)
remove(temp.filter3)
remove(temp.filter4)
gc()


# 다 쓴 내용 정리
remove(t.sorted.receipt)
remove(sorted.receipt)
gc()

# merge clust UserId and receipt each period
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
  result <- foreach(i = 1:length(p_frame),.packages = c('foreach')) %dopar% {
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
#temp.colname <- sprintf("COL%1d", c(1:ncol(purchase.sparse.1[[1]])))

for(i in 1:periodNum){ 
  for(j in 1:user.clust.num){
    #temp.text <- paste0("colnames(purchase.sparse.", i, "[[", j, "]]) <- temp.colname")
    #eval(parse(text=temp.text))
    temp.text <- paste0('export(purchase.sparse.', i, '[[', j, ']], "', dir.data, '/purchaseSparse', i, '_', j, '.sav")')
    eval(parse(text=temp.text))
  }
  remove(list = paste0("purchase.sparse.",i))
}

rm(list = ls(all=TRUE))
gc()
