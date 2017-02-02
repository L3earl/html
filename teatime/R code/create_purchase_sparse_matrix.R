
# 시나리오 넘버를 입력하세요
scenarioNum <- 60

# 기간 개수 입력
periodNum <- 3

# 작업 중간에 나오는 데이터를 저장할 폴더의 주소를 입력하세요
dir.data <- 'F:/temp/Lpoint/scenario'

# scenario 폴더의 주소를 입력하세요. 
dir.lpoint <- 'F:/googledrive/L.point 빅데이터/scenario'

# 일할 cpu 개수 입력해 주세요.
cpu.Num <- 4

# 필요한 폴더를 생성합니다. (이미 있으면 경고만 나오고 지나감)
dir.scenario <- sprintf("/s_%03d", scenarioNum)
dir.create(paste0(dir.data, dir.scenario))
dir.data <- paste0(dir.data, dir.scenario)

# 군집화된 유저 아이디를 불러옴
read.clust <- function(scenarioNum){
  clust.names <- mixedsort(dir(paste0(dir.lpoint, dir.scenario), pattern = "clu"))
  clust.list <- lapply(paste0(dir.lpoint, dir.scenario, "/", clust.names), import)
  return(clust.list)
}

clust.userID.list <- read.clust(senarioNum)  

# 유저 아이디에 컬럼명을 입력
for(i in 1:length(clust.userID.list)){
  temp.text <- paste0('colnames(clust.userID.list[[', i, ']]) <- "userID"')
  eval(parse(text = temp.text))
}

# 클러스터 개수 저장해 놓음
user.clust.num <- length(clust.userID.list)

# 분류된 상품 소분류코드 정보를 불러옴
product.category3 <- import(paste0(dir.lpoint, dir.scenario, '/product_1.csv'))
product <- as.character(unmatrix(product.category3, byrow = TRUE))
colnames(product.category3) <- 'category3code'

# 구매 희소 행렬의 껍데기를 만들어 놓음
make.frame <- function(u_id, p_id) {
    result <-  matrix(0, nrow = nrow(u_id), ncol = length(p_id)
                      , dimnames = list(u_id[,1],p_id))
    return(result)
}

purchase.matrix.frame <- lapply(clust.userID.list, make.frame, p_id = product)

# 구매 데이터를 불러와, 줄어든 상품 소분류코드와 병합 (mergy)
t.purchase <- fread("F:/googledrive/L.point 빅데이터/제3회 Big Data Competition-개인화상품추천/제3회 Big Data Competition-분석용데이터-02.구매상품TR.txt")
colnames(t.purchase) <- c('alliance', 'receiptCode', 'category1code', 'category2code' , 'category3code', 'userID', 'storeCode', 'date', 'time', 'amount')
purchase <- merge(t.purchase, product.category3, by = "category3code") 

# 자카드, training, test 에 쓰일 기간의 구매한 데이터만 뽑아냄
temp.filter1 <- filter(purchase[,c('userID','category3code')], purchase[,'date'] < 20140301)
temp.filter2 <- filter(purchase[,c('userID','category3code')], purchase[,'date'] > 20141131 & purchase[,'date'] < 20150301)
temp.filter3 <- filter(purchase[,c('userID','category3code')], purchase[,'date'] > 20151131)
period.1 <- rbind(temp.filter1,temp.filter2,temp.filter3)
period.2 <- rbind(temp.filter1,temp.filter2,temp.filter3)
period.3 <- filter(purchase[,c('userID','category3code')], purchase[,'date'] > 20141231 & purchase[,'date'] < 20150201)


# 기온 관련 구매 데이터를 쓰는 경우, 해당 데이터 import
receipt.period.1 <- import(paste0(dir.lpoint, dir.scenario, '/purchase_by_temperature_1.sav')) 
receipt.period.2 <- import(paste0(dir.lpoint, dir.scenario, '/purchase_by_temperature_1.sav'))
period.1 <- merge(receipt.period.1, product.category3, by = "category3code") 
period.2 <- merge(receipt.period.2, product.category3, by = "category3code") 
period.3 <- filter(purchase[,c('userID','category3code')], purchase[,'date'] > 20141231 & purchase[,'date'] < 20150201)


# 5번 이상 구매한 경우만, 구매한 것으로 계산
period.1$num <- 1
period.2$num <- 1
period.3$num <- 1

t_period.1 <- aggregate(num ~ userID+category3code, period.1, sum)
t_period.2 <- aggregate(num ~ userID+category3code, period.2, sum)
t_period.3 <- aggregate(num ~ userID+category3code, period.3, sum)

receipt.period.1 <- distinct(filter(t_period.1[,c(1,2)], t_period.1[,3] >= 5))
receipt.period.2 <- distinct(filter(t_period.2[,c(1,2)], t_period.2[,3] >= 5))
receipt.period.3 <- distinct(filter(t_period.3[,c(1,2)], t_period.3[,3] >= 1))

# 다 쓴 내용 정리
remove(period.1)
remove(period.2)
remove(period.3)
remove(t_period.1)
remove(t_period.2)
remove(t_period.3)
gc()


# 군집화된 유저 아이디에 맞춰 각 기간의 구매 데이터를 나눕니다
merge.receipt.clust <- function(clust, receipt){
  temp <- merge(clust, receipt, by = "userID")
  return(temp)
}

for(i in 1:periodNum){
  assign(paste0("receipt.clust.", i), lapply(clust.userID.list, merge.receipt.clust
                                                     , receipt = eval(parse(text = paste0("receipt.period.", i)))))
  remove(list = paste0("receipt.period.",i))
}

# 각 기간에 맞춘 구매 희소 행렬을 만듬
make.sparse <- function(p_frame, r_clust) { 
  result <- foreach(i = 1:length(p_frame),.packages = c('foreach')) %dopar% {
    for(j in 1:nrow(r_clust[[i]])){ 
      p_frame[[i]][as.character(r_clust[[i]][j,1]),r_clust[[i]][j,2]] <- 1
    }
    return(p_frame[[i]])
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
  for(i in 1:periodNum){
    assign(paste0("purchase.sparse.", i)
           , make.sparse(purchase.matrix.frame, eval(parse(text = paste0("receipt.clust.", i)))))
    remove(list = paste0("receipt.clust.",i))
  }
  
  # cluster stop
  stopCluster(cl)
})

# SAS에서 자카드 계산을 하기 위해 list인 purchase.sparse를 매트릭스로 분할하여 내보냅니다
for(i in 1:periodNum){ 
  for(j in 1:user.clust.num){
    temp.text <- paste0('export(purchase.sparse.', i, '[[', j, ']], "', dir.data, '/purchaseSparse', i, '_', j, '.sav")')
    eval(parse(text=temp.text))
  }
  remove(list = paste0("purchase.sparse.",i))
}

rm(list = ls(all=TRUE))
gc()
