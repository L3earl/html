#메모리 비우기
rm(list=ls())

#패키지 설치
install.packages('data.table')
install.packages('dplyr')

#패키지 부착
library(data.table)
library(dplyr)

#데이터 로드
purchase <- fread("C:/R/purchase.txt") #구매이력
product <- fread("C:/R/product.txt") #상품
user <- fread("C:/R/user.txt") #고객정보

#구매이력 matrix 생성
matrix <- data.frame(matrix(0, ncol = 4386, nrow = 19383))
names<-as.vector(product[["소분류코드"]])
setnames(matrix,names) #컬럼 이름 설정

#사용할 데이터 전처리
purchase_temp <- purchase[,c("고객번호","소분류코드")]
setkey(purchase_temp,고객번호,소분류코드)
purchase_temp <- unique(purchase_temp) #중복 제거
purchase_temp <- as.data.frame(purchase_temp)

#실행
system.time({

#구매이력 표시
for(i in 1:nrow(purchase_temp)){
  x=purchase_temp[i,1] #고객번호
  y=purchase_temp[i,2] #상품코드
  set(matrix,x,y,1) #표시
}

})