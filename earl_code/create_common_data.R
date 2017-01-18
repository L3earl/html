
# 라이브러리
library(dplyr)
library(rio)
library(data.table)

# 작업 폴더 지정
setwd("F:/googledrive/L.point 빅데이터/제3회 Big Data Competition-개인화상품추천")

### 공식 데이터를 SAS, R 두 프로그램에서 사용하기 좋은 *.sav 로 변경하는 과정
# 공식 데이터를 불러옴
user <- fread("제3회 Big Data Competition-분석용데이터-01.고객DEMO.txt")
purchase <- fread("제3회 Big Data Competition-분석용데이터-02.구매상품TR.txt")
product <- fread("제3회 Big Data Competition-분석용데이터-03.상품분류.txt") 
rival <- fread("제3회 Big Data Competition-분석용데이터-04.경쟁사이용.txt")
membership <- fread("제3회 Big Data Competition-분석용데이터-05.멤버십여부.txt")
channal <- fread("제3회 Big Data Competition-분석용데이터-06.채널이용.txt")

# sav로 저장
export(user, "F:/googledrive/L.point 빅데이터/scenario/common/user.sav")
export(purchase, "F:/googledrive/L.point 빅데이터/scenario/common/purchase.sav")
export(product, "F:/googledrive/L.point 빅데이터/scenario/common/product.sav")
export(rival, "F:/googledrive/L.point 빅데이터/scenario/common/rival.sav")
export(membership, "F:/googledrive/L.point 빅데이터/scenario/common/membership.sav")
export(channal, "F:/googledrive/L.point 빅데이터/scenario/common/channal.sav")
### 공식 데이터 변환 완료


# 상품 소분류 코드
product.category3 <- product[,4]
export(product.category3, "F:/googledrive/L.point 빅데이터/scenario/common/productCategory3.sav")

# make ordered.receipt (conclude userID & category3code)
temp <- filter(purchase[,c(6,5,8)])
sorted.receipt <- arrange(temp, temp[,1], temp[,2], temp[,3])
colnames(sorted.receipt) <- c("userID", "category3code", "date")
export(sorted.receipt, "F:/googledrive/L.point 빅데이터/scenario/common/sortedreceipt.sav")

