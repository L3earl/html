######################################################################
## 2년간의 구매 데이터 기반으로, 상품을 추천했을 때의 정답률이 높은 집단들과
## 겨울 구매 데이터 기반으로 상품을 추천했을 때 정답률이 높은 집단들을
## 각각 추출하여 조합해주는 작업의 코드 입니다.
######################################################################

library(data.table)
library(dplyr)

# 겨울 데이터 기반, 2년 전체 데이터 기반 2015년 1월 추천 정답률 파일 로드 
result.winterBased<-as.data.frame(fread("C:/R/userAccuracy43.csv"))
result.twoYearsBased<-as.data.frame(fread("C:/R/userAccuracy52.csv"))

winterBased<-result.winterBased[,c("V1","V12")]
twoYearsBased<-result.twoYearsBased[,c("V1","V12")]

diff<-winterBased
colnames(diff)<-c("고객번호","정답률차이")

# 겨울 데이터 기반 top 10 정답률에서, 2년간의 전체 데이터 기반 top 10 정답률을 빼는 작업
for(i in 1:19383){
  diff[i,2]<-winterBased[i,2]-twoYearsBased[i,2] 
}

diff<-arrange(diff,desc(정답률차이))
utils::View(diff)

# 정답률 차이가 증가한 집단과, 나머지 집단으로 분류
increase.user<-filter(diff,정답률차이>0)
others<-filter(diff,정답률차이<=0)

increase.user<-transform(increase.user,고객번호=as.integer(고객번호))
others<-transform(others,고객번호=as.integer(고객번호))

increase.user<-arrange(increase.user,고객번호)
others<-arrange(others,고객번호)

######################################################################
## 최종 추천할 아이템의 소분류 코드를 추출하는 작업 
######################################################################

# 겨울 데이터 기반 추천 아이템 파일과, 2년 전체 데이터 기반 추천 아이템 파일 로드
# 059 - 겨울 , 058 - 2년 전체
suggestItem.winterBased<-as.data.frame(fread("C:/R/s059/suggest.item.code.csv"))
suggestItem.twoYearsBased<-as.data.frame(fread("C:/R/s058/suggest.item.code.csv"))

# 추천 아이템 중 top10 까지 추출
suggestItem.winterBased<-suggestItem.winterBased[,1:11]
suggestItem.twoYearsBased<-suggestItem.twoYearsBased[,1:11]

# 겨울 데이터 기반 최종 추천 아이템 중, 정답률 증가 집단의 row 만을 추출하는 작업
index<-1
final.suggestItem.winterBased<-as.data.frame(matrix(nrow=0,ncol=11))
colnames(final.suggestItem.winterBased)<-colnames(suggestItem.winterBased)

for(i in 1:nrow(suggestItem.winterBased)){
  if(increase.user[index,c("고객번호")]==suggestItem.winterBased[i,c("userID")]){
    final.suggestItem.winterBased<-rbind(final.suggestItem.winterBased,suggestItem.winterBased[i,])
    index<-index+1
  }
}

# 2년 전체 데이터 기반 최종 추천 아이템 중, 나머지 집단의 row 만을 추출하는 작업
index<-1
final.suggestItem.twoYearsBased<-as.data.frame(matrix(nrow=0,ncol=11))
colnames(final.suggestItem.twoYearsBased)<-colnames(suggestItem.twoYearsBased)

for(i in 1:nrow(suggestItem.twoYearsBased)){
  if(others[index,c("고객번호")]==suggestItem.twoYearsBased[i,c("userID")]){
    final.suggestItem.twoYearsBased<-rbind(final.suggestItem.twoYearsBased,suggestItem.twoYearsBased[i,])
    index<-index+1
  }
}

# 최종 추천 아이템 데이터 합친 후 정렬
final<-rbind(final.suggestItem.winterBased,final.suggestItem.twoYearsBased)
final<-arrange(final,userID)

# 데이터 추출
export(final,"C:/R/final.suggestItem.code.csv")

######################################################################
## 최종 추천할 아이템의 소분류 명을 추출하는 작업 
######################################################################

# 겨울 데이터 기반 추천 아이템 파일과, 2년 전체 데이터 기반 추천 아이템 파일 로드
# 059 - 겨울 , 058 - 2년 전체
suggestName.winterBased<-as.data.frame(fread("C:/R/s059/suggest.item.name.csv"))
suggestName.twoYearsBased<-as.data.frame(fread("C:/R/s058/suggest.item.name.csv"))

# 추천 아이템 중 top10 까지 추출
suggestName.winterBased<-suggestName.winterBased[,1:11]
suggestName.twoYearsBased<-suggestName.twoYearsBased[,1:11]

# 겨울 데이터 기반 최종 추천 아이템 중, 정답률 증가 집단의 row 만을 추출하는 작업
index<-1
final.suggestName.winterBased<-as.data.frame(matrix(nrow=0,ncol=11))
colnames(final.suggestName.winterBased)<-colnames(suggestName.winterBased)

for(i in 1:nrow(suggestName.winterBased)){
  if(increase.user[index,c("고객번호")]==suggestName.winterBased[i,c("userID")]){
    final.suggestName.winterBased<-rbind(final.suggestName.winterBased,suggestName.winterBased[i,])
    index<-index+1
  }
}

# 2년 전체 데이터 기반 최종 추천 아이템 중, 나머지 집단의 row 만을 추출하는 작업
index<-1
final.suggestName.twoYearsBased<-as.data.frame(matrix(nrow=0,ncol=11))
colnames(final.suggestName.twoYearsBased)<-colnames(suggestName.twoYearsBased)

for(i in 1:nrow(suggestName.twoYearsBased)){
  if(others[index,c("고객번호")]==suggestName.twoYearsBased[i,c("userID")]){
    final.suggestName.twoYearsBased<-rbind(final.suggestName.twoYearsBased,suggestName.twoYearsBased[i,])
    index<-index+1
  }
}

# 최종 추천 아이템 데이터 합친 후 정렬
final.name<-rbind(final.suggestName.winterBased,final.suggestName.twoYearsBased)
final.name<-arrange(final.name,userID)

# 데이터 추출
export(final.name,"C:/R/final.suggestItem.name.csv")






