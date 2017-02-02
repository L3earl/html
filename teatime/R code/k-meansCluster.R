##############################################################################
## 제휴사 A,B,C,D 에 대한 구매 빈도를 정규화하여, 
## k-means 적절 군집 수를 적절한 군집 개수 fviz_nbclust 함수를 사용하여 찾고,
## 군집을 나눠 준 후 총 구매 금액과 총 구매 상품 개수에 따라 
## k-means 군집을 다시 사용하여 세부 군집을 나눠주는 코드 입니다.
##############################################################################
library(dplyr)
library(rio)
library(ggplot2)
library("cluster")
library("fpc")
library("factoextra")
library("clValid")

# 데이터 불러오기 
rawdata<-import("C:/R/시나리오11파일.csv")
rawdata[is.na(rawdata)]<-0
rawdata<-transform(rawdata,총구매금액=as.integer(총구매금액))

# 제휴사에 관한 데이터만 추출하여 정규화
need.data<-rawdata[,c(2:5)]
scaled.data<-scale(need.data)
scaled.data<-as.data.frame(scaled.data)

# 적절한 군집의 수 찾기
fviz_nbclust(scaled.data, FUN=kmeans, method = "silhouette") # k=4

# 군집 생성
k.data<-kmeans(scaled.data,4)
k.data$size

# 군집화에 대한 평가
plot(silhouette(k.data$cluster,dist=dist(scaled.data)))

#군집별로 분류 제휴사 A=2군집(8000명),B=3군집(7000명),C=1군집(3880명),D=4(250명)군집 별 분류

clust<-k.data$cluster
sample<-data.frame(clust,rawdata)
rawdata_clusta<-filter(sample,clust==2)
rawdata_clustb<-filter(sample,clust==3)
rawdata_clustc<-filter(sample,clust==1)
rawdata_clustd<-filter(sample,clust==4)

# 군집 별로 A,B,C,D 제휴사에 대한 구매 빈도의 합을 확인할 수 있다.
cluster<-as.data.frame(matrix(nrow=0,ncol=4))
colnames(cluster)<-c("A구매","B구매","C구매","D구매")

cluster[1,1]<-sum(rawdata_clusta[,c("A")])
cluster[1,2]<-sum(rawdata_clusta[,c("B")])
cluster[1,3]<-sum(rawdata_clusta[,c("C")])
cluster[1,4]<-sum(rawdata_clusta[,c("D")])

cluster[2,1]<-sum(rawdata_clustb[,c("A")])
cluster[2,2]<-sum(rawdata_clustb[,c("B")])
cluster[2,3]<-sum(rawdata_clustb[,c("C")])
cluster[2,4]<-sum(rawdata_clustb[,c("D")])

cluster[3,1]<-sum(rawdata_clustc[,c("A")])
cluster[3,2]<-sum(rawdata_clustc[,c("B")])
cluster[3,3]<-sum(rawdata_clustc[,c("C")])
cluster[3,4]<-sum(rawdata_clustc[,c("D")])

cluster[4,1]<-sum(rawdata_clustd[,c("A")])
cluster[4,2]<-sum(rawdata_clustd[,c("B")])
cluster[4,3]<-sum(rawdata_clustd[,c("C")])
cluster[4,4]<-sum(rawdata_clustd[,c("D")])

utils::View(cluster)

#####################################################################
# 제휴사 A에 해당하는 군집을 세부 군집으로 나누고 시각화 하는 코드
#####################################################################

# 총 구매금액과 총 구매 상품개수를 정규화하는 과정
need.data_clusta<-rawdata_clusta[,c(7,8)]
scaled.data_clusta<-scale(need.data_clusta)
scaled.data_clusta[is.na(scaled.data_clusta)]<-0
scaled.data_clusta<-as.data.frame(scaled.data_clusta)
colnames(scaled.data_clusta)<-c("A_총구매금액","A_총구매상품개수")

# 주성분 기반 적절한 kmeans 개수 찾기
fviz_nbclust(scaled.data_clusta, FUN=kmeans, method = "silhouette") # k=3

k.data.A<-kmeans(scaled.data_clusta,3)
clust.A<-k.data.A$cluster
sample.A<-data.frame(clust.A,rawdata_clusta)

# 시각화를 위한 변수 전처리 과정
tempA<-cbind(sample.A,scaled.data_clusta)
tempA[,c("A_총구매금액")]<-round(tempA[,c("A_총구매금액")],2)
tempA[,c("A_총구매상품개수")]<-round(tempA[,c("A_총구매상품개수")],2)
tempA<-transform(tempA,clust.A=as.character(clust.A))

# 군집 A 산점도 시각화
ggplot(tempA, aes(x=A_총구매금액, y=A_총구매상품개수,fill=clust.A)) +
  geom_point(colour="black",shape=21 ,  size=2.8)+
  scale_fill_brewer(palette="Set1")+
  theme(text = element_text(size=30),axis.text.x = element_text(angle=0, hjust=1))

# 군집 데이터를 sav 파일로 추출하기 위한 변수 생성
clust3_1 <- filter(sample.A,clust.A==1)
clust3_2 <- filter(sample.A,clust.A==2)
clust3_3 <- filter(sample.A,clust.A==3)

#####################################################################
# 제휴사 B에 해당하는 군집을 세부 군집으로 나누고 시각화 하는 코드
#####################################################################

# 총 구매금액과 총 구매 상품개수를 정규화하는 과정
need.data_clustb<-rawdata_clustb[,c(7,8)]
scaled.data_clustb<-scale(need.data_clustb)
scaled.data_clustb[is.na(scaled.data_clustb)]<-0
scaled.data_clustb<-as.data.frame(scaled.data_clustb)
colnames(scaled.data_clustb)<-c("B_총구매금액","B_총구매상품개수")

#주성분 기반 적절한 kmeans 개수 찾기
fviz_nbclust(scaled.data_clustb, FUN=kmeans, method = "silhouette") # k=2

k.data.B<-kmeans(scaled.data_clustb,2)
clust.B<-k.data.B$cluster
sample.B<-data.frame(clust.B,rawdata_clustb)

# 시각화를 위한 변수 전처리 과정
tempB<-cbind(sample.B,scaled.data_clustb)
tempB[,c("B_총구매금액")]<-round(tempB[,c("B_총구매금액")],2)
tempB[,c("B_총구매상품개수")]<-round(tempB[,c("B_총구매상품개수")],2)
tempB<-transform(tempB,clust.B=as.character(clust.B))

# 군집 B 산점도 시각화 및 회귀 직선 확인
ggplot(tempB, aes(x=B_총구매금액, y=B_총구매상품개수,fill=clust.B)) +
  geom_point(colour="black",shape=21 ,  size=2.8)+
  scale_fill_brewer(palette="Set2")+
stat_smooth(method=lm, level=0.95)+
theme(text = element_text(size=30),axis.text.x = element_text(angle=0, hjust=1))

# 군집 데이터를 sav 파일로 추출하기 위한 변수 생성
clust1_1 <- filter(sample.B,clust.B==1)
clust1_2 <- filter(sample.B,clust.B==2)

#####################################################################
# 제휴사 C에 해당하는 군집을 세부 군집으로 나누고 시각화 하는 코드
#####################################################################

# 총 구매금액과 총 구매 상품개수를 정규화하는 과정
need.data_clustc<-rawdata_clustc[,c(7,8)]
scaled.data_clustc<-scale(need.data_clustc)
scaled.data_clustc<-as.data.frame(scaled.data_clustc)
colnames(scaled.data_clustc)<-c("C_총구매금액","C_총구매상품개수")

#주성분 기반 적절한 kmeans 개수 찾기
fviz_nbclust(scaled.data_clustc, FUN=kmeans, method = "silhouette") # k=2

k.data.C<-kmeans(scaled.data_clustc,2)
clust.C<-k.data.C$cluster
sample.C<-data.frame(clust.C,rawdata_clustc)

# 시각화를 위한 변수 전처리 과정
tempC<-cbind(sample.C,scaled.data_clustc)
tempC[,c("C_총구매금액")]<-round(tempC[,c("C_총구매금액")],2)
tempC[,c("C_총구매상품개수")]<-round(tempC[,c("C_총구매상품개수")],2)
tempC<-transform(tempC,clust.C=as.character(clust.C))

#시각화
ggplot(tempC, aes(x=C_총구매금액, y=C_총구매상품개수,fill=clust.C)) +
  geom_point(colour="black",shape=21 ,  size=2.8)+
  scale_fill_brewer(palette="Set3")+
theme(text = element_text(size=30),axis.text.x = element_text(angle=0, hjust=1))


# 군집 데이터를 sav 파일로 추출하기 위한 변수 생성
clust2_1 <- filter(sample.C,clust.C==1)
clust2_2 <- filter(sample.C,clust.C==2)

#####################################################################
# 군집 분류 작업이 끝난 데이터들을 sav 파일로 추출하는 코드
#####################################################################

# 협업 필터링 작업을 하기 위한 군집 데이터 가공

clust1_1<-data.frame(clust1_1[,c("고객번호")])
clust1_2<-data.frame(clust1_2[,c("고객번호")])
clust2_1<-data.frame(clust2_1[,c("고객번호")])
clust2_2<-data.frame(clust2_2[,c("고객번호")])
clust3_1<-data.frame(clust3_1[,c("고객번호")])
clust3_2<-data.frame(clust3_2[,c("고객번호")])
clust3_3<-data.frame(clust3_3[,c("고객번호")])
clust4_1<-data.frame(rawdata_clustd[,c("고객번호")])

# 컬럼 이름 설정
colnames(clust1_1)<-c("userID")
colnames(clust1_2)<-c("userID")
colnames(clust2_1)<-c("userID")
colnames(clust2_2)<-c("userID")
colnames(clust3_1)<-c("userID")
colnames(clust3_2)<-c("userID")
colnames(clust3_3)<-c("userID")
colnames(clust4_1)<-c("userID")

# sav 파일로 군집 데이터 추출
export(clust1_1,"C:/R/s011/clust1_1.sav")
export(clust1_2,"C:/R/s011/clust1_2.sav")
export(clust2_1,"C:/R/s011/clust2_1.sav")
export(clust2_2,"C:/R/s011/clust2_2.sav")
export(clust3_1,"C:/R/s011/clust3_1.sav")
export(clust3_2,"C:/R/s011/clust3_2.sav")
export(clust3_3,"C:/R/s011/clust3_3.sav")
export(clust4_1,"C:/R/s011/clust4_1.sav")

