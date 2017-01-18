ex<-read.csv(file.choose(),header=TRUE)
str(ex)
head(ex)

#데이터 표준화 

ex1<-scale(ex[c(2,3,4,5)])
head(ex1)

# With in group sum of squares 를 저장하기 위한 변수 생성 

wss <- 0 

# within group sum of squares값 저장 : 군집개수에 따른 within group sum of squares 를 wss에 저장한다.

for(i in 1 : 15)
  
{
  
  wss[i] <- sum(kmeans( ex1 , centers = i ) $ withinss)
  
} # endof for(i in 1 : 15)

# 시각화 : 군집개수에 따른 within group sum of squares값을 시각화한다. 

plot( 1:15 , wss , type = "b" , xlab = " Number of Clusters" , ylab = "Within group sum of squares")

#kmeans 군집분석 실시
s<-kmeans(ex1,4)
s
s$cluster

#군집 번호와  고객번호가 매칭되도록 data frame 형태로 만들었다.
clust<-s$cluster
head(clust)
custom<-ex$고객번호
install.packages("dplyr")
data<-data.frame(clust,custom)
head(data)

#filter라는 함수를 이용해서 군집별로 고객번호를 추출 하였다.
library(dplyr)
clust1<-filter(data,clust==1)
clust2<-filter(data,clust==2)
clust3<-filter(data,clust==3)
clust4<-filter(data,clust==4)

clust_1<-clust1$custom
clust_2<-clust2$custom
clust_3<-clust3$custom
clust_4<-clust4$custom

clust_1<-data.frame(clust1[,2])
clust_2<-data.frame(clust_2)
clust_3<-data.frame(clust_3)
clust_4<-data.frame(clust_4)

colnames(clust_1)<-c("userID") 
colnames(clust_2)<-c("userID") 
colnames(clust_3)<-c("userID") 
colnames(clust_4)<-c("userID") 

install.packages("rio")

str(clust_4)
library(rio)

export(clust_1,"c:/Download/clust-1scaled.sav")
export(clust_2,"c:/Download/clust-2scaled.sav")
export(clust_3,"c:/Download/clust-3scaled.sav")
export(clust_4,"c:/Download/clust-4scaled.sav")



