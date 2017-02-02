##############################################################################
## ���޻� A,B,C,D �� ���� ���� �󵵸� ����ȭ�Ͽ�, 
## k-means ���� ���� ���� ������ ���� ���� fviz_nbclust �Լ��� ����Ͽ� ã��,
## ������ ���� �� �� �� ���� �ݾװ� �� ���� ��ǰ ������ ���� 
## k-means ������ �ٽ� ����Ͽ� ���� ������ �����ִ� �ڵ� �Դϴ�.
##############################################################################
library(dplyr)
library(rio)
library(ggplot2)
library("cluster")
library("fpc")
library("factoextra")
library("clValid")

# ������ �ҷ����� 
rawdata<-import("C:/R/�ó�����11����.csv")
rawdata[is.na(rawdata)]<-0
rawdata<-transform(rawdata,�ѱ��űݾ�=as.integer(�ѱ��űݾ�))

# ���޻翡 ���� �����͸� �����Ͽ� ����ȭ
need.data<-rawdata[,c(2:5)]
scaled.data<-scale(need.data)
scaled.data<-as.data.frame(scaled.data)

# ������ ������ �� ã��
fviz_nbclust(scaled.data, FUN=kmeans, method = "silhouette") # k=4

# ���� ����
k.data<-kmeans(scaled.data,4)
k.data$size

# ����ȭ�� ���� ��
plot(silhouette(k.data$cluster,dist=dist(scaled.data)))

#�������� �з� ���޻� A=2����(8000��),B=3����(7000��),C=1����(3880��),D=4(250��)���� �� �з�

clust<-k.data$cluster
sample<-data.frame(clust,rawdata)
rawdata_clusta<-filter(sample,clust==2)
rawdata_clustb<-filter(sample,clust==3)
rawdata_clustc<-filter(sample,clust==1)
rawdata_clustd<-filter(sample,clust==4)

# ���� ���� A,B,C,D ���޻翡 ���� ���� ���� ���� Ȯ���� �� �ִ�.
cluster<-as.data.frame(matrix(nrow=0,ncol=4))
colnames(cluster)<-c("A����","B����","C����","D����")

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
# ���޻� A�� �ش��ϴ� ������ ���� �������� ������ �ð�ȭ �ϴ� �ڵ�
#####################################################################

# �� ���űݾװ� �� ���� ��ǰ������ ����ȭ�ϴ� ����
need.data_clusta<-rawdata_clusta[,c(7,8)]
scaled.data_clusta<-scale(need.data_clusta)
scaled.data_clusta[is.na(scaled.data_clusta)]<-0
scaled.data_clusta<-as.data.frame(scaled.data_clusta)
colnames(scaled.data_clusta)<-c("A_�ѱ��űݾ�","A_�ѱ��Ż�ǰ����")

# �ּ��� ��� ������ kmeans ���� ã��
fviz_nbclust(scaled.data_clusta, FUN=kmeans, method = "silhouette") # k=3

k.data.A<-kmeans(scaled.data_clusta,3)
clust.A<-k.data.A$cluster
sample.A<-data.frame(clust.A,rawdata_clusta)

# �ð�ȭ�� ���� ���� ��ó�� ����
tempA<-cbind(sample.A,scaled.data_clusta)
tempA[,c("A_�ѱ��űݾ�")]<-round(tempA[,c("A_�ѱ��űݾ�")],2)
tempA[,c("A_�ѱ��Ż�ǰ����")]<-round(tempA[,c("A_�ѱ��Ż�ǰ����")],2)
tempA<-transform(tempA,clust.A=as.character(clust.A))

# ���� A ������ �ð�ȭ
ggplot(tempA, aes(x=A_�ѱ��űݾ�, y=A_�ѱ��Ż�ǰ����,fill=clust.A)) +
  geom_point(colour="black",shape=21 ,  size=2.8)+
  scale_fill_brewer(palette="Set1")+
  theme(text = element_text(size=30),axis.text.x = element_text(angle=0, hjust=1))

# ���� �����͸� sav ���Ϸ� �����ϱ� ���� ���� ����
clust3_1 <- filter(sample.A,clust.A==1)
clust3_2 <- filter(sample.A,clust.A==2)
clust3_3 <- filter(sample.A,clust.A==3)

#####################################################################
# ���޻� B�� �ش��ϴ� ������ ���� �������� ������ �ð�ȭ �ϴ� �ڵ�
#####################################################################

# �� ���űݾװ� �� ���� ��ǰ������ ����ȭ�ϴ� ����
need.data_clustb<-rawdata_clustb[,c(7,8)]
scaled.data_clustb<-scale(need.data_clustb)
scaled.data_clustb[is.na(scaled.data_clustb)]<-0
scaled.data_clustb<-as.data.frame(scaled.data_clustb)
colnames(scaled.data_clustb)<-c("B_�ѱ��űݾ�","B_�ѱ��Ż�ǰ����")

#�ּ��� ��� ������ kmeans ���� ã��
fviz_nbclust(scaled.data_clustb, FUN=kmeans, method = "silhouette") # k=2

k.data.B<-kmeans(scaled.data_clustb,2)
clust.B<-k.data.B$cluster
sample.B<-data.frame(clust.B,rawdata_clustb)

# �ð�ȭ�� ���� ���� ��ó�� ����
tempB<-cbind(sample.B,scaled.data_clustb)
tempB[,c("B_�ѱ��űݾ�")]<-round(tempB[,c("B_�ѱ��űݾ�")],2)
tempB[,c("B_�ѱ��Ż�ǰ����")]<-round(tempB[,c("B_�ѱ��Ż�ǰ����")],2)
tempB<-transform(tempB,clust.B=as.character(clust.B))

# ���� B ������ �ð�ȭ �� ȸ�� ���� Ȯ��
ggplot(tempB, aes(x=B_�ѱ��űݾ�, y=B_�ѱ��Ż�ǰ����,fill=clust.B)) +
  geom_point(colour="black",shape=21 ,  size=2.8)+
  scale_fill_brewer(palette="Set2")+
stat_smooth(method=lm, level=0.95)+
theme(text = element_text(size=30),axis.text.x = element_text(angle=0, hjust=1))

# ���� �����͸� sav ���Ϸ� �����ϱ� ���� ���� ����
clust1_1 <- filter(sample.B,clust.B==1)
clust1_2 <- filter(sample.B,clust.B==2)

#####################################################################
# ���޻� C�� �ش��ϴ� ������ ���� �������� ������ �ð�ȭ �ϴ� �ڵ�
#####################################################################

# �� ���űݾװ� �� ���� ��ǰ������ ����ȭ�ϴ� ����
need.data_clustc<-rawdata_clustc[,c(7,8)]
scaled.data_clustc<-scale(need.data_clustc)
scaled.data_clustc<-as.data.frame(scaled.data_clustc)
colnames(scaled.data_clustc)<-c("C_�ѱ��űݾ�","C_�ѱ��Ż�ǰ����")

#�ּ��� ��� ������ kmeans ���� ã��
fviz_nbclust(scaled.data_clustc, FUN=kmeans, method = "silhouette") # k=2

k.data.C<-kmeans(scaled.data_clustc,2)
clust.C<-k.data.C$cluster
sample.C<-data.frame(clust.C,rawdata_clustc)

# �ð�ȭ�� ���� ���� ��ó�� ����
tempC<-cbind(sample.C,scaled.data_clustc)
tempC[,c("C_�ѱ��űݾ�")]<-round(tempC[,c("C_�ѱ��űݾ�")],2)
tempC[,c("C_�ѱ��Ż�ǰ����")]<-round(tempC[,c("C_�ѱ��Ż�ǰ����")],2)
tempC<-transform(tempC,clust.C=as.character(clust.C))

#�ð�ȭ
ggplot(tempC, aes(x=C_�ѱ��űݾ�, y=C_�ѱ��Ż�ǰ����,fill=clust.C)) +
  geom_point(colour="black",shape=21 ,  size=2.8)+
  scale_fill_brewer(palette="Set3")+
theme(text = element_text(size=30),axis.text.x = element_text(angle=0, hjust=1))


# ���� �����͸� sav ���Ϸ� �����ϱ� ���� ���� ����
clust2_1 <- filter(sample.C,clust.C==1)
clust2_2 <- filter(sample.C,clust.C==2)

#####################################################################
# ���� �з� �۾��� ���� �����͵��� sav ���Ϸ� �����ϴ� �ڵ�
#####################################################################

# ���� ���͸� �۾��� �ϱ� ���� ���� ������ ����

clust1_1<-data.frame(clust1_1[,c("������ȣ")])
clust1_2<-data.frame(clust1_2[,c("������ȣ")])
clust2_1<-data.frame(clust2_1[,c("������ȣ")])
clust2_2<-data.frame(clust2_2[,c("������ȣ")])
clust3_1<-data.frame(clust3_1[,c("������ȣ")])
clust3_2<-data.frame(clust3_2[,c("������ȣ")])
clust3_3<-data.frame(clust3_3[,c("������ȣ")])
clust4_1<-data.frame(rawdata_clustd[,c("������ȣ")])

# �÷� �̸� ����
colnames(clust1_1)<-c("userID")
colnames(clust1_2)<-c("userID")
colnames(clust2_1)<-c("userID")
colnames(clust2_2)<-c("userID")
colnames(clust3_1)<-c("userID")
colnames(clust3_2)<-c("userID")
colnames(clust3_3)<-c("userID")
colnames(clust4_1)<-c("userID")

# sav ���Ϸ� ���� ������ ����
export(clust1_1,"C:/R/s011/clust1_1.sav")
export(clust1_2,"C:/R/s011/clust1_2.sav")
export(clust2_1,"C:/R/s011/clust2_1.sav")
export(clust2_2,"C:/R/s011/clust2_2.sav")
export(clust3_1,"C:/R/s011/clust3_1.sav")
export(clust3_2,"C:/R/s011/clust3_2.sav")
export(clust3_3,"C:/R/s011/clust3_3.sav")
export(clust4_1,"C:/R/s011/clust4_1.sav")
