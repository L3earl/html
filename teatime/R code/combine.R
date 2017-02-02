######################################################################
## 2�Ⱓ�� ���� ������ �������, ��ǰ�� ��õ���� ���� ������� ���� ���ܵ��
## �ܿ� ���� ������ ������� ��ǰ�� ��õ���� �� ������� ���� ���ܵ���
## ���� �����Ͽ� �������ִ� �۾��� �ڵ� �Դϴ�.
######################################################################

library(data.table)
library(dplyr)

# �ܿ� ������ ���, 2�� ��ü ������ ��� 2015�� 1�� ��õ ����� ���� �ε� 
result.winterBased<-as.data.frame(fread("C:/R/userAccuracy43.csv"))
result.twoYearsBased<-as.data.frame(fread("C:/R/userAccuracy52.csv"))

winterBased<-result.winterBased[,c("V1","V12")]
twoYearsBased<-result.twoYearsBased[,c("V1","V12")]

diff<-winterBased
colnames(diff)<-c("������ȣ","���������")

# �ܿ� ������ ��� top 10 ���������, 2�Ⱓ�� ��ü ������ ��� top 10 ������� ���� �۾�
for(i in 1:19383){
  diff[i,2]<-winterBased[i,2]-twoYearsBased[i,2] 
}

diff<-arrange(diff,desc(���������))
utils::View(diff)

# ����� ���̰� ������ ���ܰ�, ������ �������� �з�
increase.user<-filter(diff,���������>0)
others<-filter(diff,���������<=0)

increase.user<-transform(increase.user,������ȣ=as.integer(������ȣ))
others<-transform(others,������ȣ=as.integer(������ȣ))

increase.user<-arrange(increase.user,������ȣ)
others<-arrange(others,������ȣ)

######################################################################
## ���� ��õ�� �������� �Һз� �ڵ带 �����ϴ� �۾� 
######################################################################

# �ܿ� ������ ��� ��õ ������ ���ϰ�, 2�� ��ü ������ ��� ��õ ������ ���� �ε�
# 059 - �ܿ� , 058 - 2�� ��ü
suggestItem.winterBased<-as.data.frame(fread("C:/R/s059/suggest.item.code.csv"))
suggestItem.twoYearsBased<-as.data.frame(fread("C:/R/s058/suggest.item.code.csv"))

# ��õ ������ �� top10 ���� ����
suggestItem.winterBased<-suggestItem.winterBased[,1:11]
suggestItem.twoYearsBased<-suggestItem.twoYearsBased[,1:11]

# �ܿ� ������ ��� ���� ��õ ������ ��, ����� ���� ������ row ���� �����ϴ� �۾�
index<-1
final.suggestItem.winterBased<-as.data.frame(matrix(nrow=0,ncol=11))
colnames(final.suggestItem.winterBased)<-colnames(suggestItem.winterBased)

for(i in 1:nrow(suggestItem.winterBased)){
  if(increase.user[index,c("������ȣ")]==suggestItem.winterBased[i,c("userID")]){
    final.suggestItem.winterBased<-rbind(final.suggestItem.winterBased,suggestItem.winterBased[i,])
    index<-index+1
  }
}

# 2�� ��ü ������ ��� ���� ��õ ������ ��, ������ ������ row ���� �����ϴ� �۾�
index<-1
final.suggestItem.twoYearsBased<-as.data.frame(matrix(nrow=0,ncol=11))
colnames(final.suggestItem.twoYearsBased)<-colnames(suggestItem.twoYearsBased)

for(i in 1:nrow(suggestItem.twoYearsBased)){
  if(others[index,c("������ȣ")]==suggestItem.twoYearsBased[i,c("userID")]){
    final.suggestItem.twoYearsBased<-rbind(final.suggestItem.twoYearsBased,suggestItem.twoYearsBased[i,])
    index<-index+1
  }
}

# ���� ��õ ������ ������ ��ģ �� ����
final<-rbind(final.suggestItem.winterBased,final.suggestItem.twoYearsBased)
final<-arrange(final,userID)

# ������ ����
export(final,"C:/R/final.suggestItem.code.csv")

######################################################################
## ���� ��õ�� �������� �Һз� ���� �����ϴ� �۾� 
######################################################################

# �ܿ� ������ ��� ��õ ������ ���ϰ�, 2�� ��ü ������ ��� ��õ ������ ���� �ε�
# 059 - �ܿ� , 058 - 2�� ��ü
suggestName.winterBased<-as.data.frame(fread("C:/R/s059/suggest.item.name.csv"))
suggestName.twoYearsBased<-as.data.frame(fread("C:/R/s058/suggest.item.name.csv"))

# ��õ ������ �� top10 ���� ����
suggestName.winterBased<-suggestName.winterBased[,1:11]
suggestName.twoYearsBased<-suggestName.twoYearsBased[,1:11]

# �ܿ� ������ ��� ���� ��õ ������ ��, ����� ���� ������ row ���� �����ϴ� �۾�
index<-1
final.suggestName.winterBased<-as.data.frame(matrix(nrow=0,ncol=11))
colnames(final.suggestName.winterBased)<-colnames(suggestName.winterBased)

for(i in 1:nrow(suggestName.winterBased)){
  if(increase.user[index,c("������ȣ")]==suggestName.winterBased[i,c("userID")]){
    final.suggestName.winterBased<-rbind(final.suggestName.winterBased,suggestName.winterBased[i,])
    index<-index+1
  }
}

# 2�� ��ü ������ ��� ���� ��õ ������ ��, ������ ������ row ���� �����ϴ� �۾�
index<-1
final.suggestName.twoYearsBased<-as.data.frame(matrix(nrow=0,ncol=11))
colnames(final.suggestName.twoYearsBased)<-colnames(suggestName.twoYearsBased)

for(i in 1:nrow(suggestName.twoYearsBased)){
  if(others[index,c("������ȣ")]==suggestName.twoYearsBased[i,c("userID")]){
    final.suggestName.twoYearsBased<-rbind(final.suggestName.twoYearsBased,suggestName.twoYearsBased[i,])
    index<-index+1
  }
}

# ���� ��õ ������ ������ ��ģ �� ����
final.name<-rbind(final.suggestName.winterBased,final.suggestName.twoYearsBased)
final.name<-arrange(final.name,userID)

# ������ ����
export(final.name,"C:/R/final.suggestItem.name.csv")





