#�޸� ����
rm(list=ls())

#��Ű�� ��ġ
install.packages('data.table')
install.packages('dplyr')

#��Ű�� ����
library(data.table)
library(dplyr)

#������ �ε�
purchase <- fread("C:/R/purchase.txt") #�����̷�
product <- fread("C:/R/product.txt") #��ǰ
user <- fread("C:/R/user.txt") #��������

#�����̷� matrix ����
matrix <- data.frame(matrix(0, ncol = 4386, nrow = 19383))
names<-as.vector(product[["�Һз��ڵ�"]])
setnames(matrix,names) #�÷� �̸� ����

#����� ������ ��ó��
purchase_temp <- purchase[,c("������ȣ","�Һз��ڵ�")]
setkey(purchase_temp,������ȣ,�Һз��ڵ�)
purchase_temp <- unique(purchase_temp) #�ߺ� ����
purchase_temp <- as.data.frame(purchase_temp)

#����
system.time({

#�����̷� ǥ��
for(i in 1:nrow(purchase_temp)){
  x=purchase_temp[i,1] #������ȣ
  y=purchase_temp[i,2] #��ǰ�ڵ�
  set(matrix,x,y,1) #ǥ��
}

})