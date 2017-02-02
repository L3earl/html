##############################################################################
## ���û�� �ߺз� ������ �Ϸ� ��� ����� ũ�Ѹ��Ͽ�, ���Խ��� �̿��Ͽ� ������ �� 
## ��з� ������ ��� ���� ���ϰ� �ش� ������ �����ϴ� ������
## ���� ���� ���� ����� 5�� ������, �ܿ￡ �߻��� �����̷��� �������ִ� �ڵ� �Դϴ�.
##############################################################################
install.packages('XML')
install.packages('stringr')
install.packages('gsubfn')
install.packages('rio')
install.packages('data.table')

library(XML)
library(stringr)
library(gsubfn)
library(rio)
library(dplyr)
library(data.table)

# ���û-�����ڷ�-�����ڷ��� ��ձ�� ������ ũ�Ѹ� �Լ�
# http://www.kma.go.kr/weather/climate/past_cal.jsp

crawling_temperature<-function(data){
  stn<-data[,c("stn")]
  year<-list("2014","2015")
  month<-list("1","2","3","10","11","12")
  
  for(i in 1:length(stn)){
    for(j in 1:length(year)){
      for(k in 1:length(month)){
        url<-sprintf("http://www.kma.go.kr/weather/climate/past_cal.jsp?stn=%1$s&yy=%2$s&mm=%3$s&obs=1&x=9&y=14",stn[[i]],year[[j]],month[[k]])        
        html <- readLines(url, encoding = "euc-kr")                

        regex<-"��ձ��:(-?[0-9]*.[0-9]*)"
        temp<-strapplyc(html,regex)
        
        date<-1
        for(m in 1:length(temp)){
          if(length(temp[[m]])==1){
            com.m<-as.integer(month[[k]])              
            com.d<-as.integer(date)

            if(com.m<10 & com.d<10){ 
              col<-sprintf("%1$s0%2$s0%3$s",year[[j]],month[[k]],date)      
            }else if(com.m<10){
              col<-sprintf("%1$s0%2$s%3$s",year[[j]],month[[k]],date)
            }else if(com.d<10){
              col<-sprintf("%1$s%2$s0%3$s",year[[j]],month[[k]],date)
            }else{
              col<-sprintf("%1$s%2$s%3$s",year[[j]],month[[k]],date)
            }

            set(data,i,col,temp[[m]])
            date<-date+1
          }  
        }
      } 
    }
  }
  
  return(data)
}

# ���� ������ ������ ���� ������ �������ڸ� Ȯ���Ͽ� �ش� ��¥,������ ����� �����̷¿� �����Ѵ�.
check_temperature<-function(data, temper, user){
  # temper : ������ ��� ������ , data : ���� ������ ������ , user : ����, ������ ������
  
  for(i in 1:nrow(data)){
    u.index<-data[i,c("������ȣ")] #������ȣ
    d.index<-data[i,c("��������")] #��������

    loc<-user[u.index,2] # ������

    # 1 : ���� , 2 : ��� , 3 : �泲 , 4 : ��� , 5 : ���� , 6 : �뱸 , 7 : ���� , 8 : �λ�
    # 9 : ���� , 10 : ��� , 11 : ��õ , 12 : ���� , 13 : ���� , 14 : ���� , 15 : �泲
    # 16 : ��� , 17 : ���� , 18 : ����
    
    if(loc=="����"){
      loc<-1
    }else if(loc=="���"){
      loc<-2
    }else if(loc=="�泲"){
      loc<-3
    }else if(loc=="���"){
      loc<-4
    }else if(loc=="����"){
      loc<-5
    }else if(loc=="�뱸"){
      loc<-6
    }else if(loc=="����"){
      loc<-7
    }else if(loc=="�λ�"){
      loc<-8
    }else if(loc=="����"){
      loc<-9
    }else if(loc=="���"){
      loc<-10
    }else if(loc=="��õ"){
      loc<-11
    }else if(loc=="����"){
      loc<-12
    }else if(loc=="����"){
      loc<-13
    }else if(loc=="����"){
      loc<-14
    }else if(loc=="�泲"){
      loc<-15
    }else if(loc=="���"){
      loc<-16
    }else if(loc=="����"){
      loc<-17
    }else if(loc=="����"){
      loc<-18
    }    
    
    var<-temper[loc,d.index]
    set(data,i,"���",var) # �����̷¿� �ش� ��¥�� ������ ��µ����� ����
  }  
  return(data)
}


#����
system.time({
  
  # excel�� ����� �� matrix �о� ����
  # �⺻ �ߺз� ������ �Ϸ� ��� ����� �Է��� matrix ���� 
  rawdata<-fread("C:/R/s025/matrix.csv",header=TRUE)

  for(i in 4:ncol(rawdata)){
    rawdata[[i]] <- as.numeric(rawdata[[i]])
  }

  # �� �ߺз� ������ ����� ���û ������ ũ�Ѹ� �Ͽ� ����
  for(i in 1:nrow(rawdata)){
    rawdata[i,]<-crawling_temperature(rawdata[i,])
  }

  # ũ�Ѹ� ���� ������ �����Ͽ� NA �� ����( excel )
  export(rawdata,"c:/R/s025/��µ�����.csv")

  # ��з� �������� ��� �� ���
  temper.data<-fread("C:/R/s025/��µ�����.csv",header=TRUE)
  temper.data<- temper.data %>%
    group_by(��з�) %>%
    summarise_each(funs(round(mean(., na.rm=TRUE),1)), -�ߺз�,-stn)

  # ������ ����
  export(temper.data,"c:/R/s025/��з����.csv")

  # execel�� �۾� �� ���� ��� ��� �� �Է� �� �ٽ� �о����
  temper.data<-as.data.frame(fread("C:/R/s025/��з����.csv",header=TRUE))
  
  #������ �ε�
  purchase <- as.data.frame(fread("C:/R/��3ȸ Big Data Competition-�м��뵥����-02.���Ż�ǰTR.txt")) #�����̷�
  product <- as.data.frame(fread("C:/R/��3ȸ Big Data Competition-�м��뵥����-03.��ǰ�з�.txt")) #��ǰ
  user<-as.data.frame(fread("C:/R/������ ���õ� ����������.csv")) #��������
  user<-user[,c("������ȣ","������")]

  # ��Ī �� �ܿ� ���� ������ ���� ( 2014��~2015�� 1,2,3,10,11,12�� )
  winter<-filter(purchase,(��������>=20140101 & ��������<=20140331) | (��������>=20141001 & ��������<=20141231) | (��������>=20150101 & ��������<=20150331) | (��������>=20151001 & ��������<=20151231))
  winter<-transform(winter,��������=as.character(��������))
  winter<-winter[,c("������ȣ","�Һз��ڵ�","��������")]

  # �������� NA �� �ش��ϴ� �������� ���� ������ ����
  na.user<-filter(user,������=="")
  for(i in 1:nrow(na.user)){
    index<-na.user[i,1]
    user[index,2]<-"����"
  }

  # ���� �����Ϳ�, ��� column �����Ͽ� cbind
  matrix <- data.frame(matrix(0, ncol = 4386, nrow = 19383))
  temper<-data.frame(matrix(0,ncol=1,nrow=nrow(winter)))
  colnames(temper)<-c("���")
  winter<-cbind(winter,temper)
  winter<-transform(winter,���=as.numeric(���))
  
  # ���ŵ����Ϳ� ��� �� ����
  winter<-check_temperature(winter, temper.data, user)

  # ��°� �Էµ� ��ü ���� ������ ����
  export(winter,"c:/R/s025/all_purtem.sav")

  # 5�� ������ �����̷¸� ����
  winter<-filter(winter,���<=5)
  winter<-winter[,c("������ȣ","�Һз��ڵ�")]
  colnames(winter)<-c("userID","category3code")
  export(winter,"c:/R/s025/5_purtem.sav")


})

