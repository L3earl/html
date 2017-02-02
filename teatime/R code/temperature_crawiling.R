##############################################################################
## 기상청의 중분류 지역별 하루 평균 기온을 크롤링하여, 정규식을 이용하여 추출한 후 
## 대분류 지역별 평균 값을 구하고 해당 지역에 거주하는 고객의
## 구매 일자 당일 기온이 5도 이하인, 겨울에 발생한 구매이력을 추출해주는 코드 입니다.
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

# 기상청-기후자료-과거자료의 평균기온 데이터 크롤링 함수
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

        regex<-"평균기온:(-?[0-9]*.[0-9]*)"
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

# 구매 정보의 고객의 거주 지역과 구매일자를 확인하여 해당 날짜,지역의 기온을 구매이력에 대입한다.
check_temperature<-function(data, temper, user){
  # temper : 지역별 기온 데이터 , data : 구매 영수증 데이터 , user : 고객, 지역명 데이터
  
  for(i in 1:nrow(data)){
    u.index<-data[i,c("고객번호")] #고객번호
    d.index<-data[i,c("구매일자")] #구매일자

    loc<-user[u.index,2] # 지역명

    # 1 : 강원 , 2 : 경기 , 3 : 경남 , 4 : 경북 , 5 : 광주 , 6 : 대구 , 7 : 대전 , 8 : 부산
    # 9 : 서울 , 10 : 울산 , 11 : 인천 , 12 : 전남 , 13 : 전북 , 14 : 제주 , 15 : 충남
    # 16 : 충북 , 17 : 세종 , 18 : 전국
    
    if(loc=="강원"){
      loc<-1
    }else if(loc=="경기"){
      loc<-2
    }else if(loc=="경남"){
      loc<-3
    }else if(loc=="경북"){
      loc<-4
    }else if(loc=="광주"){
      loc<-5
    }else if(loc=="대구"){
      loc<-6
    }else if(loc=="대전"){
      loc<-7
    }else if(loc=="부산"){
      loc<-8
    }else if(loc=="서울"){
      loc<-9
    }else if(loc=="울산"){
      loc<-10
    }else if(loc=="인천"){
      loc<-11
    }else if(loc=="전남"){
      loc<-12
    }else if(loc=="전북"){
      loc<-13
    }else if(loc=="제주"){
      loc<-14
    }else if(loc=="충남"){
      loc<-15
    }else if(loc=="충북"){
      loc<-16
    }else if(loc=="세종"){
      loc<-17
    }else if(loc=="전국"){
      loc<-18
    }    
    
    var<-temper[loc,d.index]
    set(data,i,"기온",var) # 구매이력에 해당 날짜와 지역의 기온데이터 대입
  }  
  return(data)
}


#실행
system.time({
  
  # excel로 만들어 둔 matrix 읽어 오기
  # 기본 중분류 지역의 하루 평균 기온을 입력할 matrix 형태 
  rawdata<-fread("C:/R/s025/matrix.csv",header=TRUE)

  for(i in 4:ncol(rawdata)){
    rawdata[[i]] <- as.numeric(rawdata[[i]])
  }

  # 각 중분류 지역의 기온을 기상청 데이터 크롤링 하여 저장
  for(i in 1:nrow(rawdata)){
    rawdata[i,]<-crawling_temperature(rawdata[i,])
  }

  # 크롤링 끝난 데이터 추출하여 NA 값 제거( excel )
  export(rawdata,"c:/R/s025/기온데이터.csv")

  # 대분류 기준으로 평균 값 계산
  temper.data<-fread("C:/R/s025/기온데이터.csv",header=TRUE)
  temper.data<- temper.data %>%
    group_by(대분류) %>%
    summarise_each(funs(round(mean(., na.rm=TRUE),1)), -중분류,-stn)

  # 데이터 추출
  export(temper.data,"c:/R/s025/대분류기온.csv")

  # execel로 작업 한 전국 기온 평균 값 입력 후 다시 읽어오기
  temper.data<-as.data.frame(fread("C:/R/s025/대분류기온.csv",header=TRUE))
  
  #데이터 로드
  purchase <- as.data.frame(fread("C:/R/제3회 Big Data Competition-분석용데이터-02.구매상품TR.txt")) #구매이력
  product <- as.data.frame(fread("C:/R/제3회 Big Data Competition-분석용데이터-03.상품분류.txt")) #상품
  user<-as.data.frame(fread("C:/R/지역명 명시된 고객데이터.csv")) #고객정보
  user<-user[,c("고객번호","지역명")]

  # 매칭 될 겨울 구매 데이터 추출 ( 2014년~2015년 1,2,3,10,11,12월 )
  winter<-filter(purchase,(구매일자>=20140101 & 구매일자<=20140331) | (구매일자>=20141001 & 구매일자<=20141231) | (구매일자>=20150101 & 구매일자<=20150331) | (구매일자>=20151001 & 구매일자<=20151231))
  winter<-transform(winter,구매일자=as.character(구매일자))
  winter<-winter[,c("고객번호","소분류코드","구매일자")]

  # 지역명이 NA 에 해당하는 고객에게 전국 지역명 대입
  na.user<-filter(user,지역명=="")
  for(i in 1:nrow(na.user)){
    index<-na.user[i,1]
    user[index,2]<-"전국"
  }

  # 구매 데이터에, 기온 column 생성하여 cbind
  matrix <- data.frame(matrix(0, ncol = 4386, nrow = 19383))
  temper<-data.frame(matrix(0,ncol=1,nrow=nrow(winter)))
  colnames(temper)<-c("기온")
  winter<-cbind(winter,temper)
  winter<-transform(winter,기온=as.numeric(기온))
  
  # 구매데이터에 기온 값 대입
  winter<-check_temperature(winter, temper.data, user)

  # 기온값 입력된 전체 원본 데이터 추출
  export(winter,"c:/R/s025/all_purtem.sav")

  # 5도 이하의 구매이력만 추출
  winter<-filter(winter,기온<=5)
  winter<-winter[,c("고객번호","소분류코드")]
  colnames(winter)<-c("userID","category3code")
  export(winter,"c:/R/s025/5_purtem.sav")


})


