### options
options(java.parameters = "-Xmx16000m") # rJava 패키지의 heap 공간을 늘려줘야 xlsx라이브러리를 사용 가능 -Xmx메모리크기m

### 사용되는 패키지와 관련 라이브러리
#install.packages('doParallel')  # 하나의 CPU 에서 병렬처리
#install.packages('foreach') # 하나의 CPU 에서 병렬처리
#install.packages('foreign')  # SAS에서 만든 sav 형식의 파일을 import 할 수 있게 함
#install.packages('rio')   # 대용량 파일을 다양한 형식으로 import, export 지원. 단, SAS에서 만든 sav 파일은 읽어오지 못함
#install.packages('data.table')  # 큰 데이터를 탑색,연산,병합하는데 유용. 단, 희소행렬같은 데이터에는 적합하지 않은 듯 함
#install.packages("dplyr")  # 큰 데이터를 다룰때 속도가 빠름
#install.packages('itertools')  # 텍스트안에 숫자를 넣을 때 사용
#install.packages('gdata')  # 매트릭스를 벡터로 변환할 때 사용 했음
#install.packages('gtools')  # 문자와 숫자가 섞인 문장을 정렬함
#install.packages('xlsx')  # 엑셀 파일 하나에 여러 시트로 내보낼 때
#install.packages('scales') 
library(doParallel)
library(foreach)
library(foreign)
library(rio) 
library(data.table)
library(dplyr) 
library(itertools)
library(gdata) 
library(gtools)
library(rJava)
library(xlsx)


