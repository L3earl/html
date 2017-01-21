### 사용되는 패키지와 관련 라이브러리

# 하나의 CPU 에서 병렬처리
install.packages('doParallel')  
library(doParallel)

install.packages('foreach')
library(foreach)

# SAS에서 만든 sav 형식의 파일을 import 할 수 있게 함
install.packages('foreign')
library(foreign)

# 대용량 파일을 다양한 형식으로 import, export 지원. 단, SAS에서 만든 sav 파일은 읽어오지 못함
install.packages('rio')         
library(rio) 

# 큰 데이터를 탑색,연산,병합하는데 유용. 단, 희소행렬같은 데이터에는 적합하지 않은 듯 함
install.packages('data.table')
library(data.table)

# 큰 데이터를 다룰때 속도가 빠름
install.packages("dplyr")
library(dplyr) 

# 텍스트안에 숫자를 넣을 때 사용
install.packages('itertools')
library(itertools)

# 매트릭스를 벡터로 변환할 때 사용 했음 (안 써도 될 듯 함)
install.packages('gdata')
library(gdata) 

# % 쓰려고 사용
install.packages("scales")
library(scales)
