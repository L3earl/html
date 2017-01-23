install.packages('ggplot2')

library(ggplot2)

#---------------------------------------
# 맛보기, qlot 사용
# 참조 : http://visualize.tistory.com/306
#---------------------------------------

str(mpg) #mpg 데이터 속성 확인
qplot(displ,hwy,data=mpg) #두 변수를 이용하여 시각화
qplot(displ,hwy,data=mpg,color=drv) #색상 추가
qplot(hwy,data = mpg,fill=drv) #각 그룹별 히스토그램의 분포를 확인
qplot(displ, hwy, data = mpg, facets = . ~ drv) 
qplot(hwy, data = mpg, facets = . ~ drv, binwidth = 2)

#---------------------------------------
# ggplot2 기초문법
# 참조 : http://www.dodomira.com/2016/03/18/ggplot2-%EA%B8%B0%EC%B4%88/
#---------------------------------------

#기본요소
#데이터 프레임(data frame)
#색상,크기 같은 외적 요소(aes)
#점,선,모양 같은 기하학적 요소(geoms)
#통계적 처리 방법(stats)
#aes에서 사용할 스케일(scale)

#아무 모양 출력되지 않음. 모양을 정해주지 않았기 때문.
g<-ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width))
g

#geom 요소 추가 point(점)
g+geom_point()

#aes 미적 요소 : 색상, 모양, 크기 조절 가능
g+geom_point(aes(color=Species, shape=Species), size=5)


#---------------------------------------
# ggplot2 그래프 색상 설정 (colour setting)
# 참조 : http://rfriend.tistory.com/87
#---------------------------------------

library(MASS)
str(Cars93)

# default colour 색상 설정 별도로 하지 않은 경우 디폴트 테두리 검정
g<-ggplot(Cars93, aes(x=Weight,y=MPG.highway))
g+geom_point(shape=21,size=6)

# 테두리 선 색깔 지정 (colour : line colour setting)
g<-ggplot(Cars93,aes(x=Weight, y=MPG.highway))
g+geom_point(shape=21,size=6,colour="blue")

# 도형의 속 채우기 색깔 지정 (fill : inner colour fill-up)
g<-ggplot(Cars93, aes(x=Weight,y=MPG.highway))
g+geom_point(shape=21, size=6, fill="blue")

# 연속형 변수의 숫자에 따른 색깔 조절 : aes(fill = continues numeric variable)
g<-ggplot(Cars93, aes(x=Weight,y=MPG.highway, fill=Price))
g+geom_point(colour="grey",shape=21, size=6)

# 범주형 변수의 범주/요인(factor)에 따른 색깔 조절 : aes(fill = categorical variable)
g<-ggplot(Cars93, aes(x=Weight, y=MPG.highway,fill=Cylinders))
g+geom_point(colour="grey",shape=21,size=6)

# Palette = Oranges 색 설정 (pallet colour setting) : scale_fill_brewer()
g<-ggplot(Cars93,aes(x=Weight, y=MPG.highway, fill=Cylinders))
g+geom_point(colour="grey", shape=21, size=6)+scale_fill_brewer(palette="Oranges") 




