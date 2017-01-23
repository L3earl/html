install.packages('ggplot2')

library(ggplot2)

#---------------------------------------
# ������, qlot ���
# ���� : http://visualize.tistory.com/306
#---------------------------------------

str(mpg) #mpg ������ �Ӽ� Ȯ��
qplot(displ,hwy,data=mpg) #�� ������ �̿��Ͽ� �ð�ȭ
qplot(displ,hwy,data=mpg,color=drv) #���� �߰�
qplot(hwy,data = mpg,fill=drv) #�� �׷캰 ������׷��� ������ Ȯ��
qplot(displ, hwy, data = mpg, facets = . ~ drv) 
qplot(hwy, data = mpg, facets = . ~ drv, binwidth = 2)

#---------------------------------------
# ggplot2 ���ʹ���
# ���� : http://www.dodomira.com/2016/03/18/ggplot2-%EA%B8%B0%EC%B4%88/
#---------------------------------------

#�⺻���
#������ ������(data frame)
#����,ũ�� ���� ���� ���(aes)
#��,��,��� ���� �������� ���(geoms)
#����� ó�� ���(stats)
#aes���� ����� ������(scale)

#�ƹ� ��� ��µ��� ����. ����� �������� �ʾұ� ����.
g<-ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width))
g

#geom ��� �߰� point(��)
g+geom_point()

#aes ���� ��� : ����, ���, ũ�� ���� ����
g+geom_point(aes(color=Species, shape=Species), size=5)


#---------------------------------------
# ggplot2 �׷��� ���� ���� (colour setting)
# ���� : http://rfriend.tistory.com/87
#---------------------------------------

library(MASS)
str(Cars93)

# default colour ���� ���� ������ ���� ���� ��� ����Ʈ �׵θ� ����
g<-ggplot(Cars93, aes(x=Weight,y=MPG.highway))
g+geom_point(shape=21,size=6)

# �׵θ� �� ���� ���� (colour : line colour setting)
g<-ggplot(Cars93,aes(x=Weight, y=MPG.highway))
g+geom_point(shape=21,size=6,colour="blue")

# ������ �� ä��� ���� ���� (fill : inner colour fill-up)
g<-ggplot(Cars93, aes(x=Weight,y=MPG.highway))
g+geom_point(shape=21, size=6, fill="blue")

# ������ ������ ���ڿ� ���� ���� ���� : aes(fill = continues numeric variable)
g<-ggplot(Cars93, aes(x=Weight,y=MPG.highway, fill=Price))
g+geom_point(colour="grey",shape=21, size=6)

# ������ ������ ����/����(factor)�� ���� ���� ���� : aes(fill = categorical variable)
g<-ggplot(Cars93, aes(x=Weight, y=MPG.highway,fill=Cylinders))
g+geom_point(colour="grey",shape=21,size=6)

# Palette = Oranges �� ���� (pallet colour setting) : scale_fill_brewer()
g<-ggplot(Cars93,aes(x=Weight, y=MPG.highway, fill=Cylinders))
g+geom_point(colour="grey", shape=21, size=6)+scale_fill_brewer(palette="Oranges") 



