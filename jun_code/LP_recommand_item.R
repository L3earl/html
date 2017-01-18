# ���Ǵ� ��Ű����, �ѹ��� ��ġ�ϸ� ��
install.packages('doParallel')
install.packages('foreach')
install.packages('foreign')     # sav ���� import �����ϰ� �ϴ� foreign ���̺귯�� ����
install.packages('itertools')
install.packages('data.table')

# ���Ǵ� ���̺귯����
library(doParallel)
library(foreach)
library(foreign)
library(itertools)
library(data.table)

# �۾����� ����
setwd("F:/googledrive/temp")

#####################################
# ��õ ������- ����� ��� #
#####################################

# ����� ��� ��ȣ�� ��Ʈ������ �ҷ���
User.based.dist <- read.spss('Userbaseddist.sav', to.data.frame = TRUE)

max.order <- function(x, max.number = 5){  
  # �Լ� ������ �� dataframe�� ������ ����
  axis.dtfm <- data.frame(matrix(0, ncol = max.number, nrow = 1))
  value.dtfm <- data.frame(matrix(0, ncol = max.number, nrow = 1))  
  
  # ����ó�� foreach��, ������ ���� ����� data.table ���·� final�� rbind ��Ų��.
  final<-foreach(i = 1:nrow(x),.combine="rbind",.packages=c("foreach")) %dopar% {
    foreach(j = 1:max.number) %do% {
      temp.axis <- which.max(x[i,])
      temp.value <- max(x[i,])      

      x[i,temp.axis] <- 0
      
      axis.dtfm[1,j] <- temp.axis      # max ��ǥ�� ���� dataframe�� ����
      value.dtfm[1,j] <- temp.value 
    }
    # max ��ǥmax.number���� �� max.number���� cbind �Ѵ�. 
    data.frame(
      cbind(axis.dtfm[1,],value.dtfm[1,])
    )
  }

  # ��ȯ�� final �÷��� ����  
  setnames(final, c("a1","a2","a3","a4","a5","v1","v2","v3","v4","v5") )
  
  return(final)
}

# �Լ� ����
system.time({

# ���� ó�� cluster 4�� ���� �� ���
cl <- makeCluster(4)
registerDoParallel(cl)
getDoParWorkers()

# row���� max�� ��ġ�� ���� max.number ������ŭ �̾Ƴ�
recommand.user.based <- max.order(User.based.dist, 5)
str(recommand.user.based)

# cluster stop
stopCluster(cl)
})

# sav ���Ϸ� export
export(recommand.user.based, "F:/googledrive/temp/recommandUserBased.sav")

#####################################s
# ��õ ������- ������ ��� #
#####################################

# ����� ��� ��ȣ�� ��Ʈ������ �ҷ���
item.based.dist <- read.spss('ItemBasedDist.sav', to.data.frame = TRUE)

# �Լ� ����
recommand.item.based <- max.order(item.based.dist, 5)
str(recommand.item.based)

# sav ���Ϸ� export
export(recommand.item.based, "F:/googledrive/temp/recommandItemBased.sav")
