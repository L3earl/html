# ���Ǵ� ��Ű����, �ѹ��� ��ġ�ϸ� ��
install.packages("foreign")     # sav ���� import �����ϰ� �ϴ� foreign ���̺귯�� ����
install.packages('rio')         # sav ���� export �����ϰ� ���ִ� rio ���̺귯�� ����

library(magrittr)
library(stringr)
library(slam)
library(doParallel)
library(digest)
library(dplyr)
library(foreach)
library(textreuse)
library(foreign)
library(rio)

options("scipen"=100, "digits"=4)
options(digits=8)

#####################################################################################################
lsh_2 = function(txt, bands) {
  # �ؽ��� �� (signature�� ����)
  h = length(txt[[1]]$minhash)
  # ������ ��
  d = length(txt)
  # row�� �� (band ���� signature �� ��)  
  r = h / bands
  
  corpus=txt
  
  # data_list �� ���, minhash�� �̾Ƴ���.
  all_minhashes = lapply(corpus, function(z){
    z$minhash
  })

  # ���� ��ȣ ����. ex) 1~19383
  col_names = names(all_minhashes)
  
  b_assign=data.frame("band"=rep(rep(1:bands,each=r),d))
  
  # bucket ���� ���ϱ� �� ��ó�� ����
  buckets = all_minhashes %>%
    dplyr::as_data_frame()  %>%
    tidyr::gather_("doc", "hash", col_names) %>% 
    dplyr::bind_cols(b_assign) %>% 
    dplyr::group_by_(~doc,~band) 
  rm(b_assign)
  # ������� band�� � buckets ���� hash���� ���س���.
  buckets = dplyr::summarise(buckets, buckets = digest(list(hash, unique(band)))) 
  
  # band �÷��� �����ϰ� select �Ѵ�.
  buckets = buckets %>%
    dplyr::select_(~-band) %>%
    dplyr::ungroup() 
  # ��ȯ
  buckets
}

# ��ǰ�ڵ带 hash_string ���� �������ִ� �Լ�
hash_string = function(x) {
  .Call('textreuse_hash_string', PACKAGE = 'textreuse', x)
}

# candidates ���� �������, (�����ϴٰ� ����� ������� jaccard_similarity�� ���� �����Ѵ�.)
lsh_compare = function(candidates, corpus, f) {
  
  num_rows = nrow(candidates)
  # score �� ��ī�� ���絵�̴�.
  score=rep(0,num_rows)
  for(i in 1:num_rows) {
    if (!is.na(candidates[i, "score"])) next()
    a = candidates$a[i]
    b = candidates$b[i]
    score[i] = f(corpus[[a]]$tokens, corpus[[b]]$tokens)
  }
  
  candidates[,"score"]= score
  
  attr(candidates, "all-doc-ids") = names(corpus)
  
  candidates
}

# ��ǰ�ڵ带, hash string ���� �ٲ� ��, min-hashing �Ѵ�.
lsh_corpus=function(txt){
  
  corpus_list=lapply(txt, function(x) {
    token= x
    # ��ǰ �ڵ���� token ���� ����.
    hashes=hash_string(token)

    # ��� token�� minhash �Ͽ�, signature�� ������ n�� ����ŭ ���ܳ�. 
    minhashes=minhash(token)
    x1=list("tokens"=token,"hashes"=hashes,"minhash"=minhashes)
    x1
  })
  
  # �̸� ����
  names(corpus_list)=names(txt)

  #��ȯ
  corpus_list
}

# ������ ��ǰ���� �̸��� data_to_list �� �����Ͽ� ��ȯ
preprocessing=function(data)
{
  # row ���� �����̷��� 1�� column�� �̸����� ����
  data_to_list=apply(data,1, function(x) {
    which(x!=0) %>% names()
  })
  names(data_to_list) = rownames(data)
  return (data_to_list)
}

# band���� row���� ����, ������ %�� ����, ������� ����� Ȯ���� �� % ���� �ð�ȭ �Ͽ� ������. 
probability_graph=function(bands,rows)
{
  x=seq(0.1, 1, 0.01)
  prob=1-(1-x^rows)^bands %>% round(., 4)
  plot(prob, type='l', xlab='similarity s of the two sets', ylab='probability of sharing a bucket', main=paste0('bands : ',bands, ' rows : ',rows))
  names(prob)=x
  return (prob[c(1,11,21,31,41,51,61,71,81,91)])
}

###########################################################################
# a ������ ���� ID��� ������ b��, a �������� �ϴ� b���� �ݺ������� ã��
# rbind ���� �׷��� ���� ��ҽ�Ű��, ������� �� ����ִ� �Լ�.
# ������, jaccard ���絵�� 0.7 �̻��� ������� ������ ���� ��� 
# ���� �������� ���� �ݴ� ���������� ������� ���� �� ����.
###########################################################################
grouping=function(group_list,data)
{
  groups<-list("")
  count = 1
  for(i in 1:length(group_list[[1]])){
    num <- group_list[[1]][i]
    group <- subset(data,select=c(b),subset=(a==num))
    if(nrow(group)==0) next()

    start = 1
    while(start<nrow(group)){
      temp1 <- group[start:nrow(group),]
      for(j in 1:nrow(temp1)){
        val = temp1[[1]][j]
        temp2 <- subset(data,select=c(b),subset=(a==val))
        group <- rbind(group,temp2)
        data <- data[!(data$a==val),]
      }
      group <- unique(group)
      start = start + nrow(temp1)
    }
    group <- rbind(group,num)
    data <- data[!(data$a==num),]

    groups[[count]]<-group
    count = count + 1
  }
  groups
}

######################################################################################################
# �Լ� ȣ�� �κ�
system.time({ 
  # minhash �߻� ���� ����(n = signature ��, seed�� random �Լ� �߻� �� )
  minhash = minhash_generator(n = 200, seed = 3552)
  band= 40
  probability_graph(band, 200/band)
  
  # 19383 : 4386 .. ���� ������ matrix�� �о� �����Ѵ�.
  matrix = read.spss(file.choose()) 
  matrix = as.data.frame(matrix)
  a2=preprocessing(matrix)
  gc(reset=T)
  
  corpus_list=lsh_corpus(a2)
  gc(reset=T)
  
  lsh_list=lsh_2(corpus_list,band=band) 
  
  candidates=lsh_candidates(lsh_list)
  
  final=lsh_compare(candidates, corpus_list, jaccard_similarity)
})

# grouping test.. ���� ����.
system.time({
  abc<-grouping(groups,final2)  
})

# pair ���� �� Ȯ�� ( ��� row )
utils::View(final)

# score ��հ�
mean(final[["score"]])

# sav ���Ϸ� export
export(final, "C:/R/final.sav")

#save.image('20170113.rdata')

#load('20170113.rdata')

#p(m | i,j,k) = <confidence>
