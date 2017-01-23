# 사용되는 패키지들, 한번만 설치하면 됨
install.packages("foreign")     # sav 파일 import 가능하게 하는 foreign 라이브러리 존재
install.packages('rio')         # sav 파일 export 가능하게 해주는 rio 라이브러리 존재

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
  # 해쉬의 수 (signature의 개수)
  h = length(txt[[1]]$minhash)
  # 고객의 수
  d = length(txt)
  # row의 수 (band 안의 signature 의 수)  
  r = h / bands
  
  corpus=txt
  
  # data_list 에 담긴, minhash만 뽑아낸다.
  all_minhashes = lapply(corpus, function(z){
    z$minhash
  })

  # 고객 번호 저장. ex) 1~19383
  col_names = names(all_minhashes)
  
  b_assign=data.frame("band"=rep(rep(1:bands,each=r),d))
  
  # bucket 값을 구하기 전 전처리 과정
  buckets = all_minhashes %>%
    dplyr::as_data_frame()  %>%
    tidyr::gather_("doc", "hash", col_names) %>% 
    dplyr::bind_cols(b_assign) %>% 
    dplyr::group_by_(~doc,~band) 
  rm(b_assign)
  # 사용자의 band가 어떤 buckets 인지 hash값을 구해낸다.
  buckets = dplyr::summarise(buckets, buckets = digest(list(hash, unique(band)))) 
  
  # band 컬럼을 제외하고 select 한다.
  buckets = buckets %>%
    dplyr::select_(~-band) %>%
    dplyr::ungroup() 
  # 반환
  buckets
}

# 상품코드를 hash_string 으로 변경해주는 함수
hash_string = function(x) {
  .Call('textreuse_hash_string', PACKAGE = 'textreuse', x)
}

# candidates 들을 대상으로, (유사하다고 모아준 사람끼리 jaccard_similarity를 구해 대입한다.)
lsh_compare = function(candidates, corpus, f) {
  
  num_rows = nrow(candidates)
  # score 는 자카드 유사도이다.
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

# 상품코드를, hash string 으로 바꾼 후, min-hashing 한다.
lsh_corpus=function(txt){
  
  corpus_list=lapply(txt, function(x) {
    token= x
    # 상품 코드들을 token 으로 변경.
    hashes=hash_string(token)

    # 모든 token을 minhash 하여, signature가 설정한 n의 수만큼 생겨남. 
    minhashes=minhash(token)
    x1=list("tokens"=token,"hashes"=hashes,"minhash"=minhashes)
    x1
  })
  
  # 이름 설정
  names(corpus_list)=names(txt)

  #반환
  corpus_list
}

# 구매한 상품들의 이름을 data_to_list 에 저장하여 반환
preprocessing=function(data)
{
  # row 별로 구매이력이 1인 column의 이름들을 저장
  data_to_list=apply(data,1, function(x) {
    which(x!=0) %>% names()
  })
  names(data_to_list) = rownames(data)
  return (data_to_list)
}

# band수와 row수에 따라, 유사한 %에 따라, 사람들을 모아줄 확률이 몇 % 인지 시각화 하여 보여줌. 
probability_graph=function(bands,rows)
{
  x=seq(0.1, 1, 0.01)
  prob=1-(1-x^rows)^bands %>% round(., 4)
  plot(prob, type='l', xlab='similarity s of the two sets', ylab='probability of sharing a bucket', main=paste0('bands : ',bands, ' rows : ',rows))
  names(prob)=x
  return (prob[c(1,11,21,31,41,51,61,71,81,91)])
}

###########################################################################
# a 집단의 고객 ID들과 유사한 b를, a 집단으로 하는 b들을 반복적으로 찾아
# rbind 시켜 그룹의 수를 축소시키고, 사람들을 더 모아주는 함수.
# 하지만, jaccard 유사도가 0.7 이상인 사람들이 모이지 않을 경우 
# 전혀 유사하지 않은 반대 구매패턴의 사람들이 모일 수 있음.
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
# 함수 호출 부분
system.time({ 
  # minhash 발생 조건 설정(n = signature 수, seed는 random 함수 발생 값 )
  minhash = minhash_generator(n = 200, seed = 3552)
  band= 40
  probability_graph(band, 200/band)
  
  # 19383 : 4386 .. 구매 희소행렬 matrix를 읽어 대입한다.
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

# grouping test.. 쓸모 없다.
system.time({
  abc<-grouping(groups,final2)  
})

# pair 전부 다 확인 ( 모든 row )
utils::View(final)

# score 평균값
mean(final[["score"]])

# sav 파일로 export
export(final, "C:/R/final.sav")

#save.image('20170113.rdata')

#load('20170113.rdata')

#p(m | i,j,k) = <confidence>

