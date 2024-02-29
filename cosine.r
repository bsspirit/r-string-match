# 余弦相似度

library(lsa)
vec1 = c( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
vec2 = c( 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0 )
cosine(vec1,vec2) 

df<-data.frame(vec1,vec2)
dist(df,method="euclidean")


txt1<-"我买了一个手机，又配了一个手机壳"
txt2<-"我有3年没有换新手机了，还是原来的手机壳"


library(jiebaR)
wk<-worker()
w1<-wk[txt1]
w2<-wk[txt2]
w1
w2
table(w1)
table(w2)

library(plyr)
library(magrittr)
dfm<-function(w1,w2){
  t1<-table(w1) %>% ldply
  t2<-table(w2) %>% ldply
  names(t1)<-c("seg","cnt1")
  names(t2)<-c("seg","cnt2")
  mm<-merge(t1,t2,by="seg",all=TRUE)
  mm$cnt1[which(is.na(mm$cnt1))]<-0
  mm$cnt2[which(is.na(mm$cnt2))]<-0
  list(dat=t(mm[,-1]),seg=mm$seg)
}

m<-dfm(w1,w2);m
cosine(m$dat[1,],m$dat[2,]) 

txt1<-"如果这两句话的用词越相似，它们的内容就应该越相似"
txt2<-"如果这两句话的内容越相似，它们的用词也应该越相似"
w1<-wk[txt1]
w2<-wk[txt2]
m<-dfm(w1,w2);m
cosine(m$dat[1,],m$dat[2,]) 

