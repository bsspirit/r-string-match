setwd("C:/work/R/text/r-string-match")
rm(list=ls())

source("./baoli.r")
txt<-"ABCCADZABCCABBC"
pattern<-"ABCCABB"
baoli_match(txt,pattern,LOGGER=TRUE)
baoli_match(txt,pattern,1,LOGGER=TRUE)
baoli_match(txt,"ABD",LOGGER=TRUE)
baoli_match(txt,"ABD",1,LOGGER=TRUE)

source("./kmp.r")
kmp_next("ABC")
kmp_next("ABCCABB")

txt<-"ABCCADZABCCABBC"
pattern<-"ABCCABB"
kmp_match(txt,pattern,LOGGER=TRUE)
kmp_match(txt,'ABD',LOGGER=TRUE)

kmp_match(txt,"ABC")
kmp_match(txt,"ABX",2)
kmp_match(txt,"ABCCAB")
w1<-kmp_match(txt,"AB");w1
substr(txt,w1$start[1],w1$end[1])

txt2<-rep("ABCCADZABCCABBC",100) %>% paste(collapse = ",")
pattern2<-"ABCCABB"
kmp_match(txt2,pattern2)

txt3<-readLines("文字.txt") %>% paste(collapse = ",")
nchar(txt3)

library(stringr)
pattern3<-"查找文本串中所有匹配模式串的字符"
kmp = kmp_match(txt3,pattern3,1);kmp
baoli = baoli_match(txt3,pattern3,1);baoli
str = str_locate_all(txt3,pattern3);str
