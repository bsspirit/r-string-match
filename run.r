setwd("C:/work/R/text/r-string-match")
rm(list=ls())

source("./baoli.r")
txt<-"ABCCADZABCCABBC"
pattern<-"ABCCABB"
baoli_match(txt,pattern,LOGGER=TRUE)
bad_match(txt,pattern,1,LOGGER=TRUE)
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