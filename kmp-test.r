# https://www.xdull.cn/kmp.html
#https://www.ruanyifeng.com/blog/2013/05/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm.html

setwd("C:/work/R/text")
rm(list=ls())

# 暴力破解法
baoli_match<-function(txt,pattern,n=0,LOGGER=FALSE){
  rs<-data.frame(start=0,end=0)  # 默认返回值
  start<-0                       # 命中后在原始字符串的开始位置
  
  n1<-nchar(txt)                 # 原始字符串长度
  n2<-nchar(pattern)             # 匹配的字符串长度
  
  # 如果匹配的字符串为空，则返回
  if(n2==0) return(r2)          
  
  # 初始化变量索引，i用于原始字符串，j用于匹配字符串
  i<-j<-1
  # 用于计数，循环了多少次
  k<-0
  
  
  # 循环计算
  while(i<n1){
    
    # 打印日志
    if(LOGGER){  
      print(paste(i,j,substr(txt, i, i),substr(pattern, j, j),sep=","))  
      k<-k+1
    }
    
    # 匹配成功，同时移动
    if(substr(txt,i,i) == substr(pattern,j,j)){ 
      i<-i+1
      j<-j+1
    } else { # 匹配不成功，回到开始匹配的位置，重置i,j的索引
      i<-i-(j-1)  
      i<-i+1
      j<-1
    }
    
    # 把匹配到的，所有结果存到变量
    if(j==(n2+1)) {
      start<-append(start,i-n2)
      j<-1
      
      # 只取前面n个结果返回
      if(n>0 && n==length(start)-1){
        break;
      }
    }
  }
  
  # 打印日志，输出循环了多少次
  if(LOGGER){
    print(paste("k",k,sep="="))
  }
  
  # 拼接返回值，为data.frame
  if(length(start)>1) {
    start<-start[-1]
    rs<-data.frame(start=start,end=start+n2-1)
  }
  return(rs)
}

txt<-"ABCCADZABCCABBC"
pattern<-"ABCCABB"
baoli_match(txt,pattern,LOGGER=TRUE)
#bad_match(txt,pattern,1,LOGGER=TRUE)
#baoli_match(txt,"ABD",LOGGER=TRUE)
#baoli_match(txt,"ABD",1,LOGGER=TRUE)


kmp_match<-function(txt, pattern, n=0, LOGGER=FALSE) {
  rs<-data.frame(start=0,end=0)  # 默认返回值
  start<-0                       # 命中后在原始字符串的开始位置
  n1<-nchar(txt)                 # 原始字符串长度
  n2<-nchar(pattern)             # 匹配的字符串长度
  
  # 如果匹配的字符串为空，则返回
  if(n2==0) return(rs)
  
  nxt<-kmp_next(pattern)       #获取next最长公串
  if(LOGGER) print(paste("pattern:",pattern,",next:",paste(nxt,collapse = ""),sep=""))

  # 初始化变量索引，i用于原始字符串，j用于匹配字符串
  i<-j<-1
  # 用于计数，循环了多少次
  k<-0
  
  # 循环计算
  while(i<n1){
    
    # 打印日志
    if(LOGGER){
      print(paste(i,j,substr(txt, i, i),substr(pattern, j, j),sep=","))  
      k<-k+1
    }
    
    # 匹配成功，同时移动
    if(substr(txt,i,i) == substr(pattern,j,j) ){
      i=i+1
      j=j+1
    }else{  # 匹配不成功，利用最长公串，回到上个公串的位置，减少追个匹配
      if(j>1) j<-nxt[j-1]+1
      else i<-i+1
    }
    
    if(j==(n2+1)) {
      start<-append(start,i-n2)
      j<-1
      
      if(n>0 && n==length(start)-1){
        break;
      }
    }
  }
  
  # 打印日志，输出循环了多少次
  if(LOGGER){
    print(paste("k",k,sep="="))
  }
  
  # 拼接返回值，为data.frame
  if(length(start)>1) {
    start<-start[-1]
    rs<-data.frame(start=start,end=start+n2-1)
  }
  return(rs)
}

kmp_next <- function(pattern) {
  n <- nchar(pattern)
  nxt <- rep(0, n)
  i <- 2
  j <- 1
  while (i < n) {
    #print(paste(i,j,substr(pattern, i, i),substr(pattern, j, j),sep=","))  
    if (substr(pattern, i, i) == substr(pattern, j, j)) {
      nxt[i] <- j
      i = i + 1
      j = j + 1
    } else{
      if (j > 1) {
        j = nxt[j - 1]
      }
      else{
        nxt[i] = 0
        i = i + 1
      }
    }
  }
  nxt
}
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

library(stringr)
library(microbenchmark)
mybenchmark<-function(txt,pattern){
  microbenchmark(list = list(
    baoli = baoli_match(txt,pattern),
    kmp = kmp_match(txt,pattern),
    str = str_locate_all(txt,pattern)
  ), unit="ns", times = 200)
}

txt<-"ABCCADZABCCABBC"
pattern<-"ABCCABB"
str_locate_all(txt,pattern)
kmp_match(txt,pattern)
mybenchmark(txt,pattern)


txt2<-rep("ABCCADZABCCABBC",100) %>% paste(collapse = ",")
pattern2<-"ABCCABB"
mybenchmark(txt2,pattern2)

txt3<-readLines("文字.txt") %>% paste(collapse = ",")
str_length(txt3)
pattern3<-"查找文本串中所有匹配模式串的字符"
kmp = kmp_match(txt3,pattern3);kmp
kmp = kmp_match(txt3,pattern3,1);kmp
baoli = baoli_match(txt3,pattern3);baoli
str = str_locate_all(txt3,pattern3);str
mybenchmark(txt3,pattern3)



