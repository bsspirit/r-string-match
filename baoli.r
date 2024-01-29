###################
# 暴力算法
# @author Zhangdan
###################

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

# RUN

#txt<-"ABCCADZABCCABBC"
#pattern<-"ABCCABB"
#baoli_match(txt,pattern,LOGGER=TRUE)
#bad_match(txt,pattern,1,LOGGER=TRUE)
#baoli_match(txt,"ABD",LOGGER=TRUE)
#baoli_match(txt,"ABD",1,LOGGER=TRUE)