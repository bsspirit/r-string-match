# http://blog.fens.me/r-hamming-distance/

# 字符串类型
haming_string<-function(txt1,txt2){
  len1<-nchar(txt1)
  len2<-nchar(txt2)
  
  if(len1!=len2){
    stop(paste("length is not equal"))
  }
  
  n<-0
  for(i in 1:len1){
    t1<-substring(txt1,i,i)
    t2<-substring(txt2,i,i)
    if(t1!=t2){
      n<-n+1
    }
  }
  return(n)
}

haming_string("1011101","1101001")
haming_string("2143896","2233796")
haming_string("toned","roses")

# 数字类型
haming_int<-function(num1,num2){
  b1<-intToBits(num1)
  b2<-intToBits(num2)
  x<-xor(b1,b2);x
  length(which(x==1))
}

haming_int(93,73)
haming_int(137,177)