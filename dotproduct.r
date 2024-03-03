# 点积距离
a <- c(2, 5, 6)
b <- c(4, 3, 2)


fun1<-function(a,b){
  sum(a*b)
}

fun1(a,b)

fun2<-function(a,b){
  a %*% b  
}

fun2(a,b)



resume<-data.frame(
  `姓名`=c('A','B','C'),
  `性别`=c('男','女','男'),
  `学历`=c('本科','硕士','本科'),
  `籍贯`=c('浙江','北京','上海'),
  `编程语言`=c('JAVA','R','JAVA'),
  `职位`=c('开发工程师','算法工程师','开发工程师')
)
resume

m1<-resume[,-1];m1
m2<-t(m1);m2


dot_similary<-function(df){
  m1<-df[,-1];m1
  m2<-t(m1);m2
  
  n<-nrow(m1)
  mm<-matrix(0,nrow=n,ncol=n)
  for(i in 1:n){
    for(j in 1:n){
      v<-length(which(m1[i,]==m2[,j]))    
      mm[i,j]<-v
    }
  }
  
  colnames(mm)<-df[,1]
  rownames(mm)<-df[,1]
  mm
}

dot_similary(resume)

row1<-paste(
  paste(paste(m1[1,],m2[,1], sep='*'),collapse = "+"),
  paste(paste(m1[1,],m2[,2], sep='*'),collapse = "+"),
  paste(paste(m1[1,],m2[,3], sep='*'),collapse = "+"),
  sep=","
)
row1

row2<-paste(
  paste(paste(m1[2,],m2[,1], sep='*'),collapse = "+"),
  paste(paste(m1[2,],m2[,2], sep='*'),collapse = "+"),
  paste(paste(m1[2,],m2[,3], sep='*'),collapse = "+"),
  sep=","
)
row2

row3<-paste(
  paste(paste(m1[3,],m2[,1], sep='*'),collapse = "+"),
  paste(paste(m1[3,],m2[,2], sep='*'),collapse = "+"),
  paste(paste(m1[3,],m2[,3], sep='*'),collapse = "+"),
  sep=","
)
row3