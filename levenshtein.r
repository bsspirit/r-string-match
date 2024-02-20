# Levenshtein 莱文斯坦距离
# 效率不高，仅学习用
# 解读：http://blog.fens.me/r-levenshtein-distance/
lev_dist<-function(txt1,txt2){
  len1<-nchar(txt1)
  len2<-nchar(txt2)
  
  if(len1==0) return(len2)
  if(len2==0) return(len1)
  
  # 插入消耗
  insert_cost <- lev_dist(txt1, substring(txt2,1,len2-1)) + 1;
  
  # 删除消耗
  delete_cost <- lev_dist(substring(txt1,1,len1-1), txt2) + 1;
  
  # 替换消耗
  cost<-ifelse(substring(txt1,len1,len1) == substring(txt2,len2,len2),0,1)
  replaces_cost <- lev_dist(substring(txt1,1,len1-1), substring(txt2,1,len2-1)) + cost;
  
  rs<-min(insert_cost,delete_cost,replaces_cost)
  print(paste("txt1:",txt1,"txt2:",txt2,"insert:",insert_cost,"delete:",delete_cost,"replaces:",replaces_cost))
  return(rs)
}

lev_dist("ab","a")
lev_dist("ab","abc")
lev_dist("house","rose")
lev_dist("horse","ros")
lev_dist("kitten","sitting")


# adist函数
adist("house","rose")
adist("horse","ros")
drop(attr(adist("horse", "rose", counts = TRUE), "counts"))
attr(adist("horse", "rose", counts = TRUE), "trafos")

attr(adist("horse", "rosxeee", counts = TRUE), "trafos")
drop(attr(adist("horse", "rosxeee", counts = TRUE), "counts"))
