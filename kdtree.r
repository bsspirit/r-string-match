####################
# http://blog.fens.me/r-kdtree/
##########################

#install.packages("less")

library(less)

data(iris)

dat<-iris[,1:2]
head(dat)

#create a kdtree
kdt <- KDTree$new(dat)
kdt

# generate some query points that are different to those used to create the kdtree
q_data <- iris[1:5,1:2] + array(rnorm(10)*0.1, dim=c(5,2))
q_data

# query the previously generated kdtree
res<-kdt$query(query_X = q_data, k = 2)
res

src<-data.frame(col1=dat[res$nn.idx[,1],1],col2=dat[res$nn.idx[,2],2])
src

# 三维
dat<-iris[,1:3]
head(dat)
kdt <- KDTree$new(dat)
kdt
q_data <- iris[1:5,1:3] + array(rnorm(10)*0.3, dim=c(5,2))
q_data
res<-kdt$query(query_X = q_data, k = 3)
res
