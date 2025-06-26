################################
# http://blog.fens.me/r-annoy/
################################

#install.packages("RcppAnnoy")

library(RcppAnnoy)
setwd("C:/work/R/annoy")


# BUILDING ANNOY INDEX ---------------------------------------------------------
n <- 10
a <- new(AnnoyEuclidean, n)

a$setSeed(42)

# Turn on verbose status messages (0 to turn off)
a$setVerbose(1)

# Load 100 random vectors into index
for (i in 1:100) a$addItem(i - 1, runif(n)) # Annoy uses zero indexing

# Display number of items in index
a$getNItems()

# Retrieve item at postition 0 in index
a0<-a$getItemsVector(0);a0
a1<-a$getItemsVector(1);a1
a2<-a$getItemsVector(2);a2

# Calculate distance between items at postitions 0 & 1 in index
a$getDistance(0, 1)
(a1-a0)^2 %>% sum %>% sqrt

# Build forest with 50 trees
a$build(50)


# PERFORMING ANNOY SEARCH ------------------------------------------------------

# Retrieve 5 nearest neighbors to item 0
# Returned as integer vector of indices
a$getNNsByItem(0, 5)

# Retrieve 5 nearest neighbors to item 0
# search_k = -1 will invoke default search_k value of n_trees * n
# Return results as list with an element for distance
a$getNNsByItemList(0, 5, -1, TRUE)

a55<-a$getItemsVector(55);a55
a54<-a$getItemsVector(54);a54
a31<-a$getItemsVector(31);a31
a11<-a$getItemsVector(11);a11

(a55-a0)^2 %>% sum %>% sqrt
(a54-a0)^2 %>% sum %>% sqrt
(a31-a0)^2 %>% sum %>% sqrt
(a11-a0)^2 %>% sum %>% sqrt


a$getNNsByItemList(0, 5, 10, TRUE)

# Retrieve 5 nearest neighbors to item 0
# search_k = -1 will invoke default search_k value of n_trees * n
# Return results as list without an element for distance
a$getNNsByItemList(0, 5, -1, FALSE)


v <- runif(10);v
# Retrieve 5 nearest neighbors to vector v
# Returned as integer vector of indices
a$getNNsByVector(v, 5)

# Retrieve 5 nearest neighbors to vector v
# search_k = -1 will invoke default search_k value of n_trees * n
# Return results as list with an element for distance
a$getNNsByVectorList(v, 5, -1, TRUE)


# SAVING/LOADING ANNOY INDEX ---------------------------------------------------

# Save annoy tree to disk
a$save("./annoy01.ann")
dir()

# Load annoy tree from disk
a$load("./annoy01.ann")

# Unload index from memory
a$unload()
