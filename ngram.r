#################################
# http://blog.fens.me/r-text-ngram/
##############################

# install.packages("ngram")

library(ngram)

# Creating

x <- "A B A C A B B"
string.summary(x)

ng <- ngram (x , n =2)
ng

# Printing
print (ng , output ="full")

# Summarizing
get.phrasetable ( ng )


get.phrasetable ( ngram (x , n =1) )
get.phrasetable ( ngram (x , n =3) )

get.ngrams(ng)
get.string(ng)

# Babbling

babble (ng , 10)
babble (ng , 10)
babble (ng , 20)
babble (ng , 10 , seed =10)
babble (ng , 10 , seed =10)


x<-c(
"how are you",
"I like you",
"I love you",
"I think you're the best for me",
"We re good friends",
"We're all good"
)

x<-str_c(x,collapse = " ")
ng <- ngram (x , n=2)
ng
print (ng , output ="truncated")
get.phrasetable ( ng )
babble (ng , 10)
babble (ng , 10)
babble (ng , 10)

