########################
# http://blog.fens.me/r-text-tf-idf/
#######################


# install.packages("tidytext")

library(tidytext)
library(dplyr)
library(tibble)

ss <- c(
  "R的极客理想系列文章，涵盖了R的思想，使用，工具，创新等的一系列要点，以我个人的学习和体验去诠释R的强大", 
  "R语言作为统计学一门语言，一直在小众领域闪耀着光芒。", 
  "现在已不仅仅是统计领域，教育，银行，电商，互联网….都在使用R语言。",
  "直到大数据的爆发，R语言变成了一门炙手可热的数据分析的利器。",
  "随着越来越多的工程背景的人的加入，R语言的社区在迅速扩大成长。",
  "TF-IDF (词频-逆文档频率) 是一种在信息检索和文本挖掘中常用的加权技术。",
  "它衡量一个词语对于一个文档的重要性，通过结合词频(TF) 和逆文档频率(IDF) 来计算。",
  "TF (词频): 表示词语在特定文档中出现的次数。一个词语在文档中出现的次数越多，通常认为它在该文档中越重要。",
  "IDF (逆文档频率): 表示词语在整个语料库中的普遍程度。如果一个词语在很多文档中都出现，说明它比较常见，对区分文档的贡献就越小；反之，如果一个词语只出现在少数文档中，说明它对区分这些文档越重要。",
  "TF-IDF 的计算: TF-IDF 值是TF 值和IDF 值的乘积。"
) 

# 方法1

s1<- ss %>% as_tibble() %>% rowid_to_column("ID")
s1

ss_words <- s1 %>% 
  unnest_tokens(words, value) %>% 
  count(ID, words, sort = TRUE);
ss_words

ss_tfidf <- ss_words %>% 
  bind_tf_idf(ID, words, n)
ss_tfidf

ss_tfidf %>%
  arrange(desc(tf_idf))

library(widyr)
ss_tfidf %>% 
  pairwise_similarity(ID, words, tf_idf, sort = TRUE)

ss[8]
ss[9]

ss_tfidf %>% filter(ID==8) %>% print(n=50)
ss_tfidf %>% filter(ID==9) %>% print(n=50)

# 方法2

# install.packages("quanteda")
# install.packages("quanteda.textstats")

library("quanteda")
library("quanteda.textstats")

ssdfm <- dfm(tokens(ss))
ssdfm

ssidf<-dfm_tfidf(ssdfm)
ssidf

ssdfm %>%
  textstat_simil(method = "cosine") %>%
  as.matrix()

ssidf %>%
  textstat_simil(method = "cosine") %>%
  as.matrix()

