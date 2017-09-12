install.packages("proxy",dependencies = TRUE)
install.packages("lsa",dependencies = TRUE)
library(ggplot2)
library(tm)
library(wordcloud)
library(SnowballC) # for stemming
library(stringr)
library(dplyr)
library(reshape2)
library(lsa)

# Asztali gépnek vagy laptopnak megfelelő mappa választása
path0 = "E:/amunka/QDA"  # asztali gép
path0 = "C:/PE_GK/QDA"   # laptop

# sz2 dataframe elkészítése tm_for_thesisv4.R alapján vagy betöltés az alábbi helyről
setwd(paste0(path0,"/sz2"))
load("sz2.RData")

# tdm-ek betöltése
setwd(paste0(path0,"/eredmenyek"))
load("dtmstemmed.RData")  # a mátrix elemei szógyakoriságok
dtm.stemmed.bin = weightBin(dtm.stemmed)  # Binary weighting of term frequency; a mátrix elemei 0 vagy 1
load("dtmbigram.RData")  #  a mátrix elemei 0 vagy 1


# transform normal dtm to tf-idf weighted dtm
# dtm.stemmed.tfidf = weightSMART(dtm.stemmed,"ntc") # a dokumentumokat súlyozza és nem a kifejezéseket
dtm.stemmed.tfidf = weightTfIdf(dtm.stemmed,normalize = FALSE)
dissim.stemmed.tfidf = dissimil
freq=colSums(as.matrix(dtm.stemmed.tfidf))
freq=rowSums(as.matrix(dtm.stemmed.bin))

m.stemmed.bin=Data(dtm.stemmed.bin) # error

# Distance matrix
# Van 9 dolgozat, amiben egyik szó sem fordul elő. Ezeket vegyük ki, mert nem lehet koszinusz távolságot számolni 
freq=rowSums(as.matrix(dtm.stemmed.bin))
dtm.stemmed.bin = dtm.stemmed.bin[freq>0,]
t1=Sys.time()
dist.stemmed.bin=cosine(t(as.matrix(dtm.stemmed.bin))) # 11 perc
t2=Sys.time()
print(t2-t1)
setwd(paste0(path0,"/eredmenyek"))
save(dist.stemmed.bin,file="diststemmedbin.RData")
load("diststemmedbin.RData")

dist.stemmed.bin=1-dist.stemmed.bin  # transform similarity to dissimilarity
dist.stemmed.bin=as.dist(dist.stemmed.bin) # transform to dist object, because hclust() requires it
cimkek=attr(dist.stemmed.bin,"Labels")
cimkek=as.integer(cimkek)
max(cimkek)
# hierarchical clustering
hclust.cos.stemmed.bin=hclust(dist.stemmed.bin,method = "ward.D2") # 1 sec

# dendrogram
plot(hclust.cos.stemmed.bin,xlab="Szakdolgozat sorszáma")  # 1 sec

# extract clusters
rect.hclust(hclust.cos.stemmed.bin,k=3, border="red")
clindex=rect.hclust(hclust.cos.stemmed.bin,k=3, border="red")
cl1=clindex[[1]]
cl2=clindex[[2]]
cl3=clindex[[3]]
cl1.dtm=dtm.stemmed.bin[cimkek[cl1],]
cl2.dtm=dtm.stemmed.bin[cimkek[cl2],]
cl3.dtm=dtm.stemmed.bin[cimkek[cl3],]
cl1.freq=colSums(as.matrix(cl1.dtm))
cl2.freq=colSums(as.matrix(cl2.dtm))
cl3.freq=colSums(as.matrix(cl3.dtm))
cl1.ord = order(cl1.freq,decreasing = T)
cl2.ord = order(cl2.freq,decreasing = T)
cl3.ord = order(cl3.freq,decreasing = T)
dbszo=50 # hány szóból

wordcloud(words=names(cl1.freq[cl1.ord[1:dbszo]]),freq=cl1.freq[cl1.ord[1:dbszo]])
wordcloud(words=names(cl2.freq[cl2.ord[1:dbszo]]),freq=cl2.freq[cl2.ord[1:dbszo]])
wordcloud(words=names(cl3.freq[cl3.ord[1:dbszo]]),freq=cl3.freq[cl3.ord[1:dbszo]])

write.table(cl1.freq,"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(names(cl1.freq),"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(cl2.freq,"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(names(cl2.freq),"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(cl3.freq,"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(names(cl3.freq),"clipboard",row.names = FALSE,sep = "\t",dec = ",")

# xx=identify(hclust.cos.stemmed.bin) # interaktív klaszterrajzolgatás
