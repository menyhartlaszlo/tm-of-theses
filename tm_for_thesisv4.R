#install.packages("tm",dependencies = T)
#install.packages("wordcloud",dependencies = T)
#install.packages("SnowballC",dependencies = T)
library(tm)
library(wordcloud)
library(SnowballC) # for stemming
library(stringr)

path0 = "E:/amunka/QDA"  # asztali gép
path0 = "C:/PE_GK/QDA"   # laptop

## Összes szakdolgozatból (4464 db) egyetlen korpusz készítése
## Dataframe-k beolvasása, majd egyesítése
setwd(path0)
file_list = list.files(path = path0, pattern = "*.RData", full.names = TRUE, recursive = FALSE, ignore.case = TRUE)
if (exists("sz2")) rm(sz2)
t1 = Sys.time()
for (rdatafile in file_list) {
  load(rdatafile)
  if (exists("sz2")) {
    sz2 <- rbind(sz2,szakd)
  } else {sz2 <- szakd}
}

# képzési szint és szakcsoport meghatározása
# Összes szak
szakomlesztett = unique(sz2$szak)
# Idegen nyelvű szakok
angol.szak = str_detect(tolower(szakomlesztett),"fao|angol|idegennyelvi")
# Szakok képzési szint szerint
szakmernok = str_detect(szakomlesztett,"([sS]zakmernok)|([sS]zakmérnök)")
msc.szak = str_detect(tolower(szakomlesztett),"msc|orvos|k[eé]mi|agr[aá]rm[eé]rn[oö]k")
fsz.szak = str_detect(tolower(szakomlesztett),"fsz|foszk|szakképzés|szakkepzes|(m[eé]nesgazda)|(technol[oó]gus)|gyógynövény|gyogynoveny|asszisztens|informatikai")
bsc.szak = str_detect(tolower(szakomlesztett),"bsc|ba|agr[aá]rmenedzser")

# Szakok téma szerint
agrarkemia = str_detect(tolower(szakomlesztett),"k[eé]mi")
agrarmernok = str_detect(tolower(szakomlesztett),"^agr[aá]rm[eé]rn[oö]k|mez[oöő]gazdas[aá]gi|([oö]kol[oó]giai)")
gazdasagi = str_detect(tolower(szakomlesztett),"(^gazdas[aá]gi)|vid[eé]kfejleszt|(agr[aá]rmenedzser)|(agr[aá]rkereskedelmi)")
informatika = str_detect(tolower(szakomlesztett),"informatik")
termved = str_detect(tolower(szakomlesztett),"term[eé]szetv[eé]delmi")
turizmus = str_detect(tolower(szakomlesztett),"(vend[eé]gl[aá]t[oó])|(sz[aá]lloda)|turizmus|turisztika")
noveny = str_detect(tolower(szakomlesztett),"n[oö]v[eé]ny|talaj")
borasz = str_detect(tolower(szakomlesztett),"(sz[oöő]l[eé]sz)|(sz[oöő]l[oöő])")
kertesz = str_detect(tolower(szakomlesztett),"kert[eé]sz")
kornyezet = str_detect(tolower(szakomlesztett),"(k[oö]rnyezet)|(hullad[eé]k)")
allat = str_detect(tolower(szakomlesztett),"[aá]llat|m[eé]nesgazda")
tanar = str_detect(tolower(szakomlesztett),"tan[aá]r")
egyeb = str_detect(tolower(szakomlesztett),"min[oöő]s[eé]g[uü]gyi|energetikai")

szakok = data.frame(szak=szakomlesztett,szakmernok=as.integer(szakmernok),msc.szak=as.integer(msc.szak),
                    fsz=as.integer(fsz.szak),bsc.szak=as.integer(bsc.szak),angol=as.integer(angol.szak),
                    agrarkemia=as.integer(agrarkemia),agrarmernok=as.integer(agrarmernok),gazdasagi=as.integer(gazdasagi),
                    informatika=as.integer(informatika),termved=as.integer(termved),turizmus=as.integer(turizmus),
                    noveny=as.integer(noveny),borasz=as.integer(borasz),kertesz=as.integer(kertesz),
                    kornyezet=as.integer(kornyezet),allat=as.integer(allat),tanar=as.integer(tanar),egyeb=as.integer(egyeb))
# write.table(szakok,"clipboard",row.names = FALSE,sep = "\t",dec = ",")

# Szaktulajdonságok hozzárendelése a dolgozatokhoz
sz2=merge(sz2,szakok)

# Címlapon keresve bsc-t, msc-t
sz2$bsc.cimlap=as.integer(str_detect(tolower(sz2$cimlap),"bsc"))
sz2$msc.cimlap=as.integer(str_detect(tolower(sz2$cimlap),"msc"))
# dolgozat bsc-msc beállítása
# MSc-s az, akinek a szakja vagy a címlapja MSc-s és nem szakmérnök. BSc-s az, aki nem szakmérnök, nem FSZ és nem MSc.
sz2$msc = ifelse(sz2$msc.cimlap==1 | sz2$msc.szak==1,1,0)
sz2$msc = ifelse(sz2$szakmernok==1,0,sz2$msc)
sz2$bsc = ifelse(sz2$szakmernok==0 & sz2$fsz==0 & sz2$msc==0,1,0)
# angol dolgozatok törlése
sz2 = sz2[sz2$angol==0,]
# szoveg és címlap eltávolítása
# torolni=c("szov","cimlap")
# sz2 = sz2[!names(sz2) %in% torolni]
# write.table(sz2,"clipboard-16384",row.names = FALSE,sep = "\t",dec = ",")

# tolower()
# 11 perc
t0=Sys.time()
sz2$szov = tolower(sz2$szov)
t1=Sys.time()
print(t1-t0)

# Remove unnecessary characters (punctuations + numbers + others)
# 26 másodperc
sz2$szov = str_replace_all(sz2$szov,"[^a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]"," ")
t2=Sys.time()
print(t2-t1)

# Legalább 4 hosszú, azonos karakterből álló substringet lecseréli egy szóközre
# 37 seconds
sz2$szov = str_replace_all(sz2$szov,"(.)\\1{3,}"," ")
t3 = Sys.time()
print(t3-t2)

# Fölösleges whitespace eltávolítása
# 15 másodperc
sz2$szov = str_replace_all(sz2$szov," {2,}"," ")
t4 = Sys.time()
print(t4-t3)

# Szótövezés
# 1,1 perc/szótövezés
sz2$szov.s1=stemDocument(sz2$szov,language = "hungarian")
sz2$szov.s2=stemDocument(sz2$szov.s1,language = "hungarian")
sz2$szov.s3=stemDocument(sz2$szov.s2,language = "hungarian")
sz2$szov.s4=stemDocument(sz2$szov.s3,language = "hungarian")
t5=Sys.time()
print(t5-t4)

# Angol nyelvű és problémás dolgozatok eltávolítása
sz2$egyhogy=str_count(sz2$szov.s4,"\\b(egy|hogy)\\b")
# sz2 = sz2 %>% filter(egyhogy>20) # remove unnecessary documents

# sz2 mentése
setwd(paste0(path0,"/sz2"))
sz2=sz2[c("szak","ev","hallgato","szakmernok","fsz","angol","agrarkemia",
          "agrarmernok","gazdasagi","informatika","termved","turizmus",
          "noveny","borasz","kertesz","kornyezet","allat","tanar","egyeb",
          "msc","bsc","szov.s4")]
save(sz2,file="sz2.RData")
load("sz2.RData")
###############################################################################
####  Ha bigram tokenizer-rel dolgozunk, akkor folytatás a "tdm with bigram tokenizer" komment után
###############################################################################




# Kiválasztott szavak beolvasása => termsfrombook character vector
setwd(paste0(path0,"/collection of terms"))
load(file="termsfrombook.RData")


# Dictionary előállítása
dic_szoftver = c("spss","r","stata","sas","statistica","python","excel","msexcel","dataplot","instat","irristat",
                 "scilab","matlab","maple","mathematica",
                 "ssp","pspp","webstat","rweb","statiscope","lipstat","openstat","weka","statsoft","minitab",
                 "gretl","perl","rapidminer","unistat")
dic_ts = c("idősor", "idősorelemzés", "arima", "arma", "autokorreláció","autokorrelációs","autokovariancia",
           "trend","trendanalízis","trendelemzés","szezonalitás","periodicitás","periódus","szezonális")
dic_anova = c("variancia","varianciaanalízis","varianciaelemzés","anova","szórásnégyzet","szd5","szd5%","lsd","tukey",
              "bonferroni","sidak","duncan","scheffe","scheffé","dunnett")
dic_reg = c("regresszió","meredekség","együttható","regresszióelemzés","regresszióanalízis")
dic_korr = c("korreláció","korrelációelemzés","korrelációanalízis","Pearson","rangkorreláció")
dic_leiro = c("gyakoriság","átlag","medián","módusz","szórás",
              "kvartilis","alsókvartilis","felsőkvartilis","percentilis","kvantilis","decilis")
dic_grafikon = c("boxplot","kördiagram","tortadiagram","oszlopdiagram","szórásdiagram","diagram","grafikon","hisztogram")
dic_gazdstat = c("index","viszonyszám","eurostat","ksh")
dic_tobbvalt = c("többtényezős","többváltozós","kéttényezős","kétváltozós","kovariancia","kovarianciaelemzés",
                 "kovarianciaanalízis","korrespondencia","korrespondenciaelemzés","korrespondenciaanalízis",
                 "faktorelemzés","faktoranalízis","főkomponens","főkomponensanalízis","főkomponenselemzés",
                 "randomforest")
dic_egyeb = c("bayes","swot","swotelemzés","swotanalízis","statisztika","szd","szd5","szd%","szd5%","ismérv","szignifikancia","konfidencia",
              "szignifikáns","kérdőív","kiértékelés","nemparaméteres","robusztus","robosztus","Poisson","binomiális",
              "logisztikus","hipergeometriai","hipergeometrikus","diszkriminancia","diszkriminanciaelemzés",
              "diszkriminanciaanalízis","spline","transzformáció","kontroll","kontrol",
              "interpolál","interpoláció","extrapolál","extrapoláció","wilcoxon","shapiro","wilk","kolmogorov","smirnov",
              "reziduális","négyzetösszeg","keresztvalidácó","modell","neurális","mae","rmse","bootstrap","boosting",
              "bagging","boosted","bagged","conjoint","roc","mintavétel","standardizált",
              "khi","khinégyzet","fisher","fischer","kruskal","wallis","mann","whitney")
dic_osszes = c(dic_szoftver,dic_ts,dic_anova,dic_reg,dic_korr,dic_leiro,dic_grafikon,dic_gazdstat,dic_tobbvalt,dic_egyeb)
dic_ossz.s1= stemDocument(dic_osszes,language = "hungarian")
dic_ossz.s2= stemDocument(dic_ossz.s1,language = "hungarian")
dic_ossz.s3= stemDocument(dic_ossz.s2,language = "hungarian")
dic_ossz.s4= stemDocument(dic_ossz.s3,language = "hungarian")
dictionary = unique(c(dic_ossz.s4,termsfrombook))
dict.stemmed = unique(c(dic_ossz.s4,termsfrombook))
wordfilter=paste0(dict.stemmed,collapse = "|")
wordfilter = paste0("\\b(",wordfilter,")\\b")

# Releváns kifejezések kiválogatása, a többi elhagyása 38 perc
t4=Sys.time()
sz2$szov.filtered = str_extract_all(sz2$szov.s4,wordfilter)
t5=Sys.time()
print(t5-t4)

# Korpusszá alakítás
#korpusz=VCorpus(VectorSource(sz2$szov),readerControl = list(language="HU"))
korpusz.stemmed=VCorpus(VectorSource(sz2$szov.filtered),readerControl = list(language="HU"))
#korpusz.s4=VCorpus(VectorSource(sz2$szov.s4),readerControl = list(language="HU"))

#Remove stopwords 59 min
# Stopwords can be augmented here...
# korpusz = tm_map(korpusz,removeWords,stopwords("hungarian"))


#Term-document matrix előállítása
#dtm = DocumentTermMatrix(korpusz)  # Ehhez kevés a memória
t8 = Sys.time()
dtm.stemmed = DocumentTermMatrix(korpusz.stemmed)  # 41 mp
#dtm.stemmed = DocumentTermMatrix(korpusz.stemmed,control = list(dictionary=dict.stemmed))
#dtm = DocumentTermMatrix(korpusz,control = list(dictionary=dictionary)) # 2,5 perc
dtm.s4 = DocumentTermMatrix(korpusz.s4,control = list(dictionary=dic_ossz.s4))  #1,6 perc
t9 = Sys.time()
print(t9-t8)

setwd(paste0(path0,"/eredmenyek"))
save(dtm.stemmed,file="dtmstemmed.RData")
save(dtm,file="dtm.RData")
save(dtm.stemmed,file="dtmstemmed_4376sor")
load("dtmstemmed.RData")

# Mely kifejezések fordulnak valóban elő a dokumentumokban és hányszor?
freq.stemmed=colSums(as.matrix(dtm.stemmed))
write.table(freq.stemmed,"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(names(freq.stemmed),"clipboard",row.names = FALSE,sep = "\t",dec = ",")
# Hány dokumentumban fordulnak elő min egyszer?
dtm.stemmed.bin = weightBin(dtm.stemmed)
freq.stemmed.bin=colSums(as.matrix(dtm.stemmed.bin))
write.table(freq.stemmed.bin,"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(names(freq.stemmed.bin),"clipboard",row.names = FALSE,sep = "\t",dec = ",")





#################################################################
# tdm with bigram tokenizer
#################################################################
# Muszáj a dictionary használata a köv. miatt. Regexp-pel ki tudjuk szedni a számunkra érdekes szópárokat.
# De amikor a tdm előállításakor a BigramTokenizer-t használjuk "konkatenálja a szópárokból álló karaktervektort (pl 4 szópárból egy 8 szóból álló sztringet készít),
# így a tdm-ben már nem négy, hanem 7 kifejezés fog szerepelni.

# A keresett két szóból álló kifejezésekból dictionary készítése, majd 4-szeres szótövezése
dic.bigram = c("variancia analízis","variancia elemzés","szórásnégyzet analízis","szórásnégyzet elemzés",
               "ár index","volumen index","fogyasztói index","főre jutó","páronkénti összehasonlítás","post hoc",
               "korreláció elemzés","korreláció analízis","regresszió elemzés","regresszió analízis",
               "t proba","t próba", "f proba", "f próba"," t teszt","f teszt","khinégyzet próba","khinégyzet teszt","khi négyzet",
               "chi square",
               "lineáris modell","kruskal wallis","man whitney","mann whitney","kevert modell", "függő változó",
               "független változó", "normális eloszlás", "binomiális próba", "binomiális teszt", "binomiális eloszlás",
               "levene teszt", "levene teszt", "fisher teszt", "fischer teszt", "fisher próba", "fischer próba",
               "fisher féle","fischer féle", "fisher egzakt", "fischer egzakt",
               "többdimenziós skálázás", "kísérleti elrendezés", "kontroll csoport","véletlen blokk","latin négyzet",
               "döntési fa", "regressziós fa", "split plot", "r program", "r statisztikai", "r szoftver",
               "elutasítási tartomány","elfogadási tartomány","relatív szórás")
dic.bigram.s1= stemDocument(dic.bigram,language = "hungarian")
dic.bigram.s2= stemDocument(dic.bigram.s1,language = "hungarian")
dic.bigram.s3= stemDocument(dic.bigram.s2,language = "hungarian")
dic.bigram.s4= stemDocument(dic.bigram.s3,language = "hungarian")

# BigramTokenizer elkészítése
BigramTokenizer <- function(x)
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

# Korpusz, majd a dtm a dictionary felhasználásával.
korpusz.bigram = VCorpus(VectorSource(sz2$szov.s4))
t1 = Sys.time()
dtm.bigram = DocumentTermMatrix(korpusz.bigram,control = list(tokenize = BigramTokenizer,dictionary=dic.bigram.s4)) # 11 perc
t2 = Sys.time()
print(t2-t1)

# dtm mentése
setwd(paste0(path0,"/eredmenyek"))
save(dtm.bigram,file="dtmbigram.RData")

# Mely bigramok fordulnak elő a korpuszban?
freq.bigram = colSums(as.matrix(dtm.bigram))
ord=order(freq.bigram,decreasing = T)
write.table(freq.bigram,"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(names(freq.bigram),"clipboard",row.names = FALSE,sep = "\t",dec = ",")




####################################################
# Milyen mutatók, indexek, viszonyszámok szerepelnek a dolgozatban?
####################################################
#s = "az ár index volt első félévben a volumen index a másodikban a lak index még nem volt"
# 3,5 perc változónként
sz2$index_ = str_extract_all(sz2$szov.s4,"(\\b[a-záéíóöőúüű]*\\s)index(\\b)") # 3,5 perc
sz2$viszonyszam = str_extract_all(sz2$szov.s4,"(\\b[a-záéíóöőúüű]*\\s)viszonysz(\\b)") # 3,5 perc
sz2$mutato = str_extract_all(sz2$szov.s4,"(\\b[a-záéíóöőúüű]*\\s)mutató(\\b)") # 3,5 perc
# 2,5 perc változónként
sz2$index1 = str_extract_all(sz2$szov.s4,"(\\b[a-záéíóöőúüű]*)index(\\b)")
sz2$viszonyszam1 = str_extract_all(sz2$szov.s4,"(\\b[a-záéíóöőúüű]*)viszonysz(\\b)")
sz2$mutato1 = str_extract_all(sz2$szov.s4,"(\\b[a-záéíóöőúüű]*)mutató(\\b)")

tdm.index_=weightBin(DocumentTermMatrix(VCorpus(VectorSource(sz2$index_)),control=list(wordLengths=c(2,Inf))))
tdm.viszonyszam=DocumentTermMatrix(VCorpus(VectorSource(sz2$viszonyszam)),control=list(wordLengths=c(2,Inf)))
tdm.mutato=DocumentTermMatrix(VCorpus(VectorSource(sz2$mutato)),control=list(wordLengths=c(2,Inf)))
tdm.index1=weightBin(DocumentTermMatrix(VCorpus(VectorSource(sz2$index1)),control=list(wordLengths=c(2,Inf))))
tdm.viszonyszam1=DocumentTermMatrix(VCorpus(VectorSource(sz2$viszonyszam1)),control=list(wordLengths=c(2,Inf)))
tdm.mutato1=DocumentTermMatrix(VCorpus(VectorSource(sz2$mutato1)),control=list(wordLengths=c(2,Inf)))
freq=colSums(as.matrix(tdm.index_))
freq=colSums(as.matrix(tdm.viszonyszam))
freq=colSums(as.matrix(tdm.mutato))
freq=colSums(as.matrix(tdm.index1))
freq=colSums(as.matrix(tdm.viszonyszam1))
freq=colSums(as.matrix(tdm.mutato1))
ord=order(freq,decreasing = T)
write.table(freq,"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(names(freq),"clipboard",row.names = FALSE,sep = "\t",dec = ",")


######################################################
##  Összes dolgozatból dtm.total
######################################################
# 11 perc
k=VCorpus(VectorSource(sz2$szov.s4))
dtm.total=DocumentTermMatrix(k)
# dtm.total mentése
setwd(paste0(path0,"/eredmenyek"))
save(dtm.total,file="dtmtotal.RData")
load("dtmtotal.RData")
save(dtm.stemmed,file="dtmstemmed_4376sor") #statisztikai kifejezéseket tartalmazó dtm

dtm.total.bin=weightBin(dtm.total)

# Ritka kifejezések eltávolítása

#frequent.terms=findMostFreqTerms(dtm.total,100) # csoportonként/dokumentumnként adja vissza a kifejezéseket
frequent.terms=findFreqTerms(dtm.total,1000,Inf) # az összesített kifejezéshalmazból dolgozik
length(frequent.terms)
frequent.terms[1:100]

termstotal = Terms(dtm.total)
freq=colSums(as.matrix(dtm.total.bin)) #cann't allocate memory
freq=tm_term_score(dtm.total.bin,frequent.terms,FUN = colSums())


s="a módszer a varianc elemzés és a korreláció elemzés volt r statisz szoftver felhasznál"
as.matrix(DocumentTermMatrix(VCorpus(VectorSource(str_extract_all(s,wordfilter))),control = list(tokenize = BigramTokenizer)))
p=VCorpus(VectorSource(s))
p=VCorpus(VectorSource(str_extract_all(s,wordfilter)))
ptdm=DocumentTermMatrix(p,control = list(tokenize = BigramTokenizer,dictionary=dic.bigram.s4))

inspect(ptdm)

#Gyakori szavak kiiratása. Először válasszunk document-term mátrixot!
freq=colSums(as.matrix(dtm)) #Az oszlopösszegek nevei a kifejezések
freq.stemmed=colSums(as.matrix(dtm.stemmed))
ord = order(freq,decreasing = T)
ord.stemmed = order(freq.stemmed,decreasing = T)

#freq[ord[1:200]]
freq[ord]
freq[freq>0]
write.table(freq,"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(names(freq),"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(freq.stemmed,"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(names(freq.stemmed),"clipboard",row.names = FALSE,sep = "\t",dec = ",")

# wordcloud
dbszo=100 # hány szóból
wordcloud(words=names(freq[ord[1:dbszo]]),freq=freq[ord[1:dbszo]])
wordcloud(words=names(freq.stemmed[ord.stemmed[2:dbszo]]),freq=freq.stemmed[ord.stemmed[2:dbszo]])


dic2=c("keszthely","tanszék","konzulens","szója")
korpusz2 = korpusz[1:50]
# For a single document
termFreq(korpusz2[[1]],control = list(dictionary=dic2))
mintaszov=Corpus(VectorSource(c("a szója meg a szója az majdnem ket szója","a kis szója nem nagy szója keszthely sem")))
termFreq(mintaszov[[1]],control = list(dictionary=dic2))
termFreq("a szója meg a szója az majdnem ket szója és keszthely",control = list(dictionary=dic2))
termFreq("a szoja meg a szoja az majdnem ket szoja")
dtm=DocumentTermMatrix(mintaszov,list(dictionary=dic2))
dtm=DocumentTermMatrix(korpusz2,list(dictionary=dic2))
dtm=DocumentTermMatrix(korpusz,list(dictionary=dic2,weighting = function(x)
  weightTf(x)))

# Select frequencies for a particular term?
o1=as.matrix(dtm)[,22]
o2=as.matrix(dtm.stemmed)[,22]
sum(o1)
sum(o2)

i=2221
str_extract_all(as.character(korpusz[[i]]),".{20}idősor.{40}")
paste(sz2$hallgato[i],sz2$szak[i],sep = "     ")


# remove word "hely" from dtm.stemmed
torolni = which(colnames(as.matrix(dtm.stemmed))=="hely")  # 196
which(Terms(dtm.stemmed)=="hely")
      