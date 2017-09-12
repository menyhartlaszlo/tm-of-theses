#install.packages("tm",dependencies = T)
#install.packages("wordcloud",dependencies = T)
#install.packages("SnowballC",dependencies = T)
#install.packages("quanteda",dependencies = T)
library(tm)
library(wordcloud)
library(SnowballC) # for stemming
library(stringr)
library(quanteda)

# Hungarian stopwords
#stopwords(kind="hungarian")

# Konvertált adatok beolvasása .RData állományból.
setwd("E:/amunka/QDA")
setwd("C:/PE_GK/QDA")
load("szakd2006_pd.RData")
sz2 = szakd

# Első tíz megtartása
sz2 = szakd[1:20,]

# a többi törlése
rm(szakd)

## Összes szakdolgozatból (4464 db) egyetlen korpusz készítése
setwd("C:/PE_GK/QDA")
path0 = "C:/PE_GK/QDA"
file_list = list.files(path = path0, pattern = "*.RData", full.names = TRUE, recursive = FALSE, ignore.case = TRUE)
if (exists("sz2")) rm(sz2)
for (rdatafile in file_list) {
  load(rdatafile)
  if (exists("sz2")) {
    sz2 <- rbind(sz2,szakd)
  } else {sz2 <- szakd}
}


#korpusz = VCorpus(VectorSource(sz2$szov),readerControl = list(language="HU")) # A korpusz minden eleme egyelemű tömb, ami a szöveget tartalmazza
korpusz=VCorpus(DataframeSource(subset(sz2,select=szov)),readerControl = list(language="HU"))  # A korpusz minden eleme négyelemű tömb, aminek a 4. eleme a szöveg. Az első az évszám, a második a szak, a harmadik a hallgató

#Remove punctuation, but keep dashed line within words 
korpusz = tm_map(korpusz,removePunctuation,preserve_intra_word_dashes=TRUE)
# Kötőjelet lecseréli egy szóközre
korpusz = tm_map(korpusz,content_transformer(function(x) str_replace_all(x,"-"," ")))
#Remove Numbers
korpusz = tm_map(korpusz,removeNumbers)
#Szótövezés
#korpusz = tm_map(korpusz,stemDocument)
#Remove stopwords
# Stopwords can be augmented here...
korpusz = tm_map(korpusz,removeWords,stopwords("hungarian"))
# Legalább 4 hosszú, azonos karakterből álló substringetlecseréli egy szóközre kb. 2' 200doksis korpusznál
korpusz = tm_map(korpusz,content_transformer(function(x) str_replace_all(x,"(.)\\1{3,}"," ")))
# Fölösleges whitespace eltávolítása
korpusz = tm_map(korpusz,stripWhitespace)





#Document-Term mátrix
dtm=DocumentTermMatrix(korpusz) # kevesebb, mint 8 perc 200doksira
inspect(dtm[1:5,1000:1120]) # Egy részét kiírja
findFreqTerms(dtm,20) # Legalább 20-szor előforduló szavak
findAssocs(dtm,"dunasol",0.9) # Ami erősen korrelál a "dunasol" szóval


# Mennyi ideig tart a dtm előállítása nagy dokumentumszám esetén?
n=c();t=c();nc=c()
for (i in 21:40) {
  t1 = Sys.time()
  sz2 = szakd[1:i,]
  korpusz=VCorpus(DataframeSource(subset(sz2,select=szov)),readerControl = list(language="HU"))  # A korpusz minden eleme négyelemű tömb, aminek a 4. eleme a szöveg. Az első az évszám, a második a szak, a harmadik a hallgató
  korpusz = tm_map(korpusz,removePunctuation)
  korpusz = tm_map(korpusz,removeNumbers)
  korpusz = tm_map(korpusz,removeWords,stopwords("hungarian"))
  korpusz = tm_map(korpusz,content_transformer(function(x) str_replace_all(x,"(.)\\1{3,}"," ")))
  korpusz = tm_map(korpusz,stripWhitespace)
  dtm = DocumentTermMatrix(korpusz)
  dt=Sys.time()-t1
  n=c(n,i); t=c(t,dt); nc=c(nc,nchar(korpusz)[1])
}
eredm=data.frame(n,nc,t)
write.table(eredm,"clipboard", sep="\t", row.names=FALSE)



# Dictionary
dictionary = c("spss","variancia","varianciaanalízis", "regresszió", "index", "statisztika",
               "statisztikai","átlag","szórás")
dic_szoftver = c("spss","r","stata","sas","statistica","python","excel","msexcel","dataplot","instat","irristat",
                 "scilab","matlab","maple","mathematica",
                 "ssp","pspp","webstat","rweb","statiscope","sisa","lipstat","openstat","weka","statsoft","minitab",
                 "gretl","perl","rapidminer","unistat")
dic_ts = c("idősor", "idősorelemzés", "arima", "arma", "autokorreláció", "autokovariancia")
dic_anova = c("variancia","varianciaanalízis","varianciaelemzés","anova","szórásnégyzet","szd5","szd5%","lsd","tukey",
              "bonferroni","sidak","duncan","scheffe","dunnett")
dic_reg = c("regresszió","meredekség","együttható","regresszióelemzés","regresszióanalízis")
dic_korr = c("korreláció","korrelációelemzés","korrelációanalízis","Pearson","rangkorreláció")
dic_egyeb = c("bayes","swot","index","statisztika","átlag","medián","módusz","szórás",
              "kvartilis","alsókvartilis","felsőkvartilis","percentilis","kvantilis",
              "boxplot","kördiagram","tortadiagram","oszlopdiagram","szórásdiagram","diagram","grafikon","hisztogram")
dictionary = c(dic_szoftver,dic_ts,dic_anova,dic_reg,dic_korr,dic_egyeb)
dtm=DocumentTermMatrix(korpusz,list(dictionary=dictionary))
as.matrix(dtm)


#Gyakori szavak kiiratása
freq=colSums(as.matrix(dtm)) #Az oszlopösszegek nevei a kifejezések
ord = order(freq,decreasing = T)
freq[ord[1:200]]
# wordcloud
dbszo=200 # hány szóból
wordcloud(words=names(freq[ord[1:dbszo]]),freq=freq[ord[1:dbszo]])


# Adott szak kiválogatása
sz2 = szakd[szakd$szak=="Növényvédelmi Szakmérnök Szak",]
# majd adott dolgozat kiválasztása
sz2$hallgato
kell = sz2[2,]
substr(kell$szov,150000,151000)
nchar(kell$szov)

# Get the identifier of an individual document
meta(korpusz[[10]],"id")


# stemming
getStemLanguages() # Mely nyelvekre érhető el
korpusz = tm_map(korpusz,stemDocument, language="hungarian")

# Clustering
plot(hclust(dist(dtm)))



# Konkrét információk kikeresése
str_locate(korpusz[[3]]$content,"szakdolgozat|diploma") # Hányadik karakternél van a szakdolgozat vagy a diploma szó
for (i in 1:10) {
  print(str_locate(korpusz[[i]]$content,"tanár|docens|adjunktus|tanársegéd|mérnök"))
}


kv = str_locate_all(korpusz[[8]]$content,"tanár|docens|adjunktus|tanársegéd|mérnök")
kv[[1]][dim(kv[[1]])[1],2] #utolsó előfordulás végpozíciója

kvd = str_locate(korpusz[[8]]$content,"szakdolgozat|diploma")
eleje=str_sub(korpusz[[8]]$content,1,kvd[1,1]-1)

for (i in 1:10) {
  kvd = str_locate(korpusz[[i]]$content,"szakdolgozat|diploma")
  eleje=str_sub(korpusz[[i]]$content,1,kvd[1,1]-1)
  print(eleje)
}

cim_reg="(konzulens|témavezető){1}.{1,200}(diplomadolgozat|szakdolgozat)"
cim_reg="(tanár|docens|adjunktus|tanársegéd){1}.{1,200}(diplomadolgozat|szakdolgozat)"
cim_reg="(tanár|docens|adjunktus|tanársegéd){1}(?!(tanár|docens|adjunktus|tanársegéd|diplomadolgozat|szakdolgozat))(diplomadolgozat|szakdolgozat)"
hol=str_locate(korpusz[[7]]$content,cim_reg)
cim=str_extract(korpusz[[7]]$content,cim_reg)
str_extract(korpusz[[7]]$content,cim_reg)

beosztas="tanár|docens|adjunktus|tanársegéd"
str_locate(korpusz[[1]]$content,cim_reg)
cim=paste

substr(korpusz[[6]]$content,21000,21400)
substr(korpusz[[7]]$content,0,400)




str(korpusz$content)
str(sz2)
sz=sz2$szov
str(sz)
szov=sz[1]
str(szov)
length(szov)
substr(szov,100,130)
# szov[100:130]  nem jó!!

for (i in 1:10) {
  szov=sz[i]
  print(substr(szov,1000,1050))
}



dic3=c("kék","zöld")
a1="Géza kék az ég és kék a tenger!"
a2="Kék az ég és zöld a fű az élet egyszerűen gyönyörű"
termFreq(a1)

###########################################
# Corpus() függvény esetén van 1-nél nagyobb szógyakoriság, VCorpus() esetén nincs!!
###########################################

k=VCorpus(VectorSource(c(a1,a2)))
dtm_=DocumentTermMatrix(k,list(dictionary=dic3))
as.matrix(dtm_)
k=korpusz[1:5]
dic3=c("szója","éva","soya")


# NézzĂĽk meg a remove-val kezdődő utasításokat: removeWords;removeNumbers;removePunctuation;removeSparseTerms;
# Ki kellene gyűjteni a szakdolikból az anyag és módszer fejezeteket, ezekre készíteni el a dtm mátrixot s megnézni  a leggyakoribb kifejezéseket.
# strsplit()alkalmas lehet a szoveg darabolásásra regexp alapján
# regexpr-nel gyűjtsĂĽk ki a [valamilyen szó] mutató|index szókapcsolatokat
# Ăşjrafeldolgozásnál a duplikációkat szűrjĂĽk ki
# Szavak gyakoriságánál figyelembe kellene venni, hogy ha csak egy dolgozatban szerepel, de abban 500-szor, akkor még ne legyen a leggyakoribb.
# Kikeresni minden szót, ami grafikonra vagy diagram-ra végződik.





# Stemming with Snowball
wordStem(c("regressziót.","szd5%","szd","szd%","szd5","kördiagram","adatfeldolgozás","adatfeldolgozásokat","bevezetés","varianciaanalízist","varianciaanalízissel","regressziót","regresszióval","regresszióhoz"),language = "hungarian")
stemDocument(c("adatfeldolgozás","adatfeldolgozásokat","bevezetés","varianciaanalízist","varianciaanalízissel","regressziót","regresszióval","regresszióhoz"),language = "hungarian")

szavak=c("regressziónál","regressziós", "lapát","táblázat","szaval","adatokat","többváltozóssal","ablak")
szavak=c("matematikai","volumen","medián","összegeket","összesen","hisztogram","rang","rangú","minimum","maximum")
szavak=c("koordinátáikat","koordináta","sztenderd","multiplikatív","spearman","volumen","négyzetéből","négyzetösszegéből")
szavak=c("szórású","poisson","bartlett","dichotom","prediktív","boxplot","függvényről","loglikelihood")
szavak=c("kvantilis","szabadsági","illeszkedésvizsgálat","indexű","indexszel","mátrix","termésátlag")
szavak=c("autokorreláció","autokorrelációs","kontroll","kontrol","standardizált")
szavak=c("varianciaanalízissel","varianciaanalízist","varianciaanalízis")
szavak = c("mutató", "index","viszonyszám")
szavak = c("thesis", "university","faculty","introduction")
for ( i in 1:5) {
  szavak = wordStem(szavak,language = "hungarian")
  print(szavak)
}



pl=sz2[2,]
# Ha nem a korpuszt stemmeljük, hanem még a dataframe string mezőjét:
substr(pl$szov[1],1,2000)
pl.stem=stemDocument(pl$szov[1],language = "hungarian")
substr(pl.stem,1,2000)
dtm.pre=DocumentTermMatrix(VCorpus(VectorSource(c(pl$szov[1]))))
dtm.post=DocumentTermMatrix(VCorpus(VectorSource(c(pl.stem))))
str(as.matrix(dtm.pre))
str(as.matrix(dtm.post))

t1=Sys.time()
sz2$szov.stem=stemDocument(sz2$szov,language = "hungarian")
t2=Sys.time()
print(t2-t1) # 1,3 perc
substr(sz2$szov[85],1,2000)
substr(sz2$szov.stem[85],1,2000)

# korpusszá alakítás vectorsource()-szal
korpusz=VCorpus(VectorSource(sz2$szov.stem),readerControl = list(language="HU"))

korpusz=VCorpus(DataframeSource(subset(pl,select=szov)),readerControl = list(language="HU"))
korpusz=tm_map(korpusz,removePunctuation)
korpusz=tm_map(korpusz,removeNumbers)
korpusz=tm_map(korpusz,PlainTextDocument)
before.stem=as.character(korpusz[[1]])
korpusz = tm_map(korpusz,stemDocument,language = "hungarian")  # itt kellene megadni a nyelvet, de valamiért angol nyelvvel dolgozik
after.stem = as.character(korpusz[[1]])
substr(before.stem,1,2000)
substr(after.stem,1,2000)

# Szakok szerinti bontás
# Összes szak
szakomlesztett = unique(sz2$szak)
# Idegen nyelvű szakok elhagyása, ha kell
angol = str_detect(tolower(szakomlesztett),"fao|angol|idegennyelvi")
#szakomlesztett=szakomlesztett[!angol]
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
#szakomlesztett[!szakmernok&!msc&!fsz&!angol&!bsc]

szakok = data.frame(szak=szakomlesztett,szakmernok=as.integer(szakmernok),msc.szak=as.integer(msc.szak),
                    fsz=as.integer(fsz.szak),bsc.szak=as.integer(bsc.szak),angol=as.integer(angol),
                    agrarkemia=as.integer(agrarkemia),agrarmernok=as.integer(agrarmernok),gazdasagi=as.integer(gazdasagi),
                    informatika=as.integer(informatika),termved=as.integer(termved),turizmus=as.integer(turizmus),
                    noveny=as.integer(noveny),borasz=as.integer(borasz),kertesz=as.integer(kertesz),
                    kornyezet=as.integer(kornyezet),allat=as.integer(allat),tanar=as.integer(tanar),egyeb=as.integer(egyeb))
write.table(szakok,"clipboard",row.names = FALSE,sep = "\t",dec = ",")

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
torolni=c("szov","cimlap")
sz2 = sz2[!names(sz2) %in% torolni]
write.table(sz2,"clipboard-16384",row.names = FALSE,sep = "\t",dec = ",")


sz2[sz2$szak=="Novenyorvos_I_szint",]$ev
sz2[str_detect(tolower(sz2$szak),"környezetmérnök"),]$ev
szakomlesztett[str_detect(tolower(szakomlesztett),"agr[aá]rm[eé]rn[oö]k")]
sz2[sz2$hallgato=="Oszkó Dániel",]$cimlap

szakomlesztett[str_detect(tolower(szakomlesztett),"^gazdas[aá]gi")]

str_replace_all("varianciaanalízissel kertész leszek, fát nevelek; 23-szor írta","[^a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]"," ")
proba=TermDocumentMatrix(VCorpus(VectorSource(sz2$szov.filtered[345])),control = list(wordLengths=c(1,Inf)))
as.matrix(proba)
probafilter="\\b(spss|r|sas|sis)\\b"
str_extract_all("A hasis nem sis, a társas nem sas",probafilter)
str_extract_all("varianciaanalízis és regresszió segítségével áru spsst r",wordfilter)

# Bigram tokenizer
BigramTokenizer <- function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

korpusz.bigram = VCorpus(VectorSource(sz2$szov.s4[100:101]),readerControl = list(language="HU"))
tdm.bigram <- DocumentTermMatrix(korpusz.bigram, control = list(tokenize = BigramTokenizer))
freq=colSums(as.matrix(tdm.bigram))
ord=order(freq,decreasing = T)
freq[ord[1:20]]

tdm.onegram <- DocumentTermMatrix(korpusz.bigram)
freq=colSums(as.matrix(tdm.onegram))
ord=order(freq,decreasing = T)
freq[ord[1:20]]

inspect(tdm.bigram)
inspect(tdm.onegram)

s1="a variancia analízis és a regresszió analízis két fontos módszer ár index vagy komolyabb volumen index"
s2="A vizsgálat tárgya volumen index a vizsgálat eszköze variancia analízis"
kp=VCorpus(VectorSource(c(s1,s2)),readerControl = list(language="HU"))
tdm.bp <- DocumentTermMatrix(kp, control = list(tokenize = BigramTokenizer))
as.matrix(tdm.bp)

which(m.1gram[,"opens"]==1)
which(sz2$faculty==1 & sz2$university==1 & sz2$thesis==1 & sz2$introduction==1)
which(sz2$hallgato=="Szilasi_Ágnes")
sz2[1294,c("ev","szak","hallgato")]

pr.k=VCorpus(VectorSource(sz2$szov.s4[[1]]))
pr=DocumentTermMatrix(pr.k)
fr=colSums(as.matrix(pr))
or=order(fr,decreasing = T)[1:20]
sz2$egyhogy=str_count(sz2$szov.s4,"\\b(egy|hogy)\\b")
hist(sz2$egyhogy)
write.table(sz2[c("ev","szak","hallgato","egyhogy")],"clipboard-16384",row.names = FALSE,sep = "\t",dec = ",")
