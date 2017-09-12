library(ggplot2)
library(tm)
library(wordcloud)
library(SnowballC) # for stemming
library(stringr)
library(dplyr)
library(reshape2)

# Asztali gépnek vagy laptopnak megfelelő mappa választása
path0 = "E:/amunka/QDA"  # asztali gép
path0 = "C:/PE_GK/QDA"   # laptop

# sz2 dataframe elkészítése tm_for_thesisv3.R alapján vagy betöltés az alábbi helyről
setwd(paste0(path0,"/sz2"))
load("sz2.RData")

# Angol dolgozatok kigyűjtése a dolgozat szövege alapján
# 7 perc / változó
#sz2$thesis=as.integer(str_detect(tolower(sz2$szov.s4),"thesis"))
#sz2$university=as.integer(str_detect(tolower(sz2$szov.s4),"university"))
#sz2$faculty=as.integer(str_detect(tolower(sz2$szov.s4),"introduc"))
#sz2$introduction=as.integer(str_detect(tolower(sz2$szov.s4),"introduction"))


# Képzési szint és szakcsoport(=képzési terület) faktorváltozójának létrehozása
# isgt informatikából gazdaságiba sorolása; isgt=fsz+informatika
sz2$isgt=ifelse(sz2$fsz==1 & sz2$informatika==1,1,0)
# isgt esetén informatika 0-ra állítása, gazdasagi 1-re állítása
sz2$informatika=ifelse(sz2$isgt==1,0,sz2$informatika)
sz2$gazdasagi[sz2$isgt==1]=1
# Az inf. és szakig. agrármérnök tévesen msc-nek van írva, módosítsuk bsc-re.
sz2$msc[sz2$informatika==1]=0
sz2$bsc[sz2$informatika==1]=1


sz2$kepzesiszint = sz2$fsz + 2*sz2$bsc + 3*sz2$msc + 4*sz2$szakmernok
kepzesiszintek = c("FSz","BSc","Msc","Szakmérnök")
sz2$ksz=factor(kepzesiszintek[sz2$kepzesiszint],levels = rev(kepzesiszintek))
sz2$kepzesiterulet = (sz2$agrarkemia+sz2$agrarmernok+sz2$noveny+sz2$allat) +
  2*(sz2$termved+sz2$kornyezet) + 3*(sz2$borasz+sz2$kertesz) + 4*(sz2$gazdasagi+sz2$turizmus) + 5*(sz2$informatika) + 6*(sz2$tanar+sz2$egyeb)
kepzesiteruletek = c("Agrár","Környezet-, természetvédelem","Kertész, borász","Gazdasági","Informatika","egyéb")
sz2$kt = factor(kepzesiteruletek[sz2$kepzesiterulet],levels = rev(kepzesiteruletek))

sz2$szak2=sz2$agrarkemia+sz2$agrarmernok + 2*sz2$gazdasagi + 3*sz2$termved + 4*sz2$turizmus+5*sz2$noveny+6*sz2$borasz+7*sz2$kertesz+8*sz2$kornyezet+9*sz2$allat
sz2$szak2[sz2$szak2==0]=10
szakok=c("Agrár","Gazdasági","Természet","Turizmus","Növény","Borász","Kertész","Környezet","Állat","Egyéb")

sz2$szakfaktor=factor(szakok[sz2$szak2])
# tdm importálása
setwd(paste0(path0,"/eredmenyek"))
load("dtmstemmed.RData")  # a mátrix elemei szógyakoriságok
dtm.stemmed.bin = weightBin(dtm.stemmed)  # Binary weighting of term frequency; a mátrix elemei 0 vagy 1
load("dtmbigram.RData")  #  a mátrix elemei 0 vagy 1

# Mátrixok, amiből a gyakoriság változók készülnek
m.1gram=as.matrix(dtm.stemmed.bin)
m.2gram=as.matrix(dtm.bigram)

#######################################
# Plots for publication
#######################################

#######################################
# Plot 1
# szakdolik száma évenként és képzési szintenként
p = ggplot(sz2,aes(x=ev,fill=ksz))
p+geom_bar()+xlab("Év")+ylab("Darabszám")+scale_fill_manual("Képzési szint",values=c("purple","red","blue","orange"))+theme(text = element_text(size=15))


#######################################
# Plot 2
# szakdolik száma évenként és képzési területenként
p = ggplot(sz2,aes(x=ev,fill=kt))
p+geom_bar()+xlab("Év")+ylab("Darabszám")+scale_fill_manual("Szakcsoport",values=c("pink","red","green","darkorange","goldenrod","yellow"))+theme(text = element_text(size=15))


#######################################
# Plot 3
# 100 leggyakoribb szó a szógyakoriságok alapján
setwd("E:/amunka/QDA/eredmenyek")
load("dtmstemmed.RData")
freq.stemmed=colSums(as.matrix(dtm.stemmed))
ord.stemmed = order(freq.stemmed,decreasing = T)
dbszo=100 # hány szóból
# Azért 2-től megy, hogy a "hely" szó kimaradjon.
wordcloud(words=names(freq.stemmed[ord.stemmed[2:dbszo]]),freq=freq.stemmed[ord.stemmed[2:dbszo]])


#######################################
# Plot 4
# 100 leggyakoribb szó a dokumentumgyakoriságok alapján
setwd("E:/amunka/QDA/eredmenyek")
load("dtmstemmed.RData")
freq.stemmed=colSums(as.matrix(weightBin(dtm.stemmed)))
ord.stemmed = order(freq.stemmed,decreasing = T)
dbszo=100 # hány szóból
set.seed(2025)
# Azért 2-től megy, hogy a "hely" szó kimaradjon.
wordcloud(words=names(freq.stemmed[ord.stemmed[2:dbszo]]),freq=freq.stemmed[ord.stemmed[2:dbszo]])


#######################################
# Plot 5
# ANOVA arány képzési szintenként
sz2$anova=as.integer(m.1gram[,"anov"]==1|m.1gram[,"varianciaanalízis"]==1|m.1gram[,"variancianalízis"]==1|
                       m.1gram[,"varianciaelemzés"]==1|m.1gram[,"varianciatábláz"]==1|m.2gram[,"szórásnégyz elemzés"]==1|
                       m.2gram[,"varianc elemzés"]==1|m.2gram[,"varianc analízis"]==1)
p = ggplot()+stat_summary(data=sz2,mapping=aes(x=ksz,y=anova,fill=ksz),fun.y=mean,geom="bar")
p = p+theme(text = element_text(size=15))+xlab("Képzési szint")+scale_y_continuous(name="Alkalmazási arány",breaks = seq(0,0.125,0.025),labels = scales::percent)
p
p+scale_fill_manual(values=c("purple","red","blue","orange"),guide=FALSE) #guide=F removes legend

#######################################
# Plot 6
# ANOVA arány képzési szintenként és évenként szakmérnökök nélkül
# sz2$anova számolását ld. előző grafikonnál

kell = sz2[sz2$szakmernok==0,]
p = ggplot(kell,aes(x=ev,y=anova,color = ksz)) + stat_summary(fun.y = mean,geom = "line",size=2)+
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))
p+scale_color_manual("Képzési szint",values=c("red","blue","orange"))+theme(text = element_text(size=15))


#######################################
# Plot 7
# ANOVA arány szakcsoportonként, képzési szintenként és évenként szakmérnökök nélkül
# sz2$anova számolását ld. Plot 5-nél
kell = sz2[sz2$szakmernok==0 & sz2$kepzesiterulet<5,]
p = ggplot(kell,aes(x=ev,y=anova,color = ksz)) + stat_summary(fun.y = mean,geom = "line",size=2)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_color_manual("Képzési szint",values=c("red","blue","orange"),guide=F)+theme(text = element_text(size=15))
p + facet_grid(kt~.) + theme(strip.text.y = element_text(angle = 0))


#######################################
# Plot 8
# ANOVA arány termvéd szakon, ahol nem tanultak se matekot, se statisztikát
# sz2$anova számolását ld. Plot 5-nél
kell = sz2[sz2$termved==1,]
p = ggplot(kell,aes(x=ev,y=anova,color = ksz)) + stat_summary(fun.y = mean,geom = "line",size=2)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_color_manual("Képzési szint",values=c("purple","red","blue","orange"))+theme(text = element_text(size=15))
p
#######################################
# Plot 9
# ANOVA páronkénti összehasonlítás
# sz2$anova és m kiszámolását ld. Plot 5-nél
library(dplyr)
library(reshape2)
sz2$bonferroni = as.integer(m.1gram[,"bonferr"]>0)
sz2$tukey = as.integer(m.1gram[,"tukey"]>0)
sz2$duncan = as.integer(m.1gram[,"dunc"]>0)
sz2$szd = as.integer(m.1gram[,"szd"]>0)
sz2$posthoc = as.integer(m.2gram[,"pos hoc"]>0)
sz2$paronkentiosszehasonlitas = as.integer(m.2gram[,"pár összehasonlítás"]>0)

# Arányok az összes anova-s dolgozaton belül
kell = sz2 %>% filter(anova==1) %>% select(szd,bonferroni,tukey,duncan) %>% summarise(szd=mean(szd),bonferroni=mean(bonferroni),tukey=mean(tukey),duncan=mean(duncan))
kell = kell %>% melt()
p=ggplot(kell,aes(x=variable,y=value)) + geom_bar(stat = "identity",fill="red3") + coord_flip() +
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)  # coor_flip() miatt a vízszintes tengely az y
p+scale_x_discrete(name="Páronkénti összehasonlítás",labels=rev(c("Duncan","Tukey","Bonferroni","SzD5%"))) + theme(text = element_text(size=15),axis.text.y=element_text(colour = "red3",face = "bold"))


# Arányok képzési területenként
# sz2 %>% filter(anova==1) %>% select(ksz,szd,bonferroni,tukey,duncan) %>% group_by(ksz) %>% summarise(szd=mean(szd),bonferroni=mean(bonferroni),tukey=mean(tukey),duncan=mean(duncan))
# Darabszámok képzési területenként
# sz2 %>% select(ksz,szd,bonferroni,tukey,duncan) %>% group_by(ksz) %>% summarise(szd=sum(szd),bonferroni=sum(bonferroni),tukey=sum(tukey),duncan=sum(duncan))

# Hány százalék használt valamilyen posthoc tesztet?
sz2$pairwise=with(sz2,pmax(bonferroni,tukey,duncan,szd,posthoc,paronkentiosszehasonlitas))
sum(sz2$anova*sz2$pairwise)/sum(sz2$anova)


#######################################
# Plot 10
# Regresszió arány képzési szintenként
m.1gram=as.matrix(dtm.stemmed.bin)
m.2gram=as.matrix(dtm.bigram)
sz2$regresszio=as.integer(m.1gram[,"regressz"]==1|m.1gram[,"regresszió"]==1|m.1gram[,"regresszióanalízis"]==1|
                       m.1gram[,"regresszióelemzés"]==1|m.1gram[,"regressziós"]==1|m.1gram[,"regressziószámítás"]==1)
p = ggplot()+stat_summary(data=sz2,mapping=aes(x=ksz,y=regresszio,fill=ksz),fun.y=mean,geom="bar")
p = p+theme(text = element_text(size=15))+xlab("Képzési szint")+scale_y_continuous(name="Alkalmazási arány",breaks = seq(0,0.07,0.01),labels = scales::percent)
p+scale_fill_manual(values=c("purple","red","blue","orange"),guide=FALSE) #guide=F removes legend


#######################################
# Plot 11
# Regresszió arány képzési szintenként és évenként szakmérnökök nélkül
# sz2$regresszio számolását ld. Plot 10-nél
kell = sz2[sz2$szakmernok==0,]
p = ggplot(kell,aes(x=ev,y=regresszio,color = ksz)) + stat_summary(fun.y = mean,geom = "line",size=2)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))
p+scale_color_manual("Képzési szint",values=c("red","blue","orange"))+theme(text = element_text(size=15))


#######################################
# Plot 12
# regresszió arány szakcsoportonként, képzési szintenként és évenként szakmérnökök nélkül
# sz2$regresszió számolását ld. Plot 10-nél
kell = sz2[sz2$szakmernok==0 & sz2$kepzesiterulet<5,]
p = ggplot(kell,aes(x=ev,y=regresszio,color = ksz)) + stat_summary(fun.y = mean,geom = "line",size=2)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Alkalmazási arány",breaks=c(0,0.05,0.1),labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_color_manual("Képzési szint",values=c("red","blue","orange"),guide=F)+theme(text = element_text(size=15))
p + facet_grid(kt~.) + theme(strip.text.y = element_text(angle = 0))



#######################################
# Plot 13
# Idősor arány szakcsoportonként
m.1gram=as.matrix(dtm.stemmed.bin)
m.2gram=as.matrix(dtm.bigram)
sz2$idosor=as.integer(m.1gram[,"idősor"]==1|m.1gram[,"idősorelemzés"]==1|m.1gram[,"idősormodell"]==1|
                        m.1gram[,"idősoros"]==1|m.1gram[,"szezonalitás"]==1|m.1gram[,"szezonalítás"]==1|
                        m.1gram[,"tartamidősor"]==1|m.1gram[,"tren"]==1|m.1gram[,"trendanalízis"]==1|
                        m.1gram[,"trendelemzés"]==1|m.1gram[,"trendfüggvény"]==1|
                        m.1gram[,"trendhatás"]==1|m.1gram[,"trendmentes"]==1|m.1gram[,"trendszámítás"]==1)
p = ggplot()+stat_summary(data=sz2,mapping=aes(x=kt,y=idosor,fill=kt),fun.y=mean,geom="bar")
p = p+theme(text = element_text(size=15))+xlab("Szakcsoport")+scale_y_continuous(name="Alkalmazási arány",labels = scales::percent) #,limits = c(0,1) a scale_y_continuous-ba, ha az y tengelyt 100%-ig akarjuk skálázni
p+theme(axis.text.x=element_text(angle=15,hjust=1))+scale_fill_manual(values=c("pink","red","green","darkorange","goldenrod","yellow"),guide=FALSE) #guide=F removes legend


#######################################
# Plot 14
# idősor arány szakcsoportonként, képzési szintenként és évenként szakmérnökök nélkül
# sz2$idosor számolását ld. Plot 13-nál
kell = sz2[sz2$szakmernok==0 & sz2$kepzesiterulet<5,]
p = ggplot(kell,aes(x=ev,y=idosor,color = ksz)) + stat_summary(fun.y = mean,geom = "line",size=2)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_color_manual("Képzési szint",values=c("red","blue","orange"),guide=F)+theme(text = element_text(size=15))
p + facet_grid(kt~.) + theme(strip.text.y = element_text(angle = 0))


#######################################
# Plot 15
# Korreláció arány képzési szintenként
m.1gram=as.matrix(dtm.stemmed.bin)
m.2gram=as.matrix(dtm.bigram)
sz2$korrelacio=as.integer(m.1gram[,"korreláció"]==1|m.1gram[,"korrelációs"]==1|m.1gram[,"korrelációanalízis"]==1|
                            m.1gram[,"korrelációszámítás"]==1|m.1gram[,"korrelál"]==1|m.1gram[,"korrelálatl"]==1|
                            m.1gram[,"korrelálatlanság"]==1|m.1gram[,"korreláló"]==1)
p = ggplot()+stat_summary(data=sz2,mapping=aes(x=ksz,y=korrelacio,fill=ksz),fun.y=mean,geom="bar")
p = p+theme(text = element_text(size=15))+xlab("Képzési szint")+
  scale_y_continuous(name="Alkalmazási arány",breaks = seq(0,0.15,0.02),labels = scales::percent)+
  theme(text = element_text(size=15),axis.text.y=element_text(size=20))
p+scale_fill_manual(values=c("purple","red","blue","orange"),guide=FALSE) #guide=F removes legend


#######################################
# Plot 16
# korreláció arány szakcsoportonként, képzési szintenként és évenként szakmérnökök nélkül
# sz2$idosor számolását ld. Plot 13-nál
kell = sz2[sz2$szakmernok==0 & sz2$kepzesiterulet<5,] 
p = ggplot(kell,aes(x=ev,y=korrelacio,color = ksz)) + stat_summary(fun.y = mean,geom = "line",size=2)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_color_manual("Képzési szint",values=c("red","blue","orange"),guide=F)+theme(text = element_text(size=15))
p + facet_grid(kt~.) + theme(strip.text.y = element_text(angle = 0))


#######################################
# Plot 17
# KSH arány szakcsoportonként
m.1gram=as.matrix(dtm.stemmed.bin)
m.2gram=as.matrix(dtm.bigram)
sz2$ksh=as.integer(m.1gram[,"ksh"]==1)
p = ggplot()+stat_summary(data=sz2,mapping=aes(x=kt,y=ksh,fill=kt),fun.y=mean,geom="bar")
p = p+theme(text = element_text(size=15))+xlab("Szakcsoport")+scale_y_continuous(name="Alkalmazási arány",labels = scales::percent) #,limits = c(0,1) a scale_y_continuous-ba, ha az y tengelyt 100%-ig akarjuk skálázni
p+theme(axis.text.x=element_text(angle=15,hjust=1))+scale_fill_manual(values=c("pink","red","green","darkorange","goldenrod","yellow"),guide=FALSE) #guide=F removes legend


#######################################
# Plot 18
# KSH arány szakcsoportonként, képzési szintenként és évenként szakmérnökök nélkül
# sz2$ksh számolását ld. Plot 17-nél
kell = sz2[sz2$szakmernok==0 & sz2$kepzesiterulet<5,] 
p = ggplot(kell,aes(x=ev,y=ksh,color = ksz)) + stat_summary(fun.y = mean,geom = "line",size=2)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Hivatkozási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_color_manual("Képzési szint",values=c("red","blue","orange"),guide=F)+theme(text = element_text(size=15))
p + facet_grid(kt~.) + theme(strip.text.y = element_text(angle = 0))


#######################################
# Plot 19
# swot arány szakcsoportonként
m.1gram=as.matrix(dtm.stemmed.bin)
m.2gram=as.matrix(dtm.bigram)
sz2$swot=as.integer(m.1gram[,"swot"]==1)
p = ggplot()+stat_summary(data=sz2,mapping=aes(x=kt,y=swot,fill=kt),fun.y=mean,geom="bar")
p = p+theme(text = element_text(size=15))+xlab("Szakcsoport")+scale_y_continuous(name="Alkalmazási arány",labels = scales::percent) #,limits = c(0,1) a scale_y_continuous-ba, ha az y tengelyt 100%-ig akarjuk skálázni
p+theme(axis.text.x=element_text(angle=15,hjust=1))+scale_fill_manual(values=c("pink","red","green","darkorange","goldenrod","yellow"),guide=FALSE) #guide=F removes legend


#######################################
# Plot 20
# swot arány szakcsoportonként, képzési szintenként és évenként szakmérnökök nélkül
# sz2$swot számolását ld. Plot 19-nél
kell = sz2[sz2$szakmernok==0 & sz2$kepzesiterulet<5,] 
p = ggplot(kell,aes(x=ev,y=swot,fill = ksz)) + stat_summary(fun.y = mean,geom = "area",alpha=0.8)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_fill_manual("Képzési szint",values=c("red","blue","orange"),guide=F)+theme(text = element_text(size=15))
p + facet_grid(kt~.) + theme(strip.text.y = element_text(angle = 0))

kell = sz2[sz2$szakmernok==0 & sz2$kepzesiterulet<5,] 
p = ggplot(kell,aes(x=ev,y=swot,color = ksz)) + stat_summary(fun.y = mean,geom = "line",size=2)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_color_manual("Képzési szint",values=c("red","blue","orange"),guide=F)+theme(text = element_text(size=15))
p + facet_grid(kt~.) + theme(strip.text.y = element_text(angle = 0))


#######################################
# Plot 21
# statisztikai szoftverek (csak táblázat)
library(dplyr)
library(reshape2)
sz2$spss=as.integer(m.1gram[,"spss"]==1)
sz2$excel=as.integer(m.1gram[,"excel"]==1|m.1gram[,"excelből"]==1|m.1gram[,"msexcel"]==1)
sz2$matlab=as.integer(m.1gram[,"matlab"]==1)
sz2$rprogram=as.integer(m.2gram[,"r progr"]==1|m.2gram[,"r statisz"]==1)
sz2$statsoft=as.integer(m.1gram[,"statsof"]==1|m.1gram[,"statistic"]==1)
sz2$scilab=as.integer(m.1gram[,"scilab"]==1)
sz2$maple=as.integer(m.1gram[,"mapl"]==1)
sz2$minitab=as.integer(m.1gram[,"minitab"]==1)
# sz2$python=as.integer(m.1gram[,"python"]==1) # 1 dolgozatban szerepel "python script"

# Gyakoriságok
kell = sz2 %>% select(spss,excel,matlab,rprogram,statsoft,scilab,
                      maple,minitab) %>% summarise(spss=sum(spss),excel=sum(excel),matlab=sum(matlab),
                      rprogram=sum(rprogram),statsoft=sum(statsoft),scilab=sum(scilab),maple=sum(maple),minitab=sum(minitab))
kell = kell %>% melt()
# Grafikon nincs, táblázatban szerepel.


#######################################
# Plot 22
# spss gyakoriságok
# sz2$spss kiszámolását ld. Plot 21-nél
kell = sz2 %>% select(spss,kt,ksz) %>% group_by(kt,ksz) %>% summarise(spss=sum(spss))
p=ggplot(kell,aes(x=kt,y=spss,fill=ksz))+geom_bar(stat="identity")
p+scale_y_continuous(name="Gyakoriság")+scale_x_discrete(name="Szakcsoport")+
  scale_fill_manual(values=c("purple","red","blue","orange"))+
  theme(axis.text.x=element_text(angle=20,hjust=1),text = element_text(size=15))


#######################################
# Plot 23
#t-próba gyakoriságok
sz2$tproba=as.integer(m.2gram[,"t pró"]==1|m.2gram[,"t tesz"]==1)
kell=sz2%>%filter(kepzesiterulet<5)
p = ggplot()+stat_summary(data=kell,mapping=aes(x=kt,y=tproba,fill=kt),fun.y=mean,geom="bar")
p = p+theme(text = element_text(size=15))+xlab("Szakcsoport")+scale_y_continuous(name="Alkalmazási arány",labels = scales::percent) #,limits = c(0,1) a scale_y_continuous-ba, ha az y tengelyt 100%-ig akarjuk skálázni
p+theme(axis.text.x=element_text(angle=15,hjust=1))+scale_fill_manual(values=c("green","darkorange","goldenrod","yellow"),guide=FALSE) #guide=F removes legend


#######################################
# Plot 24
# egyéb (ritkábban használt) próbák gyakorisága
sz2$fproba=as.integer(m.2gram[,"f pro"]==1|m.2gram[,"f pró"]==1|m.2gram[,"f tesz"]==1)
sz2$fisher=as.integer(m.2gram[,"fischer fél"]==1|m.2gram[,"fisher fél"]==1)
sz2$khinegyzet=as.integer(m.2gram[,"khi négyz"]==1)
sz2$levene=as.integer(m.2gram[,"lev tesz"]==1)
sz2$welch=as.integer(m.1gram[,"welch"]==1)  #50
sz2$wilcoxon=as.integer(m.1gram[,"wilcox"]==1) #12
sz2$kruskalwallis=as.integer(m.1gram[,"kruskal"]==1|m.1gram[,"wallis"]==1)
sz2$shapirowilk=as.integer(m.1gram[,"shapir"]==1)
sz2$manwhitney=as.integer(m.1gram[,"whitney"]==1)
sz2$kolmogorov=as.integer(m.1gram[,"kolmogorov"]==1|m.1gram[,"smirnov"]==1|m.1gram[,"szmirnov"]==1)

kell = sz2 %>% select(fproba,fisher,khinegyzet,levene,welch,wilcoxon,
                      kruskalwallis,shapirowilk,manwhitney,kolmogorov) %>%
               summarise(fproba=sum(fproba),fisher=sum(fisher),khinegyzet=sum(khinegyzet),
                         levene=sum(levene),welch=sum(welch),wilcoxon=sum(wilcoxon),
                         kruskalwallis=sum(kruskalwallis),shapirowilk=sum(shapirowilk),
                         manwhitney=sum(manwhitney),kolmogorov=sum(kolmogorov)) %>%
               melt()
p=ggplot(kell,aes(x=variable,y=value)) + geom_bar(stat = "identity",fill="red3") + coord_flip() +
  scale_y_continuous(name="Gyakoriság")  # coor_flip() miatt a vízszintes tengely az y
p+scale_x_discrete(name="Statisztikai próba",labels=c("F-próba","Fisher egzakt teszt","Khinégyzet-próba",
                                                          "Levene teszt","Welch (d) próba","Wilcoxon próba",
                                                          "Kruskal-Wallis próba","Shapiro-Wilk próba",
                                                          "Mann-Whitney próba","Kolmogorov-Szmirnov próba")) +
  theme(text = element_text(size=15),axis.text.y=element_text(colour = "red3",face = "bold"))


#######################################
# Plot 25
# kérdőív arány szakcsoportonként
sz2$kerdoiv=as.integer(m.1gram[,"kérdőív"]==1)
p = ggplot()+stat_summary(data=sz2[sz2$kepzesiterulet<5,],mapping=aes(x=kt,y=kerdoiv,fill=kt),fun.y=mean,geom="bar")
p = p+theme(text = element_text(size=15))+xlab("Szakcsoport")+scale_y_continuous(name="Alkalmazási arány",labels = scales::percent) #,limits = c(0,1) a scale_y_continuous-ba, ha az y tengelyt 100%-ig akarjuk skálázni
p+theme(axis.text.x=element_text(angle=15,hjust=1))+scale_fill_manual(values=c("green","darkorange","goldenrod","yellow"),guide=FALSE) #guide=F removes legend


#######################################
# Plot 26, 27
# kérdőív arány szakcsoportonként, képzési szintenként és évenként szakmérnökök nélkül
# sz2$kerdov számolását ld. Plot 25-nél
kell = sz2[sz2$szakmernok==0 & sz2$szak2 %in% c(2,3,4),] 
#Plot 26
p = ggplot(kell,aes(x=ev,y=kerdoiv,color = ksz)) + stat_summary(fun.y = mean,geom = "line",size=2)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_color_manual("Képzési szint",values=c("red","blue","orange"),guide=F)+theme(text = element_text(size=15))
p + facet_grid(szakfaktor~.) + theme(strip.text.y = element_text(angle = 0))



#Plot27
p = ggplot(kell,aes(x=ev,y=kerdoiv,fill = ksz)) + stat_summary(fun.y = sum,geom = "area",alpha=0.8)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Gyakoriság")+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_fill_manual("Képzési szint",values=c("red","blue","orange"),guide=F)+theme(text = element_text(size=15))
p + facet_grid(szakfaktor~.) + theme(strip.text.y = element_text(angle = 0))

p = ggplot(kell,aes(x=ev,y=kerdoiv,color = ksz)) + stat_summary(fun.y = sum,geom = "line",size=2)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Gyakoriság")+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_color_manual("Képzési szint",values=c("red","blue","orange"),guide=F)+theme(text = element_text(size=15))
p + facet_grid(szakfaktor~.) + theme(strip.text.y = element_text(angle = 0))


p = ggplot(kell,aes(x=ev,y=kerdoiv,fill = ksz)) + stat_summary(fun.y = mean,geom = "area",alpha=0.8)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_fill_manual("Képzési szint",values=c("red","blue","orange"),guide=F)+theme(text = element_text(size=15))
p + facet_grid(kt~.) + theme(strip.text.y = element_text(angle = 0))


#######################################
# Plot 28
# index arány képzési szintenként
sz2$index=as.integer(m.1gram[,"index"]==1|m.1gram[,"árindex"]==1|m.1gram[,"volumenindex"]==1|
                       m.1gram[,"értékindex"]==1|
                       m.1gram[,"indexszámítás"]==1|m.1gram[,"indexből"]==1|
                       m.1gram[,"árindexszel"]==1|m.1gram[,"indexsor"]==1)
p = ggplot()+stat_summary(data=sz2,mapping=aes(x=ksz,y=index,fill=ksz),fun.y=mean,geom="bar")
p = p+theme(text = element_text(size=15))+xlab("Képzési szint")+scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)
p
p+scale_fill_manual(values=c("purple","red","blue","orange"),guide=FALSE) #guide=F removes legend


#######################################
# Plot 29
# index arány képzési szintenként és évenként szakmérnökök nélkül
# sz2$index számolását ld. előző grafikonnál
kell = sz2[sz2$szakmernok==0,]
p = ggplot(kell,aes(x=ev,y=index,color = ksz)) + stat_summary(fun.y = mean,geom = "line",size=2)+
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))
p+scale_color_manual("Képzési szint",values=c("red","blue","orange"))+theme(text = element_text(size=15))


#######################################
# Plot 30
# index arány szakcsoportonként, képzési szintenként és évenként szakmérnökök nélkül
# sz2$anova számolását ld. Plot 28-nál
kell = sz2[sz2$szakmernok==0 & sz2$kepzesiterulet<5,]
p = ggplot(kell,aes(x=ev,y=index,color = ksz)) + stat_summary(fun.y = mean,geom = "line",size=2)+ # ,alpha=0.9 a stat_summary-ba átlátszó színeket ad
  scale_y_continuous(name="Alkalmazási arány",labels = scales::percent)+
  scale_x_continuous(name="Év",breaks=seq(2000,2014,2),labels = seq(2000,2014,2))+
  scale_color_manual("Képzési szint",values=c("red","blue","orange"),guide=F)+theme(text = element_text(size=15))
p + facet_grid(kt~.) + theme(strip.text.y = element_text(angle = 0))


#######################################
# Plot 31
# Záródiára wordcloud az összes szó alapján
setwd(paste0(path0,"/eredmenyek"))
load("dtmtotal.RData")  # a mátrix elemei szógyakoriságok
dtm.total.reduced=removeSparseTerms(dtm.total,0.95) #0.95=>6899terms 0.99=>24000terms 0.98=>14000
freq=colSums(as.matrix(dtm.total.reduced))
ord=order(freq,decreasing = TRUE)
kell=c(12,15,17,18,21:24,27:28,35:36,39:40,43:44,46:56,65:67,69:73,75,77:81,83:85,87,89:101,106)
kell=kell-1
wordcloud(words=names(freq[ord[kell]]),freq=freq[ord[kell]],random.order=F,random.color = TRUE,colors = "red2")

write.table(names(freq[ord[1:200]]),"clipboard",row.names = FALSE,sep = "\t",dec = ",")                      



# Asistant tools
# Mely kifejezések szerepelnek valójában?
write.table(names(freq.stemmed[freq.stemmed>0]),"clipboard",row.names = FALSE,sep = "\t",dec = ",")
# save sz2 for visualization
toremove=which(colnames(sz2)=="szov.s4")
sz2=sz2[-toremove]
toremove=which(colnames(sz2)=="szak")
sz2=sz2[-toremove]
setwd(paste0(path0,"/sz2"))
save(sz2,file="sz2_to_vis.RData")
dtm596freq=as.data.frame(as.matrix(dtm.stemmed))
dtm596bin=as.data.frame(as.matrix(dtm.stemmed.bin))
setwd(paste0(path0,"/eredmenyek"))
save(dtm596bin,file="dtm596bin.RData")
save(dtm596freq,file="dtm596freq.RData")
