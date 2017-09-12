?# remove.packages("pdftools") Mivel a dependencies = TRUE n?lk?l lett telep?tve
# install.packages("pdftools", dependencies = TRUE)
# install.packages("stringr", dependencies = TRUE) for the str_count() function
library(pdftools)
library(stringr)
library(tm)
library(wordcloud)

# Guide to pdftools. Source: https://www.r-bloggers.com/introducing-pdftools-a-fast-and-portable-pdf-extractor/
download.file("http://arxiv.org/pdf/1403.2805.pdf", "1403.2805.pdf", mode = "wb")
txt <- pdf_text("1403.2805.pdf")
cat(txt[1])

###############################################
################### File operations
###############################################


# Set library
setwd("E:/amunka/QDA/feldolgozasra_var/szd2009_pdf/Agr?rm?rn?k Szak/Gubek M?nika")
setwd("E:/library/szakdolgozat/szd1999_pdf/Agrarkemikus Agrarmernok/?d?m_P?ter_Gyula")
# get the list of files
file_list = list.files()
# get the list of folders
dir_list = list.dirs()

# Csak adott kiterjeszt?s? f?jlok beolvas?sa
main_dir = "M:/feladatok"
file_list = list.files(main_dir,"*.pdf")


############################
#### pdftools package
############################

# read the first file
txt = pdf_text(file_list[1])
# concatenate the pages into a single string
txt = paste(txt, collapse = " ")
# read the table of contents; return empty list if not exist
toc = pdf_toc(file_list[1])
# tov?bbi f?ggv?nyek a pdftools-b?l
# pdf_info()
info = pdf_info(file_list[1])
info$pages # oldalsz?m     egysorba is mehet: pdf_info(file_list[1])$pages
# pdf_fonts()
fonts = pdf_fonts(file_list[1])  # Nem fog kelleni
# pdf_attachments()
attachments = pdf_attachments(file_list[1])


####################################################
### String functions in base R
####################################################
# Sorv?gjelek sz?k?zz? alak?t?sa
txt = gsub("\r\n", " ", txt)
nchar(txt) # karaktersz?m
txt = gsub("\\s+"," ",txt)  # Remove unnecessary whitespaces

####################################################
### stringr package
####################################################
# to lowercase
txt = str_to_lower(txt)
# konkaten?ci?
txt_big = str_c(txt,collapse="")
# substring
str_sub(txt_big,1,10)  # Visszaadja az els? karaktert?l a 10-ik karakterig tart? substring-et.
# length
str_length(txt)   # karaktersz?m
# count of occurences
str_count("alma","a")
str_count("a","alma")
str_count(txt,"herbicid") # list?t ad vissza. Melyik oldalon h?nyszor fordul el? a 'herbicid' sz?.
sum(str_count(txt,"herbicid"))
str_count(txt,"statisztika")
# location of a pattern
hol = str_locate(txt, "konzulens") # m?trixot ad vissza. minden oldalra meghat?rozza mett?l meddig tart a pattern.
hol = str_locate(txt_big,"konzulens")
hol = str_locate(txt_big,"herbicid") #?az els? el?fordul?s kezd? ?s utols? poz?ci?j?t adja vissza
hol = str_locate_all(txt_big,"herbicid") # lista form?j?ban visszaadja az ?sszes el?fordul?s els? ?s utols? poz?ci?j?t




#######################################################################
######  Statisztika jegyzet beolvasása kifejezésgyűjtemény készítéséhez
#######################################################################
jegyzetek=c()
path0="C:/PE_GK/QDA/collection of terms"
file_list = list.files(path = path0, pattern = "*.pdf", full.names = TRUE, recursive = FALSE, ignore.case = TRUE) # recursive = F, hogy almappabol ne olvasson
for (pdffile in file_list) {
  # read file
  txt = pdf_text(pdffile)
  # concatenate the pages into a single string
  txt = paste(txt, collapse = " ")
  # Sorv?gjelek sz?k?zz? alak?t?sa
  txt = gsub("\r\n", " ", txt)
  jegyzetek=c(jegyzetek,txt)
}
# Kisbet?s
# jegyzetek=tolower(jegyzetek) # Error with encoding
jegyzetek=str_to_lower(jegyzetek)
# Csak a magyar ABC bet?i maradjanak
jegyzetek = str_replace_all(jegyzetek,"[^a-zA-ZáéíóöőúüűÁÉÍÓÖŐÚÜŰ]"," ")
# Legal?bb 4 hossz?, azonos karakterb?l ?ll? substringet lecser?li egy sz?k?zre
jegyzetek = str_replace_all(jegyzetek,"(.)\\1{3,}"," ")
# F?l?sleges whitespace elt?vol?t?sa
jegyzetek = str_replace_all(jegyzetek," {2,}"," ")

# Sz?t?vez?s
jegyzetek.stem=stemDocument(jegyzetek,language = "hungarian")
jegyzetek.stemstem=stemDocument(jegyzetek.stem,language = "hungarian")
jegyzetek.s3=stemDocument(jegyzetek.stemstem,language = "hungarian")
jegyzetek.s4=stemDocument(jegyzetek.s3,language = "hungarian")

# Remove stopwords
jegyzetek=removeWords(jegyzetek, stopwords("hungarian"))
jegyzetek.stem=removeWords(jegyzetek.stem, stopwords("hungarian"))
jegyzetek.stemstem=removeWords(jegyzetek.stemstem, stopwords("hungarian"))
jegyzetek.s3=removeWords(jegyzetek.s3, stopwords("hungarian"))
jegyzetek.s4=removeWords(jegyzetek.s4, stopwords("hungarian"))


# Korpussz? alak?t?s
jegyzetkorpusz=VCorpus(VectorSource(jegyzetek),readerControl = list(language="HU"))
jegyzetkorpusz.stemmed=VCorpus(VectorSource(jegyzetek.stem),readerControl = list(language="HU"))
jegyzetkorpusz.stemmedstemmed=VCorpus(VectorSource(jegyzetek.stemstem),readerControl = list(language="HU"))
jegyzetkorpusz.s3=VCorpus(VectorSource(jegyzetek.s3),readerControl = list(language="HU"))
jegyzetkorpusz.s4=VCorpus(VectorSource(jegyzetek.s4),readerControl = list(language="HU"))

#Term-document matrix el??ll?t?sa
jegyzetdtm = DocumentTermMatrix(jegyzetkorpusz)
jegyzetdtm.stemmed = DocumentTermMatrix(jegyzetkorpusz.stemmed)
jegyzetdtm.stemmedstemmed = DocumentTermMatrix(jegyzetkorpusz.stemmedstemmed)
jegyzetdtm.s3 = DocumentTermMatrix(jegyzetkorpusz.s3)
jegyzetdtm.s4 = DocumentTermMatrix(jegyzetkorpusz.s4)
inspect(jegyzetdtm)
inspect(jegyzetdtm.stemmed)
inspect(jegyzetdtm.stemmedstemmed)
inspect(jegyzetdtm.s3)
inspect(jegyzetdtm.s4)

#Leggyakoribb szavak kiirat?sa
freq=colSums(as.matrix(jegyzetdtm)) #Az oszlop?sszegek nevei a kifejez?sek
freq.stemmed=colSums(as.matrix(jegyzetdtm.stemmed))
freq.stemmedstemmed=colSums(as.matrix(jegyzetdtm.stemmedstemmed))
freq.s3=colSums(as.matrix(jegyzetdtm.s3))
freq.s4=colSums(as.matrix(jegyzetdtm.s4))
ord = order(freq,decreasing = T)
ord.stemmed = order(freq.stemmed,decreasing = T)
ord.stemmedstemmed = order(freq.stemmedstemmed,decreasing = T)
ord.s3 = order(freq.s3,decreasing = T)
ord.s4 = order(freq.s4,decreasing = T)
#freq[ord[1:200]]
#freq.stemmed[ord.stemmed[1:200]]
#freq.s3[ord.s3[1:200]]
#freq.s4[ord.s4[1:200]]
freq[ord]
freq[freq>0]
write.table(freq[ord],"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(names(freq[ord]),"clipboard-128",row.names = FALSE,sep = "\t",dec = ",")
write.table(freq.stemmed[ord.stemmed],"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(names(freq.stemmed[ord.stemmed]),"clipboard-128",row.names = FALSE,sep = "\t",dec = ",")
write.table(freq.s4[ord.s4],"clipboard",row.names = FALSE,sep = "\t",dec = ",")
write.table(names(freq.s4[ord.s4]),"clipboard-128",row.names = FALSE,sep = "\t",dec = ",")

# Seg?t-e a t?bbsz?ri stemmel?s? Igen! Sok esetben seg?t! N?ha nem, pl: adat->ad
szavak=colnames(as.matrix(jegyzetdtm))
#write.table(szavak,"clipboard-1024",row.names = FALSE,sep = "\t",dec = ",")
szavak.s1=stemDocument(szavak,language = "hungarian")
#write.table(szavak.s1,"clipboard-128",row.names = FALSE,sep = "\t",dec = ",")
szavak.s2=stemDocument(szavak.s1,language = "hungarian")
#write.table(szavak.s2,"clipboard-128",row.names = FALSE,sep = "\t",dec = ",")
szavak.s3=stemDocument(szavak.s2,language = "hungarian")
#write.table(szavak.s3,"clipboard-128",row.names = FALSE,sep = "\t",dec = ",")
szavak.s4=stemDocument(szavak.s3,language = "hungarian")
#write.table(szavak.s4,"clipboard-1024",row.names = FALSE,sep = "\t",dec = ",")

# wordcloud
dbszo=80 # h?ny sz?b?l
wordcloud(words=names(freq.stemmed[ord.stemmed[1:dbszo]]),freq=freq.stemmed[ord.stemmed[1:dbszo]])
wordcloud(words=names(freq.stemmedstemmed[ord.stemmedstemmed[1:dbszo]]),freq=freq.stemmedstemmed[ord.stemmedstemmed[1:dbszo]])

# Kiválasztott szavak beolvasása
#termsfromjegyzet <- read.table("C:/PE_GK/QDA/termsfromjegyzet.csv", 
#                               header=FALSE, sep=";", na.strings="NA", dec=".", stringsAsFactors = FALSE,strip.white=TRUE)
#termsfrombook=termsfromjegyzet$V1
setwd("C:/PE_GK/QDA/collection of terms")
#save(termsfrombook,file="termsfrombook.RData")
load(file="termsfrombook.RData")
