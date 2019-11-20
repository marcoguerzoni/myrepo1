
install.packages("data.table")
install.packages("naniar")
install.packages("VIM")
install.packages("tableplot")
setwd("C:/Users/despina/Google Drive/ricerca/myprojects/myrepo1")

#import data 1995-2007
require(data.table)
require(dplyr)
data <- as.data.frame(fread("sbs_r_nuts03.tsv", na.strings=":"))
#split first variable


data1 <- setDT(data)[,paste0("A1", 1:3) := tstrsplit(V1, ",")][,-"V1", with = F]
data1[1,16] <- "geo"
#first row became colnames
colnames(data1) <- as.character(unlist(data1[1,]))
#remove first colum
data2<-data1[-1,]

#change name of the economic variable from eurostat sbs
# V11210	Local units - number
# V13320	Wages and Salaries - million euro
# V15110	Gross investment in tangible goods - million euro
# V16110	Persons employed - number
# V91290	Growth rate of employment - percentage
# V94310	Share of employment in manufacturing total - percentage
# V94414	Investment per person employed - thousands euro

data3 <- replace(data2, data2 == "V11210", "Unit")
data3 <- replace(data3, data3 == "V13320", "Wages")
data3 <- replace(data3, data3 == "V15110", "Tangibles")
data3 <- replace(data3, data3 == "V16110", "Empl")
data3 <- replace(data3, data3 == "V91290", "Empl. Growth")
data3 <- replace(data3, data3 == "V94310", "Manufacturing")
data3 <- replace(data3, data3 == "V94414", "Investment PP")

#sum(is.na(data3))


data3 <- na_if(data3, ":")
data3 <- na_if(data3, ": c")
data3 <- na_if(data3, ": cd")
data3 <- na_if(data3, ": d")
data3 <- na_if(data3, ": u")
data3 <- na_if(data3, ": b")
data3 <- na_if(data3, ": be")
data3 <- na_if(data3, ": bc")

# na: 1289881
#219611*16=3513776



data4<- as.data.frame(apply(data3[,1:13], 2, function(y) as.numeric(gsub("p", "", y))))
data4<- as.data.frame(apply(data4[,1:13], 2, function(y) as.numeric(gsub("b", "", y))))
data4<- as.data.frame(apply(data4[,1:13], 2, function(y) as.numeric(gsub("be", "", y))))
data4<- as.data.frame(apply(data4[,1:13], 2, function(y) as.numeric(gsub("z", "", y))))
data4 <- as.data.frame(apply(data4[,1:13], 2, function(y) as.numeric(gsub("[^0-9.]", "", y))))

sum(is.na(data4)) #1291751
#219611*13 #2854943
#1291751/2854943 #0.45

final9507 <- cbind(data4, data3[,14:16])

final9507$id  <- paste(final9507$nace_r1,final9507$indic_sb,final9507$geo)

#download data 2008-2017

data <- as.data.frame(fread("sbs_r_nuts06_r2.tsv"), na.strings=": c")
#split first variable
#data[72,3]
#data[29,11]

data1 <- setDT(data)[,paste0("A1", 1:3) := tstrsplit(V1, ",")][,-"V1", with=F]
data1[1,13] <-"geo"
#first row became colnames
colnames(data1) <- as.character(unlist(data1[1,]))
#remove first colum
data2<-data1[-1,]

#change name of the economic variable from eurostat sbs
# V11210	Local units - number
# V13320	Wages and Salaries - million euro
# V15110	Gross investment in tangible goods - million euro
# V16110	Persons employed - number
# V91290	Growth rate of employment - percentage
# V94310	Share of employment in manufacturing total - percentage
# V94414	Investment per person employed - thousands euro

data3 <- replace(data2, data2 == "V11210", "Unit")
data3 <- replace(data3, data3 == "V13320", "Wages")
data3 <- replace(data3, data3 == "V15110", "Tangibles")
data3 <- replace(data3, data3 == "V16110", "Empl")
data3 <- replace(data3, data3 == "V91290", "Empl. Growth")
data3 <- replace(data3, data3 == "V94310", "Manufacturing")
data3 <- replace(data3, data3 == "V94414", "Investment PP")



data3 <- na_if(data3, ":")
data3 <- na_if(data3, ": c")
data3 <- na_if(data3, ": cd")
data3 <- na_if(data3, ": d")
data3 <- na_if(data3, ": u")
data3 <- na_if(data3, ": b")
data3 <- na_if(data3, ": be")
data3 <- na_if(data3, ": bc")

data4<- as.data.frame(apply(data3[,1:10], 2, function(y) as.numeric(gsub("p", "", y))))
data4<- as.data.frame(apply(data4[,1:10], 2, function(y) as.numeric(gsub("b", "", y))))
data4<- as.data.frame(apply(data4[,1:10], 2, function(y) as.numeric(gsub("be", "", y))))
data4<- as.data.frame(apply(data4[,1:13], 2, function(y) as.numeric(gsub("z", "", y))))
final0817 <- cbind(data4, data3[,11:13])

final0817$id  <- paste(final0817$nace_r1,final0817$indic_sb,final0817$geo)

#######MISSING VALUE ANALYSIS

library(stringr)
library(tidyr)
#make it long from wide
long_9507 <- final9507 %>% gather(Year, Value, -nace_r1, -indic_sb, -geo, -id)

#create variable for country
long_9507$country <- substr(long_9507$geo, 0, 2)
NA9507 <- spread(long_9507, Year, Value)

NA9507<-NA9507[NA9507$indic_sb=="Empl",]
#del 95-96
NA9507 <- NA9507[,-6]
NA9507 <- NA9507[,-6]
NA9507 <- NA9507[,-6]
#function to delete NA over a threshold
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

NADEL9507 <- delete.na(NA9507, 2)
data<-NADEL9507[,6:15]
colnames(data)<-c("A","B","C","D","E","F","G","H","I", "L")
install.packages("mice")
library(mice)
tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
completedData <- complete(tempData,1)

stripplot(tempData, pch = 20, cex = 1.2)
####additional stat
# year <- aggregate(Value ~ Year, data=long_9507, function(x) {sum(is.na(x))}, na.action = NULL)
# yearsum <- aggregate(Value ~ Year, data=long_9507, function(x) {sum(!is.na(x))}, na.action = NULL)
# pernayear<-year$Value/(year$Value+yearsum$Value)
# year$percentace<-pernayear
# 
# 
# country <- aggregate(Value ~ country, data=long_9507, function(x) {sum(is.na(x))}, na.action = NULL)
# countrysum <- aggregate(Value ~ country, data=long_9507, function(x) {sum(!is.na(x))}, na.action = NULL)
# pernacountry<-country$Value/(country$Value+countrysum$Value)
# country$percentage<-pernacountry
# 
# indic_sb <- aggregate(Value ~ indic_sb, data=long_9507, function(x) {sum(is.na(x))}, na.action = NULL)
# indic_sbsum <- aggregate(Value ~ indic_sb, data=long_9507, function(x) {sum(!is.na(x))}, na.action = NULL)
# pernaindic_sb<-indic_sb$Value/(indic_sb$Value+indic_sbsum$Value)
# indic_sb$percentage<-pernaindic_sb
# 
# 
# geo <- aggregate(Value ~ geo, data=long_9507, function(x) {sum(is.na(x))}, na.action = NULL)
# geosum <- aggregate(Value ~ geo, data=long_9507, function(x) {sum(!is.na(x))}, na.action = NULL)
# pernageo<-geo$Value/(geo$Value+geosum$Value)
# geo$percentage<-pernageo
# 
# countryyear <- aggregate(Value ~ country+Year, data=long_9507, function(x) {sum(is.na(x))}, na.action = NULL)
# countryyearsum <- aggregate(Value ~ country+Year, data=long_9507, function(x) {sum(!is.na(x))}, na.action = NULL)
# pernacountryyear<-countryyear$Value/(countryyear$Value+countryyearsum$Value)
# countryyear$percentage<-pernacountryyear
# 
# library(VIM)
# scattmatrixMiss(long_9507, interactive = F)
# 
# countryyear<-countryyear[,-3]
# NA9507 <- spread(countryyear, Year, percentage)
# NA9507 <- countryyear



#make a tabel for latex
#library(xtable)
#xtable(NA9507)

#the same for the second


long_0817 <- final0817 %>% gather(Year, Value, -nace_r2, -indic_sb, -geo, -id)

#create variable for country
long_0817$country <- substr(long_0817$geo, 0, 2)


year <- aggregate(Value ~ Year, data=long_0817, function(x) {sum(is.na(x))}, na.action = NULL)
yearsum <- aggregate(Value ~ Year, data=long_0817, function(x) {sum(!is.na(x))}, na.action = NULL)
pernayear<-year$Value/(year$Value+yearsum$Value)
year$percentace<-pernayear



country <- aggregate(Value ~ country, data=long_0817, function(x) {sum(is.na(x))}, na.action = NULL)
countrysum <- aggregate(Value ~ country, data=long_0817, function(x) {sum(!is.na(x))}, na.action = NULL)
pernacountry<-country$Value/(country$Value+countrysum$Value)
country$percentage<-pernacountry

countryyear <- aggregate(Value ~ country+Year, data=long_0817, function(x) {sum(is.na(x))}, na.action = NULL)
countryyearsum <- aggregate(Value ~ country+Year, data=long_0817, function(x) {sum(!is.na(x))}, na.action = NULL)
pernacountryyear<-countryyear$Value/(countryyear$Value+countryyearsum$Value)
countryyear$percentage<-pernacountryyear

countryyear<-countryyear[,-3]
NA0817 <- spread(countryyear, Year, percentage)

#make a tabel for latex
library(xtable)
xtable(NA0817)

