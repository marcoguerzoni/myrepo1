
install.packages("data.table")
install.packages("VIM")
setwd("C:/Users/despina/Google Drive/ricerca/myprojects/myrepo1")

#import data 1995-2007
require(data.table)
data <- as.data.frame(fread("sbs_r_nuts03.tsv"))
#split first variable

data1 <- setDT(data)[,paste0("A1", 1:3) := tstrsplit(V1, ",")][,-"V1", with=F]
data1[1,16] <-"geo"
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

final9507 <- data3
final9507$id  <- paste(final9507$nace_r1,final9507$indic_sb,final9507$geo)

#download data 2008-2017

data <- as.data.frame(fread("sbs_r_nuts06_r2.tsv"))
#split first variable

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

final0817 <- data3
final0817$id  <- paste(final0817$nace_r1,final0817$indic_sb,final0817$geo)

#######MISSING VALUE ANALYSIS

final9507<-ifelse(final9507==":", NA, final9507)

library(VIM)
mice_plot <- aggr(final9507, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(final9507), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))


