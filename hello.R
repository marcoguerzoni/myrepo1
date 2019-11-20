
install.packages("data.table")
setwd("C:/Users/despina/Google Drive/ricerca/myprojects/myrepo1")

#import data
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

data3<- data2[which(junk$nm=="B")]<-"b"
