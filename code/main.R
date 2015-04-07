library("plyr")
library("bestglm")
setwd("~/repos/cs7280-project/data/")

min.year<- 1951
max.year <- 2012
data.summer <- data.frame(year=1951:2012)
data.fall <- data.summer
data.spring <- data.summer

for(f in list.files(pattern = "*.data")){    
    data <- read.table(f, skip = 1, nrows = 66)
    
    tmp <- data.frame(year=data$V1)
    tmp[f] <- rowMeans(data[,c("V4", "V5", "V6")])
    data.spring <- join(data.spring, tmp, by="year")
    
    tmp <- data.frame(year=data$V1)
    tmp[f] <- rowMeans(data[,c("V7", "V8", "V9")])
    data.summer <- join(data.summer, tmp, by="year")
    
    
    tmp <- data.frame(year=data$V1)
    tmp[f] <- rowMeans(data[,c("V10", "V11", "V12")])
    data.fall <- join(data.fall, tmp, by="year")
    
}

colnames(data.spring) <- lapply(colnames(data.spring), FUN=function(x) {gsub(pattern="data", replacement="spring",x=x)})
colnames(data.summer) <- lapply(colnames(data.summer), FUN=function(x) {gsub(pattern="data", replacement="summer",x=x)})
colnames(data.fall) <- lapply(colnames(data.fall), FUN=function(x) {gsub(pattern="data", replacement="fall",x=x)})

data.spring[,-1] <- scale(data.spring[,-1])
data.summer[,-1] <- scale(data.summer[,-1])
data.fall[,-1] <- scale(data.fall[,-1])

hurdata <- read.csv("hurdat.csv")
colnames(hurdata) <- replace(colnames(hurdata), list=c(1), values=c("year"))

hist(hurdata$RevisedNamedStorms, breaks=10)
pairs(data.summer)


subset(hurdata, )
hurdata[subset]
namedstorms <- hurdata[(hurdata$year >= min.year) & (hurdata$year <= max.year),"RevisedNamedStorms"]

data.train <- cbind(data.summer[,-1], namedstorms)




window()
