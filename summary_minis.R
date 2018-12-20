library(corrplot)

#Mini1 Question 1
TVdat<-read.table("TV.dat",sep="\t")

par(mfrow=c(1,2))
plot(TVdat$ppD,TVdat$ppT,xlab="people per Dr",ylab="people per TV")
title("People per TV vs People per Doctors")
plot(log10(TVdat$ppD),log10(TVdat$ppT),xlab="log(people per Dr)",ylab="log(people per TV)")
title("log10 transform: People per TV vs People per Doctors")

pairs(TVdat)

##########################################
#Mini 2
##########################################
#Load data
reloadData <- function(){
  housedata <- read.csv("kc_house_data.csv")
  return(housedata)
}
housedata <- reloadData()
#Make a subset of the whole data set
n <- 500
set.seed(42)
ii <- sample(dim(housedata)[1], n)
housedata <- housedata[ii,]

#Indices of variables that is non-numeric
no_use <- c(grep("id", colnames(housedata)),
            grep("date", colnames(housedata)),
            grep("yr_renovated", colnames(housedata)),
            grep("waterfront", colnames(housedata)),
            grep("view", colnames(housedata)),
            grep("condition", colnames(housedata)),
            grep("grade", colnames(housedata)))

housedata <- housedata[,-no_use]

mm<-lm(price~., data=housedata)

par(mfrow=c(2,2))
plot(mm)

bm <- step(mm,directions="backward") # backward selection
plot(bm)









