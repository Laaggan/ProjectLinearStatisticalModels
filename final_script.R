#setwd("~/Linj?ra statistiska modeller/Project/ProjektLinear")
library(corrplot)
library(leaps)
library(ggplot2)
library(MASS)
library("xtable")
library(AER)
par(mar=c(5.1, 4.1, 4.1, 2.1))
#################################################
# Categorical: state, region (West=4 baseline)
# Correlated: crimes, popul
# Remove: id, county
#################################################
# Load and create data set
#################################################

data<-read.table("data18.txt")
colnames(data)<-c("id","county","state","area","popul","pop1834","pop65plus","phys",
                  "beds","crimes","higrads","bachelors","poors","unemployed",
                  "percapitaincome","totalincome","region")

crm1000 <- 1000*(data$crimes/data$popul)
data$crm1000 <- crm1000

#From naive model Kings county has a too high value cooks distance
data <- data[-6,]

#Remove unwanted covariates
no_use <- c(grep("id", colnames(data)),
            grep("county", colnames(data)),
            grep("state", colnames(data)))

data <- data[,-no_use]

################################################
# Correlation
################################################
no_use2 <- c(grep("region", colnames(data)))

#Removing data that does not yield meaningful results in correlation plot
data2 <- data[,-no_use2]

corrplot(cor(data2)) # data2
################################################
# Boxplot
################################################
#Boxplot as potential outlier removal
scaledData <- scale(data)
boxplot(scaledData)

#lines showing n standard-standard deviations
n <- 5
abline(n,0)
abline(-n,0)

################################################
# Categorical data
################################################
#Setting the new labels as factors
data$region <- factor(data$region,label=c("Northeast","Midwest","South","West"))
data$region <- relevel(data$region, ref="West")

par(mfrow=c(2,2))
hist(data$crm1000[data$region == "Northeast"], main="Northeast", xlab = "crm1000")
hist(data$crm1000[data$region == "Midwest"], main="Midwest", xlab = "crm1000")
hist(data$crm1000[data$region == "South"], main="South", xlab = "crm1000")
hist(data$crm1000[data$region == "West"], main="West", xlab = "crm1000")

#Here we can see quite clearly that the point with 300 crm1000 probably
#is an outlier
par(mfrow=c(1,1))
plot(data$region, data$crm1000)

################################################
# Plots to evaluate which variables to transform
################################################

#Plots all variables with transforms and without log-transform with crm1000 on y-axis
#in different plots. The plots are saved to a folder figures
for (i in 1:dim(data)[2]){
  filepath <- paste("figures/plot_",colnames(data)[i],".png", sep="")
  png(file = filepath, width = 500, height = 500)
  par(mfrow=c(2,2))
  plot(x = data[,i], y=data$crm1000, 
       ylab = "crm1000", xlab = colnames(data)[i])
  plot(x = data[,i], y=log(data$crm1000), 
       ylab="log(crm1000)", xlab = colnames(data)[i])
  plot(x = log(data[,i]), y=data$crm1000, 
       ylab = "crm1000", xlab = paste("log(",colnames(data)[i],")",sep=""))
  dev.off()
}

################################################
# Transformation of variables
################################################
variablesToTransform <- c("area","pop65plus","phys","beds","poors","totalincome")

for (i in 1:length(variablesToTransform)){
  ind <- grep(variablesToTransform[i], colnames(data), fixed=TRUE)
  #Adding adding transform to name to minimize confusion
  colnames(data)[ind] <- paste("log(",colnames(data)[ind] ,")", sep = "")
  data[,ind] <- log(data[,ind])
}

################################################
# Remove variables due to colinearity
################################################
no_use3 <- c(grep("popul", colnames(data)),
             grep("crimes", colnames(data)))

no_use3 <- c(grep("popul", colnames(data)),
             grep("crimes", colnames(data)),
             grep("totalincome", colnames(data)),
             grep("phys", colnames(data)))

data <- data[,-no_use3]

################################################
# Training and test set
################################################

N <- dim(data)[1]
p1 <- 0.7*N

set.seed(42)
ii <- sample(seq(1,N),p1)
train <- data[ii,] # select a random subset of your full dataset for training
test <- data[-ii,] # select the rest for testing

row.names(train)<-seq(1,dim(train)[1]) # assign new IDs to each row (1,2,3 osv
row.names(test)<-seq(1,dim(test)[1])

################################################
# Model
################################################
pMSE <- function(yHat, y){
  return((length(yHat)^-1)*sum((yHat-y)^2))
}

#Naive model containing all variables of the dataset
mm1 <- lm(crm1000 ~ . , data=train)#, subset=-c(238,215))
mm1sum <- summary(mm1)
par(mfrow=c(2,2)) 
plot(mm1)

yPred1 <- predict(mm1, test)
pMSE1 <- pMSE(yPred1, test$crm1000)
par(mfrow=c(1,1))
maxVal = max(test$crm1000)
plot(yPred1, test$crm1000, xlim=c(0,maxVal), ylim=c(0,maxVal))
abline(0,1)

################################################
# Model
################################################
handPickedModel <- lm(crm1000 ~ pop1834 + `log(poors)` + region + `log(beds)`, data=train)

hpPred <- predict(handPickedModel, test)
hpPredSum <- summary(handPickedModel)
hpPMSE <- pMSE(hpPred, test$crm1000)

################################################
# Backward model selection
################################################

mm2 <- step(mm1,directions="backward") # backward selection

mm3 <- lm(formula(mm2), data = train)#, subset=-c(238,215))
summary(mm3)
par(mfrow=c(2,2)) 
plot(mm3)

yPred3 <- predict(mm3, test)
pMSE3 <- pMSE(yPred3, test$crm1000)
par(mfrow=c(1,1))
plot(yPred3, test$crm1000,, xlim=c(0,maxVal), ylim=c(0,maxVal))
abline(0,1)

################################################
# Remove region because regsubsets can't handle it
################################################
#FIXME: this mutates another dataset and then you try to use train
no_use4 <- c(grep("region", colnames(train)))
train2 <- train[,-no_use4]
test2 <- test[,-no_use4]

################################################
# Model selection with lowest predictive MSE
################################################
crmInd <- which(colnames(train2) == "crm1000")

yy <- train2[,crmInd]
xx <- train2[,-crmInd]

yyt <- test2[,crmInd]
xxt <- test2[,-crmInd]

##################################################
#Perform exhaustive search for model
##################################################
rleaps<-regsubsets(x=xx,y=yy,int=T,nbest=1000,nvmax=dim(train2)[2],really.big=T,method=c("ex"))
cleaps<-summary(rleaps,matrix=T)

pmses<-rep(0,dim(cleaps$which)[1])
bestPMSE <- Inf
for (ta in (1:dim(cleaps$which)[1])) {
  x <- as.matrix(xx[, cleaps$which[ta,-1]])
  mmr <- lm(yy ~ x)
  x <- as.matrix(xxt[, cleaps$which[ta,-1]])
  yhat <- predict(mmr, as.data.frame(x))
  PEcp <- pMSE(yhat, yyt)
  pmses[ta]<-PEcp 
  if (PEcp < bestPMSE){
    bestPMSE <- PEcp
    mm4 <-mmr
    yPred4 <- yhat
  }
}
nullpmse<-sum((yyt-mean(yy))^2)/length(yyt)
pmses<-c(nullpmse,pmses)
pmsevec<-rep(0,length(tsec))
for (tk in 1:length(tsec)) {
  pmsevec[tk]<-min(pmses[tt==tsec[tk]])}
par(mfrow=c(1,1))
plot(tsec,pmsevec,xlab="number of parameters",ylab="pMSE",main="prediction MSE", type="b",lwd=2,col=2)

#index of least pMSE
ptmin<-which.min(pmses)
ModMat<-rbind(c("TRUE",rep("FALSE",dim(cleaps$which)[2]-1)),cleaps$which)
pmod<-ModMat[ptmin,] # this is the best model for prediction (on THIS test data)
winsize<-sum(pmod==T)
mtmin<-which.min(mses[tt==sum(pmod==T)])
mod<-(ModMat[tt==sum(pmod==T),])[mtmin,] # best training model of the same size and pmod. Same model?

#mm4 and yPred4 is created in the loop above
plot(yPred4, test$crm1000, , xlim=c(0,maxVal), ylim=c(0,maxVal))
abline(0,1)

mm4summary <- summary(mm4)
xtable(mm4summary$coefficients[,-c(2,3)])

###################################################
# GLM
# Poisson model
###############################################
mm5 <- glm(crm1000 ~ ., family="poisson", data=train)
summary(mm5)
dispersiontest(mm5,alt='two.sided')

mm5summary <- summary(mm5)
xtable(mm5summary$coefficients[,-c(2,3)])

################################################
# Negative binomial model
###############################################
mm6 <- glm.nb(crm1000 ~ ., data=train)
summary(mm6)

mm6summary <- summary(mm6)
xtable(mm6summary$coefficients[,-c(2,3)])

################################################
# Predicted mean square error
################################################
pred1 <- predict(mm1, newdata = test, type = "response")
pred2 <- predict(mm3, newdata = test, type = "response")
sum((test$crm1000-pred1)^2)/dim(test)[1]
sum((test$crm1000-pred2)^2)/dim(test)[1]

pred5 <- predict(mm5, newdata = test, type = "response")
pred6 <- predict(mm6, newdata = test, type = "response")
pMSE5 <- pMSE(pred5, test$crm1000)
pMSE6 <- pMSE(pred6, test$crm1000)


################################################
# ANOVA
################################################
anova(mm5,mm6)

################################################
# Backward selection for negative binomial
################################################
mm7 <- step(mm6, direction="backward")

yPred8 <- predict(mm7, newdata = test)
pMSE8 <- pMSE(yPred8, test$crm1000)

mm8summary <- summary(mm7)
xtable(mm8summary$coefficients[,-c(2,3)])


################################################
# Final model
################################################
finalModel <- glm.nb(crm1000 ~ pop1834 + `log(beds)` + `log(poors)` + region, data=train)

finalPred8 <- predict(finalModel, newdata = test)
finalPMSE <- pMSE(finalPred8, test$crm1000)

finalModelSummary <- summary(finalModel)
xtable(finalModelSummary$coefficients[,-c(2,3)])

par(mfrow=c(2,2))
plot(finalModel)













