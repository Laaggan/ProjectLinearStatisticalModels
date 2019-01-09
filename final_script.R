#setwd("~/Linj?ra statistiska modeller/Project/ProjektLinear")
library(leaps)
library(corrplot)
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
no_use2 <- c(#Why do we remove "totalincome"?
  #grep("totalincome", colnames(data)),
  #Region is removed due to it being a categorical variable
  grep("region", colnames(data)))

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
  jpeg(file = paste("figures/plot",colnames(data)[i],sep="_"))
  par(mfrow=c(2,2))
  plot(data[,i], data$crm1000)
  plot(log(data[,i]), data$crm1000)
  plot(data[,i], log(data$crm1000))
  plot(log(data[,i]), log(data$crm1000))
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
             grep("crimes", colnames(data)),
             grep("region", colnames(data)))
data <- data[,-no_use3]

################################################
# Training and test set
################################################
N <- dim(data)[1]
#Shuffle data
set.seed(42)
ii <- sample(seq(1,N),N)
data <- data[ii,]

#K-fold cross validation
K <- 10;
bestPMSE <- Inf
for (n in 0:(K-1)){
  foldSize <- floor(N/K)
  testInd <- seq(n*foldSize+1, (n+1)*foldSize)
  
  train <- data[-testInd,] # select a random subset of your full dataset for training
  test <- data[testInd,] # select the rest for testing
  
  row.names(train)<-seq(1,dim(train)[1]) # assign new IDs to each row (1,2,3 osv
  row.names(test)<-seq(1,dim(test)[1])
  
  # ################################################
  # # Model
  # ################################################
  # 
  # #Naive model containing all variables of the dataset
  # mm1 <- lm(crm1000 ~ . , data=train) #, subset=-c(292,124)
  # summary(mm1)
  # par(mfrow=c(2,2))
  # plot(mm1)
  # 
  # ################################################
  # # Remove outliers
  # ################################################
  # outliers <- c(362,427)
  # ind <- which(rownames(train)==1) # locate outlier
  # train <- train[-c(ind),] #exclude coumns without numerical values
  # 
  # ################################################
  # # Backward model selection
  # ################################################
  # mm2 <- step(mm1,directions="backward") # backward selection
  # 
  # mm3 <- lm(formula(mm2), data = train[-c(1, 2, 3, 4, 5, 6),])
  # summary(mm3)
  # plot(mm3)
  
  ################################################
  # Model selection with lowest predictive MSE
  ################################################
  crmInd <- which(colnames(train) == "crm1000")
  
  yy <- train[,crmInd]
  xx <- train[,-crmInd]
  
  yyt <- test[,crmInd]
  xxt <- test[,-crmInd]
  
  ##################################################
  #Perform exhaustive search for model
  ##################################################
  #rleaps<-regsubsets(x=xx,y=yy,int=T,nbest=1,nvmax=dim(xx)[2],really.big=T,method=c("exhaustive")) ## all subset models
  rleaps<-regsubsets(x=xx,y=yy,int=T,nbest=1000,nvmax=dim(train)[2],really.big=T,method=c("ex"))
  cleaps<-summary(rleaps,matrix=T) ## True/False matrix. The r-th is a True/False statement about which
  ## variables are included in model r.
  tt<-apply(cleaps$which,1,sum) ## size of each model in the matrix
  mses<-cleaps$rss/length(yy) ## corresponding MSEs
  ##
  tt<-c(1,tt)
  nullrss<-sum((yy-mean(yy))^2)/length(yy)
  mses<-c(nullrss,mses)
  ###
  plot(tt,mses,xlab="number of parameters",ylab="RSS/n",main="RSS/n for all subset models")
  tmin<-min(tt)
  tmax<-max(tt)
  tsec<-seq(tmin,tmax)
  msevec<-rep(0,length(tsec))
  for (tk in 1:length(tsec)) {
    msevec[tk]<-min(mses[tt==tsec[tk]])} ## the best model for each size
  lines(tsec,msevec,lwd=2,col=2) ## a line connecting the best models.
  
  
  ###################################################
  ### code chunk number 6: l6-msefig
  ###################################################
  plot(tsec,msevec,xlab="number of parameters",ylab="MSE",main="MSE for best model of each size",type='b',col=4,lwd=2)
  # Just plotting the best model of each size
  
  ###################################################
  ### code chunk number 7: l6-pmsefig
  ###################################################
  pmses<-rep(0,dim(cleaps$which)[1])
  for (ta in (1:dim(cleaps$which)[1])) {
    # select covariates in training data for current model 
    # -1 removes the intercep stored in cleaps
    x <- as.matrix(xx[, cleaps$which[ta,-1]]) # TRAINING covariates
    mmr <- lm(yy ~ x)  # fit training data
    # now select same covariates in test data
    x <- as.matrix(xxt[, cleaps$which[ta,-1]]) # TESTING covariates
    # predict the outcome of the new data from testing covariates
    yhat <- predict(mmr, as.data.frame(x))
    # mmr<-lm(yy ~ xx[,cleaps$which[ta,-1]==T])
    PEcp<-sum((yyt-yhat)^2)/length(yyt)
    pmses[ta]<-PEcp 
  }
  nullpmse<-sum((yyt-mean(yy))^2)/length(yyt)
  pmses<-c(nullpmse,pmses)
  pmsevec<-rep(0,length(tsec))
  for (tk in 1:length(tsec)) {
    pmsevec[tk]<-min(pmses[tt==tsec[tk]])}
  plot(tsec,pmsevec,xlab="number of parameters",ylab="pMSE",main="prediction MSE", type="b",lwd=2,col=2)
  # best prediction model of each size
  
  ###################################################
  ### code chunk number 8: l6-whichwin
  ###################################################
  ptmin<-which.min(pmses)
  ModMat<-rbind(c("TRUE",rep("FALSE",dim(cleaps$which)[2]-1)),cleaps$which)
  
  if (ptmin < bestPMSE){
    pmod<-ModMat[ptmin,] # this is the best model for prediction (on THIS test data)
    winsize<-sum(pmod==T)
    mtmin<-which.min(mses[tt==sum(pmod==T)])
    mod<-(ModMat[tt==sum(pmod==T),])[mtmin,] # best training model of the same size and pmod. Same model?
  }
}
=======
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
mm1 <- lm(crm1000 ~ . , data=train) #, subset=-c(292,124)
summary(mm1)
par(mfrow=c(2,2)) 
plot(mm1)

yPred1 <- predict(mm1, test)
pMSE1 <- pMSE(yPred1, test$crm1000)
par(mfrow=c(1,1))
maxVal = max(test$crm1000)
plot(yPred1, test$crm1000, xlim=c(0,maxVal), ylim=c(0,maxVal))
abline(0,1)

################################################
# Backward model selection
################################################

mm2 <- step(mm1,directions="backward") # backward selection

mm3 <- lm(formula(mm2), data = train)
summary(mm3)
par(mfrow=c(2,2)) 
plot(mm3)

yPred3 <- predict(mm3, test)
pMSE3 <- pMSE(yPred3, test$crm1000)
par(mfrow=c(1,1))
plot(yPred3, test$crm1000,, xlim=c(0,maxVal), ylim=c(0,maxVal))
abline(0,1)

################################################
# Model selection with lowest predictive MSE
################################################
crmInd <- which(colnames(train) == "crm1000")

yy <- train[,crmInd]
xx <- train[,-crmInd]

yyt <- test[,crmInd]
xxt <- test[,-crmInd]

##################################################
#Perform exhaustive search for model
##################################################
rleaps<-regsubsets(x=xx,y=yy,int=T,nbest=1000,nvmax=dim(train)[2],really.big=T,method=c("ex"))## all subset models
cleaps<-summary(rleaps,matrix=T) ## True/False matrix. The r-th is a True/False statement about which
## variables are included in model r.
tt<-apply(cleaps$which,1,sum) ## size of each model in the matrix
mses<-cleaps$rss/length(yy) ## corresponding MSEs
##
tt<-c(1,tt)
nullrss<-sum((yy-mean(yy))^2)/length(yy)
mses<-c(nullrss,mses)
###
par(mfrow=c(1,1))
plot(tt,mses,xlab="number of parameters",ylab="RSS/n",main="RSS/n for all subset models")
tmin<-min(tt)
tmax<-max(tt)
tsec<-seq(tmin,tmax)
msevec<-rep(0,length(tsec))
for (tk in 1:length(tsec)) {
  msevec[tk]<-min(mses[tt==tsec[tk]])} ## the best model for each size
lines(tsec,msevec,lwd=2,col=2) ## a line connecting the best models.


###################################################
### code chunk number 6: l6-msefig
###################################################
plot(tsec,msevec,xlab="number of parameters",ylab="MSE",main="MSE for best model of each size",type='b',col=4,lwd=2)
# Just plotting the best model of each size

###################################################
### code chunk number 7: l6-pmsefig
###################################################
pmses<-rep(0,dim(cleaps$which)[1])
bestPMSE <- Inf
for (ta in (1:dim(cleaps$which)[1])) {
  # select covariates in training data for current model 
  # -1 removes the intercep stored in cleaps
  x <- as.matrix(xx[, cleaps$which[ta,-1]]) # TRAINING covariates
  mmr <- lm(yy ~ x)  # fit training data
  # now select same covariates in test data
  x <- as.matrix(xxt[, cleaps$which[ta,-1]]) # TESTING covariates
  # predict the outcome of the new data from testing covariates
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
plot(tsec,pmsevec,xlab="number of parameters",ylab="pMSE",main="prediction MSE", type="b",lwd=2,col=2)
# best prediction model of each size

###################################################
### code chunk number 8: l6-whichwin
###################################################
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

print(c(bestPMSE, min(pmsevec)))

###################################################
# GLM
# Poisson model
###############################################
m1 <- glm(crm1000 ~ ., family="poisson", data=train)
summary(m1)
dispersiontest(m1,alt='two.sided')

################################################
# Negative binomial model
###############################################
m2 <- glm.nb(crm1000 ~ ., data=train)
summary(m1)

################################################
# Predicted mean square error
################################################
pred1 <- predict(mm1, newdata = test, type = "response")
pred2 <- predict(mm3, newdata = test, type = "response")
sum((test$crm1000-pred1)^2)/dim(test)[1]
sum((test$crm1000-pred2)^2)/dim(test)[1]

pred3 <- predict(m1, newdata = test, type = "response")
pred4 <- predict(m2, newdata = test, type = "response")
sum((test$crm1000-pred3)^2)/dim(test)[1]
sum((test$crm1000-pred4)^2)/dim(test)[1]


################################################
# ANOVA
################################################

anova(m1,m2)



