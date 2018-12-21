setwd("~/Linj?ra statistiska modeller/Project")
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

#Remove unwanted covariates
no_use <- c(grep("id", colnames(data)),
            grep("county", colnames(data)),
            grep("state", colnames(data)))
data <- data[,-no_use]
crm1000 <- 1000*(data$crimes/data$popul)
data$crm1000 <- crm1000

################################################
# Training and test set
################################################

totalSample <- dim(data)[1]
p1 <- 0.7*totalSample

set.seed(42)
ii <- sample(seq(1,dim(data)[1]),p1)
train <- data[ii,] # select a random subset of your full dataset for training
test <- data[-ii,] # select the rest for testing

################################################
# Correlation
################################################
no_use2 <- c(grep("state", colnames(data)),
             grep("popul", colnames(data)), 
             grep("crimes", colnames(data)),
             grep("totalincome", colnames(data)))
data2 <- data[,-no_use2]

corrplot(cor(data)) # data2

################################################
# Categorical data
################################################

#Setting the new labels as factors
data$region <- factor(data$region,label=c("Northeast","Midwest","South","West"))
data$region <- relevel(data$region, ref="West")

################################################
# Model
################################################

mm1 <- lm(log(crm1000) ~ ., data=data)
summary(mm1)

################################################
# Backward model selection
################################################

mm2 <- step(mm1,directions="backward") # backward selection

mm3 <- lm(formula = log(crm1000) ~ pop1834 + pop65plus + beds + crimes + 
            bachelors + poors + percapitaincome + totalincome + region, 
          data = data)
summary(mm3)

################################################
# Transformation of variables
################################################

variablesToTransform <- c()

data3 <- data
for (i in 1:length(variablesToTransform)){
  ind <- grep(variablesToTransform[i], colnames(data3), fixed=TRUE)
  colnames(data3)[ind] <- paste("log(",colnames(data3)[ind],")")
  data3[,ind] <- log(data3[,ind])
}

