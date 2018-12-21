#setwd("~/Linj?ra statistiska modeller/Project/ProjektLinear")
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

#Remove unwanted covariates
no_use <- c(grep("id", colnames(data)),
            grep("county", colnames(data)),
            grep("state", colnames(data)))
data <- data[,-no_use]


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

variablesToTransform <- c("area","popul","phys","beds","crimes","poors","totalincome","crm1000")

for (i in 1:length(variablesToTransform)){
  ind <- grep(variablesToTransform[i], colnames(data), fixed=TRUE)
  #Adding adding transform to name to minimize confusion
#  colnames(data3)[ind] <- paste("log(",colnames(data3)[ind] ,")", sep = "")
  data[,ind] <- log(data[,ind])
}

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

#Naive model containing all variables of the dataset
mm1 <- lm(crm1000 ~ . , data=train) #, subset=-c(292,124)
summary(mm1)
par(mfrow=c(2,2))
plot(mm1)

################################################
# Remove outliers
################################################

outliers <- c(362,427)
ind <- which(rownames(train)==1) # locate outlier
train <- train[-c(ind),] #exclude coumns without numerical values


################################################
# Backward model selection
################################################

mm2 <- step(mm1,directions="backward") # backward selection

mm3 <- lm(formula(mm2), data = train[-c(1, 2, 3, 4, 5, 6),])
summary(mm3)
plot(mm3)


