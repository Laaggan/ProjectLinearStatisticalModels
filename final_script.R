##########################################################
#Loading, cleaning up and creating new data
##########################################################
data<-read.table("data18.txt")
colnames(data)<-c("id","county","state","area","popul","pop1834","pop65plus","phys",
                    "beds","crimes","higrads","bachelors","poors","unemployed","percapitaincome",
                    "totalincome","region")

#Data is sorted in ascending order of population
sort(data$popul, decreasing = TRUE) == data$popul

#Creating the crm1000 data
crm1000 <- 1000*data$crimes/data$popul

#Adding the created data to the dataframe
data$crm1000 <- crm1000

##########################################################
#Exploring data through correlation plots and data tran-
#forms
##########################################################