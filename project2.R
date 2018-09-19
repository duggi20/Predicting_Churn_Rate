rm(list=ls())
library(tidyverse)
library(corrgram)
library(usdm)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(randomForest)
library(DMwR)
library(naivebayes)
library(caret)
library(klaR)
library(class)
getwd()
setwd("/home/administrator/Documents/project")
chum_data=read.csv("Train_data.csv")
chum_test=read.csv("Test_data.csv")
View(chum_data)
str(chum_data)
set.seed(300)
###correcting the data types
chum_data$area.code=as.factor(chum_data$area.code)
chum_test$area.code=as.factor(chum_test$area.code)
chum_data$phone.number=NULL
chum_test$phone.number=NULL
##missing_value analysis
missing_index=data.frame(apply(chum_data,2,function(x){sum(is.na(x))}))
missing_index2=data.frame(apply(chum_test,2,function(x){sum(is.na(x))}))

View(missing_index2)
##visualisation
##scatter plot for customer calls vs total day,night,eve and intl calls 
numeric_index=sapply(chum_data,is.numeric)
numeric_colnames=colnames(chum_data[,numeric_index])
ggplot(data=chum_data)+
  geom_point(mapping=aes(x=number.customer.service.calls,y=total.day.minutes,color=Churn))
ggplot(data=chum_data)+facet_wrap(~international.plan)+
  geom_point(mapping=aes(x=number.customer.service.calls,y=total.day.minutes,color=Churn))
ggplot(data=chum_data)+facet_wrap(~international.plan)+
  geom_point(mapping=aes(x=number.customer.service.calls,y=total.eve.minutes,color=Churn))
ggplot(data=chum_data)+facet_wrap(~international.plan)+
  geom_point(mapping=aes(x=number.customer.service.calls,y=total.night.minutes,color=Churn))
ggplot(data=chum_data)+facet_wrap(~international.plan)+
  geom_point(mapping=aes(x=number.customer.service.calls,y=total.intl.minutes,color=Churn))
ggplot(data=chum_data)+
  geom_point(mapping=aes(x=total.intl.calls,y=total.intl.minutes,color=Churn))
           
#### barplot for customer service calls and churn
ggplot(data=chum_data)+
  geom_bar(mapping=aes(x=number.customer.service.calls,fill=Churn),position="fill") 
ggplot(data=chum_data)+
  geom_bar(mapping=aes(x=state,fill=Churn),position="fill") 
##outliers analysis
numeric_index=sapply(chum_data,is.numeric)
numeric_index_test=sapply(chum_test,is.numeric)
numeric_colnames=colnames(chum_data[,numeric_index])
View(numeric_colnames)
for (i in 1:length(numeric_colnames))
{
  assign(paste0("plot",i), ggplot(aes_string(y = (numeric_colnames[i]), x = "Churn"), data = subset(chum_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=numeric_colnames[i],x="churn")+
           ggtitle(paste("Box plot of churn for",numeric_colnames[i])))
}
gridExtra::grid.arrange(plot1,plot2,plot3,plot4,ncol=4)
gridExtra::grid.arrange(plot5,plot6,plot7,plot8,ncol=4)
gridExtra::grid.arrange(plot9,plot10,plot11,plot12,ncol=4)
gridExtra::grid.arrange(plot13,plot14,plot15,ncol=3)
for (i in numeric_colnames){
  values=chum_data[,i][chum_data[,i]%in%boxplot.stats(chum_data[,i])$out] 
  if(i!="number.customer.service.calls"){
  chum_data=chum_data[which(!chum_data[,i]%in%values),]  
}
  }
for (i in numeric_colnames){
  
  values=chum_test[,i][chum_test[,i]%in%boxplot.stats(chum_test[,i])$out] 
  if(i!="number.vmail.messages"&i!="number.customer.service.calls"){
    print(i)
  chum_test=chum_test[which(!chum_test[,i]%in%values),]  
}
  }
View(chum_test)
#3exploratory data analysis
chum_data$total.day.charge.permin=chum_data$total.day.charge/chum_data$total.day.minutes
chum_data$total.eve.charge.permin=chum_data$total.eve.charge/chum_data$total.eve.minutes
chum_data$total.night.charge.permin=chum_data$total.night.charge/chum_data$total.night.minutes
chum_data$total.intl.charge.permin=chum_data$total.intl.charge/chum_data$total.intl.minutes
chum_data$total.day.avgmin.percall=chum_data$total.day.minutes/chum_data$total.day.calls
chum_data$total.eve.avgmin.percall=chum_data$total.eve.minutes/chum_data$total.eve.calls
chum_data$total.night.avgmin.percall=chum_data$total.night.minutes/chum_data$total.night.calls
chum_data$total.intl.avgmin.percall=chum_data$total.intl.minutes/chum_data$total.intl.calls
chum_test$total.day.charge.permin=chum_test$total.day.charge/chum_test$total.day.minutes
chum_test$total.eve.charge.permin=chum_test$total.eve.charge/chum_test$total.eve.minutes
chum_test$total.night.charge.permin=chum_test$total.night.charge/chum_test$total.night.minutes
chum_test$total.intl.charge.permin=chum_test$total.intl.charge/chum_test$total.intl.minutes
chum_data=chum_data[c("state","account.length","area.code","international.plan","voice.mail.plan","number.vmail.messages","total.day.minutes","total.day.calls","total.day.charge","total.day.charge.permin","total.day.avgmin.percall","total.eve.minutes","total.eve.calls","total.eve.charge","total.eve.charge.permin","total.eve.avgmin.percall","total.night.minutes","total.night.calls","total.night.charge","total.night.charge.permin","total.night.avgmin.percall","total.intl.minutes","total.intl.calls","total.intl.charge","total.intl.charge.permin","total.intl.avgmin.percall","number.customer.service.calls","Churn")]
chum_test=chum_test[c("state","account.length","area.code","international.plan","voice.mail.plan","number.vmail.messages","total.day.minutes","total.day.calls","total.day.charge","total.day.charge.permin","total.eve.minutes","total.eve.calls","total.eve.charge","total.eve.charge.permin","total.night.minutes","total.night.calls","total.night.charge","total.night.charge.permin","total.intl.minutes","total.intl.calls","total.intl.charge","total.intl.charge.permin","number.customer.service.calls","Churn")]
####check for normailsation or standardisation
hist(chum_data$total.day.calls)
hist(chum_data$total.eve.calls)
hist(chum_data$total.night.calls)
hist(chum_data$total.intl.calls)
hist(chum_data$total.day.charge.permin)
###Standardisation
 for(i in numeric_colnames){
   print(i)
   chum_data[,i] = (chum_data[,i] - mean(chum_data[,i]))/
                                  sd(chum_data[,i])
 }
for(i in numeric_colnames){
  print(i)
  chum_test[,i] = (chum_test[,i] - mean(chum_test[,i]))/
    sd(chum_test[,i])
}
####feature selection
corrgram(chum_data[,numeric_colnames], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
numeric_index=sapply(chum_data,is.numeric)
numeric_index_test=sapply(chum_test,is.numeric)
chum_data_numeric=chum_data[,numeric_index]
chum_test_numeric=chum_test[,numeric_index_test]
vifcor(chum_data_numeric,th=0.7)
##dropping total.day.charge,total.night.charge,total.eve.charge,total.lintl.charge
chum_data_numeric=subset(chum_data_numeric,select=-c(total.day.charge,total.night.charge,total.eve.charge,total.intl.charge,total.day.avgmin.percall,total.intl.avgmin.percall,total.eve.avgmin.percall,total.night.avgmin.percall))
chum_test_numeric=subset(chum_test_numeric,select=-c(total.day.charge,total.night.charge,total.eve.charge,total.intl.charge))
chum_data=subset(chum_data,select=-c(total.day.charge,total.night.charge,total.eve.charge,total.intl.charge,total.day.avgmin.percall,total.intl.avgmin.percall,total.eve.avgmin.percall,total.night.avgmin.percall))
chum_test=subset(chum_test,select=-c(total.day.charge,total.night.charge,total.eve.charge,total.intl.charge))
chum_test$phone.number=NULL
factor_index=sapply(chum_data,is.factor)
factor_colnames=colnames(chum_data[,factor_index])
factor_data=chum_data[,factor_index]
View(factor_colnames)
##chi square analysis
for (i in factor_colnames) {
  print(i)
  print(chisq.test(table(chum_data$Churn,factor_data[,i]),simulate.p.value = TRUE))
  
}
####dropping area code
chum_data$area.code=NULL
chum_test$area.code=NULL
##chum_data$international.plan=NULL
##chum_data$voice.mail.plan=
##chum_test$international.plan=NULL
##chum_test$voice.mail.plan=NULL
###decision tree
decision_model=rpart(Churn~.,chum_data,method="class",control=rpart.control(cp=0.024291,xval=10,maxdepth = 10))
printcp(decision_model)
plotcp(decision_model)
rpart.plot(decision_model,cex=0.5,extra=4)
prediction_decision=predict(decision_model,type="class",chum_test[,-19])
  View(prediction_decision)
  res1=table(chum_test[,19],prediction_decision)
  View(res1)
confusionMatrix(table(chum_test[,19],prediction_decision))
####random forest
random_model=randomForest(Churn~.,chum_data,ntree=50,mtry=8,importance=TRUE)
View(random_model)
random_predict=predict(random_model,chum_test[,-19])
confusionMatrix(table(chum_test[,19],random_predict))
tuneRF(chum_data[,-19],chum_data[,19],stepFactor = 2,doBest = TRUE)
imp=as.data.frame(sort(importance(random_model)[,1],decreasing=TRUE))
names(imp)<-"%imp"
imp

###knn algorithm
trControl <- trainControl(method  = "cv",
                          number  = 5)
fit <- train(Churn ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = chum_data)
fit
churn_data_knn=chum_data
churn_test_knn=chum_test

churn_data_knn$international.plan=as.numeric(churn_data_knn$international.plan)
churn_data_knn$voice.mail.plan=as.numeric(churn_data_knn$voice.mail.plan)
churn_data_knn$state=as.numeric(churn_data_knn$state)
churn_test_knn$international.plan=as.numeric(churn_test_knn$international.plan)
churn_test_knn$voice.mail.plan=as.numeric(churn_test_knn$voice.mail.plan)
churn_test_knn$state=as.numeric(churn_test_knn$state)
colnames=c("international.plan","voice.mail.plan","state")
for(i in colnames){
  churn_data_knn[,i] = (churn_data_knn[,i] - mean(churn_data_knn[,i]))/
    sd(churn_data_knn[,i])
  
}
for(i in colnames){
  churn_test_knn[,i] = (churn_test_knn[,i] - mean(churn_test_knn[,i]))/
    sd(churn_test_knn[,i])
  
}
knn_model=knn(data.frame(churn_data_knn[,1:18]),data.frame(churn_test_knn[,1:18]),chum_data$Churn,k=5)
Conf_matrix = table(knn_model, chum_test$Churn)
confusionMatrix((Conf_matrix))
####naiveBayes
trControl <- trainControl(method  = "cv",
                          number  = 5)
fit <- train(Churn ~ .,
             method     = "nb",
             ##tuneGrid   = expand.grid(usekernel =(FALSE,TRUE),fL =0,adjust= FALSE),
             trControl  = trControl,
             metric     = "Accuracy",
            
             data       = chum_data)
fit
NB_model = naiveBayes(Churn ~ ., data = chum_data,fL=8,usekernal=TRUE,adjust=1)
NB_Predictions = predict(NB_model, chum_test[,-19], type = 'class')
Conf_matrix = table(NB_Predictions, chum_test[,19])
confusionMatrix((Conf_matrix))
nb_model_tuned=predict(fit,chum_test[,-19], type = 'raw')
Conf_matrix = table(nb_model_tuned, chum_test[,19])
confusionMatrix((Conf_matrix))
#####output file
out=chum_test
out$decision.prediction=prediction_decision
out$random.prediction=random_predict
out$knn.predict=knn_model
out$naive.bayes.predict=NB_Predictions
write(out,"output.csv")
