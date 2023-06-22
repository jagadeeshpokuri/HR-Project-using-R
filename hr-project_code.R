library(dplyr)
library(stringr)
library(data.table)
library(MASS)
library(randomForest)
library(purrr)
library(caret)
library(visdat)
library(tidyr)


setwd("D:\\Edvancer\\Projects\\HR")
test=read.csv("hr_test.csv")
train=read.csv("hr_train.csv")
summary(hr_test)
test$left=NA
train$data='train'
test$data='test'
main=rbind(train,test)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

glimpse(main)


vis_dat(main)



write.csv(colnames(main),"main_col_modification.csv")
table(main$sales)##Ok
table(main$salary)##Ok
table(main$number_project)##ok
table(main$average_montly_hours)##huge columns
table(main$time_spend_company)##ok
table(main$Work_accident)##ok
table(main$promotion_last_5years)##ok
table(main$satisfaction_level)##more columns
table(main$last_evaluation)## morer columns same like as satisfaction level
unique(main$satisfaction_level)
unique(main$last_evaluation)
unique(main$average_montly_hours)
round(5.35,1)
## Create dummy values

names(main)[sapply(main,function(x) is.character(x))]

cat_cols=c("sales")

for(cat in cat_cols){
  main=CreateDummies(main,cat,50)
}

cat_cols=c("salary")

for(cat in cat_cols){
  main=CreateDummies(main,cat,50)
}



vis_dat(main)
lapply(main,function(x)length(unique(x)))

summary(main)
names(main)[sapply(main,function(x) is.character(x))]

summary(main)
colnames(main)
## Training model
library(dplyr)
train=main[main$data=="train",]
test=main[main$data=="test",]
vis_dat(train)
vis_dat(test)



set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
train1=train[s,]
train2=train[-s,]

train1<-subset(train1,select=-data)
train2<-subset(train2,select=-data)
test<-subset(test,select=-data)
test<-subset(test,select=-left)
library(car)
for_vif=lm(left~., data=train1)
x=as.data.frame(vif(for_vif))
View(x)

log_fit=glm(left~.,data=train1,family = "binomial")
summary(log_fit)
log_fit=step(log_fit)
formula(log_fit)

log_fit=glm(left ~ satisfaction_level + last_evaluation + number_project + 
              average_montly_hours + time_spend_company + Work_accident + 
              promotion_last_5years + sales_hr + sales_accounting + sales_marketing + 
              sales_product_mng + sales_support + sales_technical + sales_sales + 
              salary_medium + salary_low,data=train1,family = "binomial")
summary(log_fit)

library(pROC)

val.score=predict(log_fit,newdata = train2,type = 'response')

auc(roc(train2$left,val.score))

log_fit_final=glm(left ~ satisfaction_level + last_evaluation + number_project + 
              average_montly_hours + time_spend_company + Work_accident + 
              promotion_last_5years + sales_hr + sales_accounting + sales_marketing + 
              sales_product_mng + sales_support + sales_technical + sales_sales + 
              salary_medium + salary_low,data=train,family = "binomial")
summary(log_fit_final)

test.prob.score=predict(log_fit_final,newdata = test, type = 'response')

write.csv(test.prob.score,"HR_project.csv",row.names = F)

train.score=predict(log_fit_final,newdata = train,type = 'response')

real=train$left
cutoffs=seq(0.001,0.999,0.001)
cutoff_data=data.frame(cutoff=99999,sn=99999,sp=99999,ks=99999,F5=99999,F.1=99999,M=99999)
for(cutoff in cutoffs){
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  sn=TP/P
  sp=TN/N
  precision=TP/(TP+FP)
  recall=sn
  ks=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  M=(100*FP+TP)/(5*(P+N))
  cutoff_data=rbind(cutoff_data,c(cutoff,sn,sp,ks,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]

library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=F5))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>%
  gather(Measure,Value,sn:F.1)

ggplot(cutoff_long,aes(x=cutoff,y=value,colore=Measure))+geom_line()

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$ks)]

my_cutoff


test.predicted=as.numeric(test.prob.score>my_cutoff)
write.csv(test.predicted,"hr_submission_file.csv",row.names = F)

test$predicted_left=test.predicted
View(test$predicted_left)

View(hr_train1)
View(train)
