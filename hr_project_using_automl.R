library(stringr)
library(data.table)
library(MASS)
library(randomForest)
library(purrr)
library(caret)
library(visdat)
library(tidyr)
library(dplyr)
setwd("D:\\Edvancer\\Projects\\HR")
test=read.csv("hr_test.csv")
train=read.csv("hr_train.csv")
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
library(h2o)
h2o.init(nthreads = 3)
train <- as.h2o(train)
test <- as.h2o(test)
y <- "left"
x <- setdiff(names(train),y)

train[,y] <- as.factor(train[,y])

aml <- h2o.automl(x=x, y=y,
                  training_frame = train,
                  max_models = 6,
                  seed=1)
lb <- aml@leaderboard
print(lb,n = nrow(lb))

#bm <- h2o.get_best_model(aml)
bm <- h2o.getModel("GBM_3_AutoML_1_20230410_173305")
bm
x=as.data.frame(h2o.predict(bm,test))

colnames(x)[1]="left"
x=x %>% select(-p0,-p1)

write.csv(x,"D:\\Edvancer\\Projects\\HR\\submission.csv",row.names = F)

######## manual calculation  (TP, FN, TN, FP  values taken from automl)

TP=2461
FN=614
TN=7030
FP=394

P=TP+FN
N=TN+FP

Sn=TP/P
Sp=TN/N
precision=TP/(TP+FP)
recall=Sn

KS=(TP/P)-(FP/N)
F5=(26*precision*recall)/((25*precision)+recall)
F.1=(1.01*precision*recall)/((.01*precision)+recall)

M=(4*FP+FN)/(5*(P+N))

Sn
Sp
KS
F5
F.1
M





# Quiz questions

table(train$promotion_last_5years)##ok

round((var(train$satisfaction_level[train$left=="0"], na.rm = T)),4)

y=rnorm(train$average_montly_hours)

hist(y, main = "Normal DIstribution")

train$temp=dnorm(train$average_montly_hours,mean=mean(train$average_montly_hours),sd=sd(train$average_montly_hours))
ggplot(train,aes(x=average_montly_hours))+
  geom_density(color="red")+
  geom_line(aes(x=average_montly_hours,y=temp),color="green")

summary(train)

l=train$salary[train$left == 1]
table(l)
table(train$left)

summary(train)

round(cor(train$last_evaluation, train$average_montly_hours),2)

summary(train)

h=train$time_spend_company[train$lef == 1]
h
w=sum(h)
w
x=sum(train$left == "1")
x
median(train$time_spend_company[train$left == 1])

summary(train)

u=unique(train$sales)
u
median(train$average_montly_hours[train$sales == "sales"])
median(train$average_montly_hours[train$sales == "technical"])
median(train$average_montly_hours[train$sales == "product_mng"])
median(train$average_montly_hours[train$sales == "marketing"])
median(train$average_montly_hours[train$sales == "IT"])
median(train$average_montly_hours[train$sales == "RandD"])
median(train$average_montly_hours[train$sales == "hr"])
median(train$average_montly_hours[train$sales == "accounting"])
median(train$average_montly_hours[train$sales == "support"])
median(train$average_montly_hours[train$sales == "management"])

npl=train$number_project[train$lef == 1]
unique(npl)
table(npl)

npc=train$number_project[train$lef == 0]
unique(npc)
table(npc)


