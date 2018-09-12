#session15_assignmnet_15.0

#1. Use the below given data set 

DataSet 

library(readr)
library(data.table)
library(foreach)


getwd()
p<-"C:/Users/Swapna/Documents/R files test/Dataset"
setwd(p)
train1<-fread("C:/Users/Swapna/Documents/R files test/Dataset/Training/Features_Variant_1.csv")
train2<-fread("C:/Users/Swapna/Documents/R files test/Dataset/Training/Features_Variant_2.csv")
train3<-fread("C:/Users/Swapna/Documents/R files test/Dataset/Training/Features_Variant_3.csv")
train4<-fread("C:/Users/Swapna/Documents/R files test/Dataset/Training/Features_Variant_4.csv")
train5<-fread("C:/Users/Swapna/Documents/R files test/Dataset/Training/Features_Variant_5.csv")
train<-rbind(train1,train2,train3,train4,train5)
train
View(train)

test<-fread("C:/Users/Swapna/Documents/R files test/Dataset/Testing/Features_TestSet.csv")
View(test)

# log-transform    
train[, V54 := log(1 + V54)]    
test[, V54 := log(1 + V54)]   
training<-lapply(train[,V1:V54],as.numeric)
train<-sapply(training, class)

#a. Predict the no of comments in next H hrs
library(tree)
library(C50)

model1<-lm(train$V54~., data = train)
mean(model1$residuals)
round(mean(model1$residuals),1)
acf(model1$residuals)
coefficients(model1)
confint(model1,level = 0.95)
anova(model1)

library(MASS)
fit<-lm(train$V54~.,data = train)
step<-stepAIC(fit, direction = "both")
library(lmtest)
dwtest(model1)
cor.test(train$V5,model1$residuals)
ana<-tree(train$V54, data=train)
predi<-predict(ana, churnTest[,-1], type="class")
library(caret)
library(rpart)

train_control<-trainControl(method = "cv", number = 3)
model<-train(train$V54~., data=train, trcontrol=train_control, method="glm")



confusionMatrix(predi, churnTest$churn)
library(rpart)
lys<-rpart(churn~.,data =churnTrain[,-1])
plot(lys)
text(lys)
pred<-predict(lys,churnTest[,-1], typle="class")
confusionMatrix(pred,churnTest$churn)
rpart.plot::rpart.plot(lys)

#b. Use regression technique




#c. Report the training accuracy and test accuracy 




