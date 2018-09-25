#session15_assignmnet_15.0

#1. Use the below given data set 

DataSet 

library(readr)
library(data.table)
library(foreach)


getwd()
p<-"C:/Users/gopi/Documents/R files test/Dataset"
setwd(p)
train1<-fread("C:/Users/gopi/Documents/R files test/Dataset/Training/Features_Variant_1.csv")
train2<-fread("C:/Users/gopi/Documents/R files test/Dataset/Training/Features_Variant_2.csv")
train3<-fread("C:/Users/gopi/Documents/R files test/Dataset/Training/Features_Variant_3.csv")
train4<-fread("C:/Users/gopi/Documents/R files test/Dataset/Training/Features_Variant_4.csv")
train5<-fread("C:/Users/gopi/Documents/R files test/Dataset/Training/Features_Variant_5.csv")
train<-rbind(train1,train2,train3,train4,train5)
train
View(train)

test<-fread("C:/Users/gopi/Documents/R files test/Dataset/Testing/Features_TestSet.csv")
View(test)

# log-transform    
train[, V54 := log(1 + V54)]    
test[, V54 := log(1 + V54)]   
training<-lapply(train[,V1:V54],as.numeric)
train<-sapply(training, class)

#a. Predict the no of comments in next H hrs
library(tree)
library(C50)

model1<-lm(train$V54~. , data = train)
mean(model1$residuals)
round(mean(model1$residuals),1)
plot(model1)

acf(model1$residuals)
coefficients(model1)
confint(model1,level = 0.95)
anova(model1)

library(car)
vif(model1) #to check multicolinearity between IVs

library(MASS)
fit<-lm(train$V54~.,data = train)
step<-stepAIC(fit, direction = "both")

library(lmtest)
dwtest(model1)
cor.test(train$V5,model1$residuals)
ana<-tree(train$V54~., data=train)
summary(ana)



#b. Use regression technique
library(rpart)
lys<-rpart(train$V54~.,data =train)
plot(lys)
text(lys)
pred<-predict(lys,test)
rpart.plot::rpart.plot(lys)
confusionMatrix(pred,test[,-V54])



#c. Report the training accuracy and test accuracy 
library(caret)
train_control<-trai
nControl(method = "cv", number = 3)
tune_grid<-expand.grid(interaction.depth = 2,n.trees = 500, shrinkage = 0.1,n.minobsinnode = 10)


set.seed(123)
model<-train(train$V54~., data=train,tuneGrid=gbmGrid, trcontrol=train_control, method="gbm", verbose=FALSE)
predicted<-predict(model,test[,-V54], type="link")




