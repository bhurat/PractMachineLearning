---
title: "Predicting Exercises"
author: "Basile H"
date: "March 19, 2015"
output: pdf_document
---


#Data Cleaning

```r
setwd("~/Documents/Coursera Programming Files/Practical Machine Learning/")
dat<-read.csv("pml-training.csv")
library(caret)
library(randomForest)
set.seed(96720)
```


```r
k<-1:7
for (i in 1:ncol(dat)) {
         if (sum(dat[,i]=="#DIV/0!")>0 || sum(dat[,i]==" ")>0 || sum(is.na(dat[,i]))){
                 k<-c(k,i)
                 }
         }
datCL<-dat[,-k]
```

testing models

```r
datCLSub<-datCL[sample(1:nrow(datCL),2000),]
inTrain<-createDataPartition(y=datCLSub$classe,p=.6,list=FALSE)
training<-datCLSub[inTrain,]
testing<-datCLSub[-inTrain,]
```
Linear Discriminant Analysis

```r
ModFitLDA<-train(classe~.,data=training,method="lda")
predLDA<-predict(ModFitLDA,newdata=testing)
ConfMatLDA<-confusionMatrix(testing$classe,predLDA)
ConfMatLDA$overall[1]
```

```
##  Accuracy 
## 0.7005013
```
Random Forest

```r
ModFitRF<-randomForest(classe~.,data=training)
predRF<-predict(ModFitRF,newdata=testing)
ConfMatRF<-confusionMatrix(testing$classe,predRF)
ConfMatRF$overall[1]
```

```
##  Accuracy 
## 0.9310777
```


Cross validation data splitting k = 5

```r
#K-fold cross validation with K=5
trainFolds<-createFolds(y=datCL$classe,k=5,list=TRUE,returnTrain=TRUE)
```


```r
ModFit1<-randomForest(classe~.,data=datCL[trainFolds[[1]],])
pred1<-predict(ModFit1,newdata=datCL[-trainFolds[[1]],])
ConfMat1<-confusionMatrix(datCL$classe[-trainFolds[[1]]],pred1)
ModFit2<-randomForest(classe~.,data=datCL[trainFolds[[2]],])
pred2<-predict(ModFit2,newdata=datCL[-trainFolds[[2]],])
ConfMat2<-confusionMatrix(datCL$classe[-trainFolds[[2]]],pred2)
ModFit3<-randomForest(classe~.,data=datCL[trainFolds[[3]],])
pred3<-predict(ModFit3,newdata=datCL[-trainFolds[[3]],])
ConfMat3<-confusionMatrix(datCL$classe[-trainFolds[[3]]],pred3)
ModFit4<-randomForest(classe~.,data=datCL[trainFolds[[4]],])
pred4<-predict(ModFit4,newdata=datCL[-trainFolds[[4]],])
ConfMat4<-confusionMatrix(datCL$classe[-trainFolds[[4]]],pred4)
ModFit5<-randomForest(classe~.,data=datCL[trainFolds[[5]],])
pred5<-predict(ModFit5,newdata=datCL[-trainFolds[[5]],])
ConfMat5<-confusionMatrix(datCL$classe[-trainFolds[[5]]],pred5)
Accuracies<-c(ConfMat1$overall[1],ConfMat2$overall[1],ConfMat3$overall[1],
              ConfMat4$overall[1],ConfMat5$overall[1])
ConfMat1$table
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 1116    0    0    0    0
##          B    2  757    0    0    0
##          C    0    2  682    1    0
##          D    0    0    9  634    0
##          E    0    0    0    3  719
```

```r
ConfMat2$table
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 1115    1    0    0    0
##          B    5  753    1    0    0
##          C    0    2  682    1    0
##          D    0    0    7  636    1
##          E    0    0    0    0  721
```

```r
ConfMat3$table
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 1115    1    0    0    0
##          B    3  755    1    0    0
##          C    0    2  682    0    0
##          D    0    0    2  640    1
##          E    0    0    1    1  719
```

```r
ConfMat4$table
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 1116    0    0    0    0
##          B    4  753    3    0    0
##          C    0    2  682    0    0
##          D    0    0    5  638    0
##          E    0    0    0    0  722
```

```r
ConfMat5$table
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 1115    0    0    0    1
##          B    2  758    0    0    0
##          C    0    7  676    1    0
##          D    0    0    5  638    0
##          E    0    0    0    1  720
```

```r
mean(Accuracies)
```

```
## [1] 0.9960249
```


```r
ModFitFin<-randomForest(classe~.,data=datCL)
ModFitFin
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = datCL) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.29%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 5578    1    0    0    1 0.0003584229
## B   11 3783    3    0    0 0.0036871214
## C    0   11 3410    1    0 0.0035067212
## D    0    0   23 3191    2 0.0077736318
## E    0    0    0    4 3603 0.0011089548
```
