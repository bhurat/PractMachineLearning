---
title: "Predicting Exercises"
author: "Basile H"
date: "March 21, 2015"
output: pdf_document
---

#Summary
These days, technology has allowed us to record nearly every part of our lives. This data was collected using various accelorometers and gyrometers in order to record dumbell lifts and see how well the exercise is performed. The goal of this paper is to use the data collected to predict if the exercise was done right and, if not, in what way it was done wrong. The data was classed into five categories, with 1 correct way and 4 inccorect ways. They are as follows: According to specification (A), Throwing elbows to the front (B), Lifting dumbbell only halfway (C), Lowering dumbbell only halfway (D), Throwing hips to the front (E). To do so, I looked at the data, cleaned it, then after some tests, went for a random forest model for prediction. Using 5-fold cross validation, I estimated a 98-99% accuracy. 

#Data Cleaning

First, we loaded the data set and required packages (caret and randomForest)


```r
dat<-read.csv("~/Documents/Coursera Programming Files/PractMachineLearning/pml-training.csv")
library(caret)
library(randomForest)
set.seed(96720)
```

Calling the summary function on the data, we find that there are columns with NA values, as well with columns that have empty (" ") and strange entries ("#DIV/0!"). Furthermore, looking at the first few columns, it is noticed that the first 7 are focused on time, subject, and count, which while useful to have in the data set, are not useful for prediction. 


```r
k<-1:7
for (i in 1:ncol(dat)) {
         if (sum(dat[,i]=="#DIV/0!")>0 || sum(dat[,i]==" ")>0 || sum(is.na(dat[,i]))){
                 k<-c(k,i)
                 }
         }
datCL<-dat[,-k]
```

#Choosing a Model

Even with all the data cleaning, we are left with 52 variables, and thus, 51 predictors. This is a large amount and so for my prediction, I tended to leave it all in. Instead, I looked at the various models. Found on a discussion forum for this class, I found a quick rundown of the various methods talked about in this class based on runtime. It was quoted as such: lm, glm, lda, random forest, bagging, boosting. I attempted linear discriminant analysis and random forest, as lm/glm do not work with categorical variables and bagging and boosting seemed too cpu intensive for my machine. Furthermore, to reduce strain on the computer, I reduced sample size to 2000 before splitting it into a training and test set for comparisons. Doing so, I got a 70% accuracy with the linear discriminant analysis and a 93% accuracy with the random forest, leaving me with a pretty conclusive choice for my model.

For the next part, I estimated the predictive accuracy of the model with 5-fold cross validation and average the accuracy of each fold, for which I got 99.6%. Given that, I estimate around a 98% to 99% accuracy, taking into account overfitting. Below, you see the various confusion matrices for the 5 folds, followed by the average of the accuracies at each fold: 

```r
#K-fold cross validation with K=5
trainFolds<-createFolds(y=datCL$classe,k=5,list=TRUE,returnTrain=TRUE)
#fit 5 models
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
mean(Accuracies)
```

```
## [1] 0.9959229
```
# Conclusions
In the end, the random forest method shows itself to be very accurate. When applying to the test set problems, we got 100% accuracy, though considering the sample size, that is not a fair test. However, it is nonetheless a testament to the predictive model and shows that it is indeed possible to predict the quality of a dumbbell curl by using the data from accelorometers and gyrometers. 

# Extra
## Full Code
Full code can be found in the same github repository in the file PracticalMachineLearningCourse.Rmd
## Sources
This post from discussion sections, which focuses on strategies to reduce CPU usage: https://class.coursera.org/predmachlearn-012/forum/thread?thread_id=29

Quick - R: Articles on Discriminant Function Analysis (http://www.statmethods.net/advstats/discriminant.html) and Tree-Based Models (http://www.statmethods.net/advstats/cart.html)

Site with data and description of data: http://groupware.les.inf.puc-rio.br/har
