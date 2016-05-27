<!-- rmarkdown v1 -->
---
title: "Human Activity Recognition"
author: "Andrew Marsh"
date: "May 27, 2016"
output: html_document
---



## Human Activity Recognition

Human Activity Recognition - HAR - has emerged as a key research area in the last years and is gaining increasing attention by the pervasive computing research community, especially for the development of context-aware systems. There are many potential applications for HAR, like: elderly monitoring, life log systems for monitoring energy expenditure and for supporting weight-loss programs, and digital assistants for weight lifting exercises. 

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, our goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

## Data

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv


The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv


The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. We are greatful to them for allowing their data to be used for our research.

## Download the Data

Load the training and test datasets into tables.

```r
# Training and Test Datasets
train<-read.csv("C:/amarsh/HOME/amarsh/Andrew/Data Science/Practical Machine Learning/pml-training.csv", header=T, na.strings=c("NA", "#DIV/0!"))
test<-read.csv("C:/amarsh/HOME/amarsh/Andrew/Data Science/Practical Machine Learning/pml-testing.csv", header=T, na.string=c("NA", "#DIV/0!"))
```

## Preprocess the Data
Belt, arm, dumbbell, and forearm variables that do not have any missing ("NA", "#DIV/0!"") values in the training dataset will be predictor candidates. That left us with 52 variables and 19622 class measurements in the training dataset. The same variables were maintained in the test data set to be used for predicting the 20 test cases provided. Belt, arm, dumbbell, and forearm variables that do not have any missing values in the test dataset will be predictor candidates. 


```r
require(data.table)
```

```
## Loading required package: data.table
```

```
## Warning: package 'data.table' was built under R version 3.2.4
```

```r
searchForNA <- sapply(test, function (x) any(is.na(x) | x == ""))
predVariables <- !searchForNA & grepl("belt|[^(fore)]arm|dumbbell|forearm", names(searchForNA))
modelVariables <- names(searchForNA)[predVariables]
modelVariables
```

```
##  [1] "roll_belt"            "pitch_belt"           "yaw_belt"            
##  [4] "total_accel_belt"     "gyros_belt_x"         "gyros_belt_y"        
##  [7] "gyros_belt_z"         "accel_belt_x"         "accel_belt_y"        
## [10] "accel_belt_z"         "magnet_belt_x"        "magnet_belt_y"       
## [13] "magnet_belt_z"        "roll_arm"             "pitch_arm"           
## [16] "yaw_arm"              "total_accel_arm"      "gyros_arm_x"         
## [19] "gyros_arm_y"          "gyros_arm_z"          "accel_arm_x"         
## [22] "accel_arm_y"          "accel_arm_z"          "magnet_arm_x"        
## [25] "magnet_arm_y"         "magnet_arm_z"         "roll_dumbbell"       
## [28] "pitch_dumbbell"       "yaw_dumbbell"         "total_accel_dumbbell"
## [31] "gyros_dumbbell_x"     "gyros_dumbbell_y"     "gyros_dumbbell_z"    
## [34] "accel_dumbbell_x"     "accel_dumbbell_y"     "accel_dumbbell_z"    
## [37] "magnet_dumbbell_x"    "magnet_dumbbell_y"    "magnet_dumbbell_z"   
## [40] "roll_forearm"         "pitch_forearm"        "yaw_forearm"         
## [43] "total_accel_forearm"  "gyros_forearm_x"      "gyros_forearm_y"     
## [46] "gyros_forearm_z"      "accel_forearm_x"      "accel_forearm_y"     
## [49] "accel_forearm_z"      "magnet_forearm_x"     "magnet_forearm_y"    
## [52] "magnet_forearm_z"
```

Subset the model variables dataset to include only the predictor candidates and the outcome variable "classe."


```r
totalVariables <- c("classe", modelVariables)
trainClean <- train[,totalVariables]
testClean <- test[,modelVariables]

dim(trainClean)
```

```
## [1] 19622    53
```

```r
names(trainClean)
```

```
##  [1] "classe"               "roll_belt"            "pitch_belt"          
##  [4] "yaw_belt"             "total_accel_belt"     "gyros_belt_x"        
##  [7] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"        
## [10] "accel_belt_y"         "accel_belt_z"         "magnet_belt_x"       
## [13] "magnet_belt_y"        "magnet_belt_z"        "roll_arm"            
## [16] "pitch_arm"            "yaw_arm"              "total_accel_arm"     
## [19] "gyros_arm_x"          "gyros_arm_y"          "gyros_arm_z"         
## [22] "accel_arm_x"          "accel_arm_y"          "accel_arm_z"         
## [25] "magnet_arm_x"         "magnet_arm_y"         "magnet_arm_z"        
## [28] "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"        
## [31] "total_accel_dumbbell" "gyros_dumbbell_x"     "gyros_dumbbell_y"    
## [34] "gyros_dumbbell_z"     "accel_dumbbell_x"     "accel_dumbbell_y"    
## [37] "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"   
## [40] "magnet_dumbbell_z"    "roll_forearm"         "pitch_forearm"       
## [43] "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"     
## [46] "gyros_forearm_y"      "gyros_forearm_z"      "accel_forearm_x"     
## [49] "accel_forearm_y"      "accel_forearm_z"      "magnet_forearm_x"    
## [52] "magnet_forearm_y"     "magnet_forearm_z"
```

```r
dim(testClean)
```

```
## [1] 20 52
```
Make classe into a factor.


```r
trainClean$classe <- factor(trainClean$classe) 
```

## Data Partition and Prediction Process

The cleaned downloaded data set was subset in order to generate a test set independent from the 20 cases provided set. Partitioning was performed to obtain a 80% training set and a 20% validation set.


```r
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.2.5
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```r
inTrain<-createDataPartition(y=trainClean$classe, p=0.75,list=F)
trainPartition<-trainClean[inTrain,] 
validationPartition<-trainClean[-inTrain,] 

dim(trainPartition)
```

```
## [1] 14718    53
```

```r
dim(validationPartition)
```

```
## [1] 4904   53
```
## Results and Conclusions
Random forest trees were generated for the training dataset using cross-validation. The generated algorithm was examnined under the partitioned training set to examine the accuracy and estimated error of prediction. By using 52 predictors for five classes using cross-validation at a 5-fold an accuracy of 99.2% with a 95% CI [0.9891-0.9943] was achieved accompanied by a Kappa value of 0.9899.


```r
library(caret)
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.2.5
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```r
set.seed(666)
fitControl2<-trainControl(method="cv", number=5, allowParallel=T, verbose=T)
rffit<-train(classe~.,data=trainPartition, method="rf", trControl=fitControl2, verbose=F)
```

```
## + Fold1: mtry= 2 
## - Fold1: mtry= 2 
## + Fold1: mtry=27 
## - Fold1: mtry=27 
## + Fold1: mtry=52 
## - Fold1: mtry=52 
## + Fold2: mtry= 2 
## - Fold2: mtry= 2 
## + Fold2: mtry=27 
## - Fold2: mtry=27 
## + Fold2: mtry=52 
## - Fold2: mtry=52 
## + Fold3: mtry= 2 
## - Fold3: mtry= 2 
## + Fold3: mtry=27 
## - Fold3: mtry=27 
## + Fold3: mtry=52 
## - Fold3: mtry=52 
## + Fold4: mtry= 2 
## - Fold4: mtry= 2 
## + Fold4: mtry=27 
## - Fold4: mtry=27 
## + Fold4: mtry=52 
## - Fold4: mtry=52 
## + Fold5: mtry= 2 
## - Fold5: mtry= 2 
## + Fold5: mtry=27 
## - Fold5: mtry=27 
## + Fold5: mtry=52 
## - Fold5: mtry=52 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 27 on full training set
```


```r
predrf<-predict(rffit, newdata=validationPartition)
confusionMatrix(predrf,validationPartition$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1388    8    0    0    0
##          B    4  941    6    0    0
##          C    2    0  847    8    4
##          D    0    0    2  794    2
##          E    1    0    0    2  895
## 
## Overall Statistics
##                                           
##                Accuracy : 0.992           
##                  95% CI : (0.9891, 0.9943)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9899          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9950   0.9916   0.9906   0.9876   0.9933
## Specificity            0.9977   0.9975   0.9965   0.9990   0.9993
## Pos Pred Value         0.9943   0.9895   0.9837   0.9950   0.9967
## Neg Pred Value         0.9980   0.9980   0.9980   0.9976   0.9985
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2830   0.1919   0.1727   0.1619   0.1825
## Detection Prevalence   0.2847   0.1939   0.1756   0.1627   0.1831
## Balanced Accuracy      0.9964   0.9945   0.9936   0.9933   0.9963
```

Apply the centering and scaling to the probing dataset.


```r
# Predict the outcome of the 20 test cases
predTest<-predict(rffit, newdata=testClean)
predTest
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

A boosting algorithm was also run to confirm and be able to compare predictions. The boosting approach presented slightly less accuracy (95.98%) with a 95% CI [0.954-0.9652] was achieved accompanied by a Kappa value of 0.9492. The predictions for the 20 test cases were very similar for both ran algorimths.


```r
library(gbm)
```

```
## Warning: package 'gbm' was built under R version 3.2.5
```

```
## Loading required package: survival
```

```
## 
## Attaching package: 'survival'
```

```
## The following object is masked from 'package:caret':
## 
##     cluster
```

```
## Loading required package: splines
```

```
## Loading required package: parallel
```

```
## Loaded gbm 2.1.1
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.2.4
```

```r
fitControl2<-trainControl(method="cv", number=5, allowParallel=T, verbose=T)
gmbfit<-train(classe~.,data=trainPartition, method="gbm", trControl=fitControl2, verbose=F)
```

```
## + Fold1: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
## - Fold1: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
## + Fold1: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
## - Fold1: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
## + Fold1: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
## - Fold1: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
## + Fold2: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
## - Fold2: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
## + Fold2: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
## - Fold2: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
## + Fold2: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
## - Fold2: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
## + Fold3: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
## - Fold3: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
## + Fold3: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
## - Fold3: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
## + Fold3: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
## - Fold3: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
## + Fold4: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
## - Fold4: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
## + Fold4: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
## - Fold4: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
## + Fold4: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
## - Fold4: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
## + Fold5: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
## - Fold5: shrinkage=0.1, interaction.depth=1, n.minobsinnode=10, n.trees=150 
## + Fold5: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
## - Fold5: shrinkage=0.1, interaction.depth=2, n.minobsinnode=10, n.trees=150 
## + Fold5: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
## - Fold5: shrinkage=0.1, interaction.depth=3, n.minobsinnode=10, n.trees=150 
## Aggregating results
## Selecting tuning parameters
## Fitting n.trees = 150, interaction.depth = 3, shrinkage = 0.1, n.minobsinnode = 10 on full training set
```

```r
gmbfit$finalModel
```

```
## A gradient boosted model with multinomial loss function.
## 150 iterations were performed.
## There were 52 predictors of which 40 had non-zero influence.
```

```r
class(gmbfit)
```

```
## [1] "train"         "train.formula"
```

```r
predgmb<-predict(gmbfit, newdata=validationPartition)
confusionMatrix(predgmb, validationPartition$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1361   34    0    0    1
##          B   19  880   21    2    8
##          C   11   30  826   22   14
##          D    1    3    6  774   15
##          E    3    2    2    6  863
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9592          
##                  95% CI : (0.9533, 0.9646)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9484          
##  Mcnemar's Test P-Value : 2.532e-06       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9756   0.9273   0.9661   0.9627   0.9578
## Specificity            0.9900   0.9874   0.9810   0.9939   0.9968
## Pos Pred Value         0.9749   0.9462   0.9147   0.9687   0.9852
## Neg Pred Value         0.9903   0.9826   0.9928   0.9927   0.9906
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2775   0.1794   0.1684   0.1578   0.1760
## Detection Prevalence   0.2847   0.1896   0.1841   0.1629   0.1786
## Balanced Accuracy      0.9828   0.9573   0.9735   0.9783   0.9773
```

```r
predtrain<-predict(gmbfit, newdata=trainPartition)
confusionMatrix(predtrain, trainPartition$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 4142   66    0    2    3
##          B   31 2729   54    4   16
##          C    9   50 2480   64   16
##          D    2    3   28 2331   31
##          E    1    0    5   11 2640
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9731          
##                  95% CI : (0.9704, 0.9756)
##     No Information Rate : 0.2843          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.966           
##  Mcnemar's Test P-Value : 9.438e-11       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9897   0.9582   0.9661   0.9664   0.9756
## Specificity            0.9933   0.9912   0.9886   0.9948   0.9986
## Pos Pred Value         0.9831   0.9629   0.9469   0.9733   0.9936
## Neg Pred Value         0.9959   0.9900   0.9928   0.9934   0.9945
## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1839
## Detection Rate         0.2814   0.1854   0.1685   0.1584   0.1794
## Detection Prevalence   0.2862   0.1926   0.1779   0.1627   0.1805
## Balanced Accuracy      0.9915   0.9747   0.9773   0.9806   0.9871
```

## Print out Predicted Values

```r
predTest
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
