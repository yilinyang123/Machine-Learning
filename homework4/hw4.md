``` r
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

``` r
library(ElemStatLearn)
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'ggplot2'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     margin

Convert the response variable in the “vowel.train” data frame to a factor variable prior to training, so that “randomForest” does classification rather than regression.
------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
vowel.train$y = factor(vowel.train$y)
```

Review the documentation for the “randomForest” function. Fit the random forest model to the vowel data using all of the 11 features using the default values of the tuning parameters.
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
rf <- randomForest(y~.,data=vowel.train)
```

Use 5-fold CV and tune the model by performing a grid search for the following tuning parameters: 1) the number of variables randomly sampled as candidates at each split; consider values 3, 4, and 5, and 2) the minimum size of terminal nodes; consider a sequence (1, 5, 10, 20, 40, and 80).
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
mtry = c(3,4,5)
nodesize = c(1,5,10,20,40,80)
params = expand.grid(mtry = mtry,nodesize = nodesize)
params
```

    ##    mtry nodesize
    ## 1     3        1
    ## 2     4        1
    ## 3     5        1
    ## 4     3        5
    ## 5     4        5
    ## 6     5        5
    ## 7     3       10
    ## 8     4       10
    ## 9     5       10
    ## 10    3       20
    ## 11    4       20
    ## 12    5       20
    ## 13    3       40
    ## 14    4       40
    ## 15    5       40
    ## 16    3       80
    ## 17    4       80
    ## 18    5       80

``` r
folds_5 <- createFolds(vowel.train$y,list = F, k = 5)
folds_5
```

    ##   [1] 5 4 1 1 3 3 3 4 2 4 4 5 1 2 1 5 5 2 3 5 1 3 1 5 3 5 1 3 1 3 5 4 2 4 1
    ##  [36] 3 5 5 1 5 2 3 3 3 4 4 2 5 1 4 3 4 3 3 2 1 1 4 5 3 2 3 4 1 5 4 3 5 5 3
    ##  [71] 3 5 2 1 5 4 5 4 1 4 2 2 4 1 1 3 3 4 2 2 5 4 1 1 1 5 5 5 5 4 2 4 2 3 1
    ## [106] 4 5 1 5 2 3 4 2 3 2 3 4 5 3 5 2 2 2 1 3 4 5 3 1 5 4 4 3 4 2 1 5 5 5 5
    ## [141] 1 3 1 2 3 4 3 1 4 2 4 4 1 5 3 4 5 4 2 2 2 2 4 5 4 4 3 4 3 5 1 5 2 1 1
    ## [176] 2 1 3 1 4 1 3 1 1 3 2 3 2 2 3 1 2 5 3 2 3 4 1 5 1 1 2 1 2 3 1 2 1 1 4
    ## [211] 5 5 5 2 2 5 4 1 2 5 3 4 4 3 4 4 5 2 2 3 5 1 3 3 1 5 2 4 4 2 5 1 5 4 4
    ## [246] 4 4 3 1 1 4 3 1 1 1 2 3 5 3 2 1 3 3 3 5 5 5 4 4 5 4 1 3 3 5 1 2 1 4 1
    ## [281] 2 3 3 3 1 3 5 4 5 1 4 5 1 5 5 1 2 5 5 1 3 4 1 1 2 1 4 4 3 1 5 1 4 4 3
    ## [316] 2 2 2 3 1 3 2 2 4 2 5 3 2 2 2 4 4 3 1 3 1 4 1 5 5 3 5 2 3 5 1 4 4 4 4
    ## [351] 1 3 4 5 2 3 4 1 1 1 4 3 1 4 3 2 2 3 5 3 5 1 2 4 5 3 3 1 5 4 4 3 1 5 4
    ## [386] 1 5 1 5 5 3 1 5 5 4 1 4 1 5 2 2 2 4 3 5 5 2 3 2 1 4 3 4 5 2 1 1 3 2 5
    ## [421] 2 5 1 2 1 4 4 2 2 2 2 5 2 2 4 5 3 2 4 2 3 5 1 4 3 5 2 4 4 1 1 2 3 3 2
    ## [456] 4 2 2 5 4 2 1 1 2 4 2 2 5 4 3 4 2 4 2 4 5 5 5 3 5 2 2 4 5 2 5 3 3 2 1
    ## [491] 2 4 1 4 1 3 1 1 2 5 1 5 5 4 1 4 3 3 4 4 3 3 2 3 2 5 5 5 1 3 5 3 4 4 5
    ## [526] 2 2 5

``` r
## use matrix to store all CV error
cv_error <- matrix(0,18,5)
for (i in 1:18){
  for (j in 1:5){
    train_data <- vowel.train[which(folds_5!=j),]
    test_data <- vowel.train[which(folds_5==j),]
    rf_grid <- randomForest(y ~.,data = train_data,
                            mtry = params[i,1],
                            nodesize = params[i,2])
    rf_pred <- predict(rf_grid, test_data)
    cv_error[i,j] <- mean(rf_pred != test_data$y)
  }
}
```

``` r
mean_cv_tune <- apply(cv_error,1,mean)
min_cv <- min(mean_cv_tune)
ind <- which(mean_cv_tune == min_cv)
params[ind,]
```

    ##   mtry nodesize
    ## 2    4        1

Thus mtry =3, nodesize =1 will generate smallest cross validation error

With the tuned model, make predictions using the majority vote method, and compute the misclassification rate using the ‘vowel.test’ data.
------------------------------------------------------------------------------------------------------------------------------------------

``` r
vowel.test$y = factor(vowel.test$y)
```

``` r
rf_tune <- randomForest(y ~.,data=vowel.train, mtry = 3, nodesize = 1)
rf_tune_pred <- predict(rf_tune, vowel.test)
mis_rate <- mean(rf_tune_pred != vowel.test$y)
mis_rate
```

    ## [1] 0.4047619
