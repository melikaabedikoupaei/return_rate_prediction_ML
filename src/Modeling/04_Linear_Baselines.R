#### Linear Based Models 

# Content of this file 

# Part 1: Data preparation and exploration
# - Encode some category variables
# - Check correlation among the numeric variables

# Part 2: Modeling and Validation MAE test
# - Select variables by forward selection method
# - Fit a multiple linear model
# - Fit a ridge regression model
# - Fit a lasso regression model


#### Part 1: Data preparation and exploration
### Import packages------
library(leaps)
library(psych)
library(ISLR2)
library(dplyr)
library(glmnet)
library(car)
library(dummy)

### Import the data
train <- read.csv("Data/Gold/train.csv",header=T,fileEncoding = "UTF-8")
validation <- read.csv("Data/Gold/validation.csv",header=T,fileEncoding = "UTF-8")
results <- read.csv("Data/Results/results.csv",header=T,fileEncoding = "UTF-8")


###Look at tables
# check the info of the train set
head(train)
summary(train)
nrow(train)
str(train)

# check the info of the test set
head(test)
summary(test)
nrow(test)


###Encoding
# get the column names of the train set
colnames(train)

# get categories and dummies
# transfer the factor variables in the train, validation and test set.
train$subcategory <- factor(train$subcategory,levels=union(unique(train$subcategory),unique(test$subcategory)))
test$subcategory <- factor(test$subcategory,levels=union(unique(train$subcategory),unique(test$subcategory)))
validation$subcategory <- factor(validation$subcategory,levels=union(unique(train$subcategory),unique(test$subcategory)))

# get the unique levels
cats <- categories(train[, c( "season","subcategory")])

# apply on train set (exclude reference categories)
# create a dummies funciton
encode <- function(df){
  # encode the dummy variables
  dummies_df <-dummy(df[, c( "season","subcategory")], object = cats)
  dummies_output <- subset(dummies_df, select = -c( season_Summer,subcategory_Adult))
  
  # merge the dummy variabes with the original dataframe
  df_output <- subset(df, select = -c( season,subcategory))
  df_output <- cbind(df, dummies_output)
  return(df_output)
  }

# apply the function
train <- encode(train)
test <- encode(test)
validation <- encode(validation)


# calculate the correlation
# subset the numeric variables
train_numeric <- subset(train, select = c(count_size,price,total_sold,total_returned,offline_sales,offline_return,online_sales,online_return,online_percentage,unique_channel_count,yearly_return_rate))

# get teh correlation matrix
correlation <- cor(train_numeric)
View(correlation)



#### Part 2: Modeling and Validation MAE test
###Forward selection
#Use the forward selection method to determine the features
# set seed
set.seed(1)

# get the column name
colnames(train)

# fit the model
regfit <- regsubsets(yearly_return_rate ~ numeric_size+count_size+price+total_sold
                     +total_returned+online_percentage+unique_channel_count+SR
                     +SO+introduction_time++first_return
                     +season_Winter+subcategory_Petite
                     +subcategory_End.Items
                     ,data = train, nvmax = 30
                     ,method ="forward")


# get the infomation of the regfit
reg.summary <- summary(regfit)
reg.summary

# check the r square and plot 
reg.summary$rsq
reg.summary$adjr2
plot(reg.summary$rsq,xlab = "number of variables", ylab = "rss", type = "l")
plot(reg.summary$adjr2, xlab = "number of variables", ylab = "adjuested rsq", type = "l")

# find the highest r square value 
max_index <- which.max(reg.summary$adjr2)
points(max_index, reg.summary$adjr2[max_index], col="red",cex=2,pch=20)
max_index   # 13

# get the coef values 
coef(regfit, max_index)

### Model 1 : multiple linear model
# take corresponding variables to fit the model
lm.fit <- lm(yearly_return_rate ~ numeric_size
             +count_size
             +price
             +total_sold
             #+total_returned
             +online_percentage
             #+unique_channel_count
             +SR 
             +SO
             +first_return
             #+introduction_time
             +season_Winter
             +subcategory_Petite
             , data = train)

# get summary
summary(lm.fit)  # unique_channel_count is not significant and we will delete some variable which p-value is lower than 0.05

# collinearity test          
vif_result <- vif(lm.fit)
vif_result   # all vif of variables are below 5, there are no collineary


##Test the multiple linear regression
# predict
validation$lmpreds<- predict(lm.fit, validation)

# MAE
mae <- mean(abs(validation$yearly_return_rate - validation$lmpreds))

# set x and y variables
mae  # 0.1105558  

# calculate the absolute value
abs_err_lm.fit <- (abs(validation$lmpreds - validation$yearly_return_rate))

# Append the information for the "forward" model
results <- rbind(results, c("lmpreds", mean(abs_err_lm.fit), median(abs_err_lm.fit),
                            sd(abs_err_lm.fit), IQR(abs_err_lm.fit), range(abs_err_lm.fit)))

# show
View(results)


### Model 2 : Ridge Regression
#The goal is to optimize:
#   RSS + lambda * Sum( beta_i ^ 2)
#   lambda => 0,  a tuning parameter

# # set x and y variables
colnames(train)
x <- model.matrix(yearly_return_rate ~ .-pid-brand-category-subsubcategory, train)[, -1]
colnames(x)
y <- train$yearly_return_rate

# set grid values
lambda_ridge_grid <- 10 ^ seq(10, -3, length = 100)
lambda_ridge_grid

#Cross validation to choose the best model
set.seed(1234)
ridge_cv <- cv.glmnet(x, y, alpha = 0, nfolds = 10, lambda = lambda_ridge_grid)

#The mean cross-validated error
ridge_cv$cvm

#Estimate of standard error of cvm.
ridge_cv$cvsd

#value of lambda that gives minimum cvm
best_lambda <- ridge_cv$lambda.min   #0.001353048
best_lambda

#Coefficients of regression w/ best_lambda
ridgereg <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(ridgereg)

##Test Ridge Regression
# set x variables
colnames(validation)
x_validation <- model.matrix(yearly_return_rate ~ .-pid-brand-category-subsubcategory-lmpreds, validation)[, -1]

# predict
validation$ridgereg <- predict(ridgereg, s = best_lambda, newx = x_validation)

# calculate the absolute value
abs_err_ridgereg <- (abs(validation$ridgereg - validation$yearly_return_rate))

# Append the information for the "bestsubset" model
results <- rbind(results, c("ridgereg", mean(abs_err_ridgereg), median(abs_err_ridgereg),
                            sd(abs_err_ridgereg), IQR(abs_err_ridgereg), range(abs_err_ridgereg)))
# show
View(results)


### Model 3 : Lasso Regression
#The goal is to optimize:
#   RSS + lambda * Sum(abs(beta_i))
#   lambda => 0,  a tuning parameter

#Cross validation to choose the best model
set.seed(1234)
lasso_cv <- cv.glmnet(x, y, alpha = 1, nfolds = 10,lambda = lambda_ridge_grid)

#The mean cross-validated error
lasso_cv$cvm

#Estimate of standard error of cvm.
lasso_cv$cvsd

#value of lambda that gives minimum cvm
lasso_cv$lambda.min     # 0.001
 
# fit model with best lambda
lassoreg <- glmnet(x, y, alpha = 1, lambda = lasso_cv$lambda.min)
coef(lassoreg)

##Test Lasso Regression
# # set x variables
colnames(validation)
x_validation <- model.matrix(yearly_return_rate ~ .-pid-brand-category-subsubcategory-bestsubset-ridgereg-lmpreds, validation)[, -1]
colnames(x_validation)

# predict
validation$lassoreg <- predict(lassoreg, s = ridge_cv$lambda.min, newx = x_validation)

# calculate the absolute value
abs_err_lassoreg <- (abs(validation$lassoreg - validation$yearly_return_rate))

# Append the information for the "bestsubset" model
results <- rbind(results, c("lassoreg", mean(abs_err_lassoreg), median(abs_err_lassoreg),
                            sd(abs_err_lassoreg), IQR(abs_err_lassoreg), range(abs_err_lassoreg)))

# show 
View(results)

### write to csv
write.csv(results, "results.csv", row.names = FALSE)
