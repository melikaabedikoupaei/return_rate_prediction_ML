#### Support vector regression

# Content of this file 

# Part 1: Data preparation
# - Import library and data set
# - Data encoding

# Part 2: Modeling and Validation MAE test
# - Fit Linear kernel model
# - Fit polynomial kernel model
# - Fit radial kernel model


# Part 1: Data preparation

### import packages
library(ISLR2)
library(dplyr)
library(glmnet)
library(gam)
library(splines)
library(boot)
library(e1071)

# load tables
train=read.csv("Data/Gold/train.csv",header=T,fileEncoding = "UTF-8")
validation=read.csv("Data/Gold/validation.csv",header=T,fileEncoding = "UTF-8")
results=read.csv("Data/Results/results.csv",header=T,fileEncoding = "UTF-8")

# Show the train table
head(train)
colnames(train)

# Encode season variables
train$season <-ifelse(train$season=="Winter",1,0)
test$season <-ifelse(test$season=="Winter",1,0)
validation$season <-ifelse(validation$season=="Winter",1,0)


# Part 2: Modeling and Validation MAE test

### Model 1 : Linear kernel model
# get the names of the columns
colnames(train)

# cv method to find the parameter 
# The range of cost is (0.1, 1, 5, 10)
set.seed(123)
linear_svm <- tune(svm,yearly_return_rate ~ return_rate+first_return+season+
                     introduction_season+introduction_time+SO+SR+unique_channel_count+
                     online_percentage+total_returned+total_sold+price+count_size+numeric_size
                   , data=train,kernel = "linear", ranges = list(cost = c(0.1, 1, 5, 10)))

# Select the best cost parameter
linear_svm$best.parameters #  cost 5

#Fit the model agian with the best cost
linear_svm <- svm(yearly_return_rate ~ return_rate+first_return+season+
                    introduction_season+introduction_time+SO+SR+unique_channel_count+
                    online_percentage+total_returned+total_sold+price+count_size+numeric_size
                  , data=train,kernel = "linear",cost = 5)

## Predict
validation$linear_svm <- predict(linear_svm, validation)

## #Absolute error
abs_err_linear_svm=abs(validation$linear_svm-validation$yearly_return_rate)

# Append the information for the "linear_svm" model
results <- rbind(results, c("linear_svm", mean(abs_err_linear_svm), median(abs_err_linear_svm),
                            sd(abs_err_linear_svm), IQR(abs_err_linear_svm), range(abs_err_linear_svm)))
View(results)



### Model 2 : polynomial kernel model
# set the range of cost is ( 0.1, 1, 5), the range of degree is (2,3,4)
set.seed(123)
polynomial_svm <- tune(svm, yearly_return_rate ~ return_rate+first_return+season+
                         introduction_season+introduction_time+SO+SR+unique_channel_count+
                         online_percentage+total_returned+total_sold+price+count_size+numeric_size
                       , data = train, kernel = "polynomial", ranges = list(cost = c( 0.1, 1, 5), degree=c(2,3,4)))

# Select the best parameters
polynomial_svm$best.parameters #cost 5 ,degree=2

#Fit the model agian with the best parameters
polynomial_svm <- svm( yearly_return_rate ~ return_rate+first_return+season+
                         introduction_season+introduction_time+SO+SR+unique_channel_count+
                         online_percentage+total_returned+total_sold+price+count_size+numeric_size
                       , data = train, kernel = "polynomial", cost = 5, degree = 2)


## Predict
validation$polynomial_svm <- predict(polynomial_svm, validation)

## #Absolute error
abs_err_polynomial_svm=abs(validation$polynomial_svm-validation$yearly_return_rate)

# Append the information for the "polynomial_svm" model
results <- rbind(results, c("polynomial_svm", mean(abs_err_polynomial_svm), median(abs_err_polynomial_svm),
                            sd(abs_err_polynomial_svm), IQR(abs_err_polynomial_svm), range(abs_err_polynomial_svm)))
View(results)



### Model 3 : radial kernel model
# set the range of cost is ( 0.1, 1, 5), the range of degree is (0.5,1,2)
set.seed(123)
radial_svm <- tune(svm, yearly_return_rate ~ return_rate+first_return+season+
                     introduction_season+introduction_time+SO+SR+unique_channel_count+
                     online_percentage+total_returned+total_sold+price+count_size+numeric_size
                   , data = train, kernel = "radial", ranges = list(cost = c( 0.1, 1, 5), gamma=c(0.5,1,2)))

# Select the best parameters
radial_svm$best.parameters #cost 1 ,gamma=0.5

#Fit the model agian with the best parameters
radial_svm <- svm( yearly_return_rate ~ return_rate+first_return+season+
                     introduction_season+introduction_time+SO+SR+unique_channel_count+
                     online_percentage+total_returned+total_sold+price+count_size+numeric_size
                   , data = train, kernel = "radial", cost = 1, gamma = 0.5)


## Predict
validation$radial_svm <- predict(radial_svm, validation)

## Absolute error
abs_err_radial_svm=abs(validation$radial_svm-validation$yearly_return_rate)

# Append the information for the "radial_svm" model
results <- rbind(results, c("radial_svm", mean(abs_err_radial_svm), median(abs_err_radial_svm),
                            sd(abs_err_radial_svm), IQR(abs_err_radial_svm), range(abs_err_radial_svm)))
View(results)


### write to csv
write.csv(results, "results.csv", row.names = FALSE)
