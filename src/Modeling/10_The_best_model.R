
# The best model

# according to the validation MAE, we select the best model RandomForest model

# Part 1: Data preparation

### import packages
library('tidyr')
library('dplyr')
#library("moments")
library("multcomp") 
library("glmnet")  
library(tree)             #Lasso and Elastic-Net Regularized GLM
library("rpart")          #Classification and Regression Trees 
#library("rpart.plot")     #Plot Decision Tree
library("randomForest")   #Random Forests for Classification and Regression 
library("gbm")


# load tables
train=read.csv("Data/Gold/train.csv",header=T,fileEncoding = "UTF-8")
validation=read.csv("Data/Gold/validation.csv",header=T,fileEncoding = "UTF-8")
results=read.csv("Data/Gold/Results/results.csv",header=T,fileEncoding = "UTF-8")
test=read.csv("Data/Gold/test.csv",header=T,fileEncoding = "UTF-8")
predictions=read.csv("Data/Bronze/predictions.csv",header=T,fileEncoding = "UTF-8")


# Show the train table
head(train)
length(colnames(train))

head(validation)
validation <- subset(validation, select = c(-tree,-bagging,-rf,-gbm))
length(colnames(validation))

head(test)
colnames(test)

# Encode season variables
train$season <-ifelse(train$season=="Winter",1,0)
test$season <-ifelse(test$season=="Winter",1,0)
validation$season <-ifelse(validation$season=="Winter",1,0)

# merge the train and validation set
train <- rbind(train, validation)
nrow(train)

#convert some variables into factor
# training set
train$numeric_size <- as.factor(train$numeric_size)
train$brand <- as.factor(train$brand)
train$season <- as.factor(train$season)
train$category  <- factor(train$category,level=union(unique(test$category),unique(train$category)))
train$subcategory <- as.factor(train$subcategory)
train$subsubcategory <- as.factor(train$subsubcategory)
train$SO <- as.factor(train$SO)


# test set
test$numeric_size <- as.factor(test$numeric_size)
test$brand <- as.factor(test$brand)
test$season <- as.factor(test$season)
test$category  <- factor(test$category,level=union(unique(test$category),unique(train$category)))
test$subcategory <- as.factor(test$subcategory)
test$subsubcategory <- as.factor(test$subsubcategory)
test$SO <- as.factor(test$SO)

#Fit the model agian with the best parameters
# train set 
X <- train[, c("season","category","subcategory","numeric_size","count_size",
               "price","total_sold","total_returned","return_rate","online_percentage",
               "unique_channel_count","SO","introduction_time","introduction_season","first_return")]
Y <- train$yearly_return_rate

# test set 
test_x <- test[, c("season","category","subcategory","numeric_size","count_size",
               "price","total_sold","total_returned","return_rate","online_percentage",
               "unique_channel_count","SO","introduction_time","introduction_season","first_return")]

# fit the model
set.seed(1234)
fit<- randomForest(X, Y,ntree = 68, nodesize =19, mtry=5, importance = TRUE)
fit

# get the variable importance
importance(fit)
varImpPlot(fit)

# predict
test$preds  <- predict(fit, test_x)
selected_columns <- test[, c("pid","preds")]

#merge with the prediction csv
df<-merge(predictions,selected_columns,by="pid",all.x = TRUE,all.y = FALSE)
head(df)
colSums(is.na(df))

# Drop 'return_rate'
df <- subset(df, select = -return_rate)

# Rename 'pred' to 'return_rate'
colnames(df)[colnames(df) == "preds"] <- "return_rate"
head(df)

# Fill NA values in 'return_rate' with 0
df$return_rate <- ifelse(is.na(df$return_rate), 0, df$return_rate)
df$return_rate <- ifelse(df$return_rate<0, 0, df$return_rate)
df$return_rate <- ifelse(df$return_rate>1, 1, df$return_rate)

# save file
write.csv(df, "predictions_rf.csv", row.names=FALSE)
