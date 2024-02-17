#### Tree Based Models 

# Content of this file 

# Part 1: Data preparation
# - Convert data type

# Part 2: Modeling and Validation MAE test
# - Fit a regression decision tree
# - Fit a bagging tree
# - Fit a random forest tree
# - Fit a gradient boosting


#### Part 1: Data preparation
### Import packages
library('tidyr')
library('dplyr')
library("moments")
library("multcomp") 
library("glmnet")  
library(tree)             #Lasso and Elastic-Net Regularized GLM
library("rpart")          #Classification and Regression Trees 
library("rpart.plot")     #Plot Decision Tree
library("randomForest")   #Random Forests for Classification and Regression 
library("gbm")
library("xgboost")


### load tables
library(here)
train=read.csv("Data/Gold/train.csv",header=T,fileEncoding = "UTF-8")
validation=read.csv("Data/Gold/validation.csv",header=T,fileEncoding = "UTF-8")
results=read.csv("Data/Results/results.csv",header=T,fileEncoding = "UTF-8")


#get info from the data sets
dim(train)
str(train)
View(train)


#convert some variables into factor
# validation set
validation$numeric_size <- as.factor(validation$numeric_size)
validation$brand <- as.factor(validation$brand)
validation$season <- as.factor(validation$season)
validation$category  <- as.factor(validation$category)
validation$subcategory <- as.factor(validation$subcategory)
validation$subsubcategory <- as.factor(validation$subsubcategory)
validation$SO <- as.factor(validation$SO)

# training set
train$numeric_size <- as.factor(train$numeric_size)
train$brand <- as.factor(train$brand)
train$season <- as.factor(train$season)
train$category  <- as.factor(train$category)
train$subcategory <- as.factor(train$subcategory)
train$subsubcategory <- as.factor(train$subsubcategory)
train$SO <- as.factor(train$SO)


#### Part 2: Modeling and Validation MAE test


### Model 1: Regression decision tree

# get the name of the train set
colnames(train)

# calculate the na values in the train set
colSums(is.na(train))

#Decision Tree Model Using All Variables
tree=tree(yearly_return_rate~ season
          +category
          +subcategory
          +numeric_size
          +count_size
          +price+total_sold
          +total_returned
          +return_rate
          +online_percentage
          +unique_channel_count
          +SO
          +introduction_time
          +introduction_season
          +first_return,data = train)

# cross-validation find the best size
set.seed(1)
cv.return_rate <- cv.tree(tree, K = 10)
size.cv <- cv.return_rate$size[which.min(cv.return_rate$dev)]
size.cv    # range of size is 1-7, 7 is the best parameter

# plot 
par(mar=c(1,1,1,1))
plot(cv.return_rate$size, cv.return_rate$dev, type = 'b')

# prune the tree
prune.return_rate <- prune.tree(tree, best = size.cv)

# plot
plot(prune.return_rate)
text(prune.return_rate, pretty = 0)

# predict 
validation$tree <- predict(tree, newdata = validation)

# calculate the absolute value
abs_err_tree <- (abs(validation$tree - validation$yearly_return_rate))

# update to the result table
results <- rbind(results, c("tree", mean(abs_err_tree), median(abs_err_tree), sd(abs_err_tree), IQR(abs_err_tree), range(abs_err_tree)))
View(results)



### Model 2 Bagging 
# set the rang of the parameters
param_grid <- expand.grid(
  terminal_nodes = c(10,20,30),
  trees = c(50, 100,150),
)

# Split the dataset into predictors (X) and the target variable (Y)
# train set 
X <- train[, c("season","category","subcategory","numeric_size","count_size",
               "price","total_sold","total_returned","return_rate","online_percentage",
               "unique_channel_count","SO","introduction_time","introduction_season","first_return")]
Y <- train$yearly_return_rate

# validation set 
X_validation <- validation[, c("season","category","subcategory","numeric_size","count_size",
                               "price","total_sold","total_returned","return_rate","online_percentage",
                               "unique_channel_count","SO","introduction_time","introduction_season","first_return")]
Y_validation <- validation$yearly_return_rate


# Initialize an empty data frame to store the results
bagging_results <- data.frame()

# Loop through the hyperparameter combinations
for (i in 1:nrow(param_grid)) {
  
  # Set the hyperparameters for the current iteration
  terminal_nodes <- param_grid$terminal_nodes[i]
  trees <- param_grid$trees[i]
  
  # Adjust nodesize based on the number of terminal nodes
  if (terminal_nodes > 1) {
    nodesize_val <- max(1, round(length(Y) / terminal_nodes))
  } else {
    nodesize_val <- 1
  }
  
  # Build the bagging model
  model <- randomForest(X, Y,ntree = trees, nodesize = nodesize_val, mtry=15)
  
  # Extract and store the model performance metrics
  metrics <- data.frame(
    Terminal_Nodes = terminal_nodes,
    Trees = trees,
    RMSE = sqrt(mean((predict(model, X) - Y)^2))
  )
  
  bagging_results <- rbind(bagging_results, metrics)
}

# get the results
bagging_results

# Identify the best hyperparameter values based on the criteria
best_params <- bagging_results[which.min(bagging_results$RMSE), c("Terminal_Nodes", "Trees")]
best_params  

# use the best_params to fit the model
set.seed(1234)
bagging <- randomForest(formula = y ~ x, X, Y,ntree = 700, nodesize = 50, mtry=15, importance = TRUE)
bagging


# predict
validation$bagging  <- predict(bagging, validation)

# calculate the absolute value
abs_err_bagging=abs(validation$bagging-validation$yearly_return_rate)

# Append the information for the "bagging" model
results <- rbind(results, c("bagging", mean(abs_err_bagging), median(abs_err_bagging), sd(abs_err_bagging), IQR(abs_err_bagging), range(abs_err_bagging)))
View(results)



###### Model 3 Random Forest
# set the rang of the parameters
param_grid <- expand.grid(
  terminal_nodes = c(18,19,20),
  trees = c(60, 68,100),
  mtry = c(4,5,6)
)

# Initialize an empty data frame to store the results
rf_results <- data.frame()

for (i in 1:nrow(param_grid)) {
  
  # Set the hyperparameters for the current iteration
  terminal_nodes <- param_grid$terminal_nodes[i]
  trees <- param_grid$trees[i]
  mtries <- param_grid$mtry[i]

  # Adjust nodesize based on the number of terminal nodes
  if (terminal_nodes > 1) {
    nodesize_val <- max(1, round(length(Y) / terminal_nodes))
  } else {
    nodesize_val <- 1
  }
  
  # Build the Random Forest model
  model <- randomForest(X, Y,ntree = trees,nodesize = nodesize_val,mtry= mtries)
  
  # Extract and store the model performance metrics
  metrics <- data.frame(
    Terminal_Nodes = terminal_nodes,
    Trees = trees,
    Mtries = mtries,
    RMSE = sqrt(mean((predict(model, X) - Y)^2))
  )
  
  rf_results <- rbind(rf_results, metrics)
}

# get the results
rf_results

# Identify the best hyperparameter values based on your criteria
best_params <- rf_results[which.min(rf_results$RMSE), c("Terminal_Nodes", "Trees","Mtries","RMSE")]
best_params  

# fit the model
set.seed(1234)
rf<- randomForest(X, Y,ntree = 700, nodesize =50, mtry=5, importance = TRUE)
rf

# get the variable importance
importance(rf)
varImpPlot(rf)

# predict
validation$rf  <- predict(rf, validation)

# calculate the absolute value
abs_err_rf=abs(validation$rf-validation$yearly_return_rate)

# Append the information for the "rf" model
results <- rbind(results, c("rf", 
                            mean(abs_err_rf), median(abs_err_rf), sd(abs_err_rf), 
                            IQR(abs_err_rf), range(abs_err_rf)))
View(results)


### Model 4 gradient boosting 

#Create hyper-parameter grid
par_grid <- expand.grid(shrinkage = c(0.01, 0.03, 0.1),
                        interaction_depth = c(2, 4, 5), #the maximum depth of each tree
                        tree = c(300,500,700))

#Grid search (train/validation approach)
for(i in 1:nrow(par_grid)) {
  set.seed(123)
  #train model
  gbm_tune <- gbm(formula = yearly_return_rate~season+category+
                    subcategory+numeric_size+count_size+
                    price+total_sold+total_returned+return_rate+online_percentage+
                    unique_channel_count+SO+introduction_time+introduction_season+first_return, 
                  distribution = "gaussian",
                  data = train,
                  n.trees = par_grid$tree[i],
                  interaction.depth = par_grid$interaction_depth[i],
                  shrinkage = par_grid$shrinkage[i],
                  train.fraction = 0.8,
                  cv.folds = 10,
                  n.cores = NULL, #will use all cores by default
                  verbose = FALSE)  
  #add min training error and trees to grid
  par_grid$optimal_trees[i] <- which.min(gbm_tune$valid.error)
  par_grid$min_RMSE[i]      <- sqrt(min(gbm_tune$valid.error))
}

# show
par_grid

# Identify the best hyperparameter values based on the criteria
best_params <- par_grid[which.min(par_grid$min_RMSE), c("shrinkage", "interaction_depth","tree","optimal_trees","min_RMSE")]
best_params

#fit the Model by best parameters
gbm<- gbm(formula = yearly_return_rate~ season
          +category
          +subcategory
          +numeric_size
          +count_size
          +price+total_sold
          +total_returned
          +return_rate
          +online_percentage
          +unique_channel_count
          +SO
          +introduction_time
          +introduction_season
          +first_return, 
          distribution = "gaussian",
          data = train,
          n.trees = 700,
          interaction.depth = 4,
          shrinkage = 0.03,
          train.fraction = 0.8,
          n.cores = NULL, #will use all cores by default
)  

# predict
validation$gbm <- predict(gbm,n.tree = 700,newdata = validation)

#Absolute error mean, median, sd, max, min
abs_err_gbm <- abs(validation$gbm - validation$yearly_return_rate)  

# Append the information for the "gbm" model
results <- rbind(results, c("gbm",mean(abs_err_gbm),
                            median(abs_err_gbm),
                            sd(abs_err_gbm),
                            IQR(abs_err_gbm),
                            range(abs_err_gbm)))
View(results)

### write to csv
write.csv(results, "results.csv", row.names = FALSE)
