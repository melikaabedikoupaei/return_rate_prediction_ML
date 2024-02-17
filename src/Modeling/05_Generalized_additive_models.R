#### Generalized_additive_models

# Content of this file 

# Part 1: Data preparation
# - Import library and data set

# Part 2: Modeling and Validation MAE test
# - Find out the 8 potential variables by using variables importance plot from the RamdonForest model.
# - Get the best degree of these 8 variables by cv method.
# - Make combinations of 5 features among 8 potential variables. That is (5 8)
# - Fit the model with each combination features to get the best features combination.
# - Fit the model again by using the best 5 features.
# - Predict and Calculate the Validation MAE


#### Part 1: Data preparation

### import packages
library(leaps)
library(psych)
library(ISLR2)
library(dplyr)
library(glmnet)
library(gam)
library(splines)
library(boot)
library(randomForest)
library(mgcv)

### load tables
train=read.csv("Data/Gold/train.csv",header=T,fileEncoding = "UTF-8")
validation=read.csv("Data/Gold/validation.csv",header=T,fileEncoding = "UTF-8")
results=read.csv("Data/Results/results.csv",header=T,fileEncoding = "UTF-8")


#### Part 2: Modeling and Validation MAE test
### Use RandomForest method to find out the top 5 important variables

# Set the x, y variables
X <- train[, c("season","category","subcategory","numeric_size","count_size",
               "price","total_sold","total_returned","return_rate","online_percentage",
               "unique_channel_count","SO","introduction_time","introduction_season","first_return")]
Y <- train$yearly_return_rate

# Use randomForest to find out the top 5 importance variables
rf<- randomForest(X, Y,
                  ntree = 700,
                  nodesize = 50,
                  mtry=5, importance = TRUE)

# Check the importance
importance(rf)

# Plot variable importance
varImpPlot(rf, main = "Variable Importance Plot")

### based on importance we will choose 8 possible features
#count_size/
#return_rate/
#introduction_time/
#introduction_season/
#online_percentage/
#total_returned/
#total_sold/
#price

# Set the range of degrees of freedom
df_range <- seq(2, 20, by = 1)

# Create an empty data frame to store selected features
result_df <- data.frame(Feature = character(), DF = numeric(), stringsAsFactors = FALSE)

# Loop through each feature
for (feature in c("count_size", "return_rate", "introduction_time", "introduction_season", 
                  "online_percentage", "total_returned", "total_sold", "price")) {
        
        # Set the x, y variables
        y <- train$yearly_return_rate
        x <- train[[feature]]
        
        ## Initialize an empty list to store results
        fits_list <- list()
        
        # Loop through each degree of freedom
        df_range <- seq(2, 20, by = 1)
        
        for (df_value in df_range) {
                # Fit smoothing spline
                fit.s <- smooth.spline(x, y, df = df_value, cv = TRUE)
                
                # Save the result in the list
                fits_list[[as.character(df_value)]] <- fit.s
        }
        
        # Get the optimal degrees of freedom
        df_optimal <- round(fit.s$df,0)
        
        # Save results to the data frame
        result_df <- rbind(result_df, data.frame(Feature = feature, DF = df_optimal))
}
round(fit.s$df,0)

# get the resulting data frame
result_df


# Use these potential features to fit gam model with only five features
# Get the names of features
all_features <- c("count_size", "return_rate", "introduction_time", 
                  "introduction_season", "online_percentage", 
                  "total_returned", "total_sold", "price")

# Create a data frame with features and relative degrees of freedom
result_df <- data.frame(
  Feature = c("count_size", "return_rate", "introduction_time", "introduction_season",
              "online_percentage", "total_returned", "total_sold", "price"),
  DF = c(2, 3, 2, 2, 2, 2, 2, 6)
)

# get all combinations with 5 features
selected_combinations <- combn(all_features, 5, simplify = TRUE)

# Create an empty data frame to store results
MAE_result <- data.frame(Combination = character(), MAE = numeric(), stringsAsFactors = FALSE)

# Iterate through each combination
 for (i in 1:ncol(selected_combinations)) {
    
       # Extract degrees of freedom for the selected features
       selected_dfs <- result_df[result_df$Feature %in% selected_combinations[, i], ]
       
       # Construct the formula based on the current combination
       formula_str <- paste("yearly_return_rate ~", 
                     paste(sapply(seq_along(selected_dfs$Feature), 
                     function(index) paste("s(", selected_dfs$Feature[index], ",", selected_dfs$DF[index], ")", sep = "")), collapse = "+"))
  
       tryCatch({
        # Fit GAM model
          gam_model <- gam(as.formula(formula_str), data = train)
   
        # Predict on validation set
          preds <- predict(gam_model, newdata = validation)
 
        # Calculate MAE
          mae <- mean(abs(validation$yearly_return_rate - preds), na.rm = TRUE)
          
        # Save results to the data frame
          MAE_result <- rbind(MAE_result, data.frame(Combination = formula_str, MAE = mae))
          }, error = function(e) {
            cat("Error in formula:", formula_str, "\n")
          })
}


# show the result
View(MAE_result)

# get the best 5 predictors
MAE_result[which.min(MAE_result$MAE),1]

# fit the model with best 5 predictors 
gam <- gam(yearly_return_rate ~ 
             s(return_rate,3) + s(introduction_time,2) + s(online_percentage,2) + s(total_sold,2) + s(price,6)
             ,data=train)

# predict
validation$gam<- predict(gam, validation)

#Absolute error mean, median, sd, max, min
abs_err_gam <- (abs(validation$gam - validation$yearly_return_rate))

# Append the information for the "gbm" model
results <- rbind(results, c("gam", mean(abs_err_gam), median(abs_err_gam), sd(abs_err_gam), 
                            IQR(abs_err_gam), range(abs_err_gam)))
# Show
View(results)

### write to csv
write.csv(results, "results.csv", row.names = FALSE)

