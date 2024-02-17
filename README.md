# ml23-team05

### Introduction
Welcome to our project! This is an In-class Kaggle Competition. We are the team 5. 
In this project, we are dealing with product return rate. Product return is a tricky thing in real life. It not only adds extra cost for the retailers but also trigger environmental issue. Here, we are using four weeks of sales and return data to predict individual product yearly return rates.

Dealing with missing values, our aim is to impute as many as possible variables in order to keep the full information from the raw dataset(Details see 01_Data_Cleaning.R). Thanks to this, we are able to keep most available data (5827) to train and to evaluate models. Additionally, we created 13 new features (more details see 02_Feature_Engineering.R). It shows that some created variables have highly importance for the modeling (See the poster.pdf)

Totally, we have trained 12 varies models which are across different assumptions and methods, for example, linear assumption models (Multiple linear regression, Ridge regression and Lasso regression), nonlinear assumption model (GAM) and no assumption (Tree and Deep learning models). In the end, we found that Random Forest model has the best performance. Its validation MAE is 0.0768 and the Private Score in Kaggle competition is 0.08441. More models comparison, see 09_Models_comprision.csv.

In conclusion, we have found a model to predict individual yearly return rate by using 4 weeks data and error rate of this model is acceptable which is within 0.09.

## Files organization
To better organize files in relative folders, we removed our submitted files into the folders on 12/12/2023 (new paths as below)
## Data 
### bronze
- sales_yr1.csv
- sales_yr2.csv
- products.csv
- predictions.csv
### silver
- cleaned_sales_yr1.csv
- cleaned_sales_yr2.csv
- products_cleaned.csv
### gold
- test.csv
- train.csv
- validation.csv
### results
-  results.csv
## doc
- Team05_technical_report.pdf (First submitted on the main branch 10/12/2023)
- Team05_poster.pdf
## src
- 01_Data_Cleaning.R
- 02_Feature_Engineering.R
- 03_Data_Exploration.R
- 04_Linear_Baselines.R
- - Multiple linear regression
- - Ridge regression
- - Lasso regression 
- 05_Generalized_additive_models.R
- 06_Tree-based_models.R
- - Regression decision tree
- - Bagging
- - Random forest
- - Gradient boosting
- 07_Support_vector_regression.R
- - SVR with a linear kernel
- - SVR with a polymonial kernel
- - SVR with a radial kernel
- 08_Neural_networks.R
- - regular grid search
- - racing method
- 10_The_best_model.R
