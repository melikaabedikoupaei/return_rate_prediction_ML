### loading library
library('tidyr')
library('dplyr')
library("moments")
library("MASS")

### load tables
getwd()
# set your directory here
setwd("C:/Users/Asus/Desktop/master/sem3/ML/project")
getwd()
train<-read.csv("train.csv",header=T)
test<-read.csv("test.csv",header=T)
prediction=read.csv("predictions.csv",header=T)
set.seed(123)

### Data Exploration
# show
summary(train)

## character to factor
#create levels
brand_levels=union(unique(train$brand),unique(test$brand))
category_levels=union(unique(train$category),unique(test$category))
subcategory_levels=union(unique(train$subcategory),unique(test$subcategory))
subsubcategory_levels=union(unique(train$subsubcategory),unique(test$subsubcategory))

#convert in train
train$brand=factor(train$brand,levels = brand_levels)
train$category=factor(train$category,levels = category_levels)
train$subcategory=factor(train$subcategory,levels = subcategory_levels)
train$subsubcategory=factor(train$subsubcategory,levels = subsubcategory_levels)

#convert in test
test$brand=factor(test$brand,levels = brand_levels)
test$category=factor(test$category,levels = category_levels)
test$subcategory=factor(test$subcategory,levels = subcategory_levels)
test$subsubcategory=factor(test$subsubcategory,levels = subsubcategory_levels)

#check
summary(train)
class(train$brand)

#histogram
par(mfrow = c(2, 2))
hist(train$return_rate)
hist(train$price)
hist(train$online_percentage)
hist(train$total_sales)

#qqplot
par(mfrow = c(1, 1))
qqnorm(train$return_rate)
qqline(train$return_rate,col="red")

# Perform Jarque-Bera test
jarque.test(train$return_rate)
jarque.test(train$price)
jarque.test(train$online_percentage)
jarque.test(train$total_sales)

#cox_box transformation
# if lambda=0 then ln(y)
# if lambda!=0 then (y*lamda)-1/lambda
bc_result=boxcox(train$return_rate+0.001~1,lambda = seq(-5, 5, 1/10),plotit = TRUE)
bc_result <- data.frame(bc_result$x,bc_result$y)

lambda <- bc_result[which(bc_result$bc_result.y == max(bc_result$bc_result.y)),1]
lambda

train$bc_return_rate=(train$return_rate^lambda-1)/lambda
test$bc_return_rate=(test$return_rate^lambda-1)/lambda

par(mfrow = c(1, 2))
hist(train$bc_return_rate)
hist(train$return_rate)

#test linear
colnames(train)
m1=lm(return_rate~online_percentage+season+category+total_sales+brand,data=train)
summary(m1)
m2=lm(bc_return_rate~online_percentage+season+category+total_sales+brand,data=train)
summary(m2)

##correlation test
#online_percentage
par(mfrow = c(1, 1))
plot(train$online_percentage,train$return_rate)
cor(train$online_percentage,train$return_rate)
cor.test(train$online_percentage,train$return_rate,methods="spearman")

#correlation test
par(mfrow = c(1, 1))
plot(train$price,train$return_rate)
cor(train$price,train$return_rate)
cor.test(train$price,train$return_rate,methods="spearman")
#we can see correlation between them

#box plot
par(mfrow = c(1, 1))

#tapply for mean and median
tapply(train$return_rate,train$brand,median)

#season
tapply(train$return_rate,train$season,median)
tapply(train$return_rate,train$season,mean)

#numeric_size
tapply(train$return_rate,train$numeric_size,median)
tapply(train$return_rate,train$numeric_size,mean)

#subcategory
tapply(train$return_rate,train$subcategory,median)
tapply(train$return_rate,train$subcategory,mean)

##test for relationFor Categorical vs Numerical Variables:
#Comparing means: need normality(t-test=2, ANOVA>2)
#do not need normality ( Mann-Whitney test=2, Kruskal-Wallis test>2)

# Run Kruskal-Wallis test
#If the p-value is less than your chosen significance level (e.g., 0.05),
#there are significant differences between at least two groups.
#more than 2
kruskal.test( return_rate~ brand, data = train)
kruskal.test( return_rate~ category, data = train)
kruskal.test( return_rate~ subsubcategory, data = train)

# two group
wilcox.test(return_rate~ season, data = train ,paird=FALSE)
wilcox.test(return_rate~ numeric_size, data = train ,paird=FALSE)


###outlier

# consider return rate above 1 as 1
train$return_rate <- ifelse(train$return_rate>1,1,train$return_rate)

# Apply the function to your data
sum(detect_outliers_tukey(train$return_rate))

#MAD_median rule abs(x-median)/MAD >2.5
sum(abs(train$return_rate-median(train$return_rate))/mad(train$return_rate)>2.5)
train[abs(train$return_rate-median(train$return_rate))/mad(train$return_rate)>2.5,]
  
write.csv(train, "train.csv", row.names=FALSE)
write.csv(test, "test.csv", row.names=FALSE)
