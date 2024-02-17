### loading library
library('tidyr')
library('dplyr')
library("lubridate")

### load tables
getwd()
# set your directory here
setwd("C:/Users/Asus/Desktop/master/sem3/ML/project")
getwd()
products=read.csv("products.csv",header=T)
predictions=read.csv("predictions.csv",header=T)
sales_yr1=read.csv("sales_yr1.csv",header=T)
sales_yr2=read.csv("sales_yr2.csv",header=T)

###Cleaning process

### Table 1 : product
# get info
head(products)
summary(products)
nrow(products)
length(unique(products$pid))

# 01 Price variable

# Remove currency symbols (e.g., '$') from the 'price' column
products$price <- as.numeric(sub("\\€", "", products$price))
summary(products$price)

# Clean negative price values
# check negative price 
negative_prices <- products[products$price <= 0, ]
length(negative_prices)
sum(is.na(products$price))

# replace negative prices and NA prices with the mean of their category
products <- products %>%
  group_by(category) %>%
  mutate(price = ifelse(price < 0 | is.na(price), mean(price, na.rm = TRUE), price)) %>%
  ungroup()
length(unique(products$pid))

# 02 subcategory variable
# show
head(products)

# Remove text before hyphen in the 'subcategory' column
products$subcategory <- sub(".*-", "", products$subcategory)
head(products)
unique(products$subcategory)

# 03 brand variable

# Clean up the 'brand' column
products$brand <- gsub("\\[brand::\\]", "", products$brand)
head(products$brand)

#unique_brands
unique_brands <- unique(products$brand)
unique_brands

# Check for empty strings in the 'brand' column
has_empty_strings <- any(grepl("^\\s*$", products$brand))
has_empty_strings

# Replace empty strings with NA in the 'brand' column
products$brand[products$brand == ""] <- NA

#unique_brands
unique_brands <- unique(products$brand)
sort(unique_brands)

# check if there are any instances where the same pid has two different brands
filtered_data <- products %>%
  filter(!is.na(brand))

# Find duplicate pids with different brands
duplicate_pids <- filtered_data %>%
  group_by(pid) %>%
  filter(n_distinct(brand) > 1) %>%
  ungroup()

# Print the rows with duplicate pids and different brands
print(duplicate_pids)

# there is no pid with two different brand

#fill NA brand with brand name in other same pid

# Create a new data frame with unique pid and brand combinations
unique_pid_brand_df <- products %>%
  dplyr::select(pid, brand) %>%
  filter(!is.na(brand)) %>%
  distinct()

colnames(unique_pid_brand_df)[colnames(unique_pid_brand_df) == "brand"] <- "brand2"

# merge this df with products
products <- merge(products, unique_pid_brand_df, by = "pid", all = TRUE)
head(products)

#fill NA values in the brand column with values from the brand2 column
products <- products %>%
  mutate(brand = coalesce(brand, brand2)) %>%
  dplyr :: select(-brand2)
nrow(products)

# Count NA values in each column of the 'products' data frame
colSums(is.na(products))

# Fill NA values in the 'brand' column with 'others'
products$brand <- ifelse(is.na(products$brand), "others", products$brand)

# Count NA values in each column of the 'products' data frame
colSums(is.na(products))

#show one sample record
products[products$pid=='b9c74f687ca55f2',]

# 04 sku_size varialbe

# Sort the data frame based on the 'size' column
products <- products[order(products$pid,products$sku_size), ]
head(products)

# Create a new column 'first_row_size' that contains the size of the first row for each pid
products <- products %>%
  group_by(pid) %>%
  mutate(first_row_size = first(sku_size)) %>%
  ungroup()
head(products)

#show one sample record
products[products$pid=='08ce238aa01baba',]
products[products$pid=='fffeafe6012cca8',]

# Count NA values in each column of the 'products' data frame
colSums(is.na(products))
nrow(products)
length(unique(products$pid))

# Fill NAs with 1 in first_row_size
products$first_row_size <- ifelse(is.na(products$first_row_size), 1, products$first_row_size)

# Drop 'sku_size' column 
products <- subset(products, select = -sku_size)
nrow(products)

#change the name of first_row_size
colnames(products)[colnames(products) == "first_row_size"] <- "sku_size"
head(products)

# trim
products$sku_size <- trimws(products$sku_size)

# create size_type col
products <- products %>%
  mutate(numeric_size = ifelse(sku_size %in% c("XS","M", "S","L","XL","XXL","XXXL","XL/XXL","2XL","3XL","4XL"), 0, 1))

# Create a new column 'count_size' based on the number of occurrences of each 'pid'
products <- products %>% group_by(pid) %>% mutate(count_size = n())

# Drop 'sku_size' column 
products <- subset(products, select = -sku_size)
nrow(products)


#show one sample record
products[products$pid=='b9c74f687ca55f2',]


# 05 mean price

# Replace 'price' with the mean of prices for each group
products <- products %>%
  group_by(brand, category, pid, season, subcategory, subsubcategory) %>%
  mutate(price = round(mean(price, na.rm = TRUE),3))

# Remove duplicates
products <- distinct(products)
head(products)
nrow(products)

#show one sample record
products[products$pid=='b9c74f687ca55f2',]

nrow(products)
#should be 13027
length(unique(products$pid))

# search for duplicated
duplicated_pid_rows <- products[duplicated(products$pid) | duplicated(products$pid, fromLast = TRUE), ]
duplicated_pid_rows

#convert to factor
products$brand=factor(products$brand)
products$category=factor(products$category)
products$subcategory=factor(products$subcategory)
products$subsubcategory=factor(products$subsubcategory)

# create cleaned_products.csv
write.csv(products, "cleaned_products.csv", row.names = FALSE)



### Table 2 sales_yr1
# get info 
head(sales_yr1)
tail(sales_yr1)
summary(sales_yr1)
nrow(sales_yr1)

#look at net_amount for data that date_id is not na
result <- sales_yr1 %>%
  filter(is.na(date_id)) %>%
  group_by(pid) %>%
  summarise(sum_positive = sum(ifelse(net_sales_amount > 0, net_sales_amount, 0)),
            sum_negative = sum(ifelse(net_sales_amount < 0, net_sales_amount, 0)))
result["sum"]=result$sum_positive+result$sum_negative
result

#sort the sum
result <- result[order(result$sum), ]
result

# descending order
result <- result[order(-result$sum), ]
result


# 01 channel variable

# Clean up the 'channel' column
sales_yr1$channel <- gsub("CH//", "", sales_yr1$channel)
head(sales_yr1)
tail(sales_yr1)

# Count NA values in each column of the 'sales_yr1' data frame
colSums(is.na(sales_yr1))

# Replace empty strings with NA in the 'channel' column
sales_yr1$channel[sales_yr1$channel == ""] <- NA
head(sales_yr1)

# Count NA values in each column of the 'sales_yr1' data frame
colSums(is.na(sales_yr1))

#show one sample record
sales_yr1[sales_yr1$pid=='b9c74f687ca55f2',]

#get mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode for Channels within each pid group
mode_per_pid <- sales_yr1 %>% 
  
  group_by(pid) %>% 
  summarize(ChannelMode = getmode(channel[!is.na(channel)]))
nrow(sales_yr1)

# Join the mode values back to the original sales_yr1
sales_yr1 <- left_join(sales_yr1, mode_per_pid, by = "pid")

# Replace NA in Channels with the mode of its pid
sales_yr1$channel <- 
  ifelse(is.na(sales_yr1$channel), sales_yr1$ChannelMode, sales_yr1$channel)
nrow(sales_yr1)

# Optionally, remove the ChannelMode column if not needed
sales_yr1$ChannelMode <- NULL
names(sales_yr1)

#show one sample record
sales_yr1[sales_yr1$pid=='b9c74f687ca55f2',]

# Count NA values in each column of the 'sales_yr1' data frame
colSums(is.na(sales_yr1))

# replace the na with mode of col
sales_yr1$channel[is.na(sales_yr1$channel)] <-getmode(sales_yr1$channel)

# Count NA values in each column of the 'sales_yr1' data frame
colSums(is.na(sales_yr1))

#cross table channel
table(sales_yr1$channel)

# Replace "amazon.com" with "online" in the 'channel' column  
sales_yr1$channel <- ifelse(sales_yr1$channel == "amazon.com", "online", sales_yr1$channel)

# Replace Online with "online" in the 'channel' column
sales_yr1$channel <- ifelse(sales_yr1$channel == "Online", "online", sales_yr1$channel)
table(sales_yr1$channel)
nrow(sales_yr1)

# 02 date_id variable
# get info
summary(sales_yr1$date_id)

# transform the date formation
sales_yr1$date_id <- ymd(sales_yr1$date_id)

# type od date_id
class(sales_yr1$date_id)
summary(sales_yr1$date_id)

# fill NA 'sales_yr1' with first date of dataset
sales_yr1$date_id[is.na(sales_yr1$date_id)] <- as.Date("2018-01-12")

#create year and month col
sales_yr1$year<- year(sales_yr1$date_id)
unique(sales_yr1$year)
sales_yr1$month <- month(sales_yr1$date_id)
unique(sales_yr1$month)
nrow(sales_yr1)

#keep 2021 in dataset
sales_yr1 <- sales_yr1[sales_yr1$year==2021,]
nrow(sales_yr1)

# Count NA values in each column of the 'sales_yr1' data frame
colSums(is.na(sales_yr1))


# 03 net_sales_amount variable

#show one sample record
sales_yr1[sales_yr1$pid=='b9c74f687ca55f2',]

# Calculate the mean for net_sales_amount within each pid group
mean_per_pid <- sales_yr1 %>% 
  group_by(pid) %>% 
  summarize(AvgNetSales = mean(net_sales_amount, na.rm = TRUE), .groups = 'drop')

# round up AvgNetSales
mean_per_pid$AvgNetSales <- ceiling(mean_per_pid$AvgNetSales)

# Join the mean values back to the original data
sales_yr1 <- left_join(sales_yr1, mean_per_pid, by = "pid")

# Replace NA in net_sales_amount with the mean of its pid
sales_yr1$net_sales_amount <- 
                    ifelse(is.na(sales_yr1$net_sales_amount), 
                           sales_yr1$AvgNetSales, sales_yr1$net_sales_amount)

# Optionally, remove the AvgNetSales column if not needed
sales_yr1$AvgNetSales <- NULL
names(sales_yr1)

#show one sample record
sales_yr1[sales_yr1$pid=='b9c74f687ca55f2',]

# check the na values in each column
colSums(is.na(sales_yr1))

#drop na
sales_yr1=drop_na(sales_yr1)

# 04 outliers check
# show
head(sales_yr1[order(sales_yr1$net_sales_amount,decreasing  = TRUE),])
head(sales_yr1[order(sales_yr1$net_sales_amount,decreasing  = FALSE),])

# Use Tukey method to find out outliers
#Tukey method:x> q(0.25)+1.5*IQR/x< q(0.75)-1.5*IQR

# plot
hist(sales_yr1$net_sales_amount)
boxplot(sales_yr1$net_sales_amount)

# detect_outliers function
detect_outliers_tukey <- function(x, k = 1.5) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  
  lower_bound <- q1 - k * iqr
  upper_bound <- q3 + k * iqr
  
  outliers <- x < lower_bound | x > upper_bound
  
  return(outliers)
}

# Apply the function to net_sales_amount varialbe
sum(detect_outliers_tukey(sales_yr1$net_sales_amount))

# plot the resulut
result <- boxplot.stats(sales_yr1$net_sales_amount)

# Extract the minimum and maximum values
min_value <- result$stats[1]  # Minimum value
min_value

max_value <- result$stats[5]  # Maximum value
max_value
head(sales_yr1)

# Apply the function to the whole data set
sum(detect_outliers_tukey(sales_yr1$net_sales_amount))
View(sales_yr1)

# create cleaned_sales_yr1.csv
write.csv(sales_yr1, "cleaned_sales_yr1.csv", row.names = FALSE)

### Table 3 sales_yr2
# get info
head(sales_yr2)
tail(sales_yr2)
summary(sales_yr2)
nrow(sales_yr2)

# 01 channel varialbe
# Clean up the 'channel' column
sales_yr2$channel <- gsub("CH//", "", sales_yr2$channel)
head(sales_yr2)
tail(sales_yr2)

# Count NA values in each column of the 'sales_yr2' data frame
colSums(is.na(sales_yr2))

# Replace empty strings with NA in the 'channel' column
sales_yr2$channel[sales_yr2$channel == ""] <- NA
head(sales_yr2)

# Count NA values in each column of the 'sales_yr2' data frame
colSums(is.na(sales_yr2))

#show one sample record
sales_yr2[sales_yr2$pid=='b9c74f687ca55f2',]

# mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode for Channels within each pid group
mode_per_pid <- sales_yr2 %>% 
  group_by(pid) %>% 
  summarize(ChannelMode = getmode(channel[!is.na(channel)]))
nrow(sales_yr2)

# Join the mode values back to the original sales_yr2
sales_yr2 <- left_join(sales_yr2, mode_per_pid, by = "pid")

# Replace NA in Channels with the mode of its pid
sales_yr2$channel <- 
  ifelse(is.na(sales_yr2$channel), sales_yr2$ChannelMode, sales_yr2$channel)
nrow(sales_yr2)

# Optionally, remove the ChannelMode column if not needed
sales_yr2$ChannelMode <- NULL
names(sales_yr2)

#show one sample record
sales_yr2[sales_yr2$pid=='b9c74f687ca55f2',]

# Count NA values in each column of the 'sales_yr2' data frame
colSums(is.na(sales_yr2))

# replace the na with mode of col
sales_yr2$channel[is.na(sales_yr2$channel)] <-getmode(sales_yr2$channel)

# Count NA values in each column of the 'sales_yr2' data frame
colSums(is.na(sales_yr2))

# Replace "amazon.com" with "online" in the 'channel' column
sales_yr2$channel <- ifelse(sales_yr2$channel == "amazon.com", "online", sales_yr2$channel)

# Replace Online with "online" in the 'channel' column
sales_yr2$channel <- ifelse(sales_yr2$channel == "Online", "online", sales_yr2$channel)

# 02 date_id
# get info
summary(sales_yr2$date_id)

# transform data formate
sales_yr2$date_id <- ymd(sales_yr2$date_id)

# type od date_id
class(sales_yr2$date_id)
summary(sales_yr2)

# fill NA 'sales_yr2' with first date of dataset
sales_yr2$date_id[is.na(sales_yr2$date_id)] <- as.Date("2018-01-12")

#create year and month col
sales_yr2$year<- year(sales_yr2$date_id)
unique(sales_yr2$year)
sales_yr2$month <- month(sales_yr2$date_id)
unique(sales_yr2$month)

# get info
summary(sales_yr2)
table(sales_yr2$year)

# select year 2022 data
sales_yr2=sales_yr2[sales_yr2$year==2022,]
table(sales_yr2$year)
table(sales_yr2$month)

# Count NA values in each column of the 'sales_yr2' data frame
colSums(is.na(sales_yr2))

# 03 net_sales_amount variable
#show one sample record
sales_yr2[sales_yr2$pid=='b9c74f687ca55f2',]

# Calculate the mean for net_sales_amount within each pid group
mean_per_pid <- sales_yr2 %>% 
  group_by(pid) %>% 
  summarize(AvgNetSales = mean(net_sales_amount, na.rm = TRUE), .groups = 'drop')

# round up
mean_per_pid$AvgNetSales <- ceiling(mean_per_pid$AvgNetSales)

# Join the mean values back to the original data
sales_yr2 <- left_join(sales_yr2, mean_per_pid, by = "pid")

# Replace NA in net_sales_amount with the mean of its pid
sales_yr2$net_sales_amount <- 
  ifelse(is.na(sales_yr2$net_sales_amount), 
         sales_yr2$AvgNetSales, sales_yr2$net_sales_amount)

# Optionally, remove the AvgNetSales column if not needed
sales_yr2$AvgNetSales <- NULL
names(sales_yr2)

#show one sample record
sales_yr2[sales_yr2$pid=='b9c74f687ca55f2',]

colSums(is.na(sales_yr2))

#drop na
sales_yr2=drop_na(sales_yr2)

# 04 outliers
# show
head(sales_yr2[order(sales_yr2$net_sales_amount,decreasing  = TRUE),])
head(sales_yr2[order(sales_yr2$net_sales_amount,decreasing  = FALSE),])

# use Tukey method to find out outliers
#Tukey method:x> q(0.25)+1.5*IQR/x< q(0.75)-1.5*IQR

# plot
hist(sales_yr2$net_sales_amount)
boxplot(sales_yr2$net_sales_amount)

# Apply the function to net_sales_amount
sum(detect_outliers_tukey(sales_yr2$net_sales_amount))

# plot the result
result <- boxplot.stats(sales_yr2$net_sales_amount)

# Extract the minimum and maximum values
min_value <- result$stats[1]  # Minimum value
min_value 
max_value <- result$stats[5]  # Maximum value
max_value
head(sales_yr2)

# detect again
sum(detect_outliers_tukey(sales_yr2$net_sales_amount))
nrow(sales_yr2)
head(sales_yr2)

# create cleaned_sales_yr2.csv
write.csv(sales_yr2, "cleaned_sales_yr2.csv", row.names = FALSE)
