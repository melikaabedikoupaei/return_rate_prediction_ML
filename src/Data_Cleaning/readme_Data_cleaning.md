### Missing values
In the product table:
- NA values in the “price” column are imputed by the mean of price  of their respective category. Negative price is regard as NA values.
- NA values in the 'brand' column are filled with the brand name from the same product ID (pid) which has brand value. NA values cannot be imputed, we filled with 'others'. 
- NA values in the 'sku_size' column, we didn’t impute them. Instead, we changed the size type into numeric and non-numeric size type. (see further in the feature emerging part)

In the sales_yr1 and sales_yr2 tables:
- Calculated the mode for 'channel' within each product and filled NA values with the mode value.

### String manipulation
- Remove Currency Symbol“\\€” from 'Price'.
- removing redundant 'brand' and 'channel' symbol such as [\\[brand::\\], CH//.
- removing redundant category names from 'subcategory' column such as “Formal Attire-Petite” in the subcategory, remove “Formal Attire-”

### Data splitting
- The sales data from  the year 2021 in the sales_yr1 table are used for model training (The big training set). 
- The first 4 weeks sales data from the year 2022 in the sales_yr2 table are used for the product yearly return rate prediction.
- In the big training set, randomly selected 70% of data for the model training, and the remaining data for the model validation.
