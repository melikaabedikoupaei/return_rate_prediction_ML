## Features defination
# feature name with * in the end represents this feature was created. 
- 01. pid : Product ID, a unique identifier assigned to each product.

- 02. brand: The brand or manufacturer of the product.

- 03. season: The season to which the product belongs, indicating the time of the year it is associated with (summer, winter).

- 04. category: The general category to which the product belongs (e.g., clothing, electronics, accessories).

- 05. subcategory: A more specific subcategory within the general category, providing additional detail about the product.

- 06. subsubcategory: A further refinement of the product's categorization, providing even more specific information about the product.

- 07. numeric_size*: The numeric representation of the product size type.is it based on number then this col would be 1 or based on s,m,l then it would be 0

- 08. count_size*: The variety of sizes available for the product.

- 09. price: The selling price of the product.

- 10. total_sales: Total number of units sold for the product.

- 11. total_return: Total number of units returned for the product.

- 12. offline_sales*: Number of units sold through offline channels.

- 13. offline_return*: Number of units returned from offline sales.

- 14. online_sales*: Number of units sold through online channels.

- 15. online_return*: Number of units returned from online sales.

- 16. online_percentage*: Percentage of total sales that occurred online. Calculated as (online_sales / total_sales) * 100.

- 17. unique_channel_count* :	Count of unique channels through which the product was sold.

- 18. SR*: Indicating the relationship between sales and returns. From 0 to 5 represents different relationships.
- - 0 means  no sales and no returned
- - 1 means no sales but have returned
- - 2 means the number of sales is less than the return
- - 3 means the number of sales is equal to the return
- - 4 means the number of sales is more than the return
- - 5 only sales and no return

- 19. SO*: Stockout,
- - 0 product completely sale  in store
- - 1 completely  online sales 
- - 2 both channels

- 20. introduction_time*:The percentage of the time that first appear during the time period.

- 21. introduction_season*: The season that the product is introduced,
- - 1 for January to March
- - 2 for April to June
- - 3 for July to September
- - 4 for October to December

- 22. first_return*：The percentage of the first return time occurs during the time period.

- 23. return_rate: Return_ rate = (-total_return / total_sales) for the first 4 weeks of the year

- 24. yearly_return_rate: The return rate during the year 2021
