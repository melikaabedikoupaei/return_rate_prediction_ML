#Author:: Victor Owuor
#       Fire up libraries
library(tidymodels)
tidymodels_prefer()
library(tidyverse)
library(finetune)
options(digits = 8)
library(ggthemes)
library(zoo)
library(fastshap)
library(ggrepel)
library(RColorBrewer)
library(hrbrthemes)
library(scales) 
#load data
train_set <- read.csv("https://github.ugent.be/raw/junsi/ml23-team05/main/Data/Gold/train.csv?token=GHSAT0AAAAAAAAAOSXUVC7T6ABEPSEKCPVUZMEHYGA")
sales_year1 <- read.csv("https://github.ugent.be/raw/junsi/ml23-team05/main/Data/Silver/cleaned_sales_yr1.csv?token=GHSAT0AAAAAAAAAOSXUOTRXJKG6WPWYIXVGZMFRNQA")
sales_year2 <- read.csv("https://github.ugent.be/raw/junsi/ml23-team05/main/Data/Silver/cleaned_sales_yr2.csv?token=GHSAT0AAAAAAAAAOSXUOHAISUTOMQGT7JTWZMFROHQ")
products <-  read.csv("https://github.ugent.be/raw/junsi/ml23-team05/main/Data/Silver/products_cleaned.csv?token=GHSAT0AAAAAAAAAOSXV6U6VY6PC4WGDKIPUZMFRPNA")

sales_combined <- sales_year1 %>%
        bind_rows(sales_year2) %>%
        arrange(date_id) %>%
        rename(date = date_id) %>%
        left_join(products, by = "pid") %>%
        mutate(

                sold = ifelse(net_sales_amount > 0, net_sales_amount, 0),
                returned = ifelse(net_sales_amount < 0, net_sales_amount, 0),
                month_year = format(as.Date(date), "%m-%Y", trim = FALSE),
                year = format(as.Date(date), "%Y", trim = FALSE)
        )
#Time series
sales_sold_rolling <- sales_combined%>%
        filter(date > "2020-12-31") %>% 
        mutate(rolling_mean_sold = zoo::rollapply(sold, width = 28, FUN = mean, fill = NA, align = "right"),
               rolling_mean_returned = zoo::rollapply(returned, width = 28, FUN = mean, fill = NA, align = "right"),
               rolling_mean_net = zoo::rollapply(net_sales_amount, width = 28, FUN = mean, fill = NA, align = "right"))
#Visualize time series


sales_sold_rolling %>% 
        select(date, rolling_mean_sold, rolling_mean_returned) %>% 
        ggplot(aes(x = as.Date(date), y = rolling_mean_sold)) + 
        geom_line(color = "midnightblue") +
        geom_line(aes(x = as.Date(date), y = rolling_mean_returned), color = "red") +
        scale_x_date(date_breaks = "1 month", labels = date_format("%b %Y")) +
        labs(x = "Date", y = "Rolling Mean Sold / Return", title = "Rolling Mean Sold Over Time")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))


sales_combined %>% 
        ggplot(aes(x = subcategory, y = abs(returned))) +
        geom_col(fill = "#142BFF", color = "grey80")+
        geom_col(aes(x =subcategory, y = sold), color = "#142BFF")+
        scale_x_discrete()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

sales_combined %>% 
        ggplot(aes(x = subsubcategory)) +
        geom_bar(fill = "#142BFF", color = "grey80")+
        scale_x_discrete()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

sales_combined %>% 
        ggplot(aes(x = season)) +
        geom_bar(fill = "#142BFF", color = "grey80")+
        scale_x_discrete()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

sales_combined %>% 
        ggplot(aes(x = brand)) +
        geom_bar(fill = "#142BFF", color = "grey80")+
        scale_x_discrete()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

sales_combined %>%
        filter(year > 2020) %>% 
        group_by(pid, year) %>% 
        summarise(total_sold = sum(sold),
                  total_returned = sum(returned),
                  yearly_return_rate =-(total_returned)/total_sold) %>% 
        mutate(yearly_return_rate = ifelse(yearly_return_rate > 1, 1, yearly_return_rate)) %>% 
        left_join(products, by = "pid") %>% 
        filter(yearly_return_rate > 0.5) %>% 
        ggplot(aes(x = category, y = total_sold, fill = year))+
        geom_col(position = "dodge")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

sales_combined %>%
        filter(year > 2020) %>% 
        group_by(pid, year) %>% 
        summarise(total_sold = sum(sold),
                  total_returned = sum(returned),
                  yearly_return_rate =-(total_returned)/total_sold) %>% 
        mutate(yearly_return_rate = ifelse(yearly_return_rate > 1, 1, yearly_return_rate)) %>% 
        left_join(products, by = "pid") %>% 
        filter(yearly_return_rate > 0.5) %>% 
        ggplot(aes(x = category, y = abs(total_returned), fill = year))+
        geom_col(position = "dodge")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

sold_returned <- 
sales_combined %>%
        filter(year > 2020) %>% 
        group_by(pid, year) %>% 
        summarise(
                total_sold = sum(sold),
                total_returned = sum(returned),
                yearly_return_rate = -(total_returned) / total_sold
        ) %>% 
        mutate(yearly_return_rate = ifelse(yearly_return_rate > 1, 1, yearly_return_rate)) %>% 
        left_join(products, by = "pid") %>% 
        filter(yearly_return_rate > 0.5)

#visualize sales
sold_returned %>% 
        ggplot(aes(x = category, y = total_sold, fill = factor(year))) +
        geom_col(position = "dodge") +
        facet_grid(~year, scales = "free_x", space = "free_x") +
        labs(y = "Total Sold", fill = "Year", title = "Faceted Bar Chart of Total Sold") +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

#visualize returns
sold_returned %>% 
        ggplot(aes(x = category, y = abs(total_returned), fill = factor(year))) +
        geom_col(position = "dodge") +
        facet_grid(~year, scales = "free_x", space = "free_x") +
        labs(y = "Total Returned", fill = "Year", title = "Faceted Bar Chart of Total Sold") +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Channel return rates
channel_rates <-        
sales_combined %>% 
        select(pid, channel, date) %>% 
        inner_join(train_set, by = "pid") %>% 
        group_by(channel) %>% 
        summarise(return_rate = mean(yearly_return_rate),
                  number_product = n_distinct(pid),
                  .groups = "drop") 

sales_combined %>% 
        select(pid, channel, date) %>% 
        inner_join(train_set, by = "pid") %>% 
        group_by(pid,channel) %>% 
        summarise(return_rate = mean(yearly_return_rate),
                  number_product = n_distinct(pid), 
                  .groups = "drop") %>% 
        pivot_wider(names_from = channel,
                    values_from = return_rate
                    ) %>% 
        mutate(across(everything(), ~ifelse(is.na(.), 0, .)))

# Color palettes
custom_palette <- c(
        "#002D03", "#000F01", "#280C5E", "#371970", "#710940", "#963907",
        "#102558", "#003F3F", "#2B0E8C", "#CE1020", "#0020C1", "#D42A33",
        "#0344E7", "#FF5000", "#8D008D", "#0000CC", "#191919", "#2F6168", 
        "#302A0F", "#162151"
)

#We visualize return rates vs number of products in each channel for poster
channel_rates %>%     
        ggplot(aes(x = number_product, y = return_rate, color = channel, label = channel)) +
        geom_smooth(aes(x = number_product, y = return_rate), color = "#ece3ce", fill = "grey50", size = 0.5, alpha = .50) +
        geom_point() +
        geom_text_repel(size = 9, box.padding = 0.3) +
        scale_color_manual(values = custom_palette) +  
        scale_x_continuous(n.breaks = 5, trans = "log10") +
        labs(
                x = "Number of Products (log scale)",
                y = "Average Return Rate",
                title = "Product Return Rate vs. Count of Products",
                caption = "Source: Train set"
        ) +
        theme_economist() +
        theme(
                legend.position = "none",
                plot.background = element_rect(fill = "#ece3ce", color = "#4f6f52", size = 2),
                plot.title = element_text(size = 24, face = "bold", color = "black", hjust = 0.5),
                plot.subtitle = element_text(size = 12, color = "grey"),
                axis.text = element_text(size = 24, color = "black"),
                axis.line = element_line(color = "#26493b"),
                axis.ticks.x = element_line(color = "#26493b"),
                axis.title = element_text(size = 24, face = "bold", colour = "black"),
                plot.caption = element_text(size = 20, color = "grey50"),
                panel.grid.major = element_line(color = "#748E93", linewidth = 0.25),
                panel.grid.minor = element_blank()
        )
        

        
