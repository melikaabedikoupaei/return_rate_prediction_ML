# Libraries
library(tidyverse)
library(readxl)
library(recipes)
library(tidyquant)
library(ggrepel)
options(digits = 8)
# Load datasets
train_set <- read.csv("https://github.ugent.be/raw/junsi/ml23-team05/main/csv/train.csv?token=GHSAT0AAAAAAAAAOSXUMF5BS6YL3FSYZPDSZLYKIZQ")
validation_set <- read.csv("https://github.ugent.be/raw/junsi/ml23-team05/main/csv/validation.csv?token=GHSAT0AAAAAAAAAOSXUO3QAN2WIEBDLTINYZLYKJUQ")
test_set <- read.csv("https://github.ugent.be/raw/junsi/ml23-team05/main/csv/test.csv?token=GHSAT0AAAAAAAAAOSXVEXU7XLZ4NQOWOUESZLYKKKQ")
predictions<-read.csv("https://github.ugent.be/raw/junsi/ml23-team05/main/csv/predictions.csv?token=GHSAT0AAAAAAAAAOVDWSTD2WEG7QLSFNQ6AZLYSOWQ",header=T)

# Load sales returns dataset
train_data_init <- train_set  %>% as_tibble()
validation_data_init <- validation_set  %>% as_tibble()
test_data_init <- test_set %>% as_tibble()

# Data preprocessing
recipe_obj <- recipe(~ ., data = train_data_init) %>%
        step_rm(c(pid, offline_return, subsubcategory,brand)) %>%
        step_zv(all_predictors()) %>% 
        step_normalize(all_numeric_predictors()) %>%
        step_discretize(c(SR, all_numeric()), options = list(min_unique = 2)) %>%
        step_dummy(all_nominal(), one_hot = TRUE, naming = partial(dummy_names, sep = "__"))%>% 
        prep()
#Apply the steps to train data
data_transformed_tbl <- train_data_init %>%
        bake(recipe_obj, new_data = .) 

data_transformed_tbl %>% glimpse()

#------ Correlation Analysis ----

# Prepare Correlations
correlation_tbl <- data_transformed_tbl %>%
        cor(y = data_transformed_tbl$yearly_return_rate__bin3) %>%
        as_tibble(rownames = "feature") %>%
        rename(yearly_return_rate__bin3 = V1) %>%
        separate(feature, into = c("feature", "bin"), sep = "__") %>%
        filter(!is.na(yearly_return_rate__bin3),yearly_return_rate__bin3 != Inf ) %>%
        filter(!str_detect(feature, "yearly_ret")) %>%
        arrange(abs(yearly_return_rate__bin3) %>% desc()) %>%
        mutate(feature = as_factor(feature) %>% fct_rev()) %>% 
        mutate(bin = ifelse(is.na(bin), "bin1", bin))

# Visualize Correlations
correlation_tbl %>%
        
        ggplot(aes(yearly_return_rate__bin3, y = feature, text = bin)) +
        
        # Geometries
        geom_vline(xintercept = 0, linetype = 2, color = "red") +
        geom_point(color = "#2c3e50") +
        geom_text_repel(aes(label = bin), size = 3, color = "#2c3e50") +
        
        # Formatting
        expand_limits(x = c(-0.5, 0.5)) +
        theme_tq() +
        labs(title = "Return Rate Analysis",
             subtitle = "Correlations to higher yearly return rate",
             y = "", x = "Correlation to yearly return rate")+
        theme(text = element_text(size = 15, color = "midnightblue")
              )


#---- Interpreting Correlations ----

# What are the bins?
recipe_obj %>% tidy()

bins_tbl <- recipe_obj %>% tidy(4)

print(bins_tbl, n = Inf)

bins_tbl %>% filter(terms == "return_rate")




