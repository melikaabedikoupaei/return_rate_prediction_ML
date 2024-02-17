#Author: Victor
#Fire up the libraries
library(keras)
library(tidymodels)
tidymodels_prefer()
library(tidyverse)
options(digits = 8)
# Load datasets
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)
# Load datasets
train_set <- read.csv("Data/Gold/train.csv")
validation_set <- read.csv("Data/Gold/validation.csv")
test_set <- read.csv("Data/Gold/test.csv")
predictions<-read.csv("Data/Bronze/predictions.csv",header=T)

# Load sales returns dataset
train_data_init <- train_set  %>% as_tibble()
validation_data_init <- validation_set  %>% as_tibble()
test_data_init <- test_set %>% as_tibble()

# Data preprocessing
data_recipe <-
        recipe(yearly_return_rate ~ ., data = train_data_init) %>% 
        update_role(pid, new_role = "id") %>% 
        step_rm(subsubcategory,brand) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_zv(all_predictors()) %>% 
        step_normalize(all_numeric_predictors()) %>% 
        step_corr(all_numeric_predictors())


#Setting the specifications of mlp model
mlp_spec <-
        mlp(hidden_units = tune(), 
            penalty = tune(),
            epochs = tune()
            ) %>%
        set_engine("keras") %>% 
        set_mode("regression")


mlp_param <- extract_parameter_set_dials(mlp_spec)
mlp_param %>% extract_parameter_dials("hidden_units")
mlp_param %>% extract_parameter_dials("penalty")
mlp_param %>% extract_parameter_dials("epochs")

#regular grid search
mlp_param %>% 
        grid_regular(levels = c(hidden_units = 3, penalty = 2, epochs = 2))
#random grid search
set.seed(1234)
mlp_param %>%
        grid_random(size = 1000) %>% # 'size' is the number of combinations
        summary()
#we can visualize by ploting random grid
library(ggforce)
library(ggthemes)
set.seed(1234)
mlp_param %>%
        # The 'original = FALSE' option keeps penalty inlog10 units
        grid_random(size = 20, original = FALSE) %>%
        ggplot(aes(x = .panel_x, y = .panel_y)) +
        geom_point(color = "midnightblue") +
        geom_blank() +
        facet_matrix(vars(hidden_units, penalty, epochs),
                     layer.diag = 2) +
        labs(title = "Random design with 20 candidates")

#we can visualize by ploting latin_hypercube grid
set.seed(1234)
mlp_param %>%
        grid_latin_hypercube(size = 20, original = FALSE)%>%
        ggplot(aes(x = .panel_x, y = .panel_y)) +
        geom_point(color = "#113347") +
        geom_blank() +
        facet_matrix(vars(hidden_units, penalty, epochs),
                     layer.diag = 2) +
        labs(title = "Latin Hypercube design with 20 candidates")+
        theme(legend.position = "bottom", text = element_text(size = 20))

#Given the dimensions of the data, we can compute performance metrics using 10-fold cross-validation:
set.seed(1234)
train_folds <- vfold_cv(train_data_init)

#Adding specs into current workflow
#combining the above feature engineering recipe with our neural network model specification
mlp_wflow <-
        workflow() %>%
        add_model(mlp_spec) %>%
        add_recipe(data_recipe)

mlp_param <-
        mlp_wflow %>%
        extract_parameter_set_dials() %>%
        update(epochs = epochs(c(50, 200))
               )
doParallel::registerDoParallel()
reg_res <- metric_set(yardstick::mae)
set.seed(1234)
mlp_reg_tune <-
        mlp_wflow %>%
        tune_grid(
                train_folds,
                grid = mlp_param %>% grid_regular(levels = 3),
                metrics = reg_res
        )
mlp_reg_tune
#ploting the tuning result
autoplot(mlp_reg_tune) +
        scale_color_viridis_d(direction = -1) +
        theme(legend.position = "bottom", text = element_text(size = 30))+
        scale_color_brewer(palette = "Dark2")


#selecting the best model
best_reg <- 
        show_best(mlp_reg_tune, metric = "mae", n = Inf) %>% 
        select(-c(.estimator, .metric))
write.csv(best_reg, "/users/lenovo/desktop/best_reg.csv")

reg_tune_mae <- show_best(mlp_reg_tune, metric = "mae") %>% select(-.estimator) %>% 
        select(mean) 
#Plotting all the tuning results for regular grid
mlp_reg_tune %>% 
        select(.metrics) %>% 
        unnest(cols = c(.metrics)) %>% 
        select(.estimate, hidden_units:epochs) %>% 
        rename(MAE = .estimate)%>%
        pivot_longer(hidden_units:epochs,
                     values_to = "value",
                     names_to = "parameter"
        ) %>%
        ggplot(aes(value, MAE, color = parameter)) +
        geom_hex(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~parameter, scales = "free_x") +
        theme(text = element_text(size = 30))
#Plotting all the tuning results for regular grid
best_reg %>% 
        select(mean, hidden_units:epochs) %>% 
        rename(MAE = mean)%>%
        pivot_longer(hidden_units:epochs,
                     values_to = "value",
                     names_to = "parameter"
        ) %>%
        ggplot(aes(value, MAE, color = parameter)) +
        geom_hex(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~parameter, scales = "free_x") +
        theme(text = element_text(size = 30))+
        scale_x_continuous()

dlookr::describe(show_best(mlp_reg_tune, metric = "mae", n = Inf) %>% 
                         select(-.estimator) %>% 
                 mutate(across(where(is.numeric), ~round(., digits = 10)))
                 ) %>% select(1:5,7,10,18,26, -c(2,3))


#we evaluate the same range using a maximum entropy design with 20 candidate values:
doParallel::registerDoParallel()
set.seed(1234)
mlp_sfd_tune <-
        mlp_wflow %>%
        tune_grid(
                train_folds,
                grid = 20,
                # we pass in the parameter object to use the appropriate range:
                param_info = mlp_param,
                metrics = reg_res
        )
mlp_sfd_tune
#plotting
autoplot(mlp_sfd_tune) +
        scale_color_viridis_d(direction = -1) +
        theme(legend.position = "top")

show_best(mlp_sfd_tune, n = Inf) %>% select(-.estimator) %>% unnest(.metric)

#selecting the best model
best_sfd <- 
        show_best(mlp_sfd_tune, metric = "mae", n = Inf) %>% 
        select(-c(.estimator, .metric))
write.csv(best_sfd, "/users/lenovo/desktop/best_sfd.csv")


sfd_tune_mae <- show_best(mlp_sfd_tune, metric = "mae") %>% select(-.estimator) %>% 
        select(mean) 
#Plotting all the tuning results for regular grid
mlp_sfd_tune %>% 
        select(.metrics) %>% 
        unnest(cols = c(.metrics)) %>% 
        select(.estimate, hidden_units:epochs) %>% 
        rename(MAE = .estimate)%>%
        pivot_longer(hidden_units:epochs,
                     values_to = "value",
                     names_to = "parameter"
        ) %>%
        ggplot(aes(value, MAE, color = parameter)) +
        geom_hex(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~parameter, scales = "free_x")+
        theme(text = element_text(size = 30))# +
        #scale_y_continuous(n.breaks = 20)
#Plotting all the tuning results for regular grid
best_sfd %>% 
        select(mean, hidden_units:epochs) %>% 
        rename(MAE = mean)%>%
        pivot_longer(hidden_units:epochs,
                     values_to = "value",
                     names_to = "parameter"
        ) %>%
        ggplot(aes(value, MAE, color = parameter)) +
        geom_hex(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~parameter, scales = "free_x")+
        theme(text = element_text(size = 30)) #+
        #scale_y_continuous(n.breaks = 20)

dlookr::describe(show_best(mlp_sfd_tune, metric = "mae", n = Inf) %>% 
                         select(-.estimator) %>% 
                         mutate(across(where(is.numeric), ~round(., digits = 10)))
) %>% select(1:5,7,10,18,26, -c(2,3))


#Instead of grid search, we can also use the racing method, the output is better than regular grid but not as good as sdf grid, the idea is to improve computational time. Another technique would be parallel processing 
#Best model here will output min mae of 0.0802
library(finetune)
doParallel::registerDoParallel()
set.seed(1234)
conflicted::conflicts_prefer(purrr::set_names)
mlp_sfd_race <-
        mlp_wflow %>%
        tune_race_anova(
                train_folds,
                grid = 20,
                param_info = mlp_param,
                metrics = reg_res,
                control = control_race(verbose_elim = TRUE)
        )

mlp_sfd_race
#plotting
autoplot(mlp_sfd_race) +
        scale_color_viridis_d(direction = -1) +
        theme(legend.position = "top", text = element_text(size = 30))

show_best(mlp_sfd_race, n = Inf) %>% select(-.estimator) %>% unnest(.metric)

#selecting the best model
best_race <- 
        show_best(mlp_sfd_race, metric = "mae", n = Inf) %>% 
        select(-c(.estimator, .metric))
write.csv(best_race, "/users/lenovo/desktop/best_srace.csv")


race_tune_mae <- show_best(mlp_sfd_race, metric = "mae", n = Inf) %>% select(-.estimator) %>% 
        select(mean) 
#Plotting all the tuning results for regular grid
mlp_sfd_race %>% 
        select(.metrics) %>% 
        unnest(cols = c(.metrics)) %>% 
        select(.estimate, hidden_units:epochs) %>% 
        rename(MAE = .estimate)%>%
        pivot_longer(hidden_units:epochs,
                     values_to = "value",
                     names_to = "parameter"
        ) %>%
        ggplot(aes(value, MAE, color = parameter)) +
        geom_hex(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~parameter, scales = "free_x")+
        theme(text = element_text(size = 30))# +
#scale_y_continuous(n.breaks = 20)
#Plotting all the tuning results for regular grid
best_race %>% 
        select(mean, hidden_units:epochs) %>% 
        rename(MAE = mean)%>%
        pivot_longer(hidden_units:epochs,
                     values_to = "value",
                     names_to = "parameter"
        ) %>%
        ggplot(aes(value, MAE, color = parameter)) +
        geom_hex(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~parameter, scales = "free_x")+
        theme(text = element_text(size = 20)) #+
#scale_y_continuous(n.breaks = 20)
dlookr::describe(show_best(mlp_sfd_race, metric = "mae", n = Inf) %>% 
                         select(-.estimator) %>% 
                         mutate(across(where(is.numeric), ~round(., digits = 10)))
) %>% select(1:5,7,10,18,26, -c(2,3))
#print the tuning results
show_best(mlp_sfd_race, n = Inf) %>% 
        select(-c(.metric, .estimator)) %>% 
        mutate(penalty = round(penalty, 10), mean = round(mean, 10)) %>% 
        rename(MAE = mean) %>% 
        write.csv("/users/lenovo/desktop/race2.csv")
show_best(mlp_reg_tune, n = Inf)%>% 
        select(-c(.metric, .estimator)) %>% 
        mutate(penalty = round(penalty, 10), mean = round(mean, 10)) %>% 
        rename(MAE = mean) %>% 
        write.csv("/users/lenovo/desktop/reg2.csv")
show_best(mlp_sfd_tune, n = Inf) %>% 
        select(-c(.metric, .estimator)) %>% 
        mutate(penalty = round(penalty, 10), mean = round(mean, 10)) %>% 
        rename(MAE = mean) %>% 
        write.csv("/users/lenovo/desktop/sfd2.csv")



##  For this model, simplicity corresponds to larger penalty values and/or fewer hidden units.
#reg params
reg_grid_param <-
        tibble(
                epochs = 200,
                hidden_units = 5,
                penalty = 0.00001
        )
#sfd params 
sfd_grid_param <-
        tibble(
                epochs = 111,
                hidden_units = 9,
                penalty = 0.00000104
        )
#race params with the least mse 
race_param <-
        tibble(
                epochs = 198,
                hidden_units = 9,
                penalty = 0.00014837
        )
final_mlp_wflow <-
        mlp_wflow %>%
        finalize_workflow(race_param)
final_mlp_wflow

final_mlp_fit <-
        final_mlp_wflow %>%
        fit(train_data_init)

#predictions
validation_pred <- predict(final_mlp_fit , validation_data_init)
test_pred <- predict(final_mlp_fit , test_data_init)


#Kaggle
kaggle <- 
        predictions %>% 
        left_join(test_set %>%
                          cbind(test_pred)%>%
                          select(pid, .pred), 
                  by = "pid") %>% 
        mutate(across(is.numeric, coalesce, 0)) %>% 
        select(-return_rate) %>% 
        rename(return_rate = .pred)

library(arrow)
write_parquet(kaggle, "mlp_predictions_final.parquet")
write.csv(kaggle, "mlp_predictions_final.csv", row.names = F)



