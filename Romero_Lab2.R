## --------------------------------------------------------------------------
# Source from lab 1
sourceDir <- "/Users/guillermoromero/Documents/MEDS/eds232_ml/Romero_Lab2.Rmd"
library(knitr)
source(knitr::purl(sourceDir, quiet=TRUE))


## --------------------------------------------------------------------------
# Specify a recipe
poly_pumpkins_recipe <-
  recipe(price ~ package, data = pumpkins_train) %>%
  step_integer(all_predictors(), zero_based = TRUE) %>% 
  step_poly(all_predictors(), degree = 4)


## --------------------------------------------------------------------------
# Create a model specification called poly_spec
poly_spec <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")


## --------------------------------------------------------------------------
# Bundle recipe and model spec into a workflow
poly_wf <- workflow() %>% 
  add_recipe(poly_pumpkins_recipe) %>% 
  add_model(poly_spec)


## --------------------------------------------------------------------------
# Create a model
poly_wf_fit <- fit(poly_wf, data = pumpkins_train)


## --------------------------------------------------------------------------
# Print learned model coefficients
poly_wf_fit


## --------------------------------------------------------------------------
# Make price predictions on test data
poly_results <- poly_wf_fit %>% predict(new_data = pumpkins_test) %>% 
  bind_cols(pumpkins_test %>% select(c(package, price))) %>% 
  relocate(.pred, .after = last_col())

# Print the results
poly_results %>% 
  slice_head(n = 10)


## --------------------------------------------------------------------------
metrics(data = poly_results, truth = price, estimate = .pred)


## --------------------------------------------------------------------------
# Bind encoded package column to the results
poly_results <- poly_results %>% 
  bind_cols(package_encode %>% 
              rename(package_integer = package)) %>% 
  relocate(package_integer, .after = package)


# Print new results data frame
poly_results %>% 
  slice_head(n = 5)


## --------------------------------------------------------------------------
# Make a scatter plot
poly_results %>%
  ggplot(mapping = aes(x = package_integer, y = price)) +
   geom_point(size = 1.6) +
   # Overlay a line of best fit
   geom_line(aes(y = .pred), color = "orange", size = 1.2) +
   xlab("package")


## --------------------------------------------------------------------------
# Make a smoother scatter plot 
poly_results %>%
  ggplot(mapping = aes(x = package_integer, y = price)) +
   geom_point(size = 1.6) +
  geom_smooth(method = lm, formula = y ~ poly(x, degree = 3), color = "midnightblue", size = 1.2, se = FALSE)


## --------------------------------------------------------------------------
cor(baked_pumpkins$day, baked_pumpkins$price)


## --------------------------------------------------------------------------
#Create a recipe
poly_recipe_day <-
  recipe(price ~ day, data = pumpkins_train) %>%
  step_integer(all_predictors(), zero_based = TRUE) %>% 
  step_poly(all_predictors(), degree = 4)


## --------------------------------------------------------------------------
# Create a model specification 
poly_spec_day <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")


## --------------------------------------------------------------------------
# Bundle recipe and model spec into a workflow
poly_wf_day <- workflow() %>% 
  add_recipe(poly_recipe_day) %>% 
  add_model(poly_spec_day)
poly_wf_day


## --------------------------------------------------------------------------
# Create a model by fitting the workflow
poly_fit_day <- fit(poly_wf_day, data = pumpkins_train)


## --------------------------------------------------------------------------
# Make price predictions on test data
poly_results_day <- poly_fit_day %>% predict(new_data = pumpkins_test) %>% 
  bind_cols(pumpkins_test %>% select(c(day, price))) %>% 
  relocate(.pred, .after = last_col())

# Print new results data frame
poly_results_day %>% 
  slice_head(n = 5)


## --------------------------------------------------------------------------
#Evaluate model performance on the test data
metrics(data = poly_results_variety, truth = price, estimate = .pred)


## --------------------------------------------------------------------------
# Create a visualization of model performance

poly_results_day %>%
  ggplot(mapping = aes(x = day, y = price)) +
   geom_point(size = 1.6, colour = 'black', alpha = 0.6,
              stroke = .8,fill = 'dodgerblue',
              shape = 21) +
  geom_smooth(method = lm, formula = y ~ poly(x, degree = 4), color = "midnightblue", size = 1.2, se = FALSE) +
  theme_classic()

