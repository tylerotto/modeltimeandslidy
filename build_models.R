data <- Sample_Superstore %>% 
  janitor::clean_names() %>% 
  filter(segment == select_segment) %>% 
  mutate(dteday = floor_date(ymd(order_date), "month")) %>% 
  group_by(dteday) %>% 
  summarise(units = sum(quantity))


# TRAIN / TEST SPLITS ----

splits <- time_series_split(
  data,
  assess     = assess_time,
  cumulative = TRUE
)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(dteday, units)

# FORECAST ----

# * AUTO ARIMA ----
model_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(units ~ dteday, training(splits))

model_arima

# Model: arima_boost ----
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(units ~ dteday + as.numeric(dteday) + factor(month(dteday, label = TRUE), ordered = F),
      data = training(splits))  


# Multivariate Adaptive Regression Spline model

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth") 

recipe_spec <- recipe(units ~ dteday, data = training(splits)) %>%
  step_date(dteday, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(dteday)) %>%
  step_normalize(date_num) %>%
  step_rm(dteday)

wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))


# * Prophet ----
# library(prophet)
# model_prophet <- prophet_reg(
#   seasonality_yearly = TRUE
# ) %>%
#   set_engine("prophet") %>%
#   fit(units ~ dteday, training(splits))
# 
# model_prophet

# Linear Model ----
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(units ~ as.numeric(dteday) + factor(month(dteday, label = TRUE), ordered = FALSE),
      data = training(splits))

# * Machine Learning - GLM ----
model_glmnet <- linear_reg(penalty = 0.01) %>%
  set_engine("glmnet") %>%
  fit(units ~ wday(dteday, label = TRUE)
      + month(dteday, label = TRUE)
      + as.numeric(dteday),
      training(splits)
  )

model_glmnet

#AUTO ETS Model
model_ets <- exp_smoothing()%>%
  set_engine("ets") %>%
  fit(units ~ dteday,training(splits))


# MODELTIME COMPARE ----

# * Modeltime Table ----
model_tbl <- modeltime_table(
  # model_arima,
  model_fit_arima_boosted,
  model_fit_lm,
  # model_prophet,
  model_ets,
  wflw_fit_mars,
  model_glmnet
)

# * Calibrate ----
calib_tbl <- model_tbl %>%
  modeltime_calibrate(testing(splits))


accuracy_table <-  calib_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = F
  )

#choose final model

final_test_model <- calib_tbl %>%
  modeltime_accuracy() %>% 
  mutate_if(is.numeric, funs(rank(.))) %>% 
  select(-rsq) %>%
  rowwise() %>% 
  mutate(ensemble_score = sum(c_across(mae:rmse))) %>% 
  ungroup() %>% 
  filter(ensemble_score == min(ensemble_score)) %>% 
  pull(.model_id)

accuracy_table <-  calib_tbl %>%
  modeltime_accuracy() %>%
  select(-rsq) %>%
  left_join(calib_tbl %>%
              modeltime_accuracy() %>% 
              mutate_if(is.numeric, funs(rank(.))) %>% 
              select(-rsq) %>%
              rowwise() %>% 
              mutate(ensemble_score = sum(c_across(mae:rmse))) %>% 
              select(.model_id, ensemble_score)
  ) %>% 
  table_modeltime_accuracy(
    .interactive = F
  )


final_test_model_text <- calib_tbl %>%
  modeltime_accuracy() %>% 
  mutate_if(is.numeric, funs(rank(.))) %>% 
  select(-rsq) %>%
  rowwise() %>% 
  mutate(total = sum(c_across(where(is.numeric)))) %>% 
  ungroup() %>% 
  filter(total == min(total)) %>% 
  pull(.model_desc)  


# * Accuracy ----
calib_tbl %>% modeltime_accuracy()

# * Test Set Visualization ----
test_set_plot <-  calib_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data
  ) %>%
  plot_modeltime_forecast(.plotly_slider = T,
                          # .interactive = FALSE,
                          .title = paste0("Time Series Test Set Visualization: ", select_segment)
  )

test_set_plot

# * Forecast Future ----
future_forecast_tbl <- calib_tbl %>%
  modeltime_refit(data) %>%
  modeltime_forecast(
    h           = forecasted_months,
    actual_data = data
  )

final_model_list <- future_forecast_tbl %>% 
  distinct(.model_id,.model_desc)

final_model <- final_test_model %>%
  as_tibble() %>% 
  rename(.model_id  = value) %>% 
  left_join(final_model_list) %>% 
  pull(.model_id)

final_model_text <- final_test_model %>%
  as_tibble() %>% 
  rename(.model_id  = value) %>% 
  left_join(final_model_list) %>% 
  pull(.model_desc)

forecast_plot <- future_forecast_tbl %>%
  # bind_rows(TAB_COMBINED_FORECAST %>% 
  #             mutate(.model_id = max(future_forecast_tbl$.model_id, na.rm=T) + 1)) %>%
  mutate( .model_id = as.factor(.model_id)) %>% 
  plot_modeltime_forecast(.plotly_slider = T,
                          # .interactive = FALSE,
                          .title = paste0("Time Series Forecast: ", select_segment, ' Final Model: ',final_model_text)
  )

forecast_plot



forecast_compare_tbl <- future_forecast_tbl %>%
  mutate( .model_id = as.factor(.model_id)) %>%
  # filter(.model_desc == final_model_text) %>%
  mutate(sales_year = year(.index), sales_month =  month(.index, label = T)) %>%
  filter(sales_year == 2018) %>%
  select(.model_desc, sales_month,sales_year,.value) %>%
  pivot_wider(names_from = c('sales_year','.model_desc'), values_from = .value)

forecast_compare_tbl2 <- forecast_compare_tbl %>%
  # select(tidyselect::starts_with("2018")) %>%
  janitor::adorn_totals() %>%
  as_tibble() %>% 
  mutate_if(is.numeric, ~comma(as.integer(.)))


names(forecast_compare_tbl2) = str_sub(names(forecast_compare_tbl2),0,25)
 

forecast_compare_table <- forecast_compare_tbl2 %>% 
  gt() %>% 
  tab_style(
    style = list(
      cell_fill(color = "lightblue")
    ),
    locations = cells_body(
      # columns = vars(V1, V2), # not needed if coloring all columns
      rows = 13)
  )

# 
# 
# 
