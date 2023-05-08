#tidymodels lasso --------
# Create the split (80-20 split)
data_split = data %>% initial_split(prop = 0.8)
data_train = data_split %>% training() %>% as.data.frame()
data_test = data_split %>% testing() %>% as.data.frame()
data_train <- as.data.frame(lapply(data_train, as.numeric))
#data_train$female <- factor(data_train$female)
# Lasso
# Set model
lasso_model = linear_reg(
  penalty = tune(),
  mixture = 1
) %>% set_engine('glmnet')
# Set up recipe
lasso_rec = recipe(h_basrate ~female+., data = data_train) %>%
  #update_role(1, new_role = 'id variable') %>% 
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_poly(h_dvage) %>% 
  step_interact(~female:all_numeric_predictors()) %>% 
  step_lincomb(all_numeric_predictors())
# Set up CV
folds = vfold_cv(data_train, v = 5)
# Workflow
lasso_wf = workflow() %>% 
  add_recipe(lasso_rec) %>% 
  add_model(lasso_model)
# CV
lasso_cv = lasso_wf %>% 
  tune_grid(
    resamples = folds,
    grid = tibble(penalty = c(0, 10^seq(-3, 2, length = 20)) %>% rev())) #metrics = metric_set(rmse,rsq,mae)

lowest_rmse <- lasso_cv %>%
  select_best("rmse")

autoplot(lasso_cv) #mae and rmse similar = good

final_lasso = lasso_wf %>% finalize_workflow(select_best(lasso_cv, metric = 'rmse'))

final_fit_lasso = final_lasso %>% last_fit(data_split)

final_fit_lasso %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20)

assign("lasso coef", final_fit_lasso %>% extract_fit_parsnip() %>% tidy())
`lasso coef` %>% filter(term == 'female')
`lasso coef` %<>% filter(!estimate==0)

final_fit_lasso %>% extract_fit_parsnip() %>% 
  vi(lambda = lowest_rmse$penalty) %>% vip(num_features = 20)

final_fit_lasso %>% extract_fit_parsnip() %>% 
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  )  %>% filter(Importance>0.01) %>% # & str_detect(Variable, '^female')) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)
final_fit_lasso %>% extract_fit_parsnip() %>% 
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  )  %>% filter(Importance>0.001 & str_detect(Variable, '^female')) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)

# Find best models
lasso_cv %>% collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")
