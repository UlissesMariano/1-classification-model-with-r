library(readxl)
library(tidyverse) 
library(dlookr) # correlations plot
library(visdat) # Missing data 
library(tidymodels)
library(embed)
library(ranger)  # RANDOM FOREST
library(xgboost) # XGBOOST
library(NeuralNetTools) # NEURAL NETWORK
# library(treesnip) # LIGHT-GBM
library(vip) # variable importance plot
library(caret) # Confusion Matrix
library(data.table)


# 1. PREPROCESSING PART 1 : reading the DATA SET and doing some manipulations   ---------------------------    ----

# Preparing basic data structure for *TRAINING* : **************************************************************
train_h = read_excel("Input/Data Base for Life Cycle Classification Train Set.xlsx", sheet = 1)
train_f = read_excel("Input/Data Base for Life Cycle Classification Train Set.xlsx", sheet = 2)
train_c = read_excel("Input/Data Base for Life Cycle Classification Train Set.xlsx", sheet = 3)

train_h = train_h[,-6]
train_h$Valor[train_h$Valor <= 0] = 0 
train_h$Type_period = "history"
train_f$Type_period = "forecast"
train_f = na.omit(train_f)

hist_forc = bind_rows(x= train_h, y= train_f)
preprocessing_train = merge(hist_forc, train_c, by= c("KEY", "Pais", "Basecode"), all.x = T)
preprocessing_train$Lifecycle[preprocessing_train$Lifecycle == "DESLISTED"] = "DELISTED" 


# Creating the 'encoding' and 'difference' columns: *********
preprocessing_train = preprocessing_train[order(preprocessing_train$Basecode , preprocessing_train$Periodo),] 
for (i in 1:nrow(preprocessing_train)){
  preprocessing_train$encoding[i] = ifelse (preprocessing_train$Valor[i] == 0 , yes = 0, no = 1 )
  preprocessing_train$Diferenca[i] = preprocessing_train$encoding[i] - ifelse(i == 1, 
                                                                  yes = 1, 
                                                                  no = preprocessing_train$encoding[i-1])
  print(i)
} 


# AN EXAMPLE TO UNDERSTAND: 
preprocessing_train[preprocessing_train$Basecode == 'BB0069',]


write.csv2(preprocessing_train, "Output/preprocessing_train.csv", row.names = FALSE)

  
# Preparing basic data structure for *TESTING* : **************************************************************
test_h = read_excel("Input/Data Base for Life Cycle Classification Test Set.xlsx", sheet = 1)
test_f = read_excel("Input/Data Base for Life Cycle Classification Test Set.xlsx", sheet = 2)
test_c = read_excel("Input/Actuals for Test Set.xlsx")

test_h$Valor[test_h$Valor <= 0] = 0 
test_h$Type_period = "history"
test_f$Type_period = "forecast"
test_f = na.omit(test_f)
test_c = test_c[,-c(5,6)]
colnames(test_c) = c("KEY", "Pais", "Basecode", "Lifecycle")

hist_forc = bind_rows(x= test_h, y= test_f)
preprocessing_test = merge(hist_forc, test_c, by= c("KEY", "Pais", "Basecode"), all.x = T)
preprocessing_test$Lifecycle[preprocessing_test$Lifecycle == "DESLISTED"] = "DELISTED" 

# Creating the 'encoding' and 'difference' columns: *********
preprocessing_test = preprocessing_test[order(preprocessing_test$Basecode , preprocessing_test$Periodo),] 
for (i in 1:nrow(preprocessing_test)){
  preprocessing_test$encoding[i] = ifelse (preprocessing_test$Valor[i] == 0 , yes = 0, no = 1 )
  preprocessing_test$Diferenca[i] = preprocessing_test$encoding[i] - ifelse(i == 1, 
                                                                              yes = 1, 
                                                                              no = preprocessing_test$encoding[i-1])
  print(i)
} 

write.csv2(preprocessing_test, "Output/preprocessing_test.csv", row.names = FALSE)
rm(hist_forc, test_c, test_f, test_h, train_c, train_h, train_f)

# 2. PREPROCESSING PART 2 : creating variables and changing format table (PREPROCESSING -> PROCESSED)   ---    ----

preprocessing_train = read.csv2("Output/preprocessing_train.csv")


processingData = function(preprocessing){
  
  names_columns = c("Country", "Basecode", "x1_missingdata_h", "x2_missingdata_f" , "x3_n_positive_h", 
                    "x4_n_negative_h", "x5_n_positive_f", "x6_n_negative_f", "x7_consecutive_zeros_h",
                    "x8_consecutive_zeros_f", "x9_beginning_zero_h", "x10_end_zero_h", 
                    "x11_beginning_zero_f", "x12_end_zero_f", "Y_LifeCicle")
  processed <- data.frame(matrix(0, length(unique(preprocessing$Basecode)),length(names_columns)))
  colnames(processed) = names_columns
  
  
  for (k in 1:length(unique(preprocessing$Basecode))){
      
    basec = unique(preprocessing$Basecode)[k]
    filter_history = preprocessing %>% filter(Basecode == basec, Type_period =="history") 
    filter_forecast = preprocessing %>% filter(Basecode == basec, Type_period == "forecast")
    
    processed$Country[k] = substr(basec, 1,2)
    processed$Basecode[k] = basec
  
    processed$x1_missingdata_h[k] = (1 - (sum(filter_history$encoding ) / 30)) * 100
    processed$x2_missingdata_f[k] = (1 - (sum(filter_forecast$encoding ) / 6)) * 100
    processed$x3_n_positive_h[k] = if(length(filter_history$Diferenca[filter_history$Diferenca == 1]) ==0 ){0} else{sum(filter_history$Diferenca[filter_history$Diferenca == 1])} 
    processed$x4_n_negative_h[k] = if(length(filter_history$Diferenca[filter_history$Diferenca == -1]) ==0 ){0} else{abs(sum(filter_history$Diferenca[filter_history$Diferenca == -1]))}
    processed$x5_n_positive_f[k] = if(length(filter_forecast$Diferenca[filter_forecast$Diferenca == 1]) ==0 ){0} else{sum(filter_forecast$Diferenca[filter_forecast$Diferenca == 1])}
    processed$x6_n_negative_f[k] = if(length(filter_forecast$Diferenca[filter_forecast$Diferenca == -1]) ==0 ){0} else{abs(sum(filter_forecast$Diferenca[filter_forecast$Diferenca == -1]))}
    
    # x7 
    validador = 1
    cont = 1
    x7_consecutive_zeros_h = c()
    filter_h = filter_history$Diferenca
    while (validador != 0){
      
      if(is.na(match(1, abs(filter_h)))){
        x7_consecutive_zeros_h[cont] = length(filter_h)
        validador = 0
        
      }else{
        num = match(1, abs(filter_h))
        
        if(num == length(filter_h)){
          validador = 0
          x7_consecutive_zeros_h[cont] = length(filter_h)}
        else{
          filter_h = filter_h[(num+1):length(filter_h)]
          x7_consecutive_zeros_h[cont] = if ((num - 1) < 0){0} else {num - 1} 
        }
        
      }
      cont = cont + 1
    }
    processed$x7_consecutive_zeros_h[k] = (sum(x7_consecutive_zeros_h) /30 ) * 100
  
    # x8
    validador = 1
    cont = 1
    x8_consecutive_zeros_f = c()
    filter_f = filter_forecast$Diferenca
    while (validador != 0){
      
      if(is.na(match(1, abs(filter_f)))){
        x8_consecutive_zeros_f[cont] = length(filter_f)
        validador = 0
        
      }else{
        num = match(1, abs(filter_f))
        
        if(num == length(filter_f)){
          validador = 0
          x8_consecutive_zeros_f[cont] = length(filter_f)}
          else{
            filter_f = filter_f[(num+1):length(filter_f)]
            x8_consecutive_zeros_f[cont] = if ((num - 1) < 0){0} else {num - 1} 
          }
        
      }
      cont = cont + 1
      
    }
    processed$x8_consecutive_zeros_f[k] = (sum(x8_consecutive_zeros_f) /6 ) * 100
    processed$x9_beginning_zero_h[k] = if (filter_history$Diferenca[1] == 0) {1} else {0}
    processed$x10_end_zero_h[k] = if (filter_history$Diferenca[length(filter_history$Diferenca)] == 0) {1} else {0}
    processed$x11_beginning_zero_f[k] = if (filter_forecast$Diferenca[1] != 0 | is.na(filter_forecast$Diferenca[1])) {0} else {1}
    processed$x12_end_zero_f[k] = if (filter_forecast$Diferenca[6] != 0 | is.na(filter_forecast$Diferenca[6])) {0} else {1}
    
    
    y = preprocessing %>%  filter(Basecode == basec) %>% select(Lifecycle) %>% unique()
    processed$Y_LifeCicle[k] = y[1,1] 
    
    
    print(k)
  } # end for
  
  return(processed)
    
}   # Can be TEST or TRAIN
processed_train = processingData(preprocessing_train)

write.csv2(processed_train, "Output/processed_train.csv", row.names = FALSE)

# 3. Testing some models: ---------------------------------------------------------------------------------    -----------------------------------------------------
processed_train = read.csv2("Output/processed_train.csv")

plot_correlate(processed_train)
vis_miss(processed_train) 
table(processed_train$Y_LifeCicle)

### WITH GROUPING: ###
{
    print("Do you want to run WITH grouping: 1, else: 0")
    group = scan(n = 1)
    if (group == 1){
      processed_train$Y_LifeCicle[processed_train$Y_LifeCicle == "DELISTED"] = "NO REGULAR"
      processed_train$Y_LifeCicle[processed_train$Y_LifeCicle == "IN & OUT"] = "NO REGULAR"
      processed_train$Y_LifeCicle[processed_train$Y_LifeCicle == "NPD PHASE IN"] = "NO REGULAR"
      processed_train$Y_LifeCicle[processed_train$Y_LifeCicle == "PHASE OUT"] = "REGULAR"
      processed_train$Y_LifeCicle = as.factor(processed_train$Y_LifeCicle)
    
      processed_test$Y_LifeCicle[processed_test$Y_LifeCicle == "DELISTED"] = "NO REGULAR"
      processed_test$Y_LifeCicle[processed_test$Y_LifeCicle == "IN & OUT"] = "NO REGULAR"
      processed_test$Y_LifeCicle[processed_test$Y_LifeCicle == "NPD PHASE IN"] = "NO REGULAR"
      processed_test$Y_LifeCicle[processed_test$Y_LifeCicle == "PHASE OUT"] = "REGULAR"
      processed_test$Y_LifeCicle = as.factor(processed_test$Y_LifeCicle)  
      }else{
        processed_train$Y_LifeCicle = as.factor(processed_train$Y_LifeCicle)}
}


# Recipe : 
recipe_ =
  recipe(Y_LifeCicle ~ . , data = processed_train) %>%
  step_rm(Country, Basecode) %>% 
  step_normalize(x1_missingdata_h, x2_missingdata_f, 
                 x3_n_positive_h, x4_n_negative_h, 
                 x5_n_positive_f, x6_n_negative_f,
                 x7_consecutive_zeros_h, x8_consecutive_zeros_f) %>%
  step_corr(all_numeric(), threshold = 0.8) %>%
  step_nzv(all_predictors())       # retira vari?veis com variabilidade quase 0
# summary(bake(prep(recipe_), new_data = NULL))


folds = vfold_cv(processed_train, v = 10, strata = Y_LifeCicle) #cross-validation
folds

# 3.1 ***  Random Forest  *** -------------------------    -----------------
model_random = 
  rand_forest(mtry = tune(), 
              trees = 500, 
              min_n = tune() ) %>% 
  set_mode("classification") %>% 
  set_engine("ranger", importance = "impurity") # pesquisar sobre o importance  


wf_random = 
  workflow() %>%
  add_recipe(recipe_) %>%
  add_model(model_random)  
  

grid_ran = 
  grid_latin_hypercube(
    finalize(mtry(), bake(prep(recipe_, processed_train), new_data = NULL)),
    min_n(), 
    size = 10)

tune_random = 
  tune_grid(object = wf_random, 
            resamples = folds, 
            grid = grid_ran,
            control = control_grid(save_pred = TRUE))

wf_random = wf_random %>% finalize_workflow(select_best(tune_random, "accuracy"))

fit_random =
  wf_random  %>% 
  fit_resamples(folds, 
                metrics = metric_set(accuracy, sens, spec, roc_auc), 
                control = control_resamples(verbose = FALSE, save_pred = TRUE))


metrics_random = collect_metrics(fit_random)
metrics_random

final_rand = fit(wf_random, processed_train) 
pred_rand = predict(final_rand, processed_train) # Choose between train or test
confusionMatrix(pred_rand$.pred_class, reference = as.factor(processed_train$Y_LifeCicle))

resultado = data.frame(clave = unique(processed_train$Basecode) ,LifeCicle_Status = pred_rand$.pred_class)

vip(final_rand$fit$fit) + aes(fill= cumsum(Variable == "aleatorio")) +
  ggtitle("Importance of Random Forest Variables")


# 3.2 ***  XGBoost   *** ------------------------------    ------------
model_xgb =                              
  boost_tree(mtry = tune(),              # Quantidade de colunar usar em uma arvore
             trees = 500,                # Arvores a serem feitas
             min_n = tune(),             # M?nimo de observa??es por n? pra dividir
             tree_depth = tune(),        # Profundidade m?xima da ?rvore
             learn_rate = tune(),        # Tamanho do passo. Quanto menor mais devagar.  
             loss_reduction = tune(),    # parametro regularizador. An?logo ao CP do rpart   ???
             sample_size = tune()        # propor??o de linhas para sortear por arvore
  ) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")


wf_xgboost = 
  workflow() %>% 
  add_model(model_xgb) %>% 
  add_recipe(recipe_)

grid_xgb =
  grid_latin_hypercube(
    finalize(mtry(), bake(prep(recipe_, processed_train), new_data = NULL)),
    tree_depth(), 
    min_n() , 
    learn_rate(), 
    loss_reduction(), 
    sample_size = sample_prop(),
    size = 10)


tune_xgb = 
  tune_grid(object = wf_xgboost, 
            resamples = folds, 
            grid = grid_xgb,
            control = control_grid(verbose = FALSE ,save_pred = TRUE))

wf_xgboost = wf_xgboost %>% finalize_workflow(select_best(tune_xgb, "accuracy"))

fit_xgboost =
  wf_xgboost  %>% 
  fit_resamples(folds, 
                metrics = metric_set(accuracy, sens, spec, roc_auc), 
                control = control_resamples(verbose = FALSE , save_pred = TRUE))


metrics_xgb = collect_metrics(fit_xgboost)
metrics_xgb

final_xgb = fit(wf_xgboost, processed_train)  
pred_xgb = predict(final_xgb, processed_test) # Choose train or test
confusionMatrix(pred_xgb$.pred_class, reference = as.factor(processed_test$Y_LifeCicle))


  vip(final_xgb$fit$fit) + aes(fill= cumsum(Variable == "aleatorio")) +
  ggtitle("Importance of XGBoost Variables")



# 3.3 ***  Multinomial  *** ---------------------------    ---------------
model_multi = 
  multinom_reg() %>% 
  set_engine("nnet") %>% 
  set_mode("classification")

wf_multi = 
  workflow() %>% 
  add_recipe(recipe_) %>% 
  add_model(model_multi)

fit_multi = 
  wf_multi %>% 
  fit_resamples(folds, 
                metrics = metric_set(accuracy, sens, spec, roc_auc), 
                control = control_resamples(verbose = FALSE , save_pred = TRUE))

metrics_multi = collect_metrics(fit_multi)
metrics_multi

final_multi = fit(wf_multi, processed_train)
pred_multi = predict(final_multi, processed_test) # Choose train or test
confusionMatrix(pred_multi$.pred_class, reference = as.factor(processed_test$Y_LifeCicle))

vip(final_multi$fit$fit) + aes(fill= cumsum(Variable == "aleatorio")) +  #  ???
  ggtitle("Importance of Multinomial Variables")

# Comparing Models: 

metrics_random
metrics_xgb
metrics_multi

metrics_logistic

# 3.4 ***  Light GBM  *** -----------------------------    ------------
model_lightgbm = 
  parsnip::boost_tree(mtry = tune(),              
             trees = 500,               
             min_n = tune(),             
             tree_depth = tune(),         
             learn_rate = tune(),          
             loss_reduction = tune(),    
             sample_size = tune()        
  ) %>% 
  set_mode("classification") %>% 
  set_engine("lightgbm")

wf_lightgbm = 
  workflow() %>% 
  add_recipe(recipe_) %>% 
  add_model(model_lightgbm)

grid_lightgbm = 
  grid_latin_hypercube(
    finalize(mtry(), bake(prep(recipe_, processed_train), new_data = NULL)),
    tree_depth(), 
    min_n() , 
    learn_rate(), 
    loss_reduction(), 
    sample_size = sample_prop(),
    size = 10)

tune_lightgbm = 
  tune_grid(object = wf_lightgbm, 
            resamples = folds, 
            grid = grid_lightgbm,
            control = control_grid(verbose = FALSE ,save_pred = TRUE))

wf_lightgbm = wf_lightgbm %>% finalize_workflow(select_best(tune_lightgbm, "accuracy"))

fit_lightgbm =
  wf_lightgbm  %>% 
  fit_resamples(folds, 
                metrics = metric_set(accuracy, sens, spec, roc_auc), 
                control = control_resamples(verbose = FALSE , save_pred = TRUE))


metrics_lightgbm = collect_metrics(fit_lightgbm)
metrics_lightgbm

final_lightgbm = fit(wf_lightgbm, processed_train)  
pred_lightgbm = predict(final_lightgbm, processed_train)
confusionMatrix(pred_lightgbm$.pred_class, reference = processed_train$Y_LifeCicle)

#
vip(final_xgb$fit$fit) + aes(fill= cumsum(Variable == "aleatorio")) +
  ggtitle("Importance of XGBoost Variables")



# 3.5 ***  Logistic Regression  *** -------------------    ----------------------
model_logistic = 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")


wf_logistic = 
  workflow() %>% 
  add_recipe(recipe_) %>%
  add_model(model_logistic)


fit_logistic = 
  wf_logistic  %>% 
  fit_resamples(folds, 
                metrics = metric_set(accuracy, sens, spec), 
                control = control_resamples(save_pred = TRUE))


metrics_logistic = collect_metrics(fit_logistic)
metrics_logistic

final_logistic = fit(wf_logistic, processed_train) 
pred_logistic = predict(final_logistic, processed_train)
confusionMatrix(pred_logistic$.pred_class, reference = processed_train$Y_LifeCicle) 


vip(final_logistic$fit$fit) + aes(fill= cumsum(Variable == "aleatorio")) +   # variable importance
  ggtitle("Importance of Logistic Regression Variables")


