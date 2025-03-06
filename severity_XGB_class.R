# load libraries, (not all are used)

library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(stats)
library(gridExtra)
install.packages("randomForest")
library(randomForest)
library(nnet)
install.packages("caret")
library(caret)
install.packages("gbm")
library(gbm)
install.packages("xgboost")
library(xgboost)
library(Matrix)
install.packages("keras")
library(keras)
library(tensorflow)
install.packages("gains")
library(gains)
install.packages("SHAPforxgboost")
library(SHAPforxgboost)
install.packages("DALEX")
library(DALEX)
install.packages("iml")
library(iml)
install.packages("lime")
library(lime)
install.packages("interactions")
library(interactions)



# Define the list of predictor column names
predictors <- c("fuel_", "transmission_", "occupation_group", "car_colour_", 
                "NCD_level_", "no_of_smart_devices_owned_", 
                "acceleration_levels_last_year_", "car_power_", 
                "drive_orientation_", "gender_", "exposure_year_", 
                "modifications_", "no_of_drivers_", 
                "traffic_fines_paid_last_year_", "age_of_car_", 
                "GEO_population_density_of_area_", "no_of_car_doors_", 
                "GEO_income_level_of_area_", "GEO_average_education_level_", 
                "GEO_average_house_size_", "Count_of_siblings_", 
                "Car_manufacture_origin_", "Mileage_levels_last_year_", 
                "Past_defaulter_", "insurer_prior_year_policy_", 
                "licence_", "age_cap65_age_","age_cap_age_30")

# Create the formula dynamically
# the -1 means that one hot encoding is used
formula_str <- paste("~", paste(predictors, collapse = " + "), "- 1")

# Convert to formula object
formula_obj <- as.formula(formula_str)

library(ggplot2)
library(dplyr)

# Define the predictors




# code for prepping data for XGBoost and fitting XGBoost model

train_data <- simul_gamma_train
val_data <- simul_gamma_val
test_data <- simul_gamma_test

# Apply model.matrix
train_matrix <- model.matrix(formula_obj, data = train_data)
val_matrix <- model.matrix(formula_obj, data = val_data)
test_matrix <- model.matrix(formula_obj, data = test_data)


dtrain <- xgb.DMatrix(data = train_matrix, label = train_data$claim_amount)
dval <- xgb.DMatrix(data = val_matrix, label = val_data$claim_amount)
dtest <- xgb.DMatrix(data = test_matrix, label = test_data$claim_amount)



# parameterise and fit the XGB model, Gamma regression XGB, minimising Gamma deviance
# experiment with parameter values for best results

set.seed(123)
xgb_model <- xgb.train(
  params = list(
    objective = "reg:gamma" ,
    eval_metric = "gamma-deviance",
    eta = 0.045,  # Learning rate
    max_depth = 6,
    subsample = 0.7, #sample rows
    colsample_bytree = 0.8
  ),
  data = dtrain,
  nrounds = 800, #number of tress
  watchlist = list(train = dtrain, val = dval),
  early_stopping_rounds = 250, #fit is not getting better just stop at 250 
  verbose = 1 
)

##train-gamma-deviance:0.530428	val-gamma-deviance:0.547534 
# Score predictions


simul_gamma_val$prediction_XGB <- predict(xgb_model, newdata = dval)

simul_gamma_train$prediction_XGB <- predict(xgb_model, newdata = dtrain)

simul_gamma_val$prediction_GLM <- predict(gamma_glm, newdata = simul_gamma_val, type = "response")



# Calculate Gamma Deviance using custom gamma deviance function
gamma_deviance_XGB_val <- round(calculate_gamma_deviance(simul_gamma_val, actual_col = "claim_amount", prediction_col = "prediction_XGB"),0)
gamma_deviance_GLM_val <- round(calculate_gamma_deviance(simul_gamma_val, actual_col = "claim_amount", prediction_col = "prediction_GLM"),0)

gamma_deviance_XGB_val
gamma_deviance_GLM_val

# Calculate Null Deviance
simul_gamma_val$prediction_NULL <- mean(simul_gamma_val$claim_amount)

# Calculate Gamma Deviance for Null Model
gamma_deviance_NULL_val <- round(calculate_gamma_deviance(simul_gamma_val, actual_col = "claim_amount", prediction_col = "prediction_NULL"), 0)

gamma_deviance_NULL_val  # Print Null Deviance

xgb.importance(model = xgb_model)

importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model)
xgb.plot.importance(importance_matrix, top_n = 20)  # Show top 20 important features


# compare GLM and XGB in bootstrapped samples of validation data

# Create an empty data frame to store results
bootstrap_results <- data.frame(
  iteration = integer(250),
  gamma_deviance_XGB = numeric(250),
  gamma_deviance_GLM = numeric(250),
  gamma_deviance_NULL = numeric(250)
)

for (i in 1:250) {
  bootstrap_sample <- simul_gamma_val[sample(1:nrow(simul_gamma_val), replace = TRUE), ]
  gamma_deviance_XGB <- round(calculate_gamma_deviance(bootstrap_sample, actual_col = "claim_amount", prediction_col = "prediction_XGB"),0)
  gamma_deviance_GLM <- round(calculate_gamma_deviance(bootstrap_sample, actual_col = "claim_amount", prediction_col = "prediction_GLM"),0)
  gamma_deviance_NULL <- round(calculate_gamma_deviance(bootstrap_sample, actual_col = "claim_amount", prediction_col = "prediction_NULL"), 0)
  bootstrap_results[i, ] <- c(i, gamma_deviance_XGB, gamma_deviance_GLM,gamma_deviance_NULL)
}

bootstrap_results_long <- gather(bootstrap_results, key = "Model", value = "GammaDeviance", gamma_deviance_XGB, gamma_deviance_GLM,gamma_deviance_NULL)

# Create the box plot
ggplot(bootstrap_results_long, aes(x = Model, y = GammaDeviance, fill = Model)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Gamma Deviance Comparison", y = "Gamma Deviance", x = "Model") +
  scale_fill_manual(values = c("skyblue", "lightgreen","red"))










# ------------------------------------------------------------
# note: this code was written with assistance from Chatgpt
# ------------------------------------------------------------


library(xgboost)

# Define the list of high-importance predictors (removing low-gain features)
selected_predictors <- c(
  "gender_", "NCD_level_", "modifications_", "drive_orientation_", 
  "licence_", "Past_defaulter_", "occupation_", "exposure_year_", 
  "acceleration_levels_last_year_", "car_power_", "age_cap_age_30", "age_cap65_age_"
)

# Create new formula
formula_str_selected <- paste("~", paste(selected_predictors, collapse = " + "), "- 1")
formula_obj_selected <- as.formula(formula_str_selected)

# Apply model.matrix for feature selection
train_matrix_selected <- model.matrix(formula_obj_selected, data = simul_gamma_train)
val_matrix_selected <- model.matrix(formula_obj_selected, data = simul_gamma_val)

# Convert to XGBoost format
dtrain_selected <- xgb.DMatrix(data = train_matrix_selected, label = simul_gamma_train$claim_amount)
dval_selected <- xgb.DMatrix(data = val_matrix_selected, label = simul_gamma_val$claim_amount)

# Re-train XGBoost with selected features
set.seed(123)
xgb_model_selected <- xgb.train(
  params = list(
    objective = "reg:gamma",
    eval_metric = "gamma-deviance",
    eta = 0.1,  
    max_depth = 6,
    subsample = 0.7,
    colsample_bytree = 0.7
  ),
  data = dtrain_selected,
  nrounds = 500,
  watchlist = list(train = dtrain_selected, val = dval_selected),
  early_stopping_rounds = 250,
  verbose = 1
)

# Score predictions for validation set
simul_gamma_val$prediction_XGB_selected <- predict(xgb_model_selected, newdata = dval_selected)

# Calculate Gamma Deviance for new model
gamma_deviance_XGB_selected <- round(
  calculate_gamma_deviance(simul_gamma_val, actual_col = "claim_amount", prediction_col = "prediction_XGB_selected"),
  0
)

# Compare Deviances
gamma_deviance_comparison <- data.frame(
  Model = c("Original XGBoost", "Selected Features XGBoost"),
  Gamma_Deviance = c(gamma_deviance_XGB_val, gamma_deviance_XGB_selected)
)

# Print Results
print(gamma_deviance_comparison)



set.seed(123)

n = nrow(simul_gamma_train)
dat = simul_gamma_train[sample(1:n,n),]
#View(all_data)

x = dat[,c("fuel_", "transmission_", "occupation_", "car_colour_", 
           "NCD_level_", 
           "acceleration_levels_last_year_", "car_power_", 
           "drive_orientation_", "gender_", "exposure_year_", 
           "modifications_", "no_of_drivers_", 
           "traffic_fines_paid_last_year_", "age_of_car_", 
           "GEO_population_density_of_area_", "no_of_car_doors_", 
           "GEO_income_level_of_area_", 
           "GEO_average_house_size_", 
           "Car_manufacture_origin_", "Mileage_levels_last_year_", 
           "Past_defaulter_", "insurer_prior_year_policy_", 
           "licence_","age_cap_age_30","age_cap65_age_")]
y = dat$claim_amount
lmo = lm(y~., data=x)
summary(lmo)
n = nrow(x)


# Repeated k-fold framework:
#R = 10
K = 5
errs_train = errs_test = NULL
folds = cut(1:n, K, labels=FALSE)
is = sample(1:n,n)
x = x[is,]
y = y[is]

# then perform k-fold CV:
for(k in 1:K){
  i.train = which(folds!=k)
  lmo = lm(y~., data=x, subset=i.train)
  
  i.test = which(folds==k)
  preds_train = predict(lmo, newdata=x[i.train,])
  errs_train[k] = sqrt(( mean((y[i.train]-preds_train)^2 ))) #RMSE
  preds_test = predict(lmo, newdata=x[i.test,])
  errs_test[k] = sqrt(mean(( (y[i.test]-preds_test)^2 ))) #RMSE
}

errs_train 
errs_test 

boxplot(errs_train, errs_test,names=c("Training error","Test error"),main='RMSE with categorical')




