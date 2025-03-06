# ------------------------------ Libraries ------------------------------ 

install.packages("ggplot2")
install.packages("tidyr")
install.packages("readr")
install.packages("gridExtra")

library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(stats)
library(gridExtra)

# ------------------------------ Data Preparation ------------------------------ 

# Clear environment
rm(list = ls())

# Set working directory (modify as needed)
setwd("C:/Users/Administrator/OneDrive - University College Cork/Stats Consultancy 2")

# Load datasets
simul_gamma_train_val <- read.csv("C:/Users/Administrator/OneDrive - University College Cork/Stats Consultancy 2/simulated_claim_severity_train_val.csv")
simul_gamma_test <- read.csv("C:/Users/Administrator/OneDrive - University College Cork/Stats Consultancy 2/simulated_claim_severity_test.csv")

# Pre-process variables
simul_gamma_train_val$exposure_year <- simul_gamma_train_val$exposure_year - 2000
simul_gamma_test$exposure_year <- simul_gamma_test$exposure_year - 2000

#simul_gamma_train_val <- simul_gamma_train_val[simul_gamma_train_val$occupation != "sportsperson", ]
#simul_gamma_test <- simul_gamma_test[simul_gamma_test$occupation != "sportsperson", ]


# ------------------------------ Column Renaming ------------------------------ 

# Rename columns for clarity (Before transformations)
colnames(simul_gamma_train_val) <- ifelse(colnames(simul_gamma_train_val) == "claim_amount", 
                                          "claim_amount", 
                                          paste0(colnames(simul_gamma_train_val), "_"))

colnames(simul_gamma_test) <- ifelse(colnames(simul_gamma_test) == "claim_amount", 
                                     "claim_amount", 
                                     paste0(colnames(simul_gamma_test), "_"))

# ------------------------------ Feature Engineering ------------------------------ 

# Create age-based groups BEFORE modifying age_
simul_gamma_train_val$age_grouped <- as.factor(cut(simul_gamma_train_val$age_, breaks = c(-Inf,30,Inf), labels = c("young","old")))
simul_gamma_test$age_grouped <- as.factor(cut(simul_gamma_test$age_, breaks = c(-Inf,30,Inf), labels = c("young","old")))

# Now shift age (AFTER categorical grouping)

summary(simul_gamma_train_val$age_of_car_)
simul_gamma_train_val$age_of_car_grouped <- ifelse(simul_gamma_train_val$age_of_car_ == 0, 0, 1)

# Create squared age term
simul_gamma_train_val$age_pwr2 <- simul_gamma_train_val$age_^2
simul_gamma_test$age_pwr2 <- simul_gamma_test$age_^2

# Cap at age 30
simul_gamma_train_val$age_cap_age_30 <- pmin(simul_gamma_train_val$age_, 30 ) - 18
simul_gamma_test$age_cap_age_30 <- pmin(simul_gamma_test$age_, 30 ) - 18

# Cap at age 65
simul_gamma_train_val$age_cap65_age_ <- pmax(simul_gamma_train_val$age_, 65) - 65
simul_gamma_test$age_cap65_age_ <- pmax(simul_gamma_test$age_, 65) - 65

summary(simul_gamma_train_val$age_cap65_age_)

simul_gamma_train_val$age_ <- simul_gamma_train_val$age_ - 18
simul_gamma_test$age_ <- simul_gamma_test$age_ - 18

simul_gamma_train_val$occupation_group <- case_when(
  simul_gamma_train_val$occupation %in% c("sportsperson") ~ "Sportsperson",
  simul_gamma_train_val$occupation %in% c("farmer", "garda") ~ "High_Risk",
  simul_gamma_train_val$occupation %in% c("lecturer", "secondary_teacher", "other") ~ "Moderate_Risk",
  simul_gamma_train_val$occupation %in% c("actuary", "doctor", "accountant") ~ "White_Collar_Low_Risk",
  simul_gamma_train_val$occupation %in% c("nurse", "primary_teacher") ~ "Care_Education_Low_Risk",
  TRUE ~ "Other"
)

simul_gamma_test$occupation_group <- case_when(
  simul_gamma_test$occupation %in% c("farmer", "garda") ~ "High_Risk",
  simul_gamma_test$occupation %in% c("lecturer", "secondary_teacher", "other") ~ "Moderate_Risk",
  simul_gamma_test$occupation %in% c("actuary", "doctor", "accountant") ~ "White_Collar_Low_Risk",
  simul_gamma_test$occupation %in% c("nurse", "primary_teacher") ~ "Care_Education_Low_Risk",
  TRUE ~ "Other"
)

# Convert to factor for modeling
simul_gamma_train_val$occupation_group <- factor(simul_gamma_train_val$occupation_group)
simul_gamma_test$occupation_group <- factor(simul_gamma_test$occupation_group)


# Subset data into training and validation sets
simul_gamma_train <- subset(simul_gamma_train_val, random_0_9_ %in% c(0,1,2,3,4,5))
simul_gamma_val <- subset(simul_gamma_train_val, random_0_9_ %in% c(6,7,8,9))

# Display summary and structure of data
summary(simul_gamma_train_val)
names(simul_gamma_train_val)
str(simul_gamma_train_val)
quantile(simul_gamma_train_val$claim_amount)

# ------------------------------ Modeling ------------------------------ 

# Define excluded variables and construct model formula
excluded_vars <- c("policy_number_", "claim_count_", "claim_amount", "random_0_9_", "random_0_4_","age_", "age_pwr2","age_grouped")
all_vars <- setdiff(names(simul_gamma_train_val), excluded_vars)
formula <- as.formula(paste("claim_amount ~", paste(all_vars, collapse = " + ")))

# Fit Gamma GLM models
gamma_glm <- glm(formula, data = simul_gamma_train_val, family = Gamma(link = "log"))
gamma_glm_train <- glm(formula, data = simul_gamma_train, family = Gamma(link = "log"))

# Alternative model specification for testing variable selection
gamma_glm2 <- glm(claim_amount ~  gender_ + NCD_level_+NCD_level_:acceleration_levels_last_year_ +age_cap_age_30:gender_+ age_cap65_age_+modifications_ + drive_orientation_ + Past_defaulter_ 
                + occupation_group+ insurer_prior_year_policy_+ exposure_year_+age_cap_age_30,
                   data = simul_gamma_train_val, 
                 family = Gamma(link = "log"))
summary(gamma_glm)
summary(gamma_glm2)
summary_result <- summary(gamma_glm2)
coefficients_table <- data.frame(
  Variable = rownames(summary_result$coefficients),
  Estimate = round(exp(summary_result$coefficients[, "Estimate"]), 4),
  LowerValue = round(exp(summary_result$coefficients[, "Estimate"] - 2 * summary_result$coefficients[, "Std. Error"]), 4),
  UpperValue = round(exp(summary_result$coefficients[, "Estimate"] + 2 * summary_result$coefficients[, "Std. Error"]), 4),
  p_values = round(summary_result$coefficients[, "Pr(>|t|)"], 3)
)

coefficients_table

# ------------------------------ Model Evaluation ------------------------------ 

# Model summaries
summary(gamma_glm)
summary(gamma_glm_train)

# Compute log-likelihood and dispersion parameters
logLik(gamma_glm)
scale_param <- summary(gamma_glm)$dispersion
residual_deviance <- summary(gamma_glm)$deviance

# Create coefficient tables
summary_result <- summary(gamma_glm)
coefficients_table <- data.frame(
  Variable = rownames(summary_result$coefficients),
  Estimate = round(exp(summary_result$coefficients[, "Estimate"]), 4),
  LowerValue = round(exp(summary_result$coefficients[, "Estimate"] - 2 * summary_result$coefficients[, "Std. Error"]), 4),
  UpperValue = round(exp(summary_result$coefficients[, "Estimate"] + 2 * summary_result$coefficients[, "Std. Error"]), 4),
  p_values = round(summary_result$coefficients[, "Pr(>|t|)"], 3)
)

coefficients_table

# Same table for training data only
summary_result_train <- summary(gamma_glm_train)
coefficients_table_train <- data.frame(
  Variable = rownames(summary_result_train$coefficients),
  Estimate = round(exp(summary_result_train$coefficients[, "Estimate"]), 4),
  LowerValue = round(exp(summary_result_train$coefficients[, "Estimate"] - 2 * summary_result_train$coefficients[, "Std. Error"]), 4),
  UpperValue = round(exp(summary_result_train$coefficients[, "Estimate"] + 2 * summary_result_train$coefficients[, "Std. Error"]), 4),
  p_values = round(summary_result_train$coefficients[, "Pr(>|t|)"], 3)
)

coefficients_table_train

# ------------------------------ Predictions ------------------------------ 

# Generate predictions using gamma_glm
simul_gamma_train_val$prediction <- predict(gamma_glm, newdata = simul_gamma_train_val, type = "response")
simul_gamma_train_val$null_prediction <- mean(simul_gamma_train_val$claim_amount)





simul_gamma_train$prediction <- predict(gamma_glm_train, newdata = simul_gamma_train, type = "response")
# NB! the prediction in the validation data comes from the model fitted on the train data
simul_gamma_val$prediction <- predict(gamma_glm_train, newdata = simul_gamma_val, type = "response")




# function for calculating gamma deviance - reconcile this with the R output for deviance

calculate_gamma_deviance <- function(data, actual_col, prediction_col) {
  # Extract actual and predicted values
  actual <- data[[actual_col]]
  prediction <- data[[prediction_col]]
  
  # Compute the gamma deviance for each row
  deviance_values <- 2 * (-log(actual / prediction) + ((actual - prediction) / prediction))
  
  # Sum to get the total deviance
  total_deviance <- sum(deviance_values, na.rm = TRUE)
  
  return(total_deviance)
}




# calculate deviance

# train + val data
gamma_deviance <- round(calculate_gamma_deviance(simul_gamma_train_val, actual_col = "claim_amount", prediction_col = "prediction"),0)
gamma_deviance_null <- round(calculate_gamma_deviance(simul_gamma_train_val, actual_col = "claim_amount", prediction_col = "null_prediction"),0)

print(gamma_deviance)
print(gamma_deviance_null)



















# ------------------------------ plotting ------------------------------ 

plot_sev <- function(out, xvar, title, model) {
  ggplot(out, aes(x = !!sym(xvar), group = 1)) +
    geom_point(aes(y = pred, colour = model)) +
    geom_point(aes(y = obs, colour = "observed")) +
    geom_line(aes(y = pred, colour = model), linetype = "dashed") +
    geom_line(aes(y = obs, colour = "observed"), linetype = "dashed") +
    labs(x = xvar, y = "severity", title = title) +
    #theme(legend.position = "bottom")
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))
}





#  occupation_ plots 

plot_table <- simul_gamma_train %>% group_by(occupation_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction) / sum(claim_count_) )
p1 <- plot_sev(plot_table, "occupation_", "train severity by occupation_", "GLM")

plot_table <- simul_gamma_val %>% group_by(occupation_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction) / sum(claim_count_) )
p2 <- plot_sev(plot_table, "occupation_", "validation severity by occupation_", "GLM")

# Create a bar chart using ggplot2 after grouping by Area_chart and calculating the sum of ClaimCount_row
plot_data <- simul_gamma_train %>%
  group_by(occupation_) %>%
  summarise(claim_count_ = sum(claim_count_))
p3 <- ggplot(plot_data, aes(x = occupation_, y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue",alpha=0.4) +
  labs(title = "ClaimCount_row", x = "occupation_", y = "Total ClaimCount_row")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_data <- simul_gamma_val %>%
  group_by(occupation_) %>%
  summarise(claim_count_ = sum(claim_count_))
p4 <- ggplot(plot_data, aes(x = occupation_, y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue",alpha=0.4) +
  labs(title = "ClaimCount_row", x = "occupation_", y = "Total ClaimCount_row")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(p1, p2, p3, p4, ncol = 2)



#  car power plots 

plot_table <- simul_gamma_val %>% group_by(car_power_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction_XGB) / sum(claim_count_) )
p1 <- plot_sev(plot_table, "car_power_", "train severity by car_power_", "GLM")

plot_table <- simul_gamma_val %>% group_by(car_power_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction_XGB) / sum(claim_count_) )
p2 <- plot_sev(plot_table, "car_power_", "validation severity by car_power_", "GLM")

# Create a bar chart using ggplot2 after grouping by Area_chart and calculating the sum of ClaimCount_row
plot_data <- simul_gamma_val %>%
  group_by(car_power_) %>%
  summarise(claim_count_ = sum(claim_count_))
p3 <- ggplot(plot_data, aes(x = car_power_, y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue",alpha=0.4) +
  labs(title = "ClaimCount_row", x = "car_power_", y = "Total ClaimCount_row")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_data <- simul_gamma_val %>%
  group_by(car_power_) %>%
  summarise(claim_count_ = sum(claim_count_))
p4 <- ggplot(plot_data, aes(x = car_power_, y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue",alpha=0.4) +
  labs(title = "ClaimCount_row", x = "car_power_", y = "Total ClaimCount_row")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(p1, p2, p3, p4, ncol = 2)


## insurer last year 


plot_table <- simul_gamma_train %>% group_by(insurer_prior_year_policy_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction) / sum(claim_count_) )
p1 <- plot_sev(plot_table, "insurer_prior_year_policy_", "train severity by insurer_prior_year_policy_", "GLM")

plot_table <- simul_gamma_val %>% group_by(insurer_prior_year_policy_) %>%
    summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction) / sum(claim_count_) )
p2 <- plot_sev(plot_table, "insurer_prior_year_policy_", "validation severity by insurer_prior_year_policy_", "GLM")

# Create a bar chart using ggplot2 after grouping by Area_chart and calculating the sum of ClaimCount_row
plot_data <- simul_gamma_train %>%
  group_by(insurer_prior_year_policy_) %>%
  summarise(claim_count_ = sum(claim_count_))
p3 <- ggplot(plot_data, aes(x = insurer_prior_year_policy_, y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue",alpha=0.4) +
  labs(title = "ClaimCount_row", x = "insurer_prior_year_policy_", y = "Total ClaimCount_row")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_data <- simul_gamma_val %>%
  group_by(insurer_prior_year_policy_) %>%
  summarise(claim_count_ = sum(claim_count_))
p4 <- ggplot(plot_data, aes(x = insurer_prior_year_policy_, y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue",alpha=0.4) +
  labs(title = "ClaimCount_row", x = "insurer_prior_year_policy_", y = "Total ClaimCount_row")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(p1, p2, p3, p4, ncol = 2)


# age_ plots
plot_table <- simul_gamma_train %>% group_by(age_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction) / sum(claim_count_) )
p1 <- plot_sev(plot_table, "age_", "train severity by age_", "GLM")

plot_table <- simul_gamma_val %>% group_by(age_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction) / sum(claim_count_) )
p2 <- plot_sev(plot_table, "age_", "validation severity by age_", "GLM")

# Create a bar chart using ggplot2 after grouping by Area_chart and calculating the sum of ClaimCount_row
plot_data <- simul_gamma_train %>%
  group_by(age_) %>%
  summarise(claim_count_ = sum(claim_count_))
p3 <- ggplot(plot_data, aes(x = age_, y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue",alpha=0.4) +
  labs(title = "ClaimCount_row", x = "age_", y = "Total ClaimCount_row")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_data <- simul_gamma_val %>%
  group_by(age_) %>%
  summarise(claim_count_ = sum(claim_count_))
p4 <- ggplot(plot_data, aes(x = age_, y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue",alpha=0.4) +
  labs(title = "ClaimCount_row", x = "age_", y = "Total ClaimCount_row")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(p1, p2, p3, p4, ncol = 2)


# Train set severity by age using XGB predictions
plot_table <- simul_gamma_train %>%
  group_by(age_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction_XGB) / sum(claim_count_) )

p1 <- plot_sev(plot_table, "age_", "Train Severity by Age_", "XGB")

# Validation set severity by age using XGB predictions
plot_table <- simul_gamma_val %>%
  group_by(age_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction_XGB) / sum(claim_count_) )

p2 <- plot_sev(plot_table, "age_", "Validation Severity by Age_", "XGB")

# Create a bar chart of total claim counts for training set
plot_data <- simul_gamma_train %>%
  group_by(age_) %>%
  summarise(claim_count_ = sum(claim_count_))

p3 <- ggplot(plot_data, aes(x = age_, y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.4) +
  labs(title = "Claim Count (Train)", x = "Age", y = "Total Claim Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create a bar chart of total claim counts for validation set
plot_data <- simul_gamma_val %>%
  group_by(age_) %>%
  summarise(claim_count_ = sum(claim_count_))

p4 <- ggplot(plot_data, aes(x = age_, y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.4) +
  labs(title = "Claim Count (Validation)", x = "Age", y = "Total Claim Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Arrange all plots
grid.arrange(p1, p2, p3, p4, ncol = 2)



###Past defaulter 
# Train set severity by Past_defaulter_ using XGB predictions
plot_table <- simul_gamma_train %>%
  group_by(Past_defaulter_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction_XGB) / sum(claim_count_) )

p1 <- plot_sev(plot_table, "Past_defaulter_", "Train Severity by Past Defaulter", "XGB")

# Validation set severity by Past_defaulter_ using XGB predictions
plot_table <- simul_gamma_val %>%
  group_by(Past_defaulter_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction_XGB) / sum(claim_count_) )

p2 <- plot_sev(plot_table, "Past_defaulter_", "Validation Severity by Past Defaulter", "XGB")

# Create a bar chart of total claim counts for training set grouped by Past_defaulter_
plot_data <- simul_gamma_train %>%
  group_by(Past_defaulter_) %>%
  summarise(claim_count_ = sum(claim_count_))

p3 <- ggplot(plot_data, aes(x = as.factor(Past_defaulter_), y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.4) +
  labs(title = "Claim Count (Train)", x = "Past Defaulter", y = "Total Claim Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create a bar chart of total claim counts for validation set grouped by Past_defaulter_
plot_data <- simul_gamma_val %>%
  group_by(Past_defaulter_) %>%
  summarise(claim_count_ = sum(claim_count_))

p4 <- ggplot(plot_data, aes(x = as.factor(Past_defaulter_), y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.4) +
  labs(title = "Claim Count (Validation)", x = "Past Defaulter", y = "Total Claim Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Arrange all plots
grid.arrange(p1, p2, p3, p4, ncol = 2)


###age of car 
# Train set severity by age_of_car_ using XGB predictions
plot_table <- simul_gamma_train %>%
  group_by(age_of_car_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction_XGB) / sum(claim_count_) )

p1 <- plot_sev(plot_table, "age_of_car_", "Train Severity by Age of Car", "XGB")

# Validation set severity by age_of_car_ using XGB predictions
plot_table <- simul_gamma_val %>%
  group_by(age_of_car_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction_XGB) / sum(claim_count_) )

p2 <- plot_sev(plot_table, "age_of_car_", "Validation Severity by Age of Car", "XGB")

# Create a bar chart of total claim counts for training set grouped by age_of_car_
plot_data <- simul_gamma_train %>%
  group_by(age_of_car_) %>%
  summarise(claim_count_ = sum(claim_count_))

p3 <- ggplot(plot_data, aes(x = as.factor(age_of_car_), y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.4) +
  labs(title = "Claim Count (Train)", x = "Age of Car", y = "Total Claim Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create a bar chart of total claim counts for validation set grouped by age_of_car_
plot_data <- simul_gamma_val %>%
  group_by(age_of_car_) %>%
  summarise(claim_count_ = sum(claim_count_))

p4 <- ggplot(plot_data, aes(x = as.factor(age_of_car_), y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.4) +
  labs(title = "Claim Count (Validation)", x = "Age of Car", y = "Total Claim Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Arrange all plots
grid.arrange(p1, p2, p3, p4, ncol = 2)





#interaction  plot

# make actual vs observed plots, to check for interactions
agg_data <- simul_gamma_val %>%
  group_by(age_, gender_) %>%
  summarise(
    mean_claim = mean(claim_amount),
    mean_pred = mean(prediction_XGB),
    .groups = "drop"
  )

ggplot(agg_data, aes(x = age_, color = gender_)) +
  geom_line(aes(y = mean_claim, linetype = "Observed"), linewidth = 1) +  
  geom_point(aes(y = mean_claim), size = 2) +  
  geom_line(aes(y = mean_pred, linetype = "Predicted"), linewidth = 1, alpha = 0.7) +  
  geom_point(aes(y = mean_pred), size = 2, alpha = 0.7) +  
  labs(
    title = "Average Claim Amount and Prediction by Age",
    x = "Age",
    y = "Average Amount",
    color = "Gender",
    linetype = "Type"
  ) +
  theme_minimal()

#interaction  plot

# make actual vs observed plots, to check for interactions
agg_data <- simul_gamma_val %>%
  group_by(occupation_, gender_) %>%
  summarise(
    mean_claim = mean(claim_amount),
    mean_pred = mean(prediction_XGB),
    .groups = "drop"
  )

ggplot(agg_data, aes(x = occupation_, color = gender_)) +
  geom_line(aes(y = mean_claim, linetype = "Observed"), linewidth = 1) +  
  geom_point(aes(y = mean_claim), size = 2) +  
  geom_line(aes(y = mean_pred, linetype = "Predicted"), linewidth = 1, alpha = 0.7) +  
  geom_point(aes(y = mean_pred), size = 2, alpha = 0.7) +  
  labs(
    title = "Average Claim Amount and Prediction by Age",
    x = "Occupation",
    y = "Average Amount",
    color = "Gender",
    linetype = "Type"
  ) +
  theme_minimal()

# make actual vs observed plots, to check for interactions
agg_data <- simul_gamma_val %>%
  group_by(Past_defaulter_, modifications_) %>%
  summarise(
    mean_claim = mean(claim_amount),
    mean_pred = mean(prediction_XGB),
    .groups = "drop"
  )

ggplot(agg_data, aes(x = Past_defaulter_, color = modifications_)) +
  geom_line(aes(y = mean_claim, linetype = "Observed"), linewidth = 1) +  
  geom_point(aes(y = mean_claim), size = 2) +  
  geom_line(aes(y = mean_pred, linetype = "Predicted"), linewidth = 1, alpha = 0.7) +  
  geom_point(aes(y = mean_pred), size = 2, alpha = 0.7) +  
  labs(
    title = "Average Claim Amount and Prediction by Modifications and Past defaulter",
    x = "Past defaulter",
    y = "Average Amount",
    color = "Modifications",
    linetype = "Type"
  ) +
  theme_minimal()

# make actual vs observed plots, to check for interactions
agg_data <- simul_gamma_val %>%
  group_by(NCD_level_, exposure_year_) %>%
  summarise(
    mean_claim = mean(claim_amount),
    mean_pred = mean(prediction_XGB),
    .groups = "drop"
  )

ggplot(agg_data, aes(x = NCD_level_, color = exposure_year_ )) +
  geom_line(aes(y = mean_claim, linetype = "Observed"), linewidth = 1) +  
  geom_point(aes(y = mean_claim), size = 2) +  
  geom_line(aes(y = mean_pred, linetype = "Predicted"), linewidth = 1, alpha = 0.7) +  
  geom_point(aes(y = mean_pred), size = 2, alpha = 0.7) +  
  labs(
    title = "Average Claim Amount and Prediction by Modifications and Past defaulter",
    x = "ncd",
    y = "Average Amount",
    color = "exposure_year_",
    linetype = "Type"
  ) +
  theme_minimal()


# Aggregate data to check for interactions between age_ and modifications_
agg_data <- simul_gamma_val %>%
  group_by(age_, modifications_) %>%
  summarise(
    mean_claim = mean(claim_amount),
    mean_pred = mean(prediction_XGB),  # Use XGB predictions instead of GLM
    .groups = "drop"
  )

# Create the interaction plot
ggplot(agg_data, aes(x = age_, color = as.factor(modifications_))) +
  geom_line(aes(y = mean_claim, linetype = "Observed"), linewidth = 1) +  
  geom_point(aes(y = mean_claim), size = 2) +  
  geom_line(aes(y = mean_pred, linetype = "Predicted"), linewidth = 1, alpha = 0.7) +  
  geom_point(aes(y = mean_pred), size = 2, alpha = 0.7) +  
  labs(
    title = "Average Claim Amount and Prediction by Age & Modifications",
    x = "Age",
    y = "Average Amount",
    color = "Modifications",
    linetype = "Type"
  ) +
  theme_minimal()

### NCD and car power 

agg_data <- simul_gamma_val %>%
  group_by(NCD_level_, acceleration_levels_last_year_) %>%
  summarise(
    mean_claim = mean(claim_amount),
    mean_pred = mean(prediction_XGB),  # Use XGB predictions instead of GLM
    .groups = "drop"
  )

# Create the interaction plot
ggplot(agg_data, aes(x = NCD_level_, color = as.factor(acceleration_levels_last_year_))) +
  geom_line(aes(y = mean_claim, linetype = "Observed"), linewidth = 1) +  
  geom_point(aes(y = mean_claim), size = 2) +  
  geom_line(aes(y = mean_pred, linetype = "Predicted"), linewidth = 1, alpha = 0.7) +  
  geom_point(aes(y = mean_pred), size = 2, alpha = 0.7) +  
  labs(
    title = "Average Claim Amount and Prediction by NCD & car Acceleration",
    x = "NCD",
    y = "Average Amount",
    color = "Acceleration",
    linetype = "Type"
  ) +
  theme_minimal()

### traffic fines and ncd levels 

agg_data <- simul_gamma_val %>%
  group_by(age_of_car_, car_power_) %>%
  summarise(
    mean_claim = mean(claim_amount),
    mean_pred = mean(prediction_XGB),  # Use XGB predictions instead of GLM
    .groups = "drop"
  )

# Create the interaction plot
ggplot(agg_data, aes(x = age_of_car_, color = as.factor(car_power_))) +
  geom_line(aes(y = mean_claim, linetype = "Observed"), linewidth = 1) +  
  geom_point(aes(y = mean_claim), size = 2) +  
  geom_line(aes(y = mean_pred, linetype = "Predicted"), linewidth = 1, alpha = 0.7) +  
  geom_point(aes(y = mean_pred), size = 2, alpha = 0.7) +  
  labs(
    title = "Average Claim Amount and Prediction by NCD & age of car",
    x = "Age of car",
    y = "Average Amount",
    color = "car_power_",
    linetype = "Type"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# note: this code was written with assistance from Chatgpt
# ------------------------------------------------------------

ggplot(agg_data, aes(x = occupation_, color = gender_, group = gender_)) +
  geom_line(aes(y = mean_claim, linetype = "Observed"), linewidth = 1) +  
  geom_point(aes(y = mean_claim), size = 2) +  
  geom_line(aes(y = mean_pred, linetype = "Predicted"), linewidth = 1, alpha = 0.7) +  
  geom_point(aes(y = mean_pred), size = 2, alpha = 0.7) +  
  labs(
    title = "Average Claim Amount and Prediction by Occupation",
    x = "Occupation",
    y = "Average Amount",
    color = "Gender",
    linetype = "Type"
  ) +
  theme_minimal()


###age of car 
# Train set severity by age_of_car_ using XGB predictions
plot_table <- simul_gamma_train %>%
  group_by(age_of_car_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction_XGB) / sum(claim_count_) )

p1 <- plot_sev(plot_table, "age_of_car_", "Train Severity by Age of Car", "XGB")

# Validation set severity by age_of_car_ using XGB predictions
plot_table <- simul_gamma_val %>%
  group_by(age_of_car_) %>%
  summarize(obs = sum(claim_amount) / sum(claim_count_), pred = sum(prediction_XGB) / sum(claim_count_) )

p2 <- plot_sev(plot_table, "age_of_car_", "Validation Severity by Age of Car", "XGB")

# Create a bar chart of total claim counts for training set grouped by age_of_car_
plot_data <- simul_gamma_train %>%
  group_by(age_of_car_) %>%
  summarise(claim_count_ = sum(claim_count_))

p3 <- ggplot(plot_data, aes(x = as.factor(age_of_car_), y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.4) +
  labs(title = "Claim Count (Train)", x = "Age of Car", y = "Total Claim Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create a bar chart of total claim counts for validation set grouped by age_of_car_
plot_data <- simul_gamma_val %>%
  group_by(age_of_car_) %>%
  summarise(claim_count_ = sum(claim_count_))

p4 <- ggplot(plot_data, aes(x = as.factor(age_of_car_), y = claim_count_)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.4) +
  labs(title = "Claim Count (Validation)", x = "Age of Car", y = "Total Claim Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Arrange all plots
grid.arrange(p1, p2, p3, p4, ncol = 2)


# ------------------------------ Additional ML Techniques ------------------------------ 

# Install required packages if not already installed
if (!require(randomForest)) install.packages("randomForest")
if (!require(caret)) install.packages("caret")
if (!require(rpart)) install.packages("rpart")
if (!require(rpart.plot)) install.packages("rpart.plot")
if (!require(boot)) install.packages("boot")

library(randomForest)
library(caret)
library(rpart)
library(rpart.plot)
library(boot)

# ------------------------------ 1. Random Forest Implementation ------------------------------ 

# Function to prepare data for RF (removing non-predictive variables)
prepare_rf_data <- function(data) {
  # Remove variables not used for prediction
  excluded_rf <- c("policy_number_", "claim_count_", "claim_amount", "random_0_9_", 
                   "random_0_4_", "prediction", "null_prediction","occupation_","age","age_pwr2","age_grouped")
  # Return only columns that should be used for modeling
  return(data[, !(names(data) %in% excluded_rf)])
}

# Prepare train/validation data
rf_train_data <- prepare_rf_data(simul_gamma_train)
rf_val_data <- prepare_rf_data(simul_gamma_val)

# Set up response variable (log transform for gamma-distributed target)
y_train <- log(simul_gamma_train$claim_amount)
y_val <- log(simul_gamma_val$claim_amount)

# Train a Random Forest model
set.seed(123) # For reproducibility
rf_model <- randomForest(
  x = rf_train_data,
  y = y_train,
  ntree = 500,  # Number of trees
  mtry = floor(sqrt(ncol(rf_train_data))),  # Default for regression: sqrt of predictors
  importance = TRUE
)

# Print model summary
print(rf_model)

# Generate predictions (transform back from log scale)
simul_gamma_train$prediction_rf <- exp(predict(rf_model, rf_train_data))
simul_gamma_val$prediction_rf <- exp(predict(rf_model, rf_val_data))

# Calculate gamma deviance for RF
gamma_deviance_rf_train <- round(calculate_gamma_deviance(
  simul_gamma_train, actual_col = "claim_amount", prediction_col = "prediction_rf"), 0)
gamma_deviance_rf_val <- round(calculate_gamma_deviance(
  simul_gamma_val, actual_col = "claim_amount", prediction_col = "prediction_rf"), 0)

# Print deviance results
cat("RF Gamma Deviance (Train):", gamma_deviance_rf_train, "\n")
cat("RF Gamma Deviance (Validation):", gamma_deviance_rf_val, "\n")

# Plot variable importance
varImpPlot(rf_model, main = "Random Forest Variable Importance")

# ------------------------------ 2. Cross-Validation ------------------------------ 

# Define cross-validation function for Gamma GLM
cv_gamma_glm <- function(data, k = 5, seed = 42) {
  set.seed(seed)
  
  # Create folds
  folds <- createFolds(1:nrow(data), k = k, returnTrain = FALSE)
  cv_results <- numeric(k)
  
  # Excluded variables
  excluded_vars <- c("policy_number_", "claim_count_", "claim_amount", 
                     "random_0_9_", "random_0_4_", "age_", "age_pwr2", "age_grouped")
  all_vars <- setdiff(names(data), excluded_vars)
  formula <- as.formula(paste("claim_amount ~", paste(all_vars, collapse = " + ")))
  
  # Run k-fold CV
  for (i in 1:k) {
    # Split data
    train_data <- data[-folds[[i]], ]
    test_data <- data[folds[[i]], ]
    
    # Fit model on training data
    cv_model <- glm(formula, data = train_data, family = Gamma(link = "log"))
    
    # Predict on test data
    test_data$prediction_cv <- predict(cv_model, newdata = test_data, type = "response")
    
    # Calculate deviance
    cv_results[i] <- calculate_gamma_deviance(
      test_data, actual_col = "claim_amount", prediction_col = "prediction_cv")
  }
  
  return(list(
    fold_deviances = cv_results,
    mean_deviance = mean(cv_results),
    sd_deviance = sd(cv_results)
  ))
}

# Run CV on the combined training-validation data
cv_results <- cv_gamma_glm(simul_gamma_train_val, k = 5)

# Print CV results
cat("Cross-Validation Results:\n")
cat("Mean Gamma Deviance:", cv_results$mean_deviance, "\n")
cat("SD Gamma Deviance:", cv_results$sd_deviance, "\n")
cat("Fold Deviances:", cv_results$fold_deviances, "\n")

# ------------------------------ 3. Decision Tree with Pruning ------------------------------ 

# Train decision tree
dt_model <- rpart(
  claim_amount ~ .,
  data = simul_gamma_train[, !(names(simul_gamma_train) %in% c("policy_number_", "random_0_9_", "random_0_4_"))],
  method = "anova",  # For continuous outcome
  control = rpart.control(
    minsplit = 20,     # Minimum observations in node for split
    minbucket = 7,     # Minimum observations in terminal node
    cp = 0.001,        # Complexity parameter - low value for initial large tree
    maxdepth = 30      # Maximum depth of tree
  )
)

# Plot complexity parameter table
plotcp(dt_model)

# Find optimal CP value
optimal_cp <- dt_model$cptable[which.min(dt_model$cptable[,"xerror"]), "CP"]
cat("Optimal complexity parameter:", optimal_cp, "\n")

# Prune the tree
pruned_dt <- prune(dt_model, cp = optimal_cp)

# Plot pruned tree
rpart.plot(pruned_dt, 
           main = "Pruned Decision Tree for Claim Severity",
           extra = 101,          # Display sample size and percentages
           box.palette = "RdBu", # Color scheme
           tweak = 1.2)          # Size of text

# Generate predictions using pruned tree
simul_gamma_train$prediction_dt <- predict(pruned_dt, simul_gamma_train)
simul_gamma_val$prediction_dt <- predict(pruned_dt, simul_gamma_val)

# Calculate gamma deviance for decision tree
gamma_deviance_dt_train <- round(calculate_gamma_deviance(
  simul_gamma_train, actual_col = "claim_amount", prediction_col = "prediction_dt"), 0)
gamma_deviance_dt_val <- round(calculate_gamma_deviance(
  simul_gamma_val, actual_col = "claim_amount", prediction_col = "prediction_dt"), 0)

# Print deviance results
cat("Decision Tree Gamma Deviance (Train):", gamma_deviance_dt_train, "\n")
cat("Decision Tree Gamma Deviance (Validation):", gamma_deviance_dt_val, "\n")

# ------------------------------ 4. Bootstrapping ------------------------------ 

# Define bootstrap function for coefficient estimates
bootstrap_coefs <- function(data, indices, formula) {
  # Create bootstrap sample
  d <- data[indices, ]
  
  # Fit model
  model <- glm(formula, data = d, family = Gamma(link = "log"))
  
  # Return coefficients
  return(exp(coef(model)))  # Return exponentiated coefficients for interpretability
}

# Select important variables for a simpler model (for faster bootstrapping)
important_vars <- c("gender_", "NCD_level_", "age_cap_age_30", "age_cap65_age_", 
                    "modifications_", "drive_orientation_", "Past_defaulter_", 
                    "occupation_group", "insurer_prior_year_policy_", "exposure_year_")
bootstrap_formula <- as.formula(paste("claim_amount ~", paste(important_vars, collapse = " + ")))

# Run bootstrap (this can take time)
set.seed(123)
boot_results <- boot(
  data = simul_gamma_train_val,
  statistic = bootstrap_coefs,
  R = 1000,  # Number of bootstrap replicates
  formula = bootstrap_formula
)

# Compute bootstrap confidence intervals
boot_ci <- lapply(1:length(boot_results$t0), function(i) {
  boot.ci(boot_results, type = "perc", index = i)
})

# Create summary table with bootstrap intervals
bootstrap_summary <- data.frame(
  Variable = names(boot_results$t0),
  Estimate = boot_results$t0,
  Lower_CI = sapply(boot_ci, function(x) ifelse(is.null(x$percent), NA, x$percent[4])),
  Upper_CI = sapply(boot_ci, function(x) ifelse(is.null(x$percent), NA, x$percent[5])),
  Bootstrap_SE = apply(boot_results$t, 2, sd)
)

# Print bootstrap summary
print(bootstrap_summary)

# ------------------------------ 5. Model Comparison ------------------------------ 

# Create model comparison table
model_comparison <- data.frame(
  Model = c("Gamma GLM", "Random Forest", "Decision Tree", "XGBoost"),
  Train_Deviance = c(
    calculate_gamma_deviance(simul_gamma_train, "claim_amount", "prediction"),
    gamma_deviance_rf_train,
    gamma_deviance_dt_train,
    calculate_gamma_deviance(simul_gamma_train, "claim_amount", "prediction_XGB")  # Assuming XGB predictions exist
  ),
  Validation_Deviance = c(
    calculate_gamma_deviance(simul_gamma_val, "claim_amount", "prediction"),
    gamma_deviance_rf_val,
    gamma_deviance_dt_val,
    calculate_gamma_deviance(simul_gamma_val, "claim_amount", "prediction_XGB")  # Assuming XGB predictions exist
  )
)

model_comparison$Percent_Improvement <- round(
  (model_comparison$Validation_Deviance[1] - model_comparison$Validation_Deviance) / 
    model_comparison$Validation_Deviance[1] * 100, 2)

# Print comparison table
print(model_comparison)

# ------------------------------ 6. Visualize Model Comparisons ------------------------------ 

# Create long-format data for visualization
results_long <- simul_gamma_val %>%
  select(claim_amount, prediction, prediction_rf, prediction_dt, prediction_XGB) %>%
  rename(
    "Gamma GLM" = prediction,
    "Random Forest" = prediction_rf,
    "Decision Tree" = prediction_dt,
    "XGBoost" = prediction_XGB
  ) %>%
  pivot_longer(
    cols = -claim_amount,
    names_to = "Model",
    values_to = "Prediction"
  )

# Plot predicted vs actual values by model
ggplot(results_long, aes(x = claim_amount, y = Prediction, color = Model)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~ Model) +
  labs(
    title = "Predicted vs Actual Claim Amounts by Model",
    x = "Actual Claim Amount",
    y = "Predicted Claim Amount"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, quantile(simul_gamma_val$claim_amount, 0.95)), 
                  ylim = c(0, quantile(simul_gamma_val$claim_amount, 0.95)))

# Plot model performance by variable (e.g., occupation_group)
plot_model_performance <- function(data, x_var, models = c("prediction", "prediction_rf", "prediction_dt", "prediction_XGB")) {
  # Prepare data
  data_agg <- data %>%
    group_by(!!sym(x_var)) %>%
    summarize(
      Observed = mean(claim_amount),
      "Gamma GLM" = mean(prediction),
      "Random Forest" = mean(prediction_rf),
      "Decision Tree" = mean(prediction_dt),
      "XGBoost" = mean(prediction_XGB),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = c("Observed", "Gamma GLM", "Random Forest", "Decision Tree", "XGBoost"),
      names_to = "Model",
      values_to = "Mean_Claim"
    )
  
  # Create plot
  ggplot(data_agg, aes(x = !!sym(x_var), y = Mean_Claim, color = Model, group = Model)) +
    geom_line() +
    geom_point() +
    labs(
      title = paste("Model Performance by", x_var),
      x = x_var,
      y = "Mean Claim Amount"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create performance plots for key variables
plot_model_performance(simul_gamma_val, "occupation_")
plot_model_performance(simul_gamma_val, "age_")
plot_model_performance(simul_gamma_val, "insurer_prior_year_policy_")