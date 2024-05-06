#Importing Libraries

library(dplyr)
library(ggplot2)
library(tidyr)

#Importing the Dataset
raw_data <- read.csv('Raw Data.csv')

#missing values
#Replacing blanks to NA'S

raw_data[raw_data == ""] <- NA

sum(is.na(raw_data))

cleaned_data <- na.omit(raw_data)

sum(is.na(cleaned_data))


#Replacing Column Names for better readability

colnames(cleaned_data)[colnames(cleaned_data) == 'Citizenship.Class'] <- 'Citizenship'
colnames(cleaned_data)[colnames(cleaned_data) == 'Status.of.Ownership'] <- 'Residence.Ownership'
colnames(cleaned_data)[colnames(cleaned_data) == 'Personal.Car'] <- 'Car'
colnames(cleaned_data)[colnames(cleaned_data) == 'Parent'] <- 'Living.with.Parents'


#Encoding the binary variables

cleaned_data$Spouse <- ifelse(cleaned_data$Spouse == "Living with spouse", 1, 0)
cleaned_data$Children <- ifelse(cleaned_data$Children == "Living with children", 1, 0)
cleaned_data$Living.with.Parents <- ifelse(cleaned_data$Living.with.Parents == "Living with parents", 1, 0)
cleaned_data$Full.Time.Employment <- ifelse(cleaned_data$Full.Time.Employment == "Employed full time", 1, 0)
cleaned_data$Part.Time.Employment <- ifelse(cleaned_data$Part.Time.Employment == "Employed part time", 1, 0)
cleaned_data$Student <- ifelse(cleaned_data$Student == "Student", 1, 0)
cleaned_data$Retired <- ifelse(cleaned_data$Retired == "Retired", 1, 0)
cleaned_data$Health.Insurance <- ifelse(cleaned_data$Health.Insurance == "Yes", 1, 0)
cleaned_data$US.Born <- ifelse(cleaned_data$US.Born == "Yes", 1, 0)
cleaned_data$Citizenship <- ifelse(cleaned_data$Citizenship == "Yes", 1, 0)
cleaned_data$Residence.Ownership <- ifelse(cleaned_data$Residence.Ownership == "Own", 1, 0)
cleaned_data$Car <- ifelse(cleaned_data$Car == "Yes", 1, 0)


# Gender column Encoding
cleaned_data$Gender <- factor(cleaned_data$Gender, levels = c("Female", "Male"))
gender_encoded <- model.matrix(~ Gender - 1, data = cleaned_data)
cleaned_data <- cbind(cleaned_data, gender_encoded)

# Ethnicity column Encoding 
cleaned_data$Ethnicity <- factor(cleaned_data$Ethnicity, levels = c("Asian Indian", "Chinese", "Filipino", "Korean", "Vietnamese", "Other"))
ethnicity_encoded <- model.matrix(~ Ethnicity - 1, data = cleaned_data)
cleaned_data <- cbind(cleaned_data, ethnicity_encoded)

# Marital Status column Encoding
cleaned_data$Marital.Status <- factor(cleaned_data$Marital.Status, levels = c("Single", "Living with a partner", "Married", "Other"))
marital_status_encoded <- model.matrix(~ Marital.Status - 1, data = cleaned_data)
cleaned_data <- cbind(cleaned_data, marital_status_encoded)

# Religion column Encoding 
cleaned_data$Religion <- factor(cleaned_data$Religion, levels = c("Buddhist", "Catholic", "Hindu", "Muslim", "Protestant", "Other", "None"))
religion_encoded <- model.matrix(~ Religion - 1, data = cleaned_data)
cleaned_data <- cbind(cleaned_data, religion_encoded)

# Employment Type column Encoding
cleaned_data$Employment.Type <- factor(cleaned_data$Employment.Type, levels = c("Full Time", "Part Time", "None"))
employment_type_encoded <- model.matrix(~ Employment.Type - 1, data = cleaned_data)
cleaned_data <- cbind(cleaned_data, employment_type_encoded)


#Encoding Income column

cleaned_data$Income <- with(cleaned_data, case_when(
  Income == "$0 - $9,999" ~ 10000,
  Income == "$10,000 - $19,999" ~ 20000,
  Income == "$20,000 - $29,999" ~ 30000,
  Income == "$30,000 - $39,999" ~ 40000,
  Income == "$40,000 - $49,999" ~ 50000,
  Income == "$50,000 - $59,999" ~ 60000,
  Income == "$60,000 - $69,999" ~ 70000,
  Income == "$70,000 and over" ~ 80000
))


#Encoding the Ordinal Variables

cleaned_data$English.Speaking <- as.numeric(factor(cleaned_data$English.Speaking,
                                           levels = c("Not at all", "Not well", "Well", "Very well"),
                                           ordered = TRUE))

cleaned_data$Familiarity.with.America <- as.numeric(factor(cleaned_data$Familiarity.with.America,
                                                   levels = c("Very low", "Low", "High", "Very high"),
                                                   ordered = TRUE))


cleaned_data$Familiarity.with.Ethnic.Origin <- as.numeric(factor(cleaned_data$Familiarity.with.Ethnic.Origin,
                                                         levels = c("Very low", "Low", "High", "Very high"),
                                                         ordered = TRUE))

cleaned_data$Present.Health <- as.numeric(factor(cleaned_data$Present.Health,
                                         levels = c("Poor", "Fair", "Good", "Very Good", "Excellent"),
                                         ordered = TRUE))

cleaned_data$Present.Mental.Health <- as.numeric(factor(cleaned_data$Present.Mental.Health,
                                                levels = c("Poor", "Fair", "Good", "Very Good", "Excellent"),
                                                ordered = TRUE))

cleaned_data$Satisfaction.with.Life <- as.numeric(factor(cleaned_data$Satisfaction.with.Life,
                                                 levels = c("Not at all", "Not very much", "Pretty much", "Very much"),
                                                 ordered = TRUE))



# Scaling the Income data and storing the scaling parameters
scaled_income <- scale(cleaned_data$Income, center = TRUE, scale = TRUE)
# Extracting  mean and standard deviation
original_mean_income <- attr(scaled_income, "scaled:center")
original_sd_income <- attr(scaled_income, "scaled:scale")

# Replacing the original Income column with the scaled values
cleaned_data$Income <- scaled_income[, 1]


# Scale the data and store the scaling parameters
scaled_quality_of_life <- scale(cleaned_data$Quality.of.Life, center = TRUE, scale = TRUE)
# Extracting the mean and standard deviation
original_mean_qol <- attr(scaled_quality_of_life, "scaled:center")
original_sd_qol <- attr(scaled_quality_of_life, "scaled:scale")

# Replacing the original Quality of Life column with the scaled values
cleaned_data$Quality.of.Life <- scaled_quality_of_life[, 1]

#Printing the values
print(paste("Mean for quality of life:", original_mean_qol))
print(paste("SD for quality of life:", original_sd_qol))
print(paste("Mean for income:", original_mean_income))
print(paste("SD for income:", original_sd_income))


#Creating a new data-frame, excluding unnecessary columns from the cleaned dataset
data <- cleaned_data[, -c(1, 3, 4, 6, 11, 12, 13, 14, 17)]



#Splitting the data set

library(caTools)
set.seed(123)
split <- sample.split(data$Quality.of.Life, SplitRatio = 0.80)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)



#Linear Regression model

lr <- lm(formula = Quality.of.Life ~ .,
                data = training_set)

summary(lr)


#Backward Elimination for Feature Selection

library(MASS)

#Fitting the initial model with all dependent variables

lr_initial <- lm(Quality.of.Life ~ ., data = training_set)


#Initiating Backward Elimination with a p-value of 0.05

eliminate <- function(model, p_value_threshold = 0.05) {
model_summary <- summary(model)
while(TRUE) {
    p_values <- coef(summary(model))[, "Pr(>|t|)"]
    max_p_value <- max(p_values[p_values < 1])
    if (max_p_value > p_value_threshold) {
      variable_to_remove <- names(p_values)[which.max(p_values)]
      formula_update <- as.formula(paste("Quality.of.Life ~ . -", variable_to_remove))
      model <- update(model, formula_update)
    } else {
      break
    }
  }
  return(model)
}

# Perform backward elimination

lr_final <- eliminate(lr_initial, p_value_threshold = 0.05)


# Summary of the final model after elimination

summary(lr_final)



#Fitting Linear Regression with the obtained Significant variables

lin_reg <- lm(formula = Quality.of.Life ~ Age + Income + Achieving.Ends.Meet + Duration.of.Residency +
                English.Speaking + Familiarity.with.America + Familiarity.with.Ethnic.Origin +
                Present.Mental.Health + Residence.Ownership + Car + Satisfaction.with.Life +
                GenderFemale,
              data = training_set)

summary(lin_reg)


#Predicting values on the test set

predictions_lin_reg <- predict(lin_reg, newdata = test_set)

# Evaluating the model

rmse_lin_reg <- sqrt(mean((test_set$Quality.of.Life - predictions_lin_reg)^2))

mae_lin_reg <- mean(abs(predictions_lin_reg  - test_set$Quality.of.Life))


#Printing RMSE and MAE values

print(paste("RMSE for Linear Regrerssion:", rmse_lin_reg))
print(paste("MAE for Linear Regression:", mae_lin_reg))

# Converting the predictions back to the original scale
predictions_lr_original_scale <- predictions_lin_reg * original_sd_qol + original_mean_qol

# Converting the actual scaled test set values back to the original scale
actual_lr_original_scale <- test_set$Quality.of.Life * original_sd_qol + original_mean_qol

#Visualizing the model

ggplot(data.frame(Actual = actual_lr_original_scale, Predicted = predictions_lr_original_scale), aes(x = Actual, y = Predicted)) +
  geom_point(colour = 'black') +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
  ggtitle('Actual vs Predicted (Linear Regression') +
  xlab('Actual Quality of Life') +
  ylab('Predicted Quality of Life') +
  theme_minimal()


#Support Vector Regression

library(e1071)

svr_model <- svm(Quality.of.Life ~  Age + Income + Achieving.Ends.Meet + 
                   Duration.of.Residency + English.Speaking + Familiarity.with.America +
                   Familiarity.with.Ethnic.Origin + Present.Mental.Health + 
                   Residence.Ownership + Car + Satisfaction.with.Life + GenderFemale,
                 data = training_set, 
                 type = "eps-regression",
                 kernel = "radial")

# Predicting on the test set

predictions_svr <- predict(svr_model, newdata = test_set)


# Calculating RMSE and MAE
rmse_svr <- sqrt(mean((test_set$Quality.of.Life - predictions_svr)^2))

mae_svr <- mean(abs(predictions_svr - test_set$Quality.of.Life))


#Printing RMSE and MAE values

print(paste("RMSE for SVR:", rmse_svr))
print(paste("MAE for SVR:", mae_svr))



# Converting the predictions back to the original scale
predictions_svr_original_scale <- predictions_svr * original_sd_qol + original_mean_qol

# Converting the actual scaled test set values back to the original scale
actual_svr_original_scale <- test_set$Quality.of.Life * original_sd_qol + original_mean_qol

#Visualizing
plot_data <- data.frame(Actual = actual_svr_original_scale, Predicted = predictions_svr_original_scale)


# Plotting
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = 'blue') +  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  
  labs(title = "Actual vs. Predicted Quality of Life", x = "Actual Quality of Life", y = "Predicted Quality of Life") +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5))  





#Decision Trees

library(rpart)

dt_model <- rpart(Quality.of.Life ~ Age + Income + Achieving.Ends.Meet + 
                    Duration.of.Residency + English.Speaking + Familiarity.with.America +
                    Familiarity.with.Ethnic.Origin + Present.Mental.Health + 
                    Residence.Ownership + Car + Satisfaction.with.Life + GenderFemale,
                  data = training_set,
                  method = "anova")

# Summary of the decision tree model

summary(dt_model)

#Predicting values on the test set

predictions_dt = predict(dt_model, newdata = test_set)

#Calculating RMSE and MAE

mae_dt<- mean(abs(test_set$Quality.of.Life - predictions_dt))

rmse_dt <- sqrt(mean((test_set$Quality.of.Life - predictions_dt)^2))

#Printing RMSE and MAE values

print(paste("RMSE for DT:", rmse_dt))
print(paste("MAE for DT:", mae_dt))

# Converting the predictions back to the original scale
predictions_dt_original_scale <- predictions_dt * original_sd_qol + original_mean_qol

# Converting the actual scaled test set values back to the original scale
actual_dt_original_scale <- test_set$Quality.of.Life * original_sd_qol + original_mean_qol

#Visualizing the model

ggplot(data.frame(Actual = actual_dt_original_scale, Predicted = predictions_dt_original_scale), aes(x = Actual, y = Predicted)) +
  geom_point(colour = 'darkgreen') +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  ggtitle('Actual vs Predicted (Decision Trees)') +
  xlab('Actual Quality of Life') +
  ylab('Predicted Quality of Life') +
  theme_minimal()


#Random Forest

library(randomForest)

rf_model <- randomForest(Quality.of.Life ~ Age + Income + Achieving.Ends.Meet +
                                      Duration.of.Residency + English.Speaking + Familiarity.with.America +
                                      Familiarity.with.Ethnic.Origin + Present.Mental.Health + 
                                      Residence.Ownership + Car + Satisfaction.with.Life + GenderFemale, 
                                    data = training_set, 
                        ntree=1000, mtry=4, importance=TRUE)


print(rf_model)

# Predicting values on the test set
predictions_rf <- predict(rf_model, newdata = test_set)


# Calculating RMSE and MAE
rmse_rf <- sqrt(mean((test_set$Quality.of.Life - predictions_rf)^2))

mae_rf <- mean(abs(predictions_rf - test_set$Quality.of.Life))


#Printing RMSE and MAE values

print(paste("RMSE for RF:", rmse_rf))
print(paste("MAE for RF:", mae_rf))

#Saving the Random Forest model to use in Rshiny

saveRDS(rf_model, "rf_model.rds")

# Converting the predictions back to the original scale
predictions_rf_original_scale <- predictions_rf * original_sd_qol + original_mean_qol

# Converting the actual scaled test set values back to the original scale
actual_rf_original_scale <- test_set$Quality.of.Life * original_sd_qol + original_mean_qol

#Visualization

ggplot(data.frame(Actual = actual_rf_original_scale, Predicted = predictions_rf_original_scale), aes(x = Actual, y = Predicted)) +
  geom_point(colour = 'blue') +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = 'red') +
  ggtitle('Actual vs Predicted (Random Forest Regression)') +
  xlab('Actual Quality of Life') +
  ylab('Predicted Quality of Life') +
  theme_minimal()


#Lasso Regression

library(glmnet)

x_train <- as.matrix(training_set[, c("Age", "Income", "Achieving.Ends.Meet", "Duration.of.Residency", 
                                      "English.Speaking", "Familiarity.with.America", "Familiarity.with.Ethnic.Origin", 
                                      "Present.Mental.Health", "Residence.Ownership", "Car", "Satisfaction.with.Life", 
                                      "GenderFemale")])
y_train <- training_set$Quality.of.Life

x_test <- as.matrix(test_set[, c("Age", "Income", "Achieving.Ends.Meet", "Duration.of.Residency", 
                                 "English.Speaking", "Familiarity.with.America", "Familiarity.with.Ethnic.Origin", 
                                 "Present.Mental.Health", "Residence.Ownership", "Car", "Satisfaction.with.Life", 
                                 "GenderFemale")])
y_test <- test_set$Quality.of.Life


# Fitting the Lasso model
lasso_model <- glmnet(x_train, y_train, alpha = 1)  

# Plotting the coefficient shrinkage
plot(lasso_model, xvar = "lambda", label = TRUE)


# Performing cross-validation
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
plot(cv_lasso)

# Best lambda
best_lambda <- cv_lasso$lambda.min
print(paste("Best lambda:", best_lambda))

# Making predictions
predictions_lasso <- predict(lasso_model, s = best_lambda, newx = x_test)

# Calculating RMSE and MAE
rmse_lasso <- sqrt(mean((y_test - predictions_lasso)^2))
mae_lasso <- mean(abs(y_test - predictions_lasso))

# Output
print(paste("RMSE for Lasso Regression:", rmse_lasso))
print(paste("MAE for Lasso Regression:", mae_lasso))


# Converting the predictions back to the original scale
predictions_lasso_original_scale <- predictions_lasso * original_sd_qol + original_mean_qol

# Converting the actual scaled test set values back to the original scale
actual_lasso_original_scale <- test_set$Quality.of.Life * original_sd_qol + original_mean_qol

#Visualization

ggplot(data.frame(Actual = actual_lasso_original_scale, Predicted = as.vector(predictions_lasso_original_scale) ), aes(x = Actual, y = Predicted)) +
  geom_point(colour = 'orange') +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = 'black') +
  ggtitle('Actual vs Predicted (Lasso Regression)') +
  xlab('Actual Quality of Life') +
  ylab('Predicted Quality of Life') +
  theme_minimal()



#Ridge Regression 
ridge_model <- glmnet(x_train, y_train, alpha = 0)  

# Performing cross-validation to choose the best lambda
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)
plot(cv_ridge)

# Best lambda
best_lambda_ridge <- cv_ridge$lambda.min
print(paste("Best lambda for Ridge:", best_lambda_ridge))

# Predicting using the best lambda
predictions_ridge <- predict(ridge_model, s = best_lambda_ridge, newx = x_test)

# Calculating RMSE and MAE
rmse_ridge <- sqrt(mean((y_test - predictions_ridge)^2))
mae_ridge <- mean(abs(predictions_ridge - y_test))

#Printing RMSE and MAE values
print(paste("RMSE for Ridge Regression:", rmse_ridge))
print(paste("MAE for Ridge Regression:", mae_ridge))

# Converting the predictions back to the original scale
predictions_ridge_original_scale <- predictions_ridge * original_sd_qol + original_mean_qol

# Converting the actual scaled test set values back to the original scale
actual_ridge_original_scale <- test_set$Quality.of.Life * original_sd_qol + original_mean_qol

#Visualization

ggplot(data.frame(Actual = actual_lasso_original_scale, Predicted = as.vector(predictions_ridge_original_scale)), aes(x = Actual, y = Predicted)) +
  geom_point(colour = 'orange') +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = 'blue') +
  ggtitle('Actual vs Predicted (Ridge Regression)') +
  xlab('Actual Quality of Life') +
  ylab('Predicted Quality of Life') +
  theme_minimal()


# Loading libraries
library(knitr)
library(kableExtra)

# Creating a comparison data-frame with RMSE and MAE values for the regression models
comparison_df <- data.frame(
  Model = c("Random Forest", "Ridge Regression", "Lasso Regression", "Linear Regression", "SVR", "Decision Tree"),
  RMSE = round(c(rmse_rf, rmse_ridge, rmse_lasso, rmse_lin_reg, rmse_svr, rmse_dt), 3),
  MAE = round(c(mae_rf, mae_ridge, mae_lasso, mae_lin_reg, mae_svr, mae_dt), 3)
)

rf_row <- which(comparison_df$Model == "Random Forest")

#Printing the comparison table
kable(comparison_df, caption = "Comparison of Regression Models based on RMSE and MAE", format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  row_spec(rf_row, bold = TRUE, background = "#DFF2BF") %>%
  column_spec(1, bold = TRUE) %>%
  scroll_box(width = "100%", height = "500px")



