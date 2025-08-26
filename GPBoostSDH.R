#Load required packages
library(readxl)
library(data.table)
library(mltools)
library(dplyr)
library(caret)
library(pROC)
library(gpboost)
library(fastDummies)
library(pROC)
library(SHAPforxgboost)
library(gpboost)

#Load datasets
#Dog information
dog_profile <- read.csv("~/insert_location/dog_profile.csv")
dog_profile <- as.data.frame(dog_profile)

#Data from US census based upon owners zip code
census_info <- read_excel("~/insert_location/home_address_Social_determinants.xlsx")
census_info <- as.data.frame(census_info)

#Lifestyle Data
lifestyle <- read_excel("~/insert_location/lifestyle.xlsx")
lifestyle <- as.data.frame(lifestyle)
#For lifestyle the following changers were made
#Grouped by primary and secondary lifestyles
#Grouped into categories as follows:
#Companion pet: includes those that did not list a specific purpose of their golden
#Agility and Performance sports: Agility, Dock diving, dog athlete, scent work, tracking, field trials, fishing dog, hunt test, obedience
#Hunting:
#Breeder:  Includes retired breeder
#Conformation: Show
#Therapy dog: includes training for therapy dog
#working dog: Search and rescue, service dog, ambassador, demo dog, blood donor
#Multiple if combinations of above categories

#Home details --> years were clearly reported, i.e. 2007, so calculated age of home within Excel first before importing.
home_details <-("~/insert_location/home_details.xlsx")
home_details <- as.data.frame(home_details)

#Load imputed dataset used for BIMM forest Activity study.  See manuscript for more details: https://link.springer.com/article/10.1186/s44356-025-00024-5
complete_yr3_7 <- read_excel("~/insert_location/completedatayr3_7.xlsx")
complete_yr3_7 <-as.data.frame(complete_yr3_7)

#Now merge datasets together using subject_id, year_in_study, and/or record_datae
dog_profile_lifestyle <- merge(x = dog_profile,
                               y = lifestyle,
                               by = 'subject_id',
                               all = TRUE)

#Merge with census
dog_profile_census <- merge(x = dog_profile_lifestyle,
                            y = census_info,
                            by.x = c('subject_id', 'year_in_study', 'record_date'),
                            by.y = c('subject_id', 'year_in_study', 'record_date'),
                            all = TRUE)


#Merge with home details
profile_lifestyle_home <- merge(x = dog_profile_census,
                                y = home_details,
                                by.x = c('subject_id', 'year_in_study', 'record_date'),
                                by.y = c('subject_id', 'year_in_study', 'record_date'),
                                all = TRUE)


#Subset and Focus just on primary lifestyles first and primary residence information
primaryinfo <- subset(profile_lifestyle_home, 
                      is_primary.x == 1 & #This selects primary dog lifestyle
                        is_primary.y == 1) #This selects the primary home information

df_sdh <- merge(x = primaryinfo,
                y = complete_yr3_7,
                by.x = c('subject_id', 'year_in_study'),
                by.y = c('subject_id', 'year_in_study'),
                all.x = TRUE)
str(df_sdh)

#Subset the datasets by imputation
set1 = subset(df_sdh, .imp ==1)
set2= subset(df_sdh, .imp ==2)
set3= subset(df_sdh, .imp ==3)
set4= subset(df_sdh, .imp ==4)
set5= subset(df_sdh, .imp ==5)

#This repository only includes code for the first imputation. Any users of this code must update the code if they wish to use it for multiple imputations.

#Make sure data is unordered factors before dummy encoding variables (otherwise throws errors)
df1_sdh <- dummy_cols(set1, select_columns = c("Lifestyle_Category", "PRIMSTATE", "area_type", "water", "house_type"), 
                      remove_first_dummy = FALSE, remove_selected_columns = FALSE)

#Keep desired data only
df1_sdh_subset <- df1_sdh %>% dplyr::select(-record_date, -is_primary.x, -is_primary.y, - lifestyle_other_specify,
                                            -sex_status.y, -source, -PRIMSTATE_NA, -area_type_NA, -water_NA,
                                            -SECONDADD, -SECPCT, -SECZIP, -S1901_MedianIncome2, -S1901_MeanIncome2,
                                            -S2701_NoHealthCareCoverage2, -S1501_BachelorsDegree...25, -S1501_HighSchool2,
                                            -DP05_Hispanic_Latino2, -DP05_Asian2, -DP05_White2, -DP05_Black2, -DP05_AmericanIndian_Alaskan2,
                                            -DP05_OtherRace2, -DP05_Hawaiin2, - SecondaryHome_incomestatsus, -heating_fuel_secondary,
                                            -.id, -therapy_visit_other_specify, -therapy_visit_other, -therapy_visit_hospital,
                                            -therapy_visit_library, -therapy_visit_nursing_home, - therapy_visit_school,
                                            -total_time_units, -water_o, -SECSTATE, -.imp, -house_type_NA)

#Check structure/column names
str(df1_sdh_subset)
colnames(df1_sdh_subset)

#Group the states
# List of cold state columns
# Define cold states as column names (strings)
cold_states <- c("PRIMSTATE_CO", "PRIMSTATE_CT", "PRIMSTATE_IA", "PRIMSTATE_ID", "PRIMSTATE_MA",
                 "PRIMSTATE_ME", "PRIMSTATE_MI", "PRIMSTATE_MN", "PRIMSTATE_MT", "PRIMSTATE_ND",
                 "PRIMSTATE_NH", "PRIMSTATE_NY", "PRIMSTATE_OR", "PRIMSTATE_PA", "PRIMSTATE_RI",
                 "PRIMSTATE_SD", "PRIMSTATE_VT", "PRIMSTATE_WA", "PRIMSTATE_WI", "PRIMSTATE_WY", "PRIMSTATE_AK")

mild_states <- c("PRIMSTATE_DE", "PRIMSTATE_IL", "PRIMSTATE_IN", "PRIMSTATE_KS", "PRIMSTATE_KY",
                 "PRIMSTATE_MD", "PRIMSTATE_MO", "PRIMSTATE_NC", "PRIMSTATE_NE", "PRIMSTATE_NJ", "PRIMSTATE_NM",
                 "PRIMSTATE_NV", "PRIMSTATE_OH", "PRIMSTATE_TN", "PRIMSTATE_UT", "PRIMSTATE_VA", "PRIMSTATE_WASH DC",
                 "PRIMSTATE_WV")
hot_states <- c("PRIMSTATE_AL", "PRIMSTATE_AR", "PRIMSTATE_AZ", "PRIMSTATE_CA", "PRIMSTATE_FL",
                "PRIMSTATE_GA", "PRIMSTATE_LA", "PRIMSTATE_MS", "PRIMSTATE_OK", "PRIMSTATE_SC", "PRIMSTATE_TX")

# Create the new temp columns
df1_sdh_subset$cold_temps <- ifelse(rowSums(df1_sdh_subset[, cold_states]) > 0, 1, 0)
df1_sdh_subset$mild_temps <- ifelse(rowSums(df1_sdh_subset[, mild_states]) > 0, 1, 0)
df1_sdh_subset$hot_temps <- ifelse(rowSums(df1_sdh_subset[, hot_states]) > 0, 1, 0)

all_states <- c(cold_states, mild_states, hot_states)


df1_sdh_subset <- df1_sdh_subset %>% 
  dplyr::select(-S1901_MeanIncome, -PRIMZIP, -PRIMSTATE, -PrimaryHome_incomestatus, -area_type, -water, -water_filtration, - Lifestyle_Category_Multiple,
                heating_fuel_primary, -lifestyle, -Lifestyle_Category, -birth_date, -sex_status.x, -heating_fuel_primary, -enrolled_date, -house_type,
                -all_of(all_states))

# Assess if missing data and inspect rows
rows_with_na <- df1_sdh_subset[!complete.cases(df1_sdh_subset), ]
rows_with_na
#Shows that we need to drop out years 0-2 where additional questions were not asked.
df1_sdh_nonas <- df1_sdh_subset[complete.cases(df1_sdh_subset), ]
str(df1_sdh_nonas) #20,355 obs of 70 variables

#Subset into cancer and no cancer groups
df1_sdh_nonas_cancer <- subset(df1_sdh_nonas, any == 1)
df1_sdh_nonas_NOcancer <- subset(df1_sdh_nonas, any == 0)

#Same with only social determinants variables
#Using the same names to allow same code to be run for these:
SDH_frequency1 <- df1_sdh_nonas %>%
  dplyr::select(subject_id,year_in_study,S1901_MedianIncome, S2701_NoHealthCareCoverage, S1501_BachelorsDegree...13,
                S1501_Highschool, DP05_AmericanIndian_Alaskan, DP05_Asian, DP05_Black, DP05_Hispanic_Latino, DP05_White,
                DP05_Hawaiin, DP05_OtherRace, age, central_ac, room_window_ac, frequency, Lifestyle_Category_Agility_Obedience_Sports,
                Lifestyle_Category_Breeder, Lifestyle_Category_Show, Lifestyle_Category_Therapy_dog, Lifestyle_Category_Companion_Pet,
                Lifestyle_Category_Hunting, Lifestyle_Category_Working, area_type_Rural, area_type_Suburban, area_type_Urban, water_Bottled,
                water_Municipal, water_Well, house_type_Apartment_Condo_Townhome, house_type_Motor_home, house_type_Single_family,
                cold_temps, mild_temps, hot_temps, any)

df1_sdh_nonas_cancer <- subset(SDH_frequency1, any == 1)
df1_sdh_nonas_NOcancer <- subset(SDH_frequency1, any == 0)

#Find the list of subject_ids per frequency and split 70% / 30% or 50% / 50%.  Base on model performance.
train_subjects_NOcancer1 <- df1_sdh_nonas_NOcancer %>%
  distinct(subject_id, frequency) %>%
  group_by(frequency) %>%
  group_split() %>%
  lapply(function(df_group) {
    n_train <- ceiling(0.5 * nrow(df_group))  # take 50 or 70% for training
    sample(df_group$subject_id, size = n_train)
  }) %>%
  unlist()

#Define your full subject IDs
all_subjects_NOcancer1 <- unique(df1_sdh_nonas_NOcancer$subject_id)

#Get test_subjects as all others not in training dataset
test_subjects_NOcancer1 <- setdiff(all_subjects_NOcancer1, train_subjects_NOcancer1)

#Now create the actual training and testing datasets
train_data_NOcancer1 <- df1_sdh_nonas_NOcancer %>%
  filter(subject_id %in% train_subjects_NOcancer1)

test_data_NOcancer1 <- df1_sdh_nonas_NOcancer %>%
  filter(subject_id %in% test_subjects_NOcancer1)

#Separate x and y variables
trainx_nocancer <- train_data_NOcancer1 %>%
  dplyr::select(-frequency, -subject_id)

trainy_nocancer <- train_data_NOcancer1$frequency  # Convert to vector

testx_nocancer <- test_data_NOcancer1 %>%
  dplyr::select(-frequency, -subject_id)

testy_nocancer <- test_data_NOcancer1$frequency  # Convert to vector

# Create GPBoost model
gp_model <- GPModel(
  likelihood = "poisson",
  group_data = train_data_NOcancer1$subject_id,
  cluster_ids = train_data_NOcancer1$subject_id
)

# Define parameters
params <- list(
  learning_rate = 0.01,
  min_data_in_leaf = 20,
  max_depth = 20,
  num_leaves = 100,
  lambda_l2 = 0,
  max_bin = 250,
  line_search_step_length = TRUE
)

#Define parameters based upon gridsearch results (code for this step further below): 
#"Best parameters: learning_rate: 0.1, min_data_in_leaf: 100, max_depth: -1, num_leaves: 512, lambda_l2: 0, max_bin: 250, tree_learner: serial, line_search_step_length: TRUE"
params <- list(
  learning_rate = 0.1,
  min_data_in_leaf = 100,
  max_depth = -1,
  num_leaves = 512,
  lambda_l2 = 0,
  max_bin = 250,
  line_search_step_length = TRUE
)

# Train model (NO SCALING)
freq1Model <- gpb.Dataset(data = as.matrix(trainx_nocancer), label = trainy_nocancer)
nocancer1_freq <- gpb.train(data = freq1Model, gp_model = gp_model, nrounds = 10000, params = params, seed=19) #Adjust nrounds as needed

# Predict on test data (corrected group_data_pred)
pred_resp_freqnocancer <- predict(
  nocancer1_freq,
  data = as.matrix(testx_nocancer),
  group_data_pred = test_data_NOcancer1$subject_id,  # FIXED
  predict_var = TRUE,
  pred_latent = FALSE,  # Ensure response prediction
  cluster_ids_pred = test_data_NOcancer1$subject_id
)

# Extract response predictions
predicted <- (pred_resp_freqnocancer$response_mean) 

# Evaluate predictions, will calculate McFaddon's R2 for poisson model below after null model calculations
mse <- mean((testy_nocancer - predicted)^2)
rmse <- sqrt(mse)

cat("MSE:", mse, "\nRMSE:", rmse)

# null GP model
gp_model_null <- GPModel(
  likelihood = "poisson",
  group_data = train_data_NOcancer1$subject_id,
  cluster_ids = train_data_NOcancer1$subject_id
)

# Define Null model with no predictors
dataset_null <- gpb.Dataset(data = matrix(0, nrow = nrow(trainx_nocancer), ncol = 1), label = trainy_nocancer)

# Define hyperparameters with minimal tree boosting.
params_null <- list(
  objective = "poisson",
  learning_rate = 0.01,
  max_depth = 1,  # Min depth to avoid learning the x features
  num_leaves = 2  # Min tree
)

# Train the null model 
bst_null <- gpb.train(
  data = dataset_null,
  gp_model = gp_model_null,
  nrounds = 1,  
  params = params_null
)


# Predict from null model
pred_null <- predict(
  bst_null,
  data = matrix(1, nrow = nrow(testx_nocancer), ncol = 1),  # Intercept-only model
  group_data_pred = test_data_NOcancer1$subject_id,  # Provide group data
  cluster_ids_pred = test_data_NOcancer1$subject_id
)$response_mean


# Compute log-likelihood of the null model
logL_null <- sum(testy_nocancer * log(pred_null) - pred_null - lgamma(testy_nocancer + 1))
logL_model <- sum(testy_nocancer * log(predicted) - predicted - lgamma(testy_nocancer + 1))

# McFadden's R²
R2_McFadden <- 1 - (logL_model / logL_null)

cat("McFadden's R²:", R2_McFadden)


#Now try to evaluate likilihoods and parameters via grid search.
#Try to build better model by assessing parameters via grid search
#These should be adjusted accordng to the target variable.
param_grid <- list("learning_rate" = c(0.001, 0.01, 0.1, 1), 
                   "min_data_in_leaf" = c(1, 10, 100),
                   "max_depth" = c(-1, 1, 5, 10, 15, 20), # -1 means no depth limit as we tune 'num_leaves'
                   "num_leaves" = 2^(1:10),
                   "lambda_l2" = c(0, 1, 10, 100),
                   "max_bin" = c(250, 500, 1000, min(50,10000)),
                   "tree_learner" = c("serial", "data_parallel"),
                   "line_search_step_length" = c(TRUE, FALSE))
metric = c("average_precision", "mse") # Define metric, "mse", "average_precision", "auc" for guassian. binary_logloss

# Note: can also use metric = "test_neg_log_likelihood". For more options, see https://github.com/fabsig/GPBoost/blob/master/docs/Parameters.rst#metric-parameters
# Run parameter optimization using random grid search and k-fold CV
# Note: deterministic grid search can be done by setting 'num_try_random=NULL'
# Again, adjust as needed based upon output as well as above as noted.
opt_params_freq <- gpb.grid.search.tune.parameters(param_grid = param_grid,
                                                   data = freq1Model, gp_model = gp_model,
                                                   num_try_random = 1000, nfold = 5,
                                                   nrounds = 10, early_stopping_rounds = 20,
                                                   verbose_eval = 1, metric = metric, cv_seed = 4,
                                                   num_threads=12, seed = 18)
print(paste0("Best parameters: ", paste0(unlist(lapply(seq_along(opt_params_pace$best_params), 
                                                       function(y, n, i) { paste0(n[[i]],": ", y[[i]]) }, y=opt_params_freq$best_params, 
                                                       n=names(opt_params_freq$best_params))), collapse=", ")))
print(paste0("Best number of iterations: ", opt_params_freq$best_iter))
print(paste0("Best score: ", round(opt_params_freq$best_score, digits=3)))
#[1] "Best parameters: learning_rate: 0.1, min_data_in_leaf: 100, max_depth: -1, num_leaves: 512, lambda_l2: 0, max_bin: 250, tree_learner: serial, line_search_step_length: TRUE"
#SHAP values
# Calculate SHAP values and summary plot for the cancer model
shap_long_nocancer <- shap.prep(
  xgb_model = nocancer1_freq,
  X_train = as.matrix(trainx_nocancer),
  top_n = 15       # Select the top x most important features
)

# **SHAP summary plot**
shap.plot.summary(shap_long_nocancer, dilute = 10) 

#Some graphs to further explore the data.
shap.plot.dependence(data_long = shap_long_nocancer, x = "DP05_Asian",
                     color_feature = "age", smooth = FALSE)

shap.plot.dependence(data_long = shap_long_nocancer, x = "S2701_NoHealthCareCoverage",
                     color_feature = "DP05_Hispanic_Latino", smooth = FALSE)

###############################################################################
##Now create a model for dog's with cancer
#Find the list of subject_ids per frequency and split 70% / 30% or 50% / 50%, base upon model performance
train_subjects_cancer1 <- df1_sdh_nonas_cancer %>%
  distinct(subject_id, frequency) %>%
  group_by(frequency) %>%
  group_split() %>%
  lapply(function(df_group) {
    n_train <- ceiling(0.7 * nrow(df_group))  # take 70%
    sample(df_group$subject_id, size = n_train)
  }) %>%
  unlist()

#Define your full subject IDs
all_subjects_cancer1 <- unique(df1_sdh_nonas_cancer$subject_id)

#Get test_subjects as all others not in training
test_subjects_cancer1 <- setdiff(all_subjects_cancer1, train_subjects_cancer1)

#Now create the actual training and testing datasets
train_data_cancer1 <- df1_sdh_nonas_cancer %>%
  filter(subject_id %in% train_subjects_cancer1)

test_data_cancer1 <- df1_sdh_nonas_cancer %>%
  filter(subject_id %in% test_subjects_cancer1)

#Separate variables and target variable
trainx_cancer <- train_data_cancer1 %>%
  dplyr::select(-frequency, -subject_id)

trainy_cancer <- train_data_cancer1$frequency  # Convert to vector

testx_cancer <- test_data_cancer1 %>%
  dplyr::select(-frequency, -subject_id)

testy_cancer <- test_data_cancer1$frequency  # Convert to vector

# Create GPBoost model
gp_model <- GPModel(
  likelihood = "poisson",
  group_data = train_data_cancer1$subject_id,
  cluster_ids = train_data_cancer1$subject_id
)

# Define parameters
params <- list(
  learning_rate = 0.01,
  min_data_in_leaf = 20,
  max_depth = 20,
  num_leaves = 100,
  lambda_l2 = 0,
  max_bin = 250,
  line_search_step_length = TRUE
)

#Define parameters based upon gridsearch results: 
#Didn't replace parames as didn't beat original ones above.
# Important point as if model performance does not improve, then you should not accept the gridsearch results.
params <- list(
  learning_rate = 0.001,
  min_data_in_leaf = 1,
  max_depth = 15,
  num_leaves = 64,
  lambda_l2 = 1,
  max_bin = 500,
  line_search_step_length = TRUE
)

#Train model (NO SCALING)
freq1Modelcancer <- gpb.Dataset(data = as.matrix(trainx_cancer), label = trainy_cancer)
cancer1_freq <- gpb.train(data = freq1Modelcancer, gp_model = gp_model, nrounds = 1000, params = params) #Change number of rounds as needed.


#Predict on test data
pred_resp_freqcancer <- predict(
  cancer1_freq,
  data = as.matrix(testx_cancer),
  group_data_pred = test_data_cancer1$subject_id,  # FIXED
  predict_var = TRUE,
  pred_latent = FALSE,  # Ensure response prediction
  cluster_ids_pred = test_data_cancer1$subject_id
)


# Extract response predictions
predictedcancer <- (pred_resp_freqcancer$response_mean)  

# Evaluate predictions, will calculate McFaddon's R2 for poisson model below after null model calculations
mse <- mean((testy_cancer - predictedcancer)^2)
rmse <- sqrt(mse)

cat("MSE:", mse, "\nRMSE:", rmse)


# null GP model
gp_model_null <- GPModel(
  likelihood = "poisson",
  group_data = train_data_cancer1$subject_id,
  cluster_ids = train_data_cancer1$subject_id
)

# Define Null model with no predictors
dataset_null <- gpb.Dataset(data = matrix(0, nrow = nrow(trainx_cancer), ncol = 1), label = trainy_cancer)

# Define hyperparameters with minimal tree boosting.
params_null <- list(
  objective = "poisson",
  learning_rate = 0.01,
  max_depth = 1,  # Min depth to avoid learning the x features
  num_leaves = 2  # Min tree
)

# Train the null model 
bst_null <- gpb.train(
  data = dataset_null,
  gp_model = gp_model_null,
  nrounds = 1,  
  params = params_null
)


# Predict from null model
pred_null <- predict(
  bst_null,
  data = matrix(1, nrow = nrow(testx_cancer), ncol = 1),  # Intercept-only model
  group_data_pred = test_data_cancer1$subject_id,  # Provide group data
  cluster_ids_pred = test_data_cancer1$subject_id
)$response_mean


# Compute log-likelihood of the null model
logL_null <- sum(testy_cancer * log(pred_null) - pred_null - lgamma(testy_cancer + 1))
logL_model <- sum(testy_cancer * log(predictedcancer) - predictedcancer - lgamma(testy_cancer + 1))

# McFadden's R²
R2_McFadden <- 1 - (logL_model / logL_null)

cat("McFadden's R²:", R2_McFadden)

#Now try to evaluate likilihoods and parameters via grid search.
#Try to build better model by assessing parameters via grid search
param_grid <- list("learning_rate" = c(0.001, 0.01, 0.1, 1), 
                   "min_data_in_leaf" = c(1, 10, 100),
                   "max_depth" = c(-1, 1, 5, 10, 15, 20), # -1 means no depth limit as we tune 'num_leaves'
                   "num_leaves" = 2^(1:10),
                   "lambda_l2" = c(0, 1, 10, 100),
                   "max_bin" = c(250, 500, 1000, min(50,10000)),
                   "tree_learner" = c("serial", "data_parallel"),
                   "line_search_step_length" = c(TRUE, FALSE))
metric = c("average_precision", "mse") # Define metric, "mse", "average_precision", "auc" for guassian. binary_logloss

# Note: can also use metric = "test_neg_log_likelihood". For more options, see https://github.com/fabsig/GPBoost/blob/master/docs/Parameters.rst#metric-parameters
# Run parameter optimization using random grid search and k-fold CV
# Note: deterministic grid search can be done by setting 'num_try_random=NULL'
opt_params_freq <- gpb.grid.search.tune.parameters(param_grid = param_grid,
                                                   data = freq1Modelcancer, gp_model = gp_model,
                                                   num_try_random = 100, nfold = 5,
                                                   nrounds = 1000, early_stopping_rounds = 20,
                                                   verbose_eval = 1, metric = metric, cv_seed = 4,
                                                   num_threads=12, seed = 18)
print(paste0("Best parameters: ", paste0(unlist(lapply(seq_along(opt_params_pace$best_params), 
                                                       function(y, n, i) { paste0(n[[i]],": ", y[[i]]) }, y=opt_params_freq$best_params, 
                                                       n=names(opt_params_freq$best_params))), collapse=", ")))
print(paste0("Best number of iterations: ", opt_params_freq$best_iter))
print(paste0("Best score: ", round(opt_params_freq$best_score, digits=3)))

#SHAP values
# Calculate SHAP values and summary plot for the cancer model
shap_long_cancer <- shap.prep(
  xgb_model = cancer1_freq,
  X_train = as.matrix(trainx_cancer),
  top_n = 15       # Select the top x most important features
)

shap_values_freqcancer <- shap.values(xgb_model = cancer1_freq, X_train = as.matrix(trainx_cancer))

# **SHAP summary plot**
shap.plot.summary(shap_long_cancer, dilute = 10) 
shap_values_frequency_cancer <- shap_values_cancer$shap_score

#This code was adapted for each GPBoost Poisson model created for the manuscript.  The total code was 3,411 lines.

