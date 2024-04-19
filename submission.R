# This is an example script to generate the outcome variable given the input dataset.
# 
# This script should be modified to prepare your own submission that predicts 
# the outcome for the benchmark challenge by changing the clean_df and predict_outcomes function.
# 
# The predict_outcomes function takes a data frame. The return value must
# be a data frame with two columns: nomem_encr and outcome. The nomem_encr column
# should contain the nomem_encr column from the input data frame. The outcome
# column should contain the predicted outcome for each nomem_encr. The outcome
# should be 0 (no child) or 1 (having a child).
# 
# clean_df should be used to clean (preprocess) the data.
# 
# run.R can be used to test your submission.

# List your packages here. Don't forget to update packages.R!
library(psych)

clean_df <- function(df, background_df = NULL){
  # Preprocess the input dataframe to feed the model.
  ### If no cleaning is done (e.g. if all the cleaning is done in a pipeline) leave only the "return df" command

  # Parameters:
  # df (dataframe): The input dataframe containing the raw data (e.g., from PreFer_train_data.csv or PreFer_fake_data.csv).
  # background (dataframe): Optional input dataframe containing background data (e.g., from PreFer_train_background_data.csv or PreFer_fake_background_data.csv).

  # Returns:
  # data frame: The cleaned dataframe with only the necessary columns and processed variables.


  #delete respondents with missing outcome
  df = df[df$outcome_available==1,]
  gc()
  
  # predictors 2020
  # individual characteristics
  
  #gender
  table(df$gender_bg, useNA= 'ifany')
  
  #age
  df$age = 2024 - df$birthyear_bg

  #educational level
  df$edu <- "unknown"
  df$edu[df$cw20m005<=11] <- "vmbo"
  df$edu[df$cw20m005>11 & df$cw20m005<=15] <- "havo-vwo"
  df$edu[df$cw20m005>15 & df$cw20m005<= 17] <- "mbo"
  df$edu[df$cw20m005>17 & df$cw20m005<= 24] <- "bachelor"
  df$edu[df$cw20m005>24] <- "master"
  df$edu <- as.factor(df$edu)
  
  #etnicity
  df$migration_background_bg[is.na(df$migration_background_bg)] <- "unknown"
  df$migration_background_bg <- as.factor(df$migration_background_bg)
  
  #relationship status
  df$cf20m024[is.na(df$cf20m024)] <- 2
  df$cf20m025[is.na(df$cf20m025)] <- 2
  df$cf20m030[is.na(df$cf20m030)] <- 2
  
  df$relationship_status = NA
  df$relationship_status[df$cf20m024 == 2] <- "no_partner"
  df$relationship_status[df$cf20m024 == 1] <- "live_apart"
  df$relationship_status[df$cf20m025 == 1] <- "cohabit"
  df$relationship_status[df$cf20m030 == 1 | df$burgstat_2020 == 1] <- "married"
  df$relationship_status <- as.factor(df$relationship_status)
  
  #number of children
  df$cf20m455[is.na(df$cf20m455)] <- 0
  df$cf20m455[df$cf20m455>=3] <- '3+'
  df$number_of_children <- as.factor(df$cf20m455)
  
  #age youngest child
  vars <- c("cf20m456", "cf20m457", "cf20m458", "cf20m459", "cf20m460", "cf20m461", "cf20m462")
  df$age_youngest_child <- NA
  
  for (i in 1:length(vars)){
    df$age_youngest_child[!is.na(df[vars[i]])] <- 2024 - df[vars[i]][!is.na(df[vars[i]])]
  }
  
  df$child_under_5 <- as.numeric(df$age_youngest_child<=5)
  df$child_under_5[is.na(df$child_under_5)] <- 0

  #relationship satisfaction
  df$relationship_satisfaction <- NA
  df$relationship_satisfaction[df$cf20m180<=5] <- "onvoldoende"
  df$relationship_satisfaction[df$cf20m180>5 & df$cf20m180<=7] <- "voldoende"
  df$relationship_satisfaction[df$cf20m180>7 ] <- "goed"
  df$relationship_satisfaction[is.na(df$cf20m180)] <- "no partner"
  df$relationship_satisfaction <- as.factor(df$relationship_satisfaction)
  
  #employement status
  df$student = as.numeric(df$belbezig_2020==7)
  df$student[is.na(df$student)] <- 0
  df$self_employed = as.numeric(df$belbezig_2020 == 3)
  df$self_employed[is.na(df$self_employed)] <- 0
  df$employed = as.numeric(df$belbezig_2020 %in% 1:2)

  #household income
  df$nettoink_f_2020[is.na(df$nettoink_f_2020)] <- mean(df$nettoink_f_2020, na.rm=T)

  #fertility intentions
  df$future_children <- "no"
  df$future_children[df$cf20m128==3] <- "dont know"
  df$future_children[df$cf20m130<=1] <- "next year"
  df$future_children[df$cf20m130>1 & df$cf20m130<=5] <- "next 5 years"
  df$future_children[df$cf20m130>5] <- "more than 5 years"
  df$future_children <- factor(df$future_children, levels=c("no", "next year", "next 5 years", "more than 5 years", "dont know"))

  #traditional values
  #church attendance / prayer
  df$cr20m041[is.na(df$cr20m041)] <- "unknown"
  df$church_attendence <- as.factor(df$cr20m041)
  
  df$cr20m042[is.na(df$cr20m042)] <- "unknown"
  df$pray <- as.factor(df$cr20m042)
  
  #childrearing values
  fa_1 = fa(df[,c("cv20l151", "cv20l152", "cv20l153", "cv20l154")])
  df$gender_values <- factor.scores(df[,c("cv20l151", "cv20l152", "cv20l153", "cv20l154")], fa_1, missing = T, impute=T)$scores
  
  # marital values
  df$cv20l126 = 6 - df$cv20l126
  df$cv20l127 = 6 - df$cv20l127
  df$cv20l128 = 6 - df$cv20l128
  df$cv20l129 = 6 - df$cv20l129
  df$cv20l130 = 6 - df$cv20l130
  
  fa_2 = fa(df[,c("cv20l124", "cv20l125", "cv20l126", "cv20l127", "cv20l128", "cv20l129", "cv20l130")])
  df$marital_values <- factor.scores(df[,c("cv20l124", "cv20l125", "cv20l126", "cv20l127", "cv20l128", "cv20l129", "cv20l130")], fa_2, missing = T, impute=T)$scores
  
  #health
  #anxiety
  df$ch20m013 = 7 - df$ch20m013
  fa_3 <- fa(df[,c("ch20m011", "ch20m012", "ch20m013", "ch20m014")])
  df$anxiety <- factor.scores(df[,c("ch20m011", "ch20m012", "ch20m013", "ch20m014")], fa_3)$scores
  
  #chronic illness
  df$ch20m018 = abs(df$ch20m018 - 2 )
  df$ch20m018[is.na(df$ch20m018)] <- "unknown"
  df$chronic_illness <- as.factor(df$ch20m018)

  
  #relationship mother
  df$cf20m506 = 8 - df$cf20m506
  df$cf20m507 = 8 - df$cf20m507
  df$cf20m509 = 8 - df$cf20m509
  df$cf20m510 = 8 - df$cf20m510
  df$cf20m511 = 8 - df$cf20m511
  df$cf20m512 = 8 - df$cf20m512
  
  fa_4 <- fa(df[,c("cf20m504", "cf20m505", "cf20m506", "cf20m507", "cf20m508", "cf20m510", "cf20m511", "cf20m512")])
  df$relationship_mother <- factor.scores(df[,c("cf20m504", "cf20m505", "cf20m506", "cf20m507", "cf20m508", "cf20m510", "cf20m511", "cf20m512")], fa_4, missing = T, impute = T)$scores
  
  keepcols = c('nomem_encr',
               'gender_bg',
               'age', 
               'edu', 
               'migration_background_bg',
               'relationship_status',
               'number_of_children', 
               'child_under_5', 
               'relationship_satisfaction',
               'student', 
               'employed', 
               'self_employed', 
               'nettoink_f_2020',
               'future_children', 
               'church_attendence', 
               'pray', 
               'gender_values', 
               'marital_values',
               'chronic_illness', 
               'relationship_mother')  

  ## Keeping data with variables selected
  df <- df[ , keepcols ]

  return(df)
}

predict_outcomes <- function(df, background_df = NULL, model_path = "./model.rds"){
  # Generate predictions using the saved model and the input dataframe.
    
  # The predict_outcomes function accepts a dataframe as an argument
  # and returns a new dataframe with two columns: nomem_encr and
  # prediction. The nomem_encr column in the new dataframe replicates the
  # corresponding column from the input dataframe The prediction
  # column contains predictions for each corresponding nomem_encr. Each
  # prediction is represented as a binary value: '0' indicates that the
  # individual did not have a child during 2021-2023, while '1' implies that
  # they did.
  
  # Parameters:
  # df (dataframe): The data dataframe for which predictions are to be made.
  # background_df (dataframe): The background data dataframe for which predictions are to be made.
  # model_path (str): The path to the saved model file (which is the output of training.R).

  # Returns:
  # dataframe: A dataframe containing the identifiers and their corresponding predictions.
  
  ## This script contains a bare minimum working example
  if( !("nomem_encr" %in% colnames(df)) ) {
    warning("The identifier variable 'nomem_encr' should be in the dataset")
  }

  # Load the model
  model <- readRDS(model_path)
    
  # Preprocess the fake / holdout data
  df <- clean_df(df, background_df)

  # Exclude the variable nomem_encr if this variable is NOT in your model
  vars_without_id <- colnames(df)[colnames(df) != "nomem_encr"]
  
  # Generate predictions from model
  predictions <- predict(model, 
                         subset(df, select = vars_without_id), 
                         type = "response") 
  
  # Create predictions that should be 0s and 1s rather than, e.g., probabilities
  predictions <- ifelse(predictions > 0.5, 1, 0)  
  
  # Output file should be data.frame with two columns, nomem_encr and predictions
  df_predict <- data.frame("nomem_encr" = df[ , "nomem_encr" ], "prediction" = predictions)
  # Force columnnames (overrides names that may be given by `predict`)
  names(df_predict) <- c("nomem_encr", "prediction") 
  
  # Return only dataset with predictions and identifier
  return( df_predict )
}
