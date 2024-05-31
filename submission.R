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
library(dplyr)
library(mice)


clean_df <- function(df, background_df = NULL, imputation = T){
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
  # Pre-process due to original coding of missing values -----------------------

  # Recode missing values in number of alive children with 0
  df$cf20m455[is.na(df$cf20m455)] <- 0
  df$cf19l455[is.na(df$cf19l455)] <- 0

  # Birth year of 1st, 2nd, etc. children
  children_age <- c("cf20m456", "cf20m457", "cf20m458", "cf20m459", "cf20m460", "cf20m461", "cf20m462")

  # Create an age of youngest children variable
  df$age_youngest_child <- NA
  for (i in 1:length(children_age)) {
    df$age_youngest_child[!is.na(df[children_age[i]])] <- 2024 - df[children_age[i]][!is.na(df[children_age[i]])]
  }
  df$age_youngest_child[is.na(df$age_youngest_child)] <- 0
  
  #structural zero's in relationship status
  cbind(df$cf17j024[is.na(df$cf20m024)], df$cf18k024[is.na(df$cf20m024)], df$cf19l024[is.na(df$cf20m024)])
  
  df$cf20m024[is.na(df$cf20m024)] <- df$cf19l024[is.na(df$cf20m024)]
  #df$cf20m024[is.na(df$cf20m024)] <- df$cf18k024[is.na(df$cf20m024)]
  #df$cf20m024[is.na(df$cf20m024)] <- df$cf17j024[is.na(df$cf20m024)]
  df$cf20m025[df$cf20m024==2] <- 2
  df$cf20m030[df$cf20m024==2] <- 2
  
  #relationship status
  df$relationship_status = NA ##is.na default
  df$relationship_status[df$cf20m024 == 2] <- "no_partner"
  df$relationship_status[df$cf20m024 == 1] <- "live_apart"
  df$relationship_status[df$cf20m025 == 1] <- "cohabit"
  df$relationship_status[df$cf20m030 == 1 | df$burgstat_2020 == 1] <- "married"
  #df$relationship_status[1] <- 'cohabit'
  df$relationship_status <- as.factor(df$relationship_status)
  
  
  table(df$relationship_status, useNA= 'ifany')
  
  #NA in mother availability
  table(df$cf20m011, useNA='ifany')  
  cbind(df$cf17j011[is.na(df$cf20m011)], df$cf18k011[is.na(df$cf20m011)], df$cf19l011[is.na(df$cf20m011)])
  df$cf20m011[is.na(df$cf20m011)] <- df$cf19l011[is.na(df$cf20m011)]
  df$cf20m011[is.na(df$cf20m011)] <- df$cf18k011[is.na(df$cf20m011)]
  df$cf20m011[is.na(df$cf20m011)] <- df$cf17j011[is.na(df$cf20m011)]
  
  #NA in generalized trust 
  table(df$cp20l019, useNA='ifany')
  cbind(df$cp17i019[is.na(df$cp20l019)], df$cp18j019[is.na(df$cp20l019)], df$cp19k019[is.na(df$cp20l019)])
  df$cp20l019[is.na(df$cp20l019)] <- df$cp19k019[is.na(df$cp20l019)]
  df$cp20l019[is.na(df$cp20l019)] <- df$cp18j019[is.na(df$cp20l019)]
  df$cp20l019[is.na(df$cp20l019)] <- df$cp17i019[is.na(df$cp20l019)]
  

  # Create a categorical variable for relationship satisfaction
  df$relationship_satisfaction <- NA
  df$relationship_satisfaction[df$cf20m180 <= 5] <- "onvoldoende"
  df$relationship_satisfaction[df$cf20m180 > 5 & df$cf20m180 <= 7] <- "voldoende"
  df$relationship_satisfaction[df$cf20m180 > 7] <- "goed"
  df$relationship_satisfaction[is.na(df$cf20m180)] <- "no partner"
  df$relationship_satisfaction <- as.factor(df$relationship_satisfaction)

  # Recode fertility intention weird values
  df$cf20m130[which(df$cf20m130 == 2025)] <- 5

  # Make fertility intention categorical variable
  df$future_children <- "no"
  df$future_children[df$cf20m128 == 3] <- "dont know"
  df$future_children[df$cf20m130 <= 1] <- "next year"
  df$future_children[df$cf20m130 > 1 & df$cf20m130 <= 5] <- "next 5 years"
  df$future_children[df$cf20m130 > 5] <- "more than 5 years"
  df$future_children <- factor(df$future_children, levels = c("no", "next year", "next 5 years", "more than 5 years", "dont know"))

  # Define names of all variables we want to keep for future processing --------

  # Created variables
  vars_created <- c(
    "age_youngest_child", 
    "relationship_status", 
    "relationship_satisfaction",
    "future_children"
  )

  # Background variables
  bg_variables <- c(
    id = "nomem_encr",
    gender = "gender_bg",
    birthyear_bg = "birthyear_bg",
    migration_background_bg = "migration_background_bg"
  )

  # Yearly variables
  vars_2020 <- c(
    edu = "cw20m005",
    relationship_status_1 = "cf20m024",
    #relationship_status_2 = "cf20m025",
    #relationship_status_3 = "cf20m030",
    #relationship_status_4 = "burgstat_2020",
    number_of_children = "cf20m455",
    # birth_of_child_1 = "cf20m456",
    # birth_of_child_1 = "cf20m457",
    # birth_of_child_1 = "cf20m458",
    # birth_of_child_1 = "cf20m459",
    # birth_of_child_1 = "cf20m460",
    # birth_of_child_1 = "cf20m461",
    # birth_of_child_1 = "cf20m462",
    # relationship_satisfaction = "cf20m180",
    employment = "belbezig_2020",
    nettoink_f_2020 = "nettoink_f_2020",
    # future_children = "cf20m128",
    church_attendence = "cr20m041",
    pray = "cr20m042",
    childrearing_values_1 = "cv20l151",
    childrearing_values_2 = "cv20l152",
    childrearing_values_3 = "cv20l153",
    childrearing_values_4 = "cv20l154",
    # children_plan = "cf20m130",
    marital_values_1 = "cv20l124",
    marital_values_2 = "cv20l125",
    marital_values_3 = "cv20l126",
    marital_values_4 = "cv20l127",
    marital_values_5 = "cv20l128",
    marital_values_6 = "cv20l129",
    marital_values_7 = "cv20l130",
    anxiety_1 = "ch20m011",
    anxiety_1 = "ch20m012",
    anxiety_1 = "ch20m013",
    anxiety_1 = "ch20m014",
    chronic_illness = "ch20m018",
    mother_alive = "cf20m011",
    trust = "cp20l019"
  )

  # Yearly variables
  vars_2019 <- c(
    # relationship_status = "cf19l024",
    # number_of_children = "cf19l455",
    # birth_of_child_1 = "cf19l456",
    # birth_of_child_1 = "cf19l457",
    # birth_of_child_1 = "cf19l458",
    # birth_of_child_1 = "cf19l459",
    # birth_of_child_1 = "cf19l460",
    # birth_of_child_1 = "cf19l461",
    # birth_of_child_1 = "cf19l462",
    # relationship_satisfaction = "cf19l180",
    employment = "belbezig_2019",
    nettoink_f_2020 = "nettoink_f_2019",
    # future_children = "cf19l128", # badly coded variable: it seems that response "don't know" was assigned NA
    church_attendence = "cr19l041",
    pray = "cr19l042",
    childrearing_values_1 = "cv19k151",
    childrearing_values_2 = "cv19k152",
    childrearing_values_3 = "cv19k153",
    childrearing_values_4 = "cv19k154",
    marital_values_1 = "cv19k124",
    marital_values_2 = "cv19k125",
    marital_values_3 = "cv19k126",
    marital_values_4 = "cv19k127",
    marital_values_5 = "cv19k128",
    marital_values_6 = "cv19k129",
    marital_values_7 = "cv19k130",
    anxiety_1 = "ch19l011",
    anxiety_1 = "ch19l012",
    anxiety_1 = "ch19l013",
    anxiety_1 = "ch19l014",
    chronic_illness = "ch19l018",
    mother_alive = "cf19l011",
    trust = "cp19k019"
  )

  # Keep only people that were asked a question in 2020
  #df_2020 <- df[!is.na(df$cf20m_m), ]

  # Keep only the variables we want
  df_2020 <- df[, c(bg_variables, vars_2020, vars_2019, vars_created)]

  # Correct variable type based
  var_types <- data.frame(
    Name = c(bg_variables, vars_2020, vars_2019, vars_created),
    Type = rep("numeric", ncol(df_2020))
  )

  # Make unordered factors
  var_types[var_types$Name %in% c(
    "gender_bg",
    "migration_background_bg",
    "cw20m005",
    "relationship_status",
    #"cf20m024", 
    #"cf20m025", 
    #"cf20m030",
    #"burgstat_2020",
    "belbezig_2020",
    "belbezig_2019",
    "relationship_satisfaction",
    "future_children", 
    "mother_alive"
  ), "Type"] <- "factor"

  # Transform to factor variables that should be treated as such
  for(j in 1:ncol(df_2020)){
    if(var_types$Type[j] == "factor"){
      df_2020[, j] <- as.factor(df_2020[, j])
    }
    if (var_types$Type[j] == "numeric") {
      df_2020[, j] <- as.numeric(df_2020[, j])
    }
  }

  # Perform imputation ---------------------------------------------------------
  print("start imputation")
  
  impute <- function(df){
    tryCatch({
      
      # Define methods
      imp_methods <- mice::make.method(df_2020)
      
      # Quick pred
      predmat <- mice::quickpred(df_2020)
      
      # Perform single imputation
      imps <- mice::mice(
        data = df_2020,
        m = 1,
        maxit = 80,
        method = imp_methods,
        predictorMatrix = predmat,
        printFlag = FALSE
      )
      
      # Extract imputed data
      df <- mice::complete(imps, 1)
      
      print("imputation finished")
      return(df)
      
    }, error = function(e){
      message("multiple imputation failed")
      return(df)
    }
    )
  }
  
  if (imputation ==T){ df <- impute(df) }

  
  
  # Process imputed data -------------------------------------------------------
  
  # predictors 2020
  # individual characteristics
  
  #gender
  table(is.na(df$gender_bg))
  df$gender_bg <- as.factor(df$gender_bg)
  
  #age
  df$age = 2024 - df$birthyear_bg
  df$age[is.na(df$age)] <- mean(df$age, na.rm=T)
  table(is.na(df$age))

  #educational level
  df$cw20m005 <- as.numeric(df$cw20m005)
  df$edu <- "unknown"
  df$edu[df$cw20m005<=11] <- "vmbo"
  df$edu[df$cw20m005>11 & df$cw20m005<=15] <- "havo-vwo"
  df$edu[df$cw20m005>15 & df$cw20m005<= 17] <- "mbo"
  df$edu[df$cw20m005>17 & df$cw20m005<= 24] <- "bachelor"
  df$edu[df$cw20m005>24] <- "master"
  df$edu <- as.factor(df$edu)
  table(df$edu, useNA= 'ifany')
  
  #etnicity
  df$migration_background_bg <- as.numeric(df$migration_background_bg)
  df$migration_background_bg[df$migration_background_bg==0] <- 1 ##is.na default
  df$migration_background_bg[df$migration_background_bg==101] <- 2
  df$migration_background_bg[df$migration_background_bg==102] <- 3
  df$migration_background_bg[df$migration_background_bg==201] <- 4
  df$migration_background_bg[df$migration_background_bg==202] <- 5
  df$migration_background_bg[is.na(df$migration_background_bg)] <- 1
  
  df$migration_background_bg <- as.factor(df$migration_background_bg)
  table(df$migration_background_bg, useNA= 'ifany')
  
  #relationship status
  df$relationship_status <- as.factor(df$relationship_status)
  
  #number of children
  df$cf20m455[is.na(df$cf20m455)] <- 0 ##is.na default
  df$cf20m455[df$cf20m455>=3] <- '3+'
  df$number_of_children <- as.factor(df$cf20m455)
  table(df$number_of_children, useNA= 'ifany')
  
  #age youngest child
  vars <- c("cf20m456", "cf20m457", "cf20m458", "cf20m459", "cf20m460", "cf20m461", "cf20m462")
  df$child_under_5 <- as.numeric(df$age_youngest_child<=5)
  df$child_under_5[is.na(df$child_under_5)] <- 0 ##is.na default
  table(df$child_under_5, useNA= 'ifany')
  
  #relationship satisfaction
  table(df$relationship_satisfaction, useNA= 'ifany')
  
  #employement status
  table(df$belbezig_2020, useNA= 'ifany')
  df$student = as.numeric(df$belbezig_2020==7)
  df$self_employed = as.numeric(df$belbezig_2020 == 3)
  df$employed = as.numeric(df$belbezig_2020 %in% 1:2)
  df$student[is.na(df$student)] <- 0 ##is.na default
  df$self_employed[is.na(df$self_employed)] <- 0 ##is.na default

  #household income
  table(is.na(df$nettoink_f_2020))
  df$nettoink_f_2020[is.na(df$nettoink_f_2020)] <- mean(df$nettoink_f_2020, na.rm=T)
  
  #fertility intentions
  table(df$future_children, useNA= 'ifany')
  
  #traditional values
  #church attendance / prayer
  df$cr20m041[is.na(df$cr20m041)] <- 6
  df$church_attendence <- as.factor(df$cr20m041)
  table(df$church_attendence, useNA= 'ifany')
  
  df$cr20m042[is.na(df$cr20m042)] <- 6
  df$pray <- as.factor(df$cr20m042)
  table(df$pray, useNA= 'ifany')
  
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
  df$anxiety <- factor.scores(df[,c("ch20m011", "ch20m012", "ch20m013", "ch20m014")], fa_3,  missing = T, impute=T)$scores
  
  #chronic illness
  df$ch20m018 = abs(df$ch20m018 - 2 )
  df$ch20m018[is.na(df$ch20m018)] <- 0
  df$chronic_illness <- as.factor(df$ch20m018)
  table(df$chronic_illness, useNA='ifany')
  
  #relationship mother
  #NOTE: availability is alive (cf20m011) + distance (cf20m400),  
  #distance has too many missings.. 
  df$mother_alive = df$cf20m011
  df$mother_alive[is.na(df$mother_alive)] <- 1
  table(df$mother_alive, useNA='ifany')  

  #generalized trust
  df$trust = df$cp20l019
  df$trust[is.na(df$trust)] <- mean(df$trust, na.rm=T)
  table(df$trust, useNA='ifany') 
  
  
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
               'anxiety',
               'mother_alive', 
               'trust')  

  ## Keeping data with variables selected
  df <- df[ , keepcols ]
  
  print("missing values:")
  print(cbind(colnames(df), unlist(lapply(1:ncol(df), function(i){sum(is.na(df[,i]))}))))
  
  #factors to dummies
  df <- model.matrix( 
    ~ nomem_encr + gender_bg + age + age^2 + edu + migration_background_bg + nettoink_f_2020 + 
      relationship_status + number_of_children + child_under_5 + relationship_satisfaction + #family status
      student + employed + self_employed +  #employment status
      future_children + #family plans
      church_attendence + pray + gender_values + marital_values +
      chronic_illness + mother_alive + trust, 
    data = df
  )
  
  #normalize continuous vars 
  vars = c(
    'age', 
    'nettoink_f_2020',
    'gender_values', 
    'marital_values',
    'trust')
  df[,vars] <- apply(df[,vars], 2, function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  })
  
  df <- as.data.frame(df)
  vars <- c('eduhavo-vwo', "relationship_satisfactionno partner", 'future_childrennext year', "future_childrennext 5 years", "future_childrenmore than 5 years", "future_childrendont know", "number_of_children3+")
  colnames(df)[colnames(df) %in% vars] <- c('eduhavovwo', "relationship_satisfactionnopartner", 'future_childrennextyear', "future_childrennext5years", "future_childrenmorethan5years", "future_childrendontknow", "number_of_children3")
  

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
  
  # missing variables
  missing_vars <- colnames(model$data)[!colnames(model$data) %in% colnames(df)]
  if (length(missing_vars)!=0){
    print("variables missing in dataframe:")
    print(missing_vars)
    m = as.data.frame(matrix(rep(0, nrow(df)*length(missing_vars)), nrow=nrow(df), ncol=length(missing_vars)))
    m[1,] = rep(1, length(missing_vars))
    colnames(m) <- missing_vars
    df = cbind(df, m)
  }
  
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
