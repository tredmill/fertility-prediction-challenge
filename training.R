install.packages("neuralnet")

library(lmtest)
library(neuralnet)

setwd("G:/My Drive/post-phd/PreFer/data/training_data")
df <- read.csv("PreFer_train_data.csv")
y <- read.csv("PreFer_train_outcome.csv")


setwd('G:/My Drive/post-phd/PreFer/fertility-prediction-challenge')
source("submission.R")

df = clean_df(df)
df <- merge(df, y, by = "nomem_encr")


m0 <- glm(new_child~1, data = df, family = 'binomial')
model <- glm(new_child ~ gender_bg + age + age^2 + edu + migration_background_bg + nettoink_f_2020 + 
               relationship_status + number_of_children + child_under_5 + relationship_satisfaction + #family status
               student + employed + self_employed +  #employment status
               future_children + #family plans
               church_attendence + pray + gender_values + marital_values +
               chronic_illness + relationship_mother , data = df, family = 'binomial')


#neural network
#factors to numeric
for (j in 1:ncol(df)){
  df[,j] <- as.numeric(df[,j])
}

#normalize continuous vars 
vars = c(
  'age', 
  'nettoink_f_2020',
  'gender_values', 
  'marital_values',
  'anxiety', 
  'relationship_mother')
df[,vars] <- apply(df[,vars], 2, function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
})


model = neuralnet(new_child ~ gender_bg + age + edu + migration_background_bg + nettoink_f_2020 + 
                    relationship_status + number_of_children + child_under_5 + relationship_satisfaction + #family status
                    student + employed + self_employed +  #employment status
                    future_children + #family plans
                    church_attendence + pray + gender_values + marital_values +
                    chronic_illness + relationship_mother, 
                  data=df, 
                  hidden = 5, 
                  stepmax = 1e+08, 
                  rep = 1, 
                  lifesign = "full", 
                  algorithm = "rprop+", 
                  err.fct = "ce", 
                  linear.output = F)

#evaluate
y_hat <- predict(model, df[, !colnames(df) %in% c("nomem_encr", "new_child")])
y_hat <- ifelse(y_hat >.5, 1, 0)

confusion_matrix <- table(df$new_child, y_hat)
confusion_matrix = as.data.frame(confusion_matrix)
colnames(confusion_matrix) <- c("observed", "expected", "freq")
confusion_matrix$type = c("true negative", "false negative", "false positive", "true positive")

round(sum(confusion_matrix$freq[confusion_matrix$type %in% c("true negative", "true positive")])/sum(confusion_matrix$freq),4)
#precision / noise / false positive rate
round(confusion_matrix$freq[confusion_matrix$type=="true positive"]/sum(confusion_matrix$freq[confusion_matrix$type %in% c("false positive", "true positive")]),4)
#recall /detection / true positive rate
round(confusion_matrix$freq[confusion_matrix$type=="true positive"]/sum(confusion_matrix$freq[confusion_matrix$type %in% c("false negative", "true positive")]),4)

saveRDS(model, file = 'model.rds')


