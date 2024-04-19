install.packages("lmtest")
library(lmtest)

setwd("G:/My Drive/post-phd/PreFer/data/training_data")
df <- read.csv("PreFer_train_data.csv")
y <- read.csv("PreFer_train_outcome.csv")


setwd('G:/My Drive/post-phd/PreFer/fertility-prediction-challenge')
source("submission.R")

df = clean_df(df)
df <- merge(df, y, by = "nomem_encr")

missings_count <- list()
for (i in 1:ncol(df)){missings_count[[i]] <- sum(is.na(df[,i]))}
cbind(colnames(df), unlist(missings_count))

missing <- unlist(lapply(1:nrow(df), function(i){sum(is.na(df[i,]))}))>0
df = df[!missing,]

m0 <- glm(new_child~1, data = df, family = 'binomial')
model <- glm(new_child ~ gender_bg + age + age^2 + edu + migration_background_bg + nettoink_f_2020 + 
               relationship_status + number_of_children + child_under_5 + relationship_satisfaction + #family status
               student + employed + self_employed +  #employment status
               future_children + #family plans
               church_attendence + pray + gender_values + marital_values +
               chronic_illness + relationship_mother , data = df, family = 'binomial')
summary(model)
lrtest(m0, model) 
#delta chi-sq
#367 without impute
#531 with impute

lpm <- lm(new_child ~ gender_bg + age + age^2 + edu + migration_background_bg + nettoink_f_2020 + 
               relationship_status + number_of_children + child_under_5 + relationship_satisfaction + #family status
               student + employed + self_employed +  #employment status
               future_children + #family plans
               church_attendence + pray + gender_values + marital_values +
               chronic_illness + relationship_mother , data = df)
summary(lpm)

saveRDS(model, file = 'model.rds')


