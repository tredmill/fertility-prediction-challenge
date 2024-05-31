

library(lmtest)
library(neuralnet)

setwd("G:/My Drive/post-phd/PreFer/data/training_data")
df <- read.csv("PreFer_train_data.csv")
y <- read.csv("PreFer_train_outcome.csv")

setwd('G:/My Drive/post-phd/PreFer/fertility-prediction-challenge')
source("submission.R")

df = clean_df(df, imputation = T)
df <- merge(df, y, by = "nomem_encr")

m2 = neuralnet(new_child ~ gender_bg2 + age + age^2 +                       
                 eduhavovwo + edumaster + edumbo + eduvmbo + 
                 migration_background_bg2 + migration_background_bg3 + migration_background_bg4 + migration_background_bg5 + 
                 nettoink_f_2020 +
                 relationship_statuslive_apart + relationship_statusmarried + relationship_statusno_partner + 
                 number_of_children1 + number_of_children2  + number_of_children3 +child_under_5 +
                 relationship_satisfactionnopartner + relationship_satisfactiononvoldoende + relationship_satisfactionvoldoende +
                 student + employed + self_employed +
                 future_childrennext5years + future_childrenmorethan5years + future_childrendontknow +
                 church_attendence2 + church_attendence3 + church_attendence4 + church_attendence5 + church_attendence6 +
                 pray2 +pray3 + pray4 + pray5 + pray6 + gender_values + marital_values + 
                 chronic_illness1 + mother_alive + trust, 
               data=df, 
               hidden = 5, 
               stepmax = 1e+08, 
               rep = 1, 
               lifesign = "full", 
               algorithm = "rprop+", 
               err.fct = "ce", 
               linear.output = F)

m1 <- glm(new_child ~ gender_bg2 + age + age^2 +                       
            eduhavovwo + edumaster + edumbo + eduvmbo + 
            migration_background_bg2 + migration_background_bg3 + migration_background_bg4 + migration_background_bg5 + 
            nettoink_f_2020 +
            relationship_statuslive_apart + relationship_statusmarried + relationship_statusno_partner + 
            number_of_children1 + number_of_children2  + number_of_children3 +child_under_5 +
            relationship_satisfactionnopartner + relationship_satisfactiononvoldoende + relationship_satisfactionvoldoende +
            student + employed + self_employed +
            future_childrennext5years + future_childrenmorethan5years + future_childrendontknow +
            church_attendence2 + church_attendence3 + church_attendence4 + church_attendence5 + church_attendence6 +
            pray2 +pray3 + pray4 + pray5 + pray6 + gender_values + marital_values + 
            chronic_illness1 + mother_alive + trust, 
          data = df, family = 'binomial')







#evaluate
model = m2

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


setwd('G:/My Drive/post-phd/PreFer/fertility-prediction-challenge')
df <- read.csv("PreFer_fake_data.csv")
df <- clean_df(df)

colnames(df)[!colnames(df) %in% colnames(model$data)]
colnames(model$data)[!colnames(model$data) %in% colnames(df)]
x = predict_outcomes(df)
