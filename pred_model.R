library(ISLR)
library(dplyr)
df <- read.csv("data/Airlines.csv")


#########3 Validation set ##################
set.seed(123)

df_sample <- sample(nrow(df), 0.05*nrow(df))
df_sample <- df[df_sample,]

train <- sample(nrow(df_sample), 0.7*nrow(df_sample))# 
train_df <- df_sample[train,] %>% 
  mutate(DayOfWeek = as.character(DayOfWeek))
test_df <- df_sample[-train,] %>% 
  mutate(DayOfWeek = as.character(DayOfWeek))





############# logistic regresison ##################33
fit_glm_log <- glm(Delay~., data = train_df, family = binomial)
# CV
summary(fit_glm_log)
test_pred <- predict(fit_glm_log, newdata = test_df, type = "response")


test_pred[test_pred > 0.45] <- 1
test_pred[test_pred <= 0.45] <- 0


test_df$pred <- test_pred


table(test_df$Delay, test_df$pred)
library(boot)
cv_log <- cv.glm(train_df,fit_glm_log, K = 10) # without the K it would be a loocv
cv_log$delta # cv error - that many percent were wrong



########### random forest ###################

library(randomForest)
set.seed(123)
fit_rf <- randomForest(as.factor(Delay)~., data=train_df %>% dplyr::select(-id,
                                                                           -Flight),
                       mtry=3,
                       importance =TRUE,
                       type = "classification")
test_pred = predict(fit_rf ,newdata=test_df, type = "prob")

test_pred <- test_pred[, 2]

test_pred[test_pred > 0.5] <- 1
test_pred[test_pred <= 0.5] <- 0

test_df$pred <- test_pred


table(test_df$Delay, test_df$pred)

# Investigating variable importance 
importance(fit_rf)
# Two measures of variable importance are reported:
# 1) The first is based upon the mean *decrease of accuracy*
# in predictions on the out of bag samples when a given variable 
# is excluded from the model.
# 2) THe second is a measure of the total *decrease in node impurity* 
# that results from splits over that variable, averaged over all trees.
varImpPlot(fit_rf)


saveRDS(fit_rf, "rf_model.rds")
