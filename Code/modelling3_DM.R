rm(list = ls())

#read data
data <- read.csv2("IPL_auction_performance_data_DM.csv", header = TRUE, sep = ',')
colnames(data)

df <- data

#replace missing and NAs with 0
df[is.na(df)] <- 0
df[df==""] <- 0

#force numeric values
df[,c(3:53)] <- lapply(df[,c(3:53)], as.numeric)

#rescale price in millions
df[,3] <- df[,3]/10^6

#scale & center all features
df[,c(6:53)] <- scale(df[,c(6:53)], center=TRUE, scale=TRUE)

#split into bat, bowl, allrounder
df_bat <- subset(df, Key == 'Bat_I' | Key == 'WK_I' | Key == 'Bat_O' | Key == 'WK_O')
df_bowl <- subset(df, Key == 'Bowl_I' | Key == 'Bowl_O')
df_AR <- subset(df, Key == 'AR_I' | Key == 'AR_O')

df_bat <- df_bat[,c(1,3:11,18:23,30:35,42:47)]
df_bowl <- df_bowl[, c(1,3:5,12,17,24:29,36:41,48:53)]

train_bat  <- df_bat[df_bat['Season_num'] <= 12, c(2:28)]
test_bat   <- df_bat[!df_bat['Season_num'] <= 12, c(2:28)]

train_bowl  <- df_bowl[df_bowl['Season_num'] <= 12, c(2:24)]
test_bowl   <- df_bowl[!df_bowl['Season_num'] <= 12, c(2:24)]

train_AR  <- df_AR[df_AR['Season_num'] <= 12, c(2:53)]
test_AR   <- df_AR[!df_AR['Season_num'] <= 12, c(2:53)]


#run baseline LM
#batsmen
lm.bat.model <- lm(Price ~ . - 1, data = train_bat)
summary(lm.bat.model)
baseline_bat_predict = predict(lm.bat.model, test_bat)
library(Metrics)
#calculate RMSE
baseline_RMSE_bat = rmse(test_bat$Price, baseline_bat_predict)
baseline_bat_MAPE = mape(test_bat$Price, baseline_bat_predict)

#bowlers
lm.bowl.model <- lm(Price ~ . - 1, data = train_bowl)
summary(lm.bowl.model)
baseline_bowl_predict = predict(lm.bowl.model, test_bowl)
#calculate RMSE
baseline_RMSE_bowl = rmse(test_bowl$Price, baseline_bowl_predict)
baseline_bowl_MAPE = mape(test_bowl$Price, baseline_bowl_predict)

#Allrounders
lm.AR.model <- lm(Price ~ . - 1, data = train_AR)
summary(lm.AR.model)
baseline_AR_predict = predict(lm.AR.model, test_AR)
#calculate RMSE
baseline_RMSE_AR = rmse(test_AR$Price, baseline_AR_predict)
baseline_AR_MAPE = mape(test_AR$Price, baseline_AR_predict)

#lmm
library(lme4)
df_lmm <- df[,c(1:3,5:53)]
df_lmm_train <- df_lmm[df_lmm$Season_num <= 12, c(2:52)]
df_lmm_test <- df_lmm[!df_lmm$Season_num <= 12, c(2:52)]

lmm.model <- lmer(Price ~ . - 1 + (1|Key), data = df_lmm_train)
summary(lmm.model)

lmm_predictions <- predict(lmm.model, newdata = df_lmm_test)

lmm_RMSE = rmse(df_lmm_test$Price, lmm_predictions)
lmm_MAPE = mape(df_lmm_test$Price, lmm_predictions)

results_df <- data.frame(Predictions = lmm_predictions, Actuals = df_lmm_test$Price)
results_df['AbsErrPct'] = round(abs(results_df$Actuals - results_df$Predictions)/results_df$Actuals*100,3)

