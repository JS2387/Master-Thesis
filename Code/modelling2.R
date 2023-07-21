#read data
data <- read.csv2("IPL_auction_performance_data.csv", header = TRUE, sep = ',')
colnames(data)

#subset to keep name, price, nationality, type
df <- data[,c(2,8:102)]

#drop missing names
df <- df[df["Name2"] != "",]

#replace missing and NAs with 0
df[is.na(df)] <- 0
df[df==""] <- 0

#force numeric values
df[,c(4:96)] <- lapply(df[,c(4:96)], as.numeric)

#rescale price in millions
df[,4] <- df[,4]/10^6

#scale & center all features
df[,c(5:96)] <- scale(df[,c(5:96)], center=TRUE, scale=TRUE)

library(fastDummies)
# encode the factor column using the dummy_cols() function
df_encoded <- dummy_cols(df, select_columns = "Nationality")
# drop the original column
df_encoded <- df_encoded[, c(1:2,4:98)]

#split into bat, bowl, allrounder
df_bat <- subset(df_encoded, Type == 'Bat' | Type == 'WK')
df_bowl <- subset(df_encoded, Type == 'Bowl')
df_AR <- subset(df_encoded, Type == 'AR')

#remove unnecessary covariates according to role
colnames(df_AR)
df_bat <- df_bat[,c(3:14,27:37,50:60,73:83,96:97)]
df_bowl <- df_bowl[,c(3,15:26,38:49,61:72,84:97)]
df_AR <- df_AR[,c(3:97)]

#test-train split
set.seed(12345)
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df_bat), replace=TRUE, prob=c(0.7,0.3))
train_bat  <- df_bat[sample, ]
test_bat   <- df_bat[!sample, ]

sample <- sample(c(TRUE, FALSE), nrow(df_bowl), replace=TRUE, prob=c(0.7,0.3))
train_bowl  <- df_bowl[sample, ]
test_bowl   <- df_bowl[!sample, ]

sample <- sample(c(TRUE, FALSE), nrow(df_AR), replace=TRUE, prob=c(0.7,0.3))
train_AR  <- df_AR[sample, ]
test_AR   <- df_AR[!sample, ]

#run baseline LM

#batsmen
lm.bat.model <- lm(Price ~ . - 1, data = train_bat)
summary(lm.bat.model)
baseline_bat_predict = predict(lm.bat.model, test_bat)
library(Metrics)
#calculate RMSE
baseline_RMSE_bat = rmse(test_bat$Price, baseline_bat_predict)

#bowlers
lm.bowl.model <- lm(Price ~ . - 1, data = train_bowl)
summary(lm.bowl.model)
baseline_bowl_predict = predict(lm.bowl.model, test_bowl)
#calculate RMSE
baseline_RMSE_bowl = rmse(test_bowl$Price, baseline_bowl_predict)

#Allrounders
lm.AR.model <- lm(Price ~ . - 1, data = train_AR)
summary(lm.AR.model)
baseline_AR_predict = predict(lm.AR.model, test_AR)
#calculate RMSE
baseline_RMSE_AR = rmse(test_AR$Price, baseline_AR_predict)

#run PCA
#Batsmen

# identify covariates
covariate_columns = c(2:47)

# perform PCA on the training data
pca <- prcomp(train_bat[, covariate_columns], scale = TRUE)
summary(pca)

# apply the same transformation to the training and test data
train_data_pca <- predict(pca, train_bat[, covariate_columns])
test_data_pca <- predict(pca, test_bat[, covariate_columns])

# fit a regression model on the transformed data
pca.lm.bat.model <- lm(train_bat$Price ~ .-1, data = data.frame(train_data_pca))
summary(pca.lm.bat.model)
# make predictions on the test data
pca_bat_predictions <- predict(pca.lm.bat.model, newdata = data.frame(test_data_pca))

#calculate RMSE
pca_RMSE_bat = rmse(test_bat$Price, pca_bat_predictions)


# find derived metrics



#run LMM
library(lme4)
df_bat_lmm <- subset(df, Type == 'Bat' | Type == 'WK')
df_bat_lmm <- df_bat_lmm[,c(1,3:14,27:37,50:60,73:83,96:97)]
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df_bat_lmm), replace=TRUE, prob=c(0.7,0.3))
train_bat_lmm  <- df_bat_lmm[sample, ]
test_bat_lmm   <- df_bat_lmm[!sample, ]
lmm.model.bat <- lmer(Price ~ . - 1 + (1|Name2), data = train_bat_lmm)
summary(lmm.model.bat)
fixef(lmm.model.bat)
lmm_bat_predictions <- predict(lmm.model.bat, newdata = test_bat_lmm)
