library(corrplot)
library(Metrics)
library(lme4)
library(ggplot2)


rm(list = ls())

#read data
data <- read.csv2("IPL_auction_performance_data_DM_CumLY_WtLY_CumIPL.csv", header = TRUE, sep = ',')
colnames(data)

#drop missing names
data <- data[data["Name2"] != "",]
#drop players that have no historical data
library(dplyr)
df <- data %>% filter(!(Cum_LY_bat_I == 0 & 
                                     Cum_LY_bowl_I == 0 & 
                                     Cum_IPL_agg_bat_I == 0 &
                                     Cum_IPL_agg_bowl_I == 0))

colnames(df)

#force numeric values
df[,c(11:126)] <- lapply(df[,c(11:126)], as.numeric)

#rescale price in millions
df[,15] <- df[,15]/10^6

#scale & center all features
df[,c(16:126)] <- scale(df[,c(16:126)], center=TRUE, scale=TRUE)



##############################################


#Covariates set1: raw performance stats, no weights
df1 <- df[,c(2,4,9,11:19,22:30,34:38,53:56,59:67,71:75)]
colnames(df1)

#################
#batsmen
df2 <- df1[df1$Type == 'Bat' | df1$Type == 'WK', c(2,4:17,27:35)]
colnames(df2)

#check correlations
corr_df <- cor(df2[,7:24])
# Create a correlation heatmap
corrplot(corr_df, type = "upper", order = "hclust", tl.cex = 0.7,
         method = "color", addCoef.col = "black", number.cex = 0.7, 
         tl.col = "black", diag = FALSE, tl.srt = 45, 
         title = "Correlation Matrix of Batsmen with Raw Performance Metrics", 
         mar = c(0, 0, 2, 0), width = 10, height = 10)


#train-test split
train  <- df2[df2['Season_num'] <= 12, c(2:24)]
test   <- df2[!df2['Season_num'] <= 12, c(2:24)]

#baseline lm model
lm.model <- lm(Price ~ . - 1, data = train)
summary(lm.model)
predictions <- predict(lm.model, newdata = test)

results_df <- data.frame(Model = 'Simple LM-Batsmen:Raw stats - no weights',
                         RMSE = rmse(test$Price, predictions),
                         MAPE = mape(test$Price, predictions))

#PCA and lm
# perform PCA on the training data
colnames(train)
covariate_columns <- c(1:4,6:23)
pca <- prcomp(train[, covariate_columns], scale = TRUE)
summary(pca)

# apply the same transformation to the training and test data
train_pca <- predict(pca, train[, covariate_columns])
test_pca <- predict(pca, test[, covariate_columns])

train_pca <- train_pca[,c(1:5)]
test_pca <- test_pca[,c(1:5)]

# fit a regression model on the transformed data
pca.model <- lm(train$Price ~ .-1, data = data.frame(train_pca))
summary(pca.model)
# make predictions on the test data
predictions <- predict(pca.model, newdata = data.frame(test_pca))
result <- data.frame(Model = 'Simple LM with PCA-Batsmen:Raw stats - no weights',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)


########################
#bowlers
colnames(df1)
df2 <- df1[df1$Type == 'Bowl', c(2,4,6:8,18:26,36:44)]

#check correlations
colnames(df2)
#check correlations
corr_df <- cor(df2[,6:23])
# Create a correlation heatmap
corrplot(corr_df, type = "upper", order = "hclust", tl.cex = 0.7,
         method = "color", addCoef.col = "black", number.cex = 0.7, 
         tl.col = "black", diag = FALSE, tl.srt = 45, 
         title = "Correlation Matrix of Bowlers with Raw Performance Metrics", 
         mar = c(0, 0, 2, 0), width = 10, height = 10)

#train-test split
train  <- df2[df2['Season_num'] <= 12, c(2:23)]
test   <- df2[!df2['Season_num'] <= 12, c(2:23)]

#baseline lm model
lm.model <- lm(Price ~ . - 1, data = train)
summary(lm.model)
predictions <- predict(lm.model, newdata = test)

result <- data.frame(Model = 'Simple LM-Bowlers:Raw stats - no weights',
                         RMSE = rmse(test$Price, predictions),
                         MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)

#PCA and lm
#perform PCA on the training data
colnames(train)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
covariate_columns <- c(1:3,5:22)
pca <- prcomp(train[, covariate_columns], scale = TRUE)
summary(pca)

# apply the same transformation to the training and test data
train_pca <- predict(pca, train[, covariate_columns])
test_pca <- predict(pca, test[, covariate_columns])

train_pca <- train_pca[,c(1:5)]
test_pca <- test_pca[,c(1:5)]

# fit a regression model on the transformed data
pca.model <- lm(train$Price ~ .-1, data = data.frame(train_pca))
summary(pca.model)
# make predictions on the test data
predictions <- predict(pca.model, newdata = data.frame(test_pca))
result <- data.frame(Model = 'Simple LM with PCA-Bowlers:Raw stats - no weights',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)


################################
#AR
colnames(df1)
df2 <- df1[df1$Type == 'AR', c(2,4,6:44)]


#check correlations
colnames(df2)
#check correlations
corr_df <- cor(df2[,6:41])
# Create a correlation heatmap
corrplot(corr_df, type = "upper", order = "hclust", tl.cex = 0.7,
         method = "color", addCoef.col = "black", number.cex = 0.7, 
         tl.col = "black", diag = FALSE, tl.srt = 45, 
         title = "Correlation Matrix of Bowlers with Raw Performance Metrics", 
         mar = c(0, 0, 2, 0), width = 10, height = 10)


#train-test split
train  <- df2[df2['Season_num'] <= 12, c(2:41)]
test   <- df2[!df2['Season_num'] <= 12, c(2:41)]

#baseline lm model
lm.model <- lm(Price ~ . - 1, data = train)
summary(lm.model)
predictions <- predict(lm.model, newdata = test)

result <- data.frame(Model = 'Simple LM-Allrounders:Raw stats - no weights',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)

#PCA and lm

# Use apply() to check for constant/zero columns
constant_cols <- apply(train, 2, function(x) all(x == 0) | length(unique(x)) == 1)
# Print the indices of constant/zero columns
which(constant_cols)


train <- train[,!constant_cols]
test <- test[, !constant_cols]

#perform PCA on the training data
colnames(train)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
covariate_columns <- c(1:3,5:39)
pca <- prcomp(train[, covariate_columns], center = TRUE, scale = TRUE)
summary(pca)

# apply the same transformation to the training and test data
train_pca <- predict(pca, train[, covariate_columns])
test_pca <- predict(pca, test[, covariate_columns])

train_pca <- train_pca[,c(1:7)]
test_pca <- test_pca[,c(1:7)]


# fit a regression model on the transformed data
pca.model <- lm(train$Price ~ .-1, data = data.frame(train_pca))
summary(pca.model)
# make predictions on the test data
predictions <- predict(pca.model, newdata = data.frame(test_pca))
result <- data.frame(Model = 'Simple LM with PCA-Allrounders:Raw stats - no weights',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)

#LMM
colnames(df)
df1 <- df[,c(2,4,9,11:19,22:30,34:38,53:56,59:67,71:75)]
colnames(df1)

#train-test split
train  <- df1[df1['Season_num'] <= 12, c(3:44)]
test   <- df1[!df1['Season_num'] <= 12, c(3:44)]

colnames(train)

lmm.model <- lmer(Price ~ . - 1 + (1|Type), data = data.frame(train), REML = FALSE)
summary(lmm.model)
ranef(lmm.model)
vcov_mat <- as.matrix(vcov(lmm.model))
corr_mat <- cor(vcov_mat)

corrplot(corr_mat, type = "upper", order = "hclust", tl.cex = 0.7,
         method = "color", addCoef.col = "black", number.cex = 0.7, 
         tl.col = "black", diag = FALSE, tl.srt = 45, 
         title = "Correlation Matrix of LMM with Raw Performance Metrics", 
         mar = c(0, 0, 2, 0), width = 10, height = 10)

predictions <- predict(lmm.model, newdata = test)


result <- data.frame(Model = 'LMM-Raw stats - no weights',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)


####################################


#Covariates set2: Derived performance stats, no weights
colnames(df)
df1 <- df[,c(2,4,9,11:15,39:52,76:89)]
colnames(df1)
#################
#batsmen
df2 <- df1[df1$Type == 'Bat' | df1$Type == 'WK', c(2,4:15,23:29)]
colnames(df2)
#check correlations
corr_df <- cor(df2[,7:20])
# Create a correlation heatmap
corrplot(corr_df, type = "upper", order = "hclust", tl.cex = 0.7,
         method = "color", addCoef.col = "black", number.cex = 0.7, 
         tl.col = "black", diag = FALSE, tl.srt = 45, 
         title = "Correlation Matrix of Batsmen with Derived Performance Metrics", 
         mar = c(0, 0, 2, 0), width = 10, height = 10)

#train-test split
train  <- df2[df2['Season_num'] <= 12, c(2:20)]
test   <- df2[!df2['Season_num'] <= 12, c(2:20)]

#baseline lm model
lm.model <- lm(Price ~ . - 1, data = train)
summary(lm.model)
predictions <- predict(lm.model, newdata = test)

residuals <- resid(lm.model)
plot(predict(lm.model), residuals, xlab = "Predicted values", ylab = "Residuals")

result <- data.frame(Model = 'Simple LM-Batsmen:Derived stats - no weights',
                         RMSE = rmse(test$Price, predictions),
                         MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)

#PCA and lm
# perform PCA on the training data
colnames(train)
covariate_columns <- c(1:4,6:19)
pca <- prcomp(train[, covariate_columns], center = TRUE, scale = TRUE)
summary(pca)

# apply the same transformation to the training and test data
train_pca <- predict(pca, train[, covariate_columns])
test_pca <- predict(pca, test[, covariate_columns])

train_pca <- train_pca[,c(1:9)]
test_pca <- test_pca[,c(1:9)]

# fit a regression model on the transformed data
pca.model <- lm(train$Price ~ .-1, data = data.frame(train_pca))
summary(pca.model)
# make predictions on the test data
predictions <- predict(pca.model, newdata = data.frame(test_pca))
result <- data.frame(Model = 'Simple LM with PCA-Batsmen:Derived stats - no weights',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)


########################
#bowlers
colnames(df1)
df2 <- df1[df1$Type == 'Bowl', c(2,4,6:8,16:22,30:36)]

#check correlations
colnames(df2)
corr_df <- cor(df2[,6:19])
corrplot(corr_df, type = "upper", order = "hclust", tl.cex = 0.7,
         method = "color", addCoef.col = "black", number.cex = 0.7, 
         tl.col = "black", diag = FALSE, tl.srt = 45, 
         title = "Correlation Matrix of Bowlers with Derived Performance Metrics", 
         mar = c(0, 0, 2, 0), width = 10, height = 10)


#train-test split
train  <- df2[df2['Season_num'] <= 12, c(2:19)]
test   <- df2[!df2['Season_num'] <= 12, c(2:19)]

#baseline lm model
lm.model <- lm(Price ~ . - 1, data = train)
summary(lm.model)
predictions <- predict(lm.model, newdata = test)

result <- data.frame(Model = 'Simple LM-Bowlers:Derived stats - no weights',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)

#PCA and lm
#perform PCA on the training data
colnames(train)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
covariate_columns <- c(1:3,5:18)
pca <- prcomp(train[, covariate_columns], scale = TRUE)
summary(pca)

# apply the same transformation to the training and test data
train_pca <- predict(pca, train[, covariate_columns])
test_pca <- predict(pca, test[, covariate_columns])

train_pca <- train_pca[,c(1:8)]
test_pca <- test_pca[,c(1:8)]

# fit a regression model on the transformed data
pca.model <- lm(train$Price ~ .-1, data = data.frame(train_pca))
summary(pca.model)
# make predictions on the test data
predictions <- predict(pca.model, newdata = data.frame(test_pca))
result <- data.frame(Model = 'Simple LM with PCA-Bowlers:Derived stats - no weights',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)


################################
#AR
colnames(df1)
df2 <- df1[df1$Type == 'AR', c(2,4,6:36)]

#check correlations
colnames(df2)
corr_df <- cor(df2[,6:33])
corrplot(corr_df, type = "upper", order = "hclust", tl.cex = 0.7,
         method = "color", addCoef.col = "black", number.cex = 0.7, 
         tl.col = "black", diag = FALSE, tl.srt = 45, 
         title = "Correlation Matrix of AR with Derived Performance Metrics", 
         mar = c(0, 0, 2, 0), width = 10, height = 10)

#train-test split
train  <- df2[df2['Season_num'] <= 12, c(2:33)]
test   <- df2[!df2['Season_num'] <= 12, c(2:33)]

#baseline lm model
lm.model <- lm(Price ~ . - 1, data = train)
summary(lm.model)
predictions <- predict(lm.model, newdata = test)

result <- data.frame(Model = 'Simple LM-Allrounders:Derived stats - no weights',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)

#PCA and lm

# Use apply() to check for constant/zero columns
constant_cols <- apply(train, 2, function(x) all(x == 0) | length(unique(x)) == 1)
# Print the indices of constant/zero columns
which(constant_cols)
train <- train[,!constant_cols]
test <- test[, !constant_cols]

#perform PCA on the training data
colnames(train)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
covariate_columns <- c(1:3,5:32)
pca <- prcomp(train[, covariate_columns], center = TRUE, scale = TRUE)
summary(pca)

# apply the same transformation to the training and test data
train_pca <- predict(pca, train[, covariate_columns])
test_pca <- predict(pca, test[, covariate_columns])

train_pca <- train_pca[,c(1:13)]
test_pca <- test_pca[,c(1:13)]

# fit a regression model on the transformed data
pca.model <- lm(train$Price ~ .-1, data = data.frame(train_pca))
summary(pca.model)
# make predictions on the test data
predictions <- predict(pca.model, newdata = data.frame(test_pca))
result <- data.frame(Model = 'Simple LM with PCA-Allrounders:Derived stats - no weights',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)


#LMM
df1 <- df[,c(2,4,9,11:15,39:52,76:89)]
colnames(df1)

#train-test split
train  <- df1[df1['Season_num'] <= 12,]
test   <- df1[!df1['Season_num'] <= 12,]

colnames(train)

lmm.model <- lmer(Price ~ . - 1 + (1|Name2), data = data.frame(train), REML = FALSE)
summary(lmm.model)

ranef(lmm.model)
vcov_mat <- as.matrix(vcov(lmm.model))
corr_mat <- cor(vcov_mat)

corrplot(corr_mat, type = "upper", order = "hclust", tl.cex = 0.7,
         method = "color", addCoef.col = "black", number.cex = 0.7, 
         tl.col = "black", diag = FALSE, tl.srt = 45, 
         title = "Correlation Matrix of LMM with Derived Performance Metrics", 
         mar = c(0, 0, 2, 0), width = 10, height = 10)

predictions <- predict(lmm.model, newdata = test)


result <- data.frame(Model = 'LMM-Derived stats - no weights',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions))

results_df <- rbind(results_df, result)


####################################


df_nonDM_wt <- df[,c(2,4,9,11:15,90:112,53:75)]
df_DM_wt <- df[,c(2,4,9,11:15,113:126,76:89)]

