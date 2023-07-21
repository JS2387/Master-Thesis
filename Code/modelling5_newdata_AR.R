rm(list = ls())

#install.packages("mgcv")

library(corrplot)
library(Metrics)
library(lme4)
library(ggplot2)
library(ggfortify)
library(MCMCglmm)
library(brglm)
library(bayeslm)
library(nlme)
library(gamm4)
library(mgcv)
library(car)
library(lmerTest)

#read data
data <- read.csv2("IPL_salary_new_final_data.csv", header = TRUE, sep = ',')
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

#drop first 5 seasons
df <- df[df$Season_num >= 6,]

#force numeric values
df[,c(10:125)] <- lapply(df[,c(10:125)], as.numeric)

#rescale price in millions
df[,14] <- df[,14]/10^6

#scale & center all features
#df[,c(15:125)] <- scale(df[,c(15:125)], center=TRUE, scale=TRUE)

#Covariates set: Derived performance stats, no weights
colnames(df)
df1 <- df[,c(1,3,8:14,38:51,75:88)]
colnames(df1)


#AR only
colnames(df1)
df2 <- df1[df1$Type == 'AR', c(1,2,5,7:37)]
colnames(df2)

#scaling
df2[,c(7:34)] <- scale(df2[,c(7:34)], center=TRUE, scale=TRUE)

#check correlations
corr_df <- cor(df2[,7:34])
# Create a correlation heatmap
corrplot(corr_df, type = "upper", order = "hclust", tl.cex = 0.7,
         method = "color", addCoef.col = "black", number.cex = 0.7, 
         tl.col = "black", diag = FALSE, tl.srt = 45, 
         title = "Correlation Matrix of Bowlers with Derived Performance Metrics", 
         mar = c(0, 0, 2, 0), width = 10, height = 10)

#train-test split
train  <- df2[df2['Season_num'] <= 12, c(3:34)]
test   <- df2[!df2['Season_num'] <= 12, c(3:34)]

#baseline lm model
lm.model <- lm(Price ~ ., data = train)
summary(lm.model)

Anova(lm.model)

predictions <- predict(lm.model, newdata = test)

residuals <- resid(lm.model)
plot(predict(lm.model), residuals, xlab = "Predicted values", ylab = "Residuals")

pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE = ape(test$Price, predictions))

result <- data.frame(Model = 'Simple LM-AR: With Intercept',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lm.model),
                     BIC = BIC(lm.model),
                     LL = logLik(lm.model),
                     DIC = NA)

results_df <- result


#restricted LM
anova(lm.model)
lm.model <- lm(Price ~ Overseas_player + Star_player + Active_T20I + Cum_LY_Bat_I + Cum_LY_Bowl._SR + Cum_IPL_bat_DM_Bat_I + 
                 Cum_IPL_bat_DM_Bat_avg, 
               data = train)
summary(lm.model)
predictions <- predict(lm.model, newdata = test)

residuals <- resid(lm.model)
plot(predict(lm.model), residuals, xlab = "Predicted values", ylab = "Residuals")

pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE = ape(test$Price, predictions))

result <- data.frame(Model = 'Restricted Simple LM-AR',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lm.model),
                     BIC = BIC(lm.model),
                     LL = logLik(lm.model),
                     DIC = NA)

results_df <- rbind(results_df, result)

#restricted LM2 - dont use
Anova(lm.model)
lm.model <- lm(Price ~ Star_player + Cum_LY_50plus_scored + Cum_LY_bowl_46_conc + 
                 Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bowl_DM_Bowl_Avg + Cum_IPL_bowl_DM_Bowl._SR, 
               data = train)
summary(lm.model)
predictions <- predict(lm.model, newdata = test)

residuals <- resid(lm.model)
plot(predict(lm.model), residuals, xlab = "Predicted values", ylab = "Residuals")

pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE = ape(test$Price, predictions))

result <- data.frame(Model = 'Restricted Simple LM2-AR',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lm.model),
                     BIC = BIC(lm.model),
                     LL = logLik(lm.model),
                     DIC = NA)

results_df <- rbind(results_df, result)


# GLM
# set starting values for coefficients
glm.model <- glm(Price ~ Overseas_player + Star_player + Active_T20I + Cum_LY_Bat_I + Cum_LY_Bowl._SR + Cum_IPL_bat_DM_Bat_I + 
                   Cum_IPL_bat_DM_Bat_avg,
                 family = gaussian(),
                 data = train)
summary(glm.model)

predictions <- predict(glm.model, newdata = test)

residuals <- resid(glm.model)
plot(predict(glm.model), residuals, xlab = "Predicted values", ylab = "Residuals")

pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1],
                         actual = test$Price,
                         prediction = predictions,
                         APE = ape(test$Price, predictions))

result <- data.frame(Model = 'Simple GLM-AR:Gaussian family',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(glm.model),
                     BIC = BIC(glm.model),
                     LL = logLik(glm.model),
                     DIC = NA)

results_df <- rbind(results_df, result)


#LMM
#train-test split
train  <- df2[df2['Season_num'] <= 12,]
test   <- df2[!df2['Season_num'] <= 12,]

colnames(train)

lmm.model <- lmer(Price ~ Season_num + Overseas_player + Star_player + Active_T20I + 
                    Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                    Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                    Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                    Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW +
                    Cum_LY_Bowl_I + Cum_LY_Bowl_Econ + Cum_LY_Bowl_Avg + Cum_LY_Bowl._SR +
                    Cum_LY_BW.Taker + Cum_LY_bowl_46_conc + Cum_LY_bowl_Dots. + 
                    Cum_IPL_bowl_DM_Bowl_I + Cum_IPL_bowl_DM_Bowl_Econ + Cum_IPL_bowl_DM_Bowl_Avg +
                    Cum_IPL_bowl_DM_Bowl._SR + Cum_IPL_bowl_DM_BW.Taker + Cum_IPL_bowl_DM_46_conc + Cum_IPL_bowl_DM_Dots. +
                    (1 | Name2), 
                  data = train,
                  REML = FALSE)

# Get the summary of the model
summary(lmm.model)
# 
# # Create a scatter plot of residuals against predicted values
# ggplot(data = data.frame(pred = fitted(lmm.model), resid = residuals(lmm.model)),
#        aes(x = pred, y = resid)) +
#   geom_point() +
#   xlab("Predicted values") +
#   ylab("Residuals") +
#   ggtitle("Residuals plot with Bowlers - LMM") +
#   # Add a smoother to the plot
#   geom_smooth(method = "loess", se = FALSE, color = "red")


# Make predictions for test data using estimated random effects
predictions <- predict(lmm.model, 
                       newdata = test, 
                       re.form = ~(1 | Name2),
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

result <- data.frame(Model = 'LMM-AR: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lmm.model),
                     BIC = BIC(lmm.model),
                     LL = logLik(lmm.model),
                     DIC = NA)

results_df <- rbind(results_df, result)


#LMM-Restricted
Anova(lmm.model)
lmm.model <- lmer(Price ~ Star_player + Cum_LY_50plus_scored + Cum_LY_bowl_46_conc + Cum_IPL_bowl_DM_Bowl_Avg + Cum_IPL_bowl_DM_Bowl._SR +
                    (1 + Star_player | Name2), 
                  data = train,
                  REML = FALSE)

# Get the summary of the model
summary(lmm.model)


# Make predictions for test data using estimated random effects
predictions <- predict(lmm.model, 
                       newdata = test, 
                       re.form = ~(1 + Cum_LY_50plus_scored | Name2),
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

result <- data.frame(Model = 'Restricted LMM-Bowl: RE Cum_LY_50plus_scored Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lmm.model),
                     BIC = BIC(lmm.model),
                     LL = logLik(lmm.model),
                     DIC = NA)

results_df <- rbind(results_df, result)


#GLMM

glmm_model <- glmer(Price ~ Season_num + Overseas_player + Star_player + Active_T20I + 
                      Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                      Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                      Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                      Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW +
                      Cum_LY_Bowl_I + Cum_LY_Bowl_Econ + Cum_LY_Bowl_Avg + Cum_LY_Bowl._SR +
                      Cum_LY_BW.Taker + Cum_LY_bowl_46_conc + Cum_LY_bowl_Dots. + 
                      Cum_IPL_bowl_DM_Bowl_I + Cum_IPL_bowl_DM_Bowl_Econ + Cum_IPL_bowl_DM_Bowl_Avg +
                      Cum_IPL_bowl_DM_Bowl._SR + Cum_IPL_bowl_DM_BW.Taker + Cum_IPL_bowl_DM_46_conc + Cum_IPL_bowl_DM_Dots. +
                      (1 | Name2), 
                    data = train,
                    family = "gaussian")

summary(glmm_model)
Anova(glmm_model)
#residuals
#residuals <- resid(glmm_model)

# Create a scatter plot of residuals against predicted values
# ggplot(data = data.frame(pred = fitted(glmm_model), resid = residuals(glmm_model)),
#        aes(x = pred, y = resid)) +
#   geom_point() +
#   xlab("Predicted values") +
#   ylab("Residuals") +
#   ggtitle("Residuals plot with Batsmen - LMM") +
#   # Add a smoother to the plot
#   geom_smooth(method = "loess", se = FALSE, color = "red")

#ranef(glmm_model)
#random_effects <- ranef(lmm.model)$Name2[test$Name2, ]

# Make predictions for test data using estimated random effects
predictions <- predict(glmm_model, 
                       newdata = test, 
                       re.form = ~(1 | Name2),
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

result <- data.frame(Model = 'GLMM-AR: Gaussian Family: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(glmm_model),
                     BIC = BIC(glmm_model),
                     LL = logLik(glmm_model),
                     DIC = NA)

results_df <- rbind(results_df, result)



#Restricted GLMM

glmm_model <- glmer(Price ~ Star_player + Cum_LY_50plus_scored + Cum_IPL_bat_DM_HardHitter + 
                      Cum_IPL_bat_DM_Bat_avg + Cum_LY_Bowl_I + Cum_LY_Bowl_Econ + Cum_LY_bowl_46_conc + 
                      Cum_IPL_bowl_DM_Bowl_Avg + Cum_IPL_bowl_DM_Bowl._SR +
                      (1 + Star_player + Cum_IPL_bat_DM_HardHitter | Name2), 
                    data = train,
                    family = "gaussian")

summary(glmm_model)
Anova(glmm_model)

# Make predictions for test data using estimated random effects
predictions <- predict(glmm_model, 
                       newdata = test, 
                       re.form = ~(1 + Star_player + Cum_IPL_bat_DM_HardHitter | Name2),
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

result <- data.frame(Model = 'Restricted GLMM-AR: Gaussian Family: RE Star_player Cum_IPL_bat_DM_HardHitter Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(glmm_model),
                     BIC = BIC(glmm_model),
                     LL = logLik(glmm_model),
                     DIC = NA)

results_df <- rbind(results_df, result)



#MCMC GLMM
#prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
MCMCglmm_model <- MCMCglmm(fixed = Price ~ Season_num + Overseas_player + Star_player + Active_T20I + 
                             Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                             Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                             Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                             Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW +
                             Cum_LY_Bowl_I + Cum_LY_Bowl_Econ + Cum_LY_Bowl_Avg + Cum_LY_Bowl._SR +
                             Cum_LY_BW.Taker + Cum_LY_bowl_46_conc + Cum_LY_bowl_Dots. + 
                             Cum_IPL_bowl_DM_Bowl_I + Cum_IPL_bowl_DM_Bowl_Econ + Cum_IPL_bowl_DM_Bowl_Avg +
                             Cum_IPL_bowl_DM_Bowl._SR + Cum_IPL_bowl_DM_BW.Taker + Cum_IPL_bowl_DM_46_conc + Cum_IPL_bowl_DM_Dots.,
                           random = ~ Name2,
                           data = train,
                           #prior = prior,
                           family = "exponential",
                           verbose = FALSE,
                           nitt = 10000,
                           burnin = 200,
                           thin = 2)

summary(MCMCglmm_model)
#anova(MCMCglmm_model)
# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm_model, 
                       newdata = test, 
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

result <- data.frame(Model = 'MCMC GLMM-AR unspecified prior exponential Family: RE Name2',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm_model$DIC)

results_df <- rbind(results_df, result)


# Restricted MCMC GLMM with gaussian
#prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
MCMCglmm_model <- MCMCglmm(fixed = Price ~ Star_player + Cum_LY_50plus_scored +  
                             Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Bat_avg + 
                             Cum_LY_Bowl_I + Cum_LY_Bowl_Econ + Cum_LY_bowl_46_conc + Cum_IPL_bowl_DM_Bowl_Avg +
                             Cum_IPL_bowl_DM_Bowl._SR,
                           random = ~ Name2,
                           data = train,
                           #prior = prior,
                           family = "gaussian",
                           verbose = FALSE,
                           nitt = 10000,
                           burnin = 200,
                           thin = 2)

summary(MCMCglmm_model)
#anova(MCMCglmm_model)
# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm_model, 
                       newdata = test, 
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

result <- data.frame(Model = 'Restricted MCMC GLMM-AR Unspecified prior gaussian Family: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm_model$DIC)

results_df <- rbind(results_df, result)


# Restricted MCMC GLMM with exponential
#prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
MCMCglmm_model <- MCMCglmm(fixed = Price ~ Star_player + Cum_LY_50plus_scored +  
                             Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_46_scored + Cum_IPL_bat_DM_RBW + 
                             Cum_LY_bowl_46_conc + Cum_IPL_bowl_DM_Bowl_Avg + Cum_IPL_bowl_DM_Bowl._SR,
                           random = ~ Name2,
                           data = train,
                           #prior = prior,
                           family = "exponential",
                           verbose = FALSE,
                           nitt = 10000,
                           burnin = 200,
                           thin = 2)

summary(MCMCglmm_model)
#anova(MCMCglmm_model)
# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm_model, 
                       newdata = test, 
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

result <- data.frame(Model = 'Restricted MCMC GLMM-AR Unspecified prior exponential Family: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm_model$DIC)

results_df <- rbind(results_df, result)



#Full params MCMC GLMM exponential with RE
#prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
set.seed(12345)
MCMCglmm_model <- MCMCglmm(fixed = Price ~ Season_num + Overseas_player + Star_player + Active_T20I + 
                             Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                             Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                             Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                             Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW +
                             Cum_LY_Bowl_I + Cum_LY_Bowl_Econ + Cum_LY_Bowl_Avg + Cum_LY_Bowl._SR +
                             Cum_LY_BW.Taker + Cum_LY_bowl_46_conc + Cum_LY_bowl_Dots. + 
                             Cum_IPL_bowl_DM_Bowl_I + Cum_IPL_bowl_DM_Bowl_Econ + Cum_IPL_bowl_DM_Bowl_Avg +
                             Cum_IPL_bowl_DM_Bowl._SR + Cum_IPL_bowl_DM_BW.Taker + Cum_IPL_bowl_DM_46_conc + Cum_IPL_bowl_DM_Dots.,
                           random = ~ Cum_LY_50plus_scored:Name2,
                           data = train,
                           #prior = prior,
                           family = "exponential",
                           verbose = FALSE,
                           nitt = 100000,
                           burnin = 5000,
                           thin = 10)

summary(MCMCglmm_model)
#anova(MCMCglmm_model)
# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm_model, 
                       newdata = test, 
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

result <- data.frame(Model = 'MCMC GLMM-AR unspecified prior exponential Family: RE Cum_LY_50plus_scored:Name2',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm_model$DIC)

results_df <- rbind(results_df, result)

plot(MCMCglmm_model$VCV)

