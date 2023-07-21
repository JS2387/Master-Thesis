rm(list = ls())

#install.packages("rlang")

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
library(equatiomatic)
library(lattice)
library(rlang)

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


###DONT USE##################################
#train-test split
# train  <- df1[df1['Season_num'] <= 12,]
# test   <- df1[!df1['Season_num'] <= 12,]
# 
# colnames(train)
# 
# lmm.model <- lmer(Price ~ Season_num + WK + Overseas_player + Star_player + Active_T20I + 
#                     Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
#                     Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_LY_Bowl_I + 
#                     Cum_LY_Bowl_Econ + Cum_LY_Bowl_Avg + Cum_LY_Bowl._SR + Cum_LY_BW.Taker + 
#                     Cum_LY_bowl_46_conc + Cum_LY_bowl_Dots. + Cum_IPL_bat_DM_Bat_I + 
#                     Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
#                     Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW + 
#                     Cum_IPL_bowl_DM_Bowl_I + Cum_IPL_bowl_DM_Bowl_Econ + Cum_IPL_bowl_DM_Bowl_Avg + 
#                     Cum_IPL_bowl_DM_Bowl._SR + Cum_IPL_bowl_DM_BW.Taker + Cum_IPL_bowl_DM_46_conc + 
#                     Cum_IPL_bowl_DM_Dots. + 
#                     (1 + 
#                        (Cum_LY_Bat_avg + 
#                           Cum_LY_HardHitter + 
#                           Cum_LY_46_scored| Name2) + 
#                        (Cum_LY_Bowl_Avg + 
#                           Cum_LY_Bowl_Econ + 
#                           Cum_IPL_bowl_DM_46_conc| Name2)), 
#                   data = train,
#                   REML = FALSE)
# 
# summary(lmm.model)
# ranef(lmm.model)
# random_effects <- ranef(lmm.model)$Name2[test$Name2, ]
# 
# # Make predictions for test data using estimated random effects
# predictions <- predict(lmm.model, 
#                        newdata = test, 
#                        re.form = ~(1 + (Cum_LY_Bat_avg + 
#                                           Cum_LY_HardHitter | Name2) +
#                                              (Cum_LY_Bowl_Avg + 
#                                                 Cum_LY_Bowl_Econ | Name2)),
#                        allow.new.levels = TRUE)
# 
# 
# pred_check <- data.frame(Name = test$Name2, actual = test$Price, prediction = predictions)
# 
# result <- data.frame(Model = 'LMM-Derived stats - no weights',
#                      RMSE = rmse(test$Price, predictions),
#                      MAPE = mape(test$Price, predictions))
# 
# ############################################

#batsmen only
colnames(df1)
df2 <- df1[df1$Type == 'Bat' | df1$Type == 'WK', c(1,2,5:16,24:30)]
colnames(df2)

#scaling
df2[,c(8:21)] <- scale(df2[,c(8:21)], center=TRUE, scale=TRUE)

#check correlations
corr_df <- cor(df2[,7:20])
# Create a correlation heatmap
corrplot(corr_df, type = "upper", order = "hclust", tl.cex = 0.7,
         method = "color", addCoef.col = "black", number.cex = 0.7, 
         tl.col = "black", diag = FALSE, tl.srt = 45, 
         title = "Correlation Matrix of Batsmen with Derived Performance Metrics", 
         mar = c(0, 0, 2, 0), width = 10, height = 10)

#train-test split
train  <- df2[df2['Season_num'] <= 12, c(3:21)]
test   <- df2[!df2['Season_num'] <= 12, c(3:21)]

set.seed(12345)

#baseline lm model
lm.model <- lm(Price ~ ., data = train)
summary(lm.model, digits = 4)

extract_eq(lm.model)


Anova(lm.model)

predictions <- predict(lm.model, newdata = test)

residuals <- resid(lm.model)
plot(predict(lm.model), residuals, xlab = "Predicted values", ylab = "Residuals")

pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE = ape(test$Price, predictions))

result <- data.frame(Model = 'Simple LM-Batsmen: With Intercept',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lm.model),
                     BIC = BIC(lm.model),
                     LL = logLik(lm.model),
                     DIC = NA)

results_df <- result


#restricted LM
anova(lm.model)
lm.model.res <- lm(Price ~ Overseas_player + Star_player + Active_T20I + Cum_LY_HardHitter + Cum_LY_46_scored + Cum_LY_RBW +
                 Cum_IPL_bat_DM_Bat_I + Cum_IPL_bat_DM_Finisher.,
                 data = train)
summary(lm.model.res)
predictions <- predict(lm.model.res, newdata = test)

residuals <- resid(lm.model.res)
plot(predict(lm.model.res), residuals, xlab = "Predicted values", ylab = "Residuals")

pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE = ape(test$Price, predictions))

result <- data.frame(Model = 'Restricted Simple LM-Batsmen',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lm.model.res),
                     BIC = BIC(lm.model.res),
                     LL = logLik(lm.model.res),
                     DIC = NA)

results_df <- rbind(results_df, result)

extract_eq(lm.model.res, 
           wrap = TRUE, 
           terms_per_line = 2, 
           operator_location = "start", 
           use_coefs = TRUE)

#GLM only check
# 
# # set starting values for coefficients
# glm.model <- glm(Price ~ -1 + WK + Overseas_player + Star_player + Active_T20I + 
#                    Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
#                    Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
#                    Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
#                    Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW, 
#                  family = gaussian(),
#                  data = train)
# summary(glm.model)
# 
# fit <- glm(train$Price ~ 1, family = inverse.gaussian())
# summary(fit)$dispersion[1]
# 
# predictions <- predict(glm.model, newdata = test)
# 
# residuals <- resid(glm.model)
# plot(predict(glm.model), residuals, xlab = "Predicted values", ylab = "Residuals")
# 
# pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
#                          actual = test$Price, 
#                          prediction = predictions, 
#                          APE = ape(test$Price, predictions))
# 
# result <- data.frame(Model = 'Simple GLM-Batsmen:Derived stats - no weights',
#                      RMSE = rmse(test$Price, predictions),
#                      MAPE = mape(test$Price, predictions))
# 
# results_df <- rbind(results_df, result)




#LMM

#train-test split
train  <- df2[df2['Season_num'] <= 12,]
test   <- df2[!df2['Season_num'] <= 12,]

colnames(train)

set.seed(12345)
lmm.model <- lmer(Price ~ Season_num + WK + Overseas_player + Star_player + Active_T20I + 
                    Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                    Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                    Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                    Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW + 
                    (1 | Name2), 
                  data = train,
                  REML = FALSE)
                  #verbose = TRUE)

# Get the summary of the model
summary(lmm.model)
Anova(lmm.model)
# 
# # Create a scatter plot of residuals against predicted values
# ggplot(data = data.frame(pred = fitted(lmm.model), resid = residuals(lmm.model)),
#        aes(x = pred, y = resid)) +
#   geom_point() +
#   xlab("Predicted values") +
#   ylab("Residuals") +
#   ggtitle("Residuals plot with Batsmen - LMM") +
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

result <- data.frame(Model = 'LMM-Bat: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lmm.model),
                     BIC = BIC(lmm.model),
                     LL = logLik(lmm.model),
                     DIC = NA)

results_df <- rbind(results_df, result)


#analysis
fixef(lmm.model)
ranef(lmm.model)

# Covariance matrix of beta hat
beta_cov <- sigma(lmm.model)^2 * chol2inv(getME(lmm.model, "RX")) 

#profiling of lmm
pr.lmm <- profile(lmm.model)

#confidence intervals
confint(lmm.model)

#deviance
deviance(lmm.model)

#plot
plot(lmm.model)

plot(lmm.model, type = c("p", "smooth"))

plot(lmm.model, sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p", "smooth"))

qqmath(lmm.model, id = 0.05)

xyplot(pr.lmm)

densityplot(pr.lmm)

splom(pr.lmm)

dotplot(ranef(lmm.model, condVar = TRUE))
qqmath(ranef(lmm.model, condVar = TRUE))

#LMM-Restricted
lmm.model.res.1 <- lmer(Price ~ Season_num + Overseas_player + Star_player + Active_T20I +  
                    Cum_LY_HardHitter + Cum_LY_46_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I +  
                    Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_50plus_scored +
                    #(1 | Name2),
                    (1 + Star_player + Cum_IPL_bat_DM_Bat_I | Name2), 
                  data = train,
                  REML = FALSE)

# Get the summary of the model
summary(lmm.model.res.1)


# Make predictions for test data using estimated random effects
predictions <- predict(lmm.model.res.1, 
                       newdata = test, 
                       #re.form = ~(1 | Name2),
                       re.form = ~(1 + Star_player + Cum_IPL_bat_DM_Bat_I| Name2),
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

#Star_player + Cum_IPL_bat_DM_Bat_I

result <- data.frame(Model = 'Restricted LMM-Bat: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lmm.model.res.1),
                     BIC = BIC(lmm.model.res.1),
                     LL = logLik(lmm.model.res.1),
                     DIC = NA)

results_df <- rbind(results_df, result)


extract_eq(lmm.model.res.1, 
           wrap = TRUE, 
           terms_per_line = 2, 
           operator_location = "start", 
           use_coefs = TRUE)



#GLMM

glmm_model <- glmer(Price ~ Season_num + WK + Overseas_player + Star_player + Active_T20I + 
                      Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                      Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                      Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                      Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW + 
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

result <- data.frame(Model = 'GLMM-Bat: Gaussian Family: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(glmm_model),
                     BIC = BIC(glmm_model),
                     LL = logLik(glmm_model),
                     DIC = NA)

results_df <- rbind(results_df, result)



#Restricted GLMM

glmm_model <- glmer(Price ~ Season_num + Overseas_player + Star_player + Active_T20I + 
                      Cum_LY_HardHitter + Cum_LY_46_scored + Cum_LY_RBW +
                      Cum_IPL_bat_DM_Bat_I + Cum_IPL_bat_DM_50plus_scored + 
                      (1 | Name2), 
                    data = train,
                    family = "gaussian")

summary(glmm_model)
Anova(glmm_model)

# Make predictions for test data using estimated random effects
predictions <- predict(glmm_model, 
                       newdata = test, 
                       re.form = ~(1 | Name2),
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

result <- data.frame(Model = 'Restricted GLMM-Bat: Gaussian Family: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(glmm_model),
                     BIC = BIC(glmm_model),
                     LL = logLik(glmm_model),
                     DIC = NA)

results_df <- rbind(results_df, result)


#Restricted GLMM- with RE

glmm_model <- glmer(Price ~ Season_num + Overseas_player + Star_player + Active_T20I + 
                      Cum_LY_HardHitter + Cum_LY_46_scored + Cum_LY_RBW +
                      Cum_IPL_bat_DM_Bat_I + Cum_IPL_bat_DM_50plus_scored + 
                      (1 + Star_player + Cum_IPL_bat_DM_Bat_I | Name2), 
                    data = train,
                    family = "gaussian")

summary(glmm_model)
Anova(glmm_model)

# Make predictions for test data using estimated random effects
predictions <- predict(glmm_model, 
                       newdata = test, 
                       re.form = ~(1 + Star_player + Cum_IPL_bat_DM_Bat_I | Name2),
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

result <- data.frame(Model = 'Restricted GLMM-Bat: Gaussian Family: RE Star_player + Cum_IPL_bat_DM_Bat_I  + Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(glmm_model),
                     BIC = BIC(glmm_model),
                     LL = logLik(glmm_model),
                     DIC = NA)

results_df <- rbind(results_df, result)

extract_eq(glmm_model, 
           wrap = TRUE, 
           terms_per_line = 2, 
           operator_location = "start", 
           use_coefs = TRUE)


#MCMC GLMM
prior<-list(R=list(V=3, nu=0.0001), G=list(G1=list(V=3, nu=0.0001)))
set.seed(12345)
MCMCglmm_model <- MCMCglmm(fixed = Price ~ Season_num + WK + Overseas_player + Star_player + Active_T20I + 
                      Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                      Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                      Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                      Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW,
                      random = ~ Name2,
                      data = train,
                      prior = prior,
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

result <- data.frame(Model = 'MCMC GLMM-Bat: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm_model$DIC)

results_df <- rbind(results_df, result)

plot(MCMCglmm_model)

#Restricted MCMC GLMM

prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
MCMCglmm_model <- MCMCglmm(fixed = Price ~ Season_num + Overseas_player + Star_player + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                             Cum_IPL_bat_DM_Bat_I + Cum_IPL_bat_DM_50plus_scored,
                           random = ~ Name2,
                           data = train,
                           prior = prior,
                           family = "exponential",
                           verbose = FALSE,
                           nitt = 10000,
                           burnin = 200,
                           thin = 2)

summary(MCMCglmm_model)
# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm_model, 
                       newdata = test, 
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

result <- data.frame(Model = 'Restricted MCMC GLMM-Bat: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA)

results_df <- rbind(results_df, result)


#Unrestricted MCMC GLMM no prior
MCMCglmm_model <- MCMCglmm(fixed = Price ~ Season_num + WK + Overseas_player + Star_player + Active_T20I + 
                             Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                             Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                             Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                             Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW,
                           random = ~ Name2,
                           data = train,
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

result <- data.frame(Model = 'MCMC GLMM-Bat Unspecified prior: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm_model$DIC)

results_df <- rbind(results_df, result)

#Restricted MCMC GLMM no prior
MCMCglmm_model <- MCMCglmm(fixed = Price ~ 0 + Season_num + Overseas_player + Star_player + Active_T20I + 
                             Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                             Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                             Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_50plus_scored,
                           random = ~ Name2,
                           data = train,
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

result <- data.frame(Model = 'Restricted MCMC GLMM-Bat Unspecified prior: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA)

results_df <- rbind(results_df, result)


#Full MCMC GLMM no prior RE
#optional prior
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))


MCMCglmm_model <- MCMCglmm(fixed = Price ~ Season_num + WK + Overseas_player + Star_player + Active_T20I + 
                             Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                             Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                             Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                             Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW,
                           random = ~ Cum_IPL_bat_DM_Bat_I:Name2,
                           data = train,
                           #prior = prior,
                           family = "exponential",
                           verbose = FALSE,
                           nitt = 10000,
                           burnin = 200,
                           thin = 2)

summary(MCMCglmm_model)
# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm_model, 
                       newdata = test, 
                       allow.new.levels = TRUE)


pred_check <- data.frame(Name = df2[!df2['Season_num'] <= 12, 1], 
                         actual = test$Price, 
                         prediction = predictions, 
                         APE_pct = round(ape(test$Price, predictions)*100,3))

result <- data.frame(Model = 'MCMC GLMM-Bat Unspecified prior: RE Cum_IPL_bat_DM_Bat_I|Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm_model$DIC)

results_df <- rbind(results_df, result)

plot(MCMCglmm_model$Sol)
