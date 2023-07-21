# FINAL CODE CLEANED - ALL ROLES

#install.packages("GGally")

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
library(GGally)

rm(list = ls())


##########SETUP#############
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
#Covariates set: Derived performance stats, no weights
colnames(df)
df1 <- df[,c(1,3,8:14,38:51,75:88)]
colnames(df1)

#########BATSMEN##############
#batsmen only
colnames(df1)
df2 <- df1[df1$Type == 'Bat' | df1$Type == 'WK', c(1,2,5:16,24:30)]
colnames(df2)
#scaling
df2[,c(8:21)] <- scale(df2[,c(8:21)], center=TRUE, scale=TRUE)
#treat categorical variables
df2[,c(3:6)] <- lapply(df2[,c(3:6)], factor)


#inspect
str(df2)
#summary(df2)
#apply(df2, 2, sd)
#length(unique(df2$Name2))

#EDA
plot_df <- df1[, c(1,2,9)]
xyplot(Price ~ Season_num | Name2, plot_df, type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[1],
       xlab = "IPL Season",
       ylab = "Player Salary", aspect = "xy")

boxplot(plot_df$Price ~ plot_df$Season_num,
        xlab = "IPL Season", # x-axis label
        ylab = "Player Salary", # y-axis label
        las = 1, # Rotate numbers on y-axis
        ylim = c(0,3.5), # Range of y-axis
        col = "lightblue", # Boxplot fill color
        border = "darkred", # Boxplot border color
        pch = 19) # Symbol for outliers)

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


#######################BASELINE


set.seed(12345)

#baseline lm model
lm1 <- lm(Price ~ ., data = train)
summary(lm1)
predictions <- predict(lm1, newdata = test)
result <- data.frame(Model = 'Simple LM-Batsmen: With Intercept',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lm1),
                     BIC = BIC(lm1),
                     LL = logLik(lm1),
                     DIC = NA)

results_df <- result
hist(resid(lm1), breaks = 40)
plot(resid(lm1) ~ fitted(lm1), pch=19, las=1); abline(0,0, col="red")

#lm with log(Price)
lm2 <- lm(log(Price) ~ ., data = train)
summary(lm2)
predictions <- predict(lm2, newdata = test)
result <- data.frame(Model = 'Simple LM-Batsmen: With Intercept: log(Price)',
                     RMSE = rmse(log(test$Price), predictions),
                     MAPE = mape(log(test$Price), predictions),
                     AIC = AIC(lm2),
                     BIC = BIC(lm2),
                     LL = logLik(lm2),
                     DIC = NA)

results_df <- rbind(results_df, result)
hist(resid(lm2), breaks = 40)
plot(resid(lm2) ~ fitted(lm2), pch=19, las=1); abline(0,0, col="red")

#GLM
glm1 <- glm(Price ~ .,
            family = Gamma,
            data = train)

summary(glm1)
cbind(confint(glm1),coef(glm1))
hist(resid(glm1), breaks = 40)
plot(resid(glm1) ~ fitted(glm1), pch=19, las=1); abline(0,0, col="red")

predictions <- predict(glm1, newdata = test)
result <- data.frame(Model = 'GLM-Batsmen',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(glm1),
                     BIC = BIC(glm1),
                     LL = logLik(glm1),
                     DIC = NA)

results_df <- rbind(results_df, result)


#restricted LM
Anova(lm1)
summary(lm1)
cbind(confint(lm1),coef(lm1))


lm3 <- lm(Price ~ Overseas_player + Star_player + Active_T20I + Cum_LY_HardHitter + Cum_LY_46_scored + Cum_LY_RBW +
                     Cum_IPL_bat_DM_Bat_I + Cum_IPL_bat_DM_Finisher.,
                   data = train)
summary(lm3)
cbind(confint(lm3),coef(lm3))
hist(resid(lm3), breaks = 40)
plot(resid(lm3) ~ fitted(lm3), pch=19, las=1); abline(0,0, col="red")

predictions <- predict(lm3, newdata = test)
result <- data.frame(Model = 'Restricted Simple LM-Batsmen',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lm3),
                     BIC = BIC(lm3),
                     LL = logLik(lm3),
                     DIC = NA)

results_df <- rbind(results_df, result)

extract_eq(lmm4, 
           wrap = TRUE, 
           terms_per_line = 2, 
           operator_location = "start", 
           use_coefs = TRUE)
###########################

#########################LMM
#train-test split
train  <- df2[df2['Season_num'] <= 12,]
test   <- df2[!df2['Season_num'] <= 12,]

set.seed(12345)
lmm1 <- lmer(Price ~ Season_num + WK + Overseas_player + Star_player + Active_T20I + 
                    Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                    Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                    Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                    Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW + 
                    (1 | Name2), 
                  data = train)
#verbose = TRUE)

# Get the summary of the model
summary(lmm1)

lmm2 <- update(lmm1, REML = FALSE)
summary(lmm2)
Anova(lmm2)
pr2 <- profile(lmm2)
confint(lmm2)

#LMM-Restricted
lmm3 <- lmer(Price ~ Season_num + Overseas_player + Star_player + Active_T20I +
                          Cum_LY_46_scored + Cum_IPL_bat_DM_Bat_I +
                          Cum_IPL_bat_DM_50plus_scored +
                          (1 | Name2),
                          #(1 + Star_player + Cum_IPL_bat_DM_Bat_I | Name2),
                        data = train,
                         REML = FALSE)

# Get the summary of the model
summary(lmm3)
Anova(lmm3)
confint(lmm3)


# Make predictions for test data using estimated random effects
predictions <- predict(lmm3, 
                       newdata = test, 
                       re.form = ~(1 | Name2),
                       #re.form = ~(1 + Star_player + Cum_IPL_bat_DM_Bat_I| Name2),
                       allow.new.levels = TRUE)

#Star_player + Cum_IPL_bat_DM_Bat_I

result <- data.frame(Model = 'Restricted LMM-Bat: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lmm3),
                     BIC = BIC(lmm3),
                     LL = logLik(lmm3),
                     DIC = NA)

results_df <- rbind(results_df, result)


#LMM-Restricted
lmm4 <- lmer(Price ~ Season_num + Overseas_player + Star_player + Active_T20I +
               Cum_LY_46_scored + Cum_IPL_bat_DM_Bat_I +
               Cum_IPL_bat_DM_50plus_scored +
             #  (1 | Name2),
             (1 + Star_player + Cum_IPL_bat_DM_Bat_I | Name2),
             data = train,
             REML = FALSE)

# Get the summary of the model
summary(lmm4)
Anova(lmm4)
#confint(lmm5)

fixef(lmm4)
vcov(lmm4)
VarCorr(lmm4)
terms(lmm4)
coef(lmm4)

# Make predictions for test data using estimated random effects
predictions <- predict(lmm4, 
                       newdata = test, 
                       #re.form = ~(1 | Name2),
                       re.form = ~(1 + Star_player + Cum_IPL_bat_DM_Bat_I| Name2),
                       allow.new.levels = TRUE)

result <- data.frame(Model = 'Restricted LMM-Bat: RE Star_player + Cum_IPL_bat_DM_Bat_I Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lmm4),
                     BIC = BIC(lmm4),
                     LL = logLik(lmm4),
                     DIC = NA)

results_df <- rbind(results_df, result)

#profiling of lmm
pr1 <- profile(lmm4)
#plot
plot(lmm4, sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p", "smooth"))
qqmath(lmm4, id = 0.05)

##############
#GLMM

glmm_model <- glmer(Price ~ Season_num + WK + Overseas_player + Star_player + Active_T20I + 
                      Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                      Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                      Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                      Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW + 
                      (1 | Name2), 
                    data = train,
                    verbose = 1,
                    family = "Gamma")

summary(glmm_model)
# Make predictions for test data using estimated random effects
predictions <- predict(lmm4, 
                       newdata = test, 
                       re.form = ~(1 | Name2),
                       #re.form = ~(1 + Star_player + Cum_IPL_bat_DM_Bat_I| Name2),
                       allow.new.levels = TRUE)

result <- data.frame(Model = 'GLMM-Bat: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(glmm_model),
                     BIC = BIC(glmm_model),
                     LL = logLik(glmm_model),
                     DIC = NA)

results_df <- rbind(results_df, result)


################################

######MCMC GLMM

set.seed(12345)
MCMCglmm1 <- MCMCglmm(fixed = Price ~ Season_num + WK + Overseas_player + Star_player + Active_T20I + 
                             Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                             Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                             Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                             Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW,
                           random = ~ Name2,
                           data = train,
                           family = "exponential",
                           verbose = FALSE,
                           nitt = 100000,
                           burnin = 5000,
                           thin = 2)

summary(MCMCglmm1)

# MCMC no intercept
MCMCglmm2 <- MCMCglmm(fixed = Price ~ 0 + Season_num + WK + Overseas_player + Star_player + Active_T20I + 
                        Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                        Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                        Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                        Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW,
                      random = ~ Name2,
                      data = train,
                      family = "exponential",
                      verbose = FALSE,
                      nitt = 100000,
                      burnin = 5000,
                      thin = 2)

summary(MCMCglmm2)



# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm2, 
                       newdata = test, 
                       allow.new.levels = TRUE)


result <- data.frame(Model = 'MCMC GLMM-Bat: NI RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm2$DIC)

results_df <- rbind(results_df, result)

#MCMC with RE
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
MCMCglmm3 <- MCMCglmm(fixed = Price ~ 0 + Season_num + WK + Overseas_player + Star_player + Active_T20I + 
                        Cum_LY_Bat_I + Cum_LY_HardHitter + Cum_LY_Finisher. + Cum_LY_46_scored + 
                        Cum_LY_Bat_avg + Cum_LY_50plus_scored + Cum_LY_RBW + Cum_IPL_bat_DM_Bat_I + 
                        Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_Finisher. + Cum_IPL_bat_DM_46_scored + 
                        Cum_IPL_bat_DM_Bat_avg + Cum_IPL_bat_DM_50plus_scored + Cum_IPL_bat_DM_RBW,
                      random = ~ Cum_IPL_bat_DM_Bat_I:Name2,
                      data = train,
                      family = "exponential",
                      prior = prior,
                      verbose = FALSE,
                      nitt = 100000,
                      burnin = 5000,
                      thin = 2)

summary(MCMCglmm3)



# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm3, 
                       newdata = test, 
                       allow.new.levels = TRUE)


result <- data.frame(Model = 'MCMC GLMM-Bat: NI RE Cum_IPL_bat_DM_Bat_I:Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm3$DIC)

results_df <- rbind(results_df, result)

plot(MCMCglmm3)

MCMCglmm3$VCV

###############################

###############Bowlers
df1 <- df[,c(1,3,8:14,38:51,75:88)]
df2 <- df1[df1$Type == 'Bowl', c(1,2,5,7:9,17:23,31:37)]
colnames(df2)
#treat categorical variables
df2[,c(3:5)] <- lapply(df2[,c(3:5)], factor)
#scaling
df2[,c(7:20)] <- scale(df2[,c(7:20)], center=TRUE, scale=TRUE)

#inspect
str(df2)
summary(df2)
apply(df2, 2, sd)
length(unique(df2$Name2))

#EDA
plot_df <- df2[, c(1,2,6)]
xyplot(Price ~ Season_num | Name2, plot_df, type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[1],
       xlab = "IPL Season",
       ylab = "Player Salary", aspect = "xy")

boxplot(plot_df$Price ~ plot_df$Season_num,
        xlab = "IPL Season", # x-axis label
        ylab = "Player Salary", # y-axis label
        las = 1, # Rotate numbers on y-axis
        ylim = c(0,3.5), # Range of y-axis
        col = "lightblue", # Boxplot fill color
        border = "darkred", # Boxplot border color
        pch = 19) # Symbol for outliers)

#train-test split
train  <- df2[df2['Season_num'] <= 12, c(3:20)]
test   <- df2[!df2['Season_num'] <= 12, c(3:20)]

############################

#baseline lm model
lm4 <- lm(Price ~ ., data = train)
summary(lm4)
predictions <- predict(lm4, newdata = test)
result <- data.frame(Model = 'Simple LM-Bowlers: With Intercept',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lm4),
                     BIC = BIC(lm4),
                     LL = logLik(lm4),
                     DIC = NA)
results_df <- result

hist(resid(lm4), breaks = 40)
plot(resid(lm4) ~ fitted(lm4), pch=19, las=1); abline(0,0, col="red")

#restricted LM
Anova(lm4)
lm5 <- lm(Price ~ Overseas_player + Star_player + Active_T20I + Cum_LY_Bowl_I + Cum_IPL_bowl_DM_Bowl_I
               , data = train)
summary(lm5)
predictions <- predict(lm5, newdata = test)
result <- data.frame(Model = 'Restricted Simple LM-Bowlers',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lm5),
                     BIC = BIC(lm5),
                     LL = logLik(lm5),
                     DIC = NA)

results_df <- rbind(results_df, result)

extract_eq(lm5, 
           wrap = TRUE, 
           terms_per_line = 2, 
           operator_location = "start", 
           use_coefs = TRUE)

##################

#LMM

#train-test split
train  <- df2[df2['Season_num'] <= 12,]
test   <- df2[!df2['Season_num'] <= 12,]

colnames(train)

lmm7 <- lmer(Price ~ Season_num + Overseas_player + Star_player + Active_T20I + 
                    Cum_LY_Bowl_I + Cum_LY_Bowl_Econ + Cum_LY_Bowl_Avg + Cum_LY_Bowl._SR +
                    Cum_LY_BW.Taker + Cum_LY_bowl_46_conc + Cum_LY_bowl_Dots. + 
                    Cum_IPL_bowl_DM_Bowl_I + Cum_IPL_bowl_DM_Bowl_Econ + Cum_IPL_bowl_DM_Bowl_Avg +
                    Cum_IPL_bowl_DM_Bowl._SR + Cum_IPL_bowl_DM_BW.Taker + Cum_IPL_bowl_DM_46_conc + Cum_IPL_bowl_DM_Dots. +
                    (1 | Name2), 
                  data = train,
                  REML = FALSE)

# Get the summary of the model
summary(lmm7)
predictions <- predict(lmm7, 
                       newdata = test, 
                       re.form = ~(1 | Name2),
                       allow.new.levels = TRUE)
result <- data.frame(Model = 'LMM-Bowl: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lmm7),
                     BIC = BIC(lmm7),
                     LL = logLik(lmm7),
                     DIC = NA)

results_df <- rbind(results_df, result)

Anova(lmm7)

confint(lmm7)


#LMM-Restricted
lmm8 <- lmer(Price ~ Season_num + Star_player + Active_T20I + Cum_LY_Bowl_I +  Cum_IPL_bowl_DM_Bowl_I +
                    (1 + Cum_LY_Bowl_I | Name2), 
                  data = train,
                  REML = FALSE)

# Get the summary of the model
summary(lmm8)
# Make predictions for test data using estimated random effects
predictions <- predict(lmm8, 
                       newdata = test, 
                       re.form = ~(1 + Cum_LY_Bowl_I | Name2),
                       allow.new.levels = TRUE)
result <- data.frame(Model = 'Restricted LMM-Bowl: RE Cum_LY_Bowl_I Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lmm8),
                     BIC = BIC(lmm8),
                     LL = logLik(lmm8),
                     DIC = NA)
results_df <- rbind(results_df, result)

plot(lmm8, sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p", "smooth"))
qqmath(lmm8, id = 0.05)

VarCorr(lmm8)


##################

#MCMC GLMM
#prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
MCMCglmm5 <- MCMCglmm(fixed = Price ~ Season_num + Overseas_player + Star_player + Active_T20I + 
                             Cum_LY_Bowl_I + Cum_LY_Bowl_Econ + Cum_LY_Bowl_Avg + Cum_LY_Bowl._SR +
                             Cum_LY_BW.Taker + Cum_LY_bowl_46_conc + Cum_LY_bowl_Dots. + 
                             Cum_IPL_bowl_DM_Bowl_I + Cum_IPL_bowl_DM_Bowl_Econ + Cum_IPL_bowl_DM_Bowl_Avg +
                             Cum_IPL_bowl_DM_Bowl._SR + Cum_IPL_bowl_DM_BW.Taker + Cum_IPL_bowl_DM_46_conc + Cum_IPL_bowl_DM_Dots.,
                           random = ~ Active_T20I:Name2,
                           data = train,
                           #prior = prior,
                           family = "gaussian",
                           verbose = FALSE,
                           nitt = 100000,
                           burnin = 5000,
                           thin = 2)

summary(MCMCglmm5)
# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm5, 
                       newdata = test, 
                       allow.new.levels = TRUE)

result <- data.frame(Model = 'MCMC GLMM-Bowl unspecified prior gaussian Family: RE Active_T20I:Name2',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm5$DIC)

results_df <- rbind(results_df, result)

plot(MCMCglmm5)

######
#############All rounders

df2 <- df1[df1$Type == 'AR', c(1,2,5,7:37)]
colnames(df2)

#scaling
df2[,c(7:34)] <- scale(df2[,c(7:34)], center=TRUE, scale=TRUE)
#treat categorical variables
df2[,c(3:5)] <- lapply(df2[,c(3:5)], factor)

#inspect
str(df2)
summary(df2)
length(unique(df2$Name2))

#train-test split
train  <- df2[df2['Season_num'] <= 12, c(3:34)]
test   <- df2[!df2['Season_num'] <= 12, c(3:34)]

#baseline lm model
lm6 <- lm(Price ~ ., data = train)
summary(lm6)
Anova(lm6)
predictions <- predict(lm6, newdata = test)
result <- data.frame(Model = 'Simple LM-AR: With Intercept',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lm6),
                     BIC = BIC(lm6),
                     LL = logLik(lm6),
                     DIC = NA)
results_df <- result

#restricted LM
anova(lm6)
lm7 <- lm(Price ~ Overseas_player + Star_player + Active_T20I + Cum_LY_Bat_I + Cum_LY_Bowl._SR + Cum_IPL_bat_DM_Bat_I + 
                 Cum_IPL_bat_DM_Bat_avg, 
               data = train)
summary(lm7)
predictions <- predict(lm7, newdata = test)
result <- data.frame(Model = 'Restricted Simple LM-AR',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lm7),
                     BIC = BIC(lm7),
                     LL = logLik(lm7),
                     DIC = NA)

results_df <- rbind(results_df, result)

#restricted LM2
lm8 <- lm(Price ~ Star_player + Cum_IPL_bat_DM_Bat_I + 
            Cum_IPL_bat_DM_Bat_avg, 
          data = train)
summary(lm8)
predictions <- predict(lm8, newdata = test)
result <- data.frame(Model = 'Restricted Simple LM2-AR',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lm8),
                     BIC = BIC(lm8),
                     LL = logLik(lm8),
                     DIC = NA)

results_df <- rbind(results_df, result)

extract_eq(lm8, 
           wrap = TRUE, 
           terms_per_line = 2, 
           operator_location = "start", 
           use_coefs = TRUE)


#LMM
#train-test split
train  <- df2[df2['Season_num'] <= 12,]
test   <- df2[!df2['Season_num'] <= 12,]

lmm9 <- lmer(Price ~ Season_num + Overseas_player + Star_player + Active_T20I + 
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
summary(lmm9)
Anova(lmm9)

# Make predictions for test data using estimated random effects
predictions <- predict(lmm9, 
                       newdata = test, 
                       re.form = ~(1 | Name2),
                       allow.new.levels = TRUE)


result <- data.frame(Model = 'LMM-AR: RE Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lmm9),
                     BIC = BIC(lmm9),
                     LL = logLik(lmm9),
                     DIC = NA)

results_df <- rbind(results_df, result)


#LMM-Restricted
lmm10 <- lmer(Price ~ Star_player + Cum_LY_50plus_scored + Cum_LY_bowl_46_conc + Cum_IPL_bowl_DM_Bowl_Avg + Cum_IPL_bowl_DM_Bowl._SR +
                    (1 + Cum_LY_50plus_scored | Name2), 
                  data = train,
                  REML = FALSE)

# Get the summary of the model
summary(lmm10)
Anova(lmm10)
confint(lmm10)
# Make predictions for test data using estimated random effects
predictions <- predict(lmm10, 
                       newdata = test, 
                       re.form = ~(1 + Cum_LY_50plus_scored | Name2),
                       allow.new.levels = TRUE)


result <- data.frame(Model = 'Restricted LMM-Bowl: RE RI RS Cum_LY_50plus_scored Name',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = AIC(lmm10),
                     BIC = BIC(lmm10),
                     LL = logLik(lmm10),
                     DIC = NA)

results_df <- rbind(results_df, result)


#MCMC GLMM ---- works-----
#prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
MCMCglmm6 <- MCMCglmm(fixed = Price ~ Season_num + Overseas_player + Star_player + Active_T20I + 
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
                           nitt = 100000,
                           burnin = 2000,
                           thin = 2)

summary(MCMCglmm6)
#anova(MCMCglmm_model)
# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm6, 
                       newdata = test, 
                       allow.new.levels = TRUE)

result <- data.frame(Model = 'MCMC GLMM-AR unspecified prior exponential Family: RE Name2',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm6$DIC)

results_df <- rbind(results_df, result)

plot(MCMCglmm6)


#re
MCMCglmm7 <- MCMCglmm(fixed = Price ~ Season_num + Overseas_player + Star_player + Active_T20I + 
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
                      nitt = 100000,
                      burnin = 2000,
                      thin = 2)

summary(MCMCglmm6)
#anova(MCMCglmm_model)
# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm6, 
                       newdata = test, 
                       allow.new.levels = TRUE)

result <- data.frame(Model = 'MCMC GLMM-AR unspecified prior exponential Family: RE Name2',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm6$DIC)

results_df <- rbind(results_df, result)













#res1
MCMCglmm9 <- MCMCglmm(fixed = Price ~ Star_player + Cum_LY_50plus_scored + Cum_IPL_bat_DM_HardHitter + Cum_IPL_bat_DM_RBW +
                        Cum_LY_bowl_46_conc + Cum_IPL_bowl_DM_Bowl_Avg + Cum_IPL_bowl_DM_Bowl._SR,
                      random = ~ Name2,
                      data = train,
                      #prior = prior,
                      family = "exponential",
                      verbose = FALSE,
                      nitt = 100000,
                      burnin = 2000,
                      thin = 2)

summary(MCMCglmm9)
#anova(MCMCglmm_model)
# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm9, 
                       newdata = test, 
                       allow.new.levels = TRUE)

result <- data.frame(Model = ' res MCMC GLMM-AR unspecified prior gaussian Family: RE Name2',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm9$DIC)

results_df <- rbind(results_df, result)



#RE
MCMCglmm8 <- MCMCglmm(fixed = Price ~ Season_num + Overseas_player + Star_player + Active_T20I + 
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
                           family = "gaussian",
                           verbose = FALSE,
                           nitt = 100000,
                           burnin = 5000,
                           thin = 10)

summary(MCMCglmm8)
#anova(MCMCglmm_model)
# Make predictions for test data using estimated random effects
predictions <- predict(MCMCglmm8, 
                       newdata = test, 
                       allow.new.levels = TRUE)

result <- data.frame(Model = 'MCMC GLMM-AR unspecified prior Gaussian Family: RE Cum_LY_50plus_scored:Name2',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm8$DIC)

results_df <- rbind(results_df, result)


#Restricted

MCMCglmm11 <- MCMCglmm(fixed = Price ~ Star_player + Cum_LY_50plus_scored +  
                         Cum_IPL_bat_DM_Bat_I + Cum_IPL_bat_DM_Bat_I + 
                         Cum_IPL_bowl_DM_Bowl_Avg + Cum_LY_Bowl_I + Cum_IPL_bowl_DM_Bowl._SR,
                           random = ~ Name2,
                           data = train,
                           #prior = prior,
                           family = "exponential",
                           verbose = FALSE,
                           nitt = 100000,
                           burnin = 2000,
                           thin = 2)

summary(MCMCglmm11)

predictions <- predict(MCMCglmm11, 
                       newdata = test, 
                       allow.new.levels = TRUE)

result <- data.frame(Model = 'Res MCMC GLMM-AR unspecified prior Exp Family: RE Name2',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm11$DIC)
results_df <- rbind(results_df, result)


#Restricted RE
set.seed(12345)
MCMCglmm13 <- MCMCglmm(fixed = Price ~ Star_player + Cum_LY_50plus_scored +  
                         Cum_IPL_bat_DM_Bat_I + Cum_IPL_bowl_DM_Bowl_Avg + Cum_LY_Bowl_I + Cum_IPL_bowl_DM_Bowl._SR,
                       random = ~ Star_player:Name2,
                       data = train,
                       #prior = prior,
                       family = "exponential",
                       verbose = FALSE,
                       nitt = 100000,
                       burnin = 2000,
                       thin = 2)

summary(MCMCglmm13)

predictions <- predict(MCMCglmm13, 
                       newdata = test, 
                       allow.new.levels = TRUE)

result <- data.frame(Model = 'Res MCMC GLMM-AR unspecified prior Exp Family: RE Star_player:Name2',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm13$DIC)
results_df <- rbind(results_df, result)


MCMCglmm14 <- MCMCglmm(fixed = Price ~ Star_player +   
                         Cum_IPL_bat_DM_Bat_I + Cum_LY_Bowl_I,
                       random = ~ Star_player:Name2,
                       data = train,
                       #prior = prior,
                       family = "exponential",
                       verbose = FALSE,
                       nitt = 100000,
                       burnin = 2000,
                       thin = 2)

summary(MCMCglmm14)

predictions <- predict(MCMCglmm14, 
                       newdata = test, 
                       allow.new.levels = TRUE)

result <- data.frame(Model = 'Res MCMC GLMM-AR unspecified prior Exp Family: RE Star_player:Name2',
                     RMSE = rmse(test$Price, predictions),
                     MAPE = mape(test$Price, predictions),
                     AIC = NA,
                     BIC = NA,
                     LL = NA,
                     DIC = MCMCglmm14$DIC)
results_df <- rbind(results_df, result)

plot(MCMCglmm14)
