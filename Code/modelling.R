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

#BATSMEN
df_bat <- subset(df, Type == 'Bat' | Type == 'WK')
df_bat <- df_bat[,c(4:15,28:38,51:61,74:84)]

#test-train split
set.seed(12345)
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df_bat), replace=TRUE, prob=c(0.7,0.3))
train_bat  <- df_bat[sample, ]
test_bat   <- df_bat[!sample, ]

#baseline model
lm.model <- lm(Price ~ . - 1, data = train_bat)

summary(lm.model)

baseline_predict = predict(lm.model, test_bat)

library(Metrics)
#calculate RMSE
baseline_RMSE = rmse(test_bat$Price, baseline_predict)

#PCA
price <- df$Price
covariates <- df[,c(5:96)]
pca <- prcomp(covariates, center = TRUE, scale. = TRUE)
pca_summary <- summary(pca)
pca_summary

# Step 2: Extract the variances of the first 20 PCs
variances <- pca$sdev^2
variances <- variances[1:20]

# Step 3: Create a bar plot of the variances
library(ggplot2)
df <- data.frame(PC = paste0("PC", 1:20), Variance = variances)
ggplot(df, aes(x = PC, y = Variance)) + 
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  labs(x = "Principal Component", y = "Variance Explained") + 
  ggtitle("Variance Explained Plot for the First 20 PCs")

PC1_20 <- data.frame(pca$x[,1:20])

lm.pca.model <- lm(price ~ .-1, data = PC1_20)
summary(lm.pca.model)
