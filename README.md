# Master Thesis: Applications of Mixed Models to Predict Player Auction Prices in the Indian Premier League

This repo contains my manuscript and other working files for my Master Thesis in Machine Learning at LiU. The topic of my thesis was to construct a prediction model to predict a player's auction price in the Indian Premier League based on their past performances in the IPL and other related competitions. The thesis study specifically focusses on utilizing Mixed Models for this prediction task.

## Thesis Abstract

The Indian Premier League (IPL) is a professional T20 cricket league that attracts some of the best players from around the world. The auction of IPL players is a critical event that determines the composition of the teams for the upcoming season. In this thesis, I used mixed models to predict the auction prices of IPL players based on their performance data in the previous seasons of the IPL and their recent performance in other leagues. I collected data on the performance of players and their auction prices for the past seasons of the IPL and also other renowned T20 leagues and used this data to train and test the prediction models.

In this thesis, I use classical linear regression models as baseline models and later train linear mixed models (LMMs) and use Markov Chain Monte Carlo (MCMC) simulations of Generalized Linear Mixed Models (GLMM) to predict the auction prices of Indian Premier League (IPL) players. I modeled the players separately based on their roles, i.e., batsmen, bowlers, and all-rounders. I collected data on the performance of IPL players in previous seasons, including their batting and bowling statistics, and used this data as predictors to train and test the models.

I found that the LMMs and GLMM MCMC simulations generally outperformed the simple linear regression models in predicting the auction prices of IPL players. The mixed models also captured some relationships between the auction price and the predictors that could not be seen with the baseline models in each of the player roles. From this study shows that for batsmen, the player's ability to score quickly in recent competitions in addition to their overall batting experience in the IPL is considered important for a higher auction price. For bowlers, its the overall experience and consistency of play that is considered more important than the wickets they take. Additionally, the study shows that a player's _'Star'_ status is a very important predictor for auction price regardless of the role that the player fulfills.

The findings have important implications for the IPL teams and players, as they can use this information to make informed decisions during the auction and the season. Overall, this study contributes to the growing body of research on sports analytics, cricket and prediction modeling and highlights the potential of _Mixed Models_ for predicting auction prices in T20 cricket leagues like the IPL.

_Keywords:_ Mixed Models, Longitudinal Data Modelling, Regression, MCMC, Sports, Cricket, IPL, Player Salary Prediction.

## Evaluation

This thesis was passed and received a _"Very Good"_ **(B)** grade from the evaluation committee. The manuscript will soon be e-published with a link posted here for future citations.
