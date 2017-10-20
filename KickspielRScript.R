library("XLConnect")
library("randomForest")
library("gbm")
library("fOptions")
library("rpart")

# setwd and clean up variables
setwd("C:\\Users\\user1\\Desktop\\")
wb = loadWorkbook("Kicktipp.xlsx")
df = readWorksheet(wb, sheet = "Training", header = TRUE)
df$TeamL = as.factor(df$TeamL)
df$TeamR = as.factor(df$TeamR)

# set seed to zero for RF/Boosting model
set.seed(0)

# random forest
rf_model <- randomForest(ScoreDiff ~ ProbR + ProbL + RankDiff, data = df, importance = TRUE, ntree = 2000, do.Trace = TRUE)
rf_model1 <- randomForest(ScoreDiff ~ ProbR + ProbL + RankL + RankR, data = df, importance = TRUE, ntree = 2000, do.Trace = TRUE)

# calculate MSE
(mse_rf <- (1/length(df$ScoreDiff))*sum(rf_model$predicted - df$ScoreDiff)^2)
(mse_rf1 <- (1/length(df$ScoreDiff))*sum(rf_model1$predicted - df$ScoreDiff)^2)
# rf_model1 is not good enough!

summary(rf_model)

# boosting
boost_model <- gbm(ScoreDiff ~ ProbR + ProbL + RankDiff, data = df, distribution = "gaussian", n.trees = 2000)
boost_model1 <- gbm(ScoreDiff ~ ProbR + ProbL + RankR + RankL, data = df, distribution = "gaussian", n.trees = 2000)

# set rounding options to avoid calculating with 0
fOptions('scipen'=+20)

boost_pred <- predict.gbm(boost_model, newdata = df, n.trees = 1)
boost_pred1 <- predict.gbm(boost_model, newdata = df, n.trees = 1)

(mse_boost <- (1/length(df$ScoreDiff))*sum(boost_pred - df$ScoreDiff)^2)
(mse_boost1 <- (1/length(df$ScoreDiff))*sum(boost_pred1 - df$ScoreDiff)^2)

par(mfrow=c(1,2))
plot.gbm(boost_model)
plot.gbm(boost_model1)
# pretty identical values so far

# compare all MSE values :
# mse_rf = 0.01482759
# mse_rf1 = 0.05267928
# mse_boost = mse_bost1 = 0.00001720166.... (it only makes sense that MSE of boosting models converge)
# idea: TeamInfo may also be incuded? 

rf_model2 <- randomForest(ScoreDiff ~ ProbR + ProbL + RankDiff + TeamL + TeamR, data = df, importance = TRUE, ntree = 2000, do.Trace = TRUE)
rf_model3 <- randomForest(ScoreDiff ~ ProbR + ProbL + RankR + RankL + TeamL + TeamR, data = df, importance = TRUE, ntree = 2000, do.Trace = TRUE)

(mse_rf2 <- (1/length(df$ScoreDiff))*sum(rf_model2$predicted - df$ScoreDiff)^2)
(mse_rf3<- (1/length(df$ScoreDiff))*sum(rf_model3$predicted - df$ScoreDiff)^2)
# Nope, does not work

# Summary: Use CV, boosting, or both. probably both, since our bias is really high and variance is low, because of low feature count.
