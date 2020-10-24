#Citation
# https://rcompanion.org/handbook/I_12.html

#Read Data
data = read.csv("Desktop/STA108/mortality.csv")
data

par(mfrow = c(2,4))
# Histogram of Predictors (Check which predictor is skewed)
hist(data$PRECIP, col = "blue", main = paste('Histogram of Mean Precipitation'), xlab = paste('PRECIP'))
hist(data$EDUC, col = "blue", main = paste('Histogram of Median School Years Completed'), xlab = paste('EDUC'))
hist(data$NONWHITE, col = "blue", main = paste('Histogram of Nonwhite Population'), xlab = paste('NONWHITE'))
hist(data$POOR, col = "blue", main = paste('Histogram of Households Income Under $3000'), xlab = paste('POOR'))
hist(data$NOX, col = "blue", main = paste('Histogram of Nitrogen'), xlab = paste('NOX'))
hist(data$SO2, col = "blue", main = paste('Histogram of Sulphur Dioxide'), xlab = paste('SO2'))
hist(data$MORTALITY, col = "blue", main = paste('Histogram of Mortality'), xlab = paste('MORT'))

par(mfrow = c(2,4))
#Transformation (tranformed by natural algorithm and cube root)
hist(data$PRECIP, col = "red", main = paste('Histogram of Mean Precipitation'), xlab = paste('PRECIP'))
hist(data$EDUC, col = "red", main = paste('Histogram of Median School Years Completed'), xlab = paste('EDUC'))

data[3] = sign(data[3]) * abs(data[3])^(1/3)
hist(data$NONWHITE, col = "red", main = paste('Histogram of Nonwhite Population'), xlab = paste('NONWHITE'))
data[4] = sign(data[4]) * abs(data[4])^(1/3)
hist(data$POOR, col = "red", main = paste('Histogram of Households Income Under $3000'), xlab = paste('POOR'))

data[5] = log(data[5])
data[6] = log(data[6])
hist(data$NOX, col = "red", main = paste('Histogram of Nitrogen'), xlab = paste('NOX'))
hist(data$SO2, col = "red", main = paste('Histogram of Sulphur Dioxide'), xlab = paste('SO2'))
hist(data$MORTALITY, col = "red", main = paste('Histogram of Mortality'), xlab = paste('MORT'))

#2.
# Reorder the data to form model of regression: y=b0+b1x1...
data.tran = data
data.tran = cbind(data.tran[7],data.tran[1],data.tran[2],data.tran[3],data.tran[4],data.tran[5],data.tran[6])

plot(data.tran, main=paste('Matrix Plot of the Data')) # matrix plot of the data
cor(data.tran) # correlation matrix

# Fit the regression
names(data.tran) = c('y','x1','x2','x3','x4','x5','x6')
fit = lm(y ~ ., data = data.tran)
summary(fit)
anova(fit) # ANOVA

#3.
# plots
par(mfrow = c(2,2))
boxplot(fit$res, main = 'Residuals') #boxplot of residual
hist(fit$res, main = 'Histogram of Residuals', xlab = 'Residuals') #histogram of residual
plot(fit$fitted, data.tran$y, main = 'Observed Y vs. Fitted Y', xlab = 'Fitted Y', ylab = 'Observed Y') # observed y vs fitted y
qqnorm(fit$res, main = 'Normal QQ-Plot', xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles') # normal plot
qqline(fit$res)

# plots of residuals vs independent variables
par(mfrow=c(2,4))
plot(fit$fitted, fit$res,main = 'Residuals vs. Fitted Y ', xlab = 'Fitted Y', ylab = 'Residuals')
abline(h=0)
plot(data.tran$x1, fit$res,main = 'Residuals vs. X1 ', xlab = 'X1', ylab = 'Residuals')
abline(h=0)
plot(data.tran$x2, fit$res,main = 'Residuals vs. X2 ', xlab = 'X2', ylab = 'Residuals')
abline(h=0)
plot(data.tran$x3, fit$res,main = 'Residuals vs. X3 ', xlab = 'X3', ylab = 'Residuals')
abline(h=0)
plot(data.tran$x4, fit$res,main = 'Residuals vs. X4 ', xlab = 'X4', ylab = 'Residuals')
abline(h=0)
plot(data.tran$x5, fit$res,main = 'Residuals vs. X5 ', xlab = 'X5', ylab = 'Residuals')
abline(h=0)
plot(data.tran$x6, fit$res,main = 'Residuals vs. X6 ', xlab = 'X6', ylab = 'Residuals')
abline(h=0)

# 4.
par(mfrow = c(1,2))
plot(data.tran$x4,data.tran$y, main = 'Scatter Plot of X4 vs. Y', xlab = 'X4', ylab = 'Y') # Shows x4 is nonlinear
plot(data.tran$x6,data.tran$y, main = 'Scatter Plot of X6 vs. Y', xlab = 'X6', ylab = 'Y') # Shows x6 is nonlinear
# polynomial regression
y = data.tran$y
X = data.tran$x4
x = data.tran$x4 - mean(data.tran$x4)
x2 = x ^ 2
#fit/anova/estimations
fit2 = lm(y ~ x)
summary(fit2)
anova(fit2)

#plots
par(mfrow = c(2,2))
boxplot(fit2$res, main = 'Residuals')
hist(fit2$res, main = 'Histogram of Residuals', xlab = 'Residuals')
plot(fit2$fitted, data.tran$y, main = 'Observed Y vs. Fitted Y', xlab = 'Fitted Y', ylab = 'Observed Y')
qqnorm(fit2$res, main = 'Normal QQ-Plot', xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles')
qqline(fit2$res)

par(mfrow=c(2,4))
plot(fit2$fitted, fit2$res,main = 'Residuals vs. Fitted Y ', xlab = 'Fitted Y', ylab = 'Residuals')
abline(h=0)
plot(data.tran$x1, fit2$res,main = 'Residuals vs. X1 ', xlab = 'X1', ylab = 'Residuals')
abline(h=0)
plot(data.tran$x2, fit2$res,main = 'Residuals vs. X2 ', xlab = 'X2', ylab = 'Residuals')
abline(h=0)
plot(data.tran$x3, fit2$res,main = 'Residuals vs. X3 ', xlab = 'X3', ylab = 'Residuals')
abline(h=0)
plot(data.tran$x4, fit2$res,main = 'Residuals vs. X4 ', xlab = 'X4', ylab = 'Residuals')
abline(h=0)
plot(data.tran$x5, fit2$res,main = 'Residuals vs. X5 ', xlab = 'X5', ylab = 'Residuals')
abline(h=0)
plot(data.tran$x6, fit2$res,main = 'Residuals vs. X6 ', xlab = 'X6', ylab = 'Residuals')
abline(h=0)

#5. Variable Deletion 
# Step
step(lm(y~x1+x2+x3+x4+x5+x6, data = data.tran), ~1, direction = 'backward')
#subset
library('leap')
install.packages('leaps')
fit3 = leaps(x=data.tran[,-1], y=data.tran[,1], method = 'adjr2')
ind = order(fit3$adjr2, decreasing = TRUE)
lm(y~x1+x2+x3+x5+x6, data = data.tran)
summary(lm(y~x1+x2+x3+x5+x6, data = data.tran))
