##########################################
### Regression Analysis Assignment     ###
### Group 19                          ###        
##########################################

rm(list = ls())
library(MASS)
data.full = read.table("dataset.txt",header=T)
data.full$Sex <- as.factor(data.full$Sex)
data.full$Natural <- as.factor(data.full$Natural)
data.full$Forest <- as.factor(data.full$Forest)


set.seed(0199519)
n <- dim(data.full)[1]

d.test <- sample(1:dim(data.full)[1], round(dim(data.full)[1]/2) )
data.test <- data.full[d.test, ]
data.training <- data.full[-d.test, ]
##################
# Exploratory Analysis
##################
attach(data.training)
names(data.training)
summary(data.training)

par(mfrow = c(1,5))
hist(Length)
hist(Size)
hist(Shrub)
hist(Canopy)
hist(Effort)

#Boxplot of categorical
par(mfrow = c(1,3))
table(Sex)
boxplot(Length[which(Sex == 'female')], Length[which(Sex == 'male')], names = c("female(n=85)", "male(n=75)"), ylab = "Length")
table(Natural)
boxplot(Length[which(Natural == 'yes')], Length[which(Natural == 'no')], names = c("Natural=yes (n = 72)", "Natural = no(n = 88)"), ylab = "Length")
table(Forest)
boxplot(Length[which(Forest == 'Chawia')], Length[which(Forest == 'Ngangao N')],  Length[which(Forest == 'Ngangao S')],names = c("Chawia (n=55)", "Ngangao N (n=46)","Ngangao S (n=59)"), ylab = "Length")

#Boxplot of categorical
par(mfrow = c(1,2))
boxplot(Canopy[which(Natural == 'yes')], Canopy[which(Natural == 'no')], names = c("Natural=yes (n = 72)", "Natural = no(n = 88)"), ylab = "Canopy")
boxplot(Shrub[which(Natural == 'yes')], Shrub[which(Natural == 'no')], names = c("Natural=yes (n = 72)", "Natural = no(n = 88)"), ylab = "Shrub")

par(mfrow = c(2,1))
boxplot(Canopy[which(Forest == 'Chawia')], Canopy[which(Forest == 'Ngangao N')],  Canopy[which(Forest == 'Ngangao S')],names = c("Chawia (n=55)", "Ngangao N (n=46)","Ngangao S (n=59)"), ylab = "Canopy")
boxplot(Effort[which(Forest == 'Chawia')], Effort[which(Forest == 'Ngangao N')],  Effort[which(Forest == 'Ngangao S')],names = c("Chawia (n=55)", "Ngangao N (n=46)","Ngangao S (n=59)"), ylab = "Effort")
# Scatter plot
pairs(data.training)
pairs(data.training, panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})

# check for multiconllinearity 
cor(data.training[,c(2,3,4,5)])
(VIF_values <- diag(solve(cor(data.training[,c(2,3,4,5)]))))

#-----------------#
# Model Selection #
#-----------------#

# Backward elimination based on AIC
fit.full <- lm(Length~.,data=data.training)
summary(fit.full)
stepAIC(fit.full, scope = list(upper = ~., lower = ~ 1), direction = "backward")
# AIC = -787.28
# Length ~ Sex + Canopy + Shrub + Effort + Natural

# Forward selection based on AIC
fit.null <- lm(Length ~ 1, data = data.training)
fit.null
stepAIC(fit.null, scope = list(upper = ~Sex+Canopy+Shrub+Effort+Size+Natural+Forest , lower = ~ 1), direction = "forward")
# AIC = -787.28
#Length ~ Sex + Shrub + Canopy + Effort + Natural

# Stepwise selection based on AIC (started at full model)
stepAIC(fit.full, scope=list(upper = ~Sex+Canopy+Shrub+Effort+Size+Natural+Forest, lower = ~ 1), direction = "both")
# AIC = -787.28
#  Length ~ Sex + Canopy + Shrub + Effort + Natural

# Stepwise selection based on AIC (started at null model)
stepAIC(fit.null, scope=list(upper = ~Sex+Canopy+Shrub+Effort+Size+Natural+Forest, lower = ~ 1), direction = "both")
# sane results


# Model 1: Sex + Canopy + Shrub + Effort + Natural
fit1 <- lm(Length ~ Sex + Canopy + Shrub + Effort + Natural, data = data.training)
summary(fit1)

# fit1 assumptions check
fit1.res <- residuals(fit1)
fit1.stdres <- stdres(fit1)
fit1.fittedvalues <- fitted.values(fit1)
par(mfrow = c(2,2))
qqnorm(fit1.stdres, main="")
qqline(fit1.stdres)
plot(fit1.res, xlab = "Index", ylab = "Residual")
plot(fit1.fittedvalues, fit1.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit1.res ~ fit1.fittedvalues), col = "red")
plot(fit1.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

par(mfrow = c(1,3))
plot(Shrub, fit1.res, ylab = "Residual")
lines(lowess(fit1.res ~ Shrub), col = "red")
plot(Canopy, fit1.res, ylab = "Residual")
lines(lowess(fit1.res ~ Canopy), col = "red")
plot(Effort, fit1.res, ylab = "Residual")
lines(lowess(fit1.res ~ Effort), col = "red")



# adding interaction term
# fit2 <- lm(Length ~ Sex + Canopy + Shrub + Effort + Natural+I(Canopy^2), data = data.training)
# fit2 <- lm(Length ~ Sex + Canopy + Shrub + Effort + Natural+I(Effort^2), data = data.training)

fit2 <- lm(Length ~ Sex + Canopy + Shrub + Effort + Natural+Canopy*Shrub, data = data.training)
summary(fit2)

# fit2 assumptions check
fit2.res <- residuals(fit2)
fit2.stdres <- stdres(fit2)
fit2.fittedvalues <- fitted.values(fit2)
par(mfrow = c(2,2))
qqnorm(fit2.stdres, main="")
qqline(fit2.stdres)
plot(fit2.res, xlab = "Index", ylab = "Residual")
plot(fit2.fittedvalues, fit2.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit2.res ~ fit2.fittedvalues), col = "red")
plot(fit2.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

# Weighted least squares of model (with interaction)
w <- 1/lm(abs(fit2.stdres) ~ Sex + Canopy + Shrub + Effort + Natural+Canopy: Shrub)$fitted.values^2
fit3 <- lm(Length ~ Sex + Canopy + Shrub + Effort + Natural+Canopy: Shrub, weight = w)
summary(fit3)
fit3.res <- residuals(fit3)
fit3.stdres <- stdres(fit3)
fit3.fittedvalues <- fitted.values(fit3)
par(mfrow = c(2,2))
qqnorm(fit3.stdres, main="")
qqline(fit3.stdres)
plot(resid(fit3)*sqrt(w), xlab = "Index", ylab = "Weighted Residual")
plot(fit3.fittedvalues, resid(fit3)*sqrt(w), xlab = "Fitted value", ylab = "Weighted Residual")
plot(fit3.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)



# Box-Cox transformation of model 
par(mfrow = c(1,1))
out <- boxcox(Length ~ Sex + Canopy + Shrub + Effort+Natural+Canopy*Shrub, lambda = seq(-1,1,0.001), plotit = TRUE)
lambda <- out$x[which(out$y == max(out$y))]
fit4<- lm(((Length)^lambda - 1)/lambda ~ Sex + Canopy + Shrub + Effort+Natural+Canopy*Shrub )
summary(fit4)

# Box-Cox transformation assumptions check
fit4.res <- residuals(fit4)
fit4.stdres <- stdres(fit4)
fit4.fittedvalues <- fitted.values(fit4)
par(mfrow = c(2,2))
qqnorm(fit4.stdres, main="")
qqline(fit4.stdres)
plot(fit4.res, xlab = "Index", ylab = "Residual")
plot(fit4.fittedvalues, fit4.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit4.res ~ fit4.fittedvalues), col = "red")
plot(fit4.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)


#robust method

# Standardized residuals
library(MASS)
par(mfrow = c(2,2))
plot(fit2.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

# Studentized residuals
fit2.studres <- studres(fit2)
plot(fit2.studres, ylim = c(-4,4), ylab = "Studentized residuals")
abline(h = c(-2.5,2.5), col = "red")

# Diagonal elements of hat matrix
fit2.influence <- influence(fit2)
plot(fit2.influence$hat, ylab = "Diagonal elements of hat matrix")
n <- dim(data.training)[1]
p <- dim(data.training)[2]
abline(h = 2*p/n, col = "red")

# DFFITS
fit2.dffits <- dffits(fit2)
plot(fit2.dffits, ylab = "DFFITS")
abline(h = 2*sqrt(p/n), col = "red")

par(mfrow = c(1,1))
# Cook's distance
fit2.Cd <- cooks.distance(fit2)
plot(fit2.Cd,ylim = c(0,1.5), ylab = "Cook's distance")
abline(h = 1, col = "red")

# DFBETAS
fit2.dfbetas <- dfbetas(fit2)
fit2.dfbetas
2/sqrt(n)

library(robustbase)
# RLTS (5% breakdown value)
RLTS <- ltsReg(Length ~ Sex + Canopy + Shrub + Effort + Natural+Canopy*Shrub, data = data.training, alpha = 0.95)
summary(RLTS)

# Detection of outliers
plot(RLTS, which = "rindex")
plot(RLTS, which = "rdiag")

lmrob1 <-lmrob(Length ~ Sex + Canopy + Shrub + Effort +Natural+Canopy*Shrub, data = data.training)
summary(lmrob1)

#------------------#
# Model validation #
#------------------#

model1=fit3
model2=fit4
model3=lmrob1
detach(data.training)
attach(data.test)

# MSEP
MSEP1 <- mean((predict(model1, newdata = data.test) - Length)^2)
MSEP2 <- mean((predict(model2, newdata = data.test) -  ((Length)^lambda - 1)/lambda)^2)
MSEP3 <- mean((predict(model3, newdata = data.test) -  Length)^2)
MSEP <- c(MSEP1, MSEP2, MSEP3)
names(MSEP) <- c("model1", "model2", "model3")
sort(MSEP)

detach(data.test)

