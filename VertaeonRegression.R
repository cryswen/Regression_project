setwd('~/Dropbox/regression/project/analysis/')

# select data
# overall energy intensity in footprint as response.
# for procurement, we use: plant(1), year(3), procurement cost for nature gas, oil, electricity and steam as predictors
full_data = read.csv('data.csv', header = T, fill = T)
#names(full_data)
data = full_data[,c(6,1,2,10,12,14:24,26:33,37:39)]
names(data) = c('eI', 'Plant', 'yr', 'GHG', 'Esold', 'productP', 'productA', 'productS', 'saleP', 'saleA', 'saleS', 'costC', 'costG', 'costO', 'costE', 'costS', 'ePE', 'ePS', 'ebS', 'eIEG', 'eRR', 'ePC', 'ePG', 'ePO', 'eUseP', 'eUseA', 'eUseS' )

# Plant
attach(data)
plot(Plant,eI, xlab = "Plant", ylab = "Energy Intensity", main = "Plants and their effect on Energy Intensity")
# yr
library(ggplot2)
ggplot(data = data[c(1:3)], aes(x = factor(yr), y = eI, color = Plant)) + geom_line(aes(group = Plant)) + geom_point() + ggtitle("The Effect of Time on Energy Intensity")
# GHG, Esold
par(mfrow = c(3, 4))
plot(GHG,eI)
plot(Esold,eI)
plot(productP,eI,xlab = "Annual production for Performance product")
plot(productA,eI,xlab = "Annual production for Argriculture product")
plot(productS,eI,xlab = "Annual production for Specialty product")
plot(saleP,eI,xlab = "Unit sale price for Performance product")
plot(saleA,eI,xlab = "Unit sale price for Argriculture product")
plot(saleS,eI,xlab = "Unit sale price for Specialty product")
plot(costC,eI,xlab = "Procurement.cost for Coal")
plot(costG,eI,xlab = "Procurement.cost for Natural.Gas")
plot(costO,eI,xlab = "Procurement.cost for Oil")
plot(costE,eI,xlab = "Procurement.cost for Electricity")
plot(costS,eI,xlab = "Procurement.cost for Steam")
plot(ePE,eI,xlab = "Energy purchase of Electricity")
plot(ePS,eI,xlab = "Energy purchase of Steam")
plot(ePC,eI,xlab = "Energy purchase of Coal")
plot(ePG,eI,xlab = "Energy purchase of Nature Gas")
plot(ePO,eI,xlab = "Energy purchase of Oil")
plot(ebS,eI,xlab = "Energy from byproduct Steam")
plot(eIEG,eI,xlab = "Energy from Internal Electricity Generation")
plot(eRR,eI,xlab = "Energy from Renewable Resource")
plot(eUseP,eI,xlab = "Energy use of Performance product")
plot(eUseA,eI,xlab = "Energy use of Argriculture product")
plot(eUseS,eI,xlab = "Energy use of Specialty product")

# due to Plant variable: GHG, productP, productA, productS, costC, costG, costE, costS, ePE, ePS, ePC, ePG, eUseP, eUseA, eUseS
library(ggplot2)
a <- ggplot(data = data, aes(x = GHG, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
b <- ggplot(data = data, aes(x = productP, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
c <- ggplot(data = data, aes(x = productA, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
d <- ggplot(data = data, aes(x = productS, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
e <- ggplot(data = data, aes(x = costC, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
f <- ggplot(data = data, aes(x = costG, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
g <- ggplot(data = data, aes(x = costE, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
h <- ggplot(data = data, aes(x = costS, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
i <- ggplot(data = data, aes(x = ePE, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
j <- ggplot(data = data, aes(x = ePS, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
k <- ggplot(data = data, aes(x = ePC, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
l <- ggplot(data = data, aes(x = ePG, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
m <- ggplot(data = data, aes(x = eUseP, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
n <- ggplot(data = data, aes(x = eUseA, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
o <- ggplot(data = data, aes(x = eUseS, y = eI, color = Plant)) + geom_point(aes(group = Plant)) + geom_point() #have effect
library('Rmisc')
multiplot(a, b, c, d, cols = 2)
multiplot(e, f, g, h, cols = 2)
multiplot(i, j, k, l, cols = 2)
multiplot(m, n, o, cols = 2)
# eniminate missing data effects
par(mfrow = c(2, 2))
plot(as.factor(Esold),eI,xlab = "Energy sold to grid") #no effect
plot(as.factor(ebS),eI,xlab = "Energy from byproduct Steam") #have effect
plot(as.factor(eIEG),eI,xlab = "Energy from Internal Electricity Generation")
plot(as.factor(eRR),eI,xlab = "Energy from Renewable Resource")

par(mfrow = c(1, 1))
M1 <- cor(cbind(saleP, saleA, saleS, costO, ePO, GHG, costC, costE, costG, costS, ePE, ePC, ePS, ePG, eUseP, eUseS, eUseA, productA, productP, productS))
corrplot(M1, type = 'lower')
# delete ePC and eUseP
M2 <- cor(cbind(saleP, saleA, saleS, costO, ePO, GHG, costC, costE, costG, costS, ePE, ePS, ePG, eUseS, eUseA, productA, productP, productS))
corrplot(M2, type = 'lower')

# start with plant and yr
fit1 = lm(eI ~ Plant + yr, data = data)
summary(fit1)

fit2 = lm(eI ~ saleP + saleA + saleS + costO + ePO + (GHG + costC + costE + costG + costS + ePE + ePC + ePS + ePG + eUseP + eUseS + eUseA + productA + productP + productS)* Plant)
plot(fit2)

# fit without Plant, yr,Esold, ebS, eIEG, eRR
fullmodel = lm(eI ~ saleP+saleA+saleS+costO+ePO+(GHG+productP+productA+productS+costC+costG+costE+costS+ePE+ePS+ePC+ePG+eUseP+eUseA+eUseS)*Plant, data = data)
summary(fullmodel)
par(mfrow = c(1, 3))
plot(fullmodel, which = c(1,2,4))
# fit with only significant variables
reducedmodel = lm(eI ~ saleA+saleS+costE+ePE+eUseS+(productP+productA+costE)*Plant, data = data)
summary(reducedmodel)
par(mfrow = c(1, 3))
plot(reducedmodel, which = c(1,2,4))

# BIC stepwise
library('MASS')
null <- lm(eI ~1, data=data)
stepback <- stepAIC(fullmodel,  k=log(nrow(data)), scope=list(lower=null, upper=fullmodel),direction = 'back', trace = FALSE)
summary(stepback)
# formula = eI ~ saleP + saleA + saleS + costO + GHG + costC + 
#         costE + costG + costS + eUseS + productA + productP + productS + 
#         Plant + GHG:Plant + costC:Plant + costE:Plant + costG:Plant + 
#         costS:Plant + eUseS:Plant + productA:Plant + productP:Plant + 
#         productS:Plant
# R-squared = 1
backmodel = lm(formula = eI ~ saleP + saleA + saleS + costO + GHG + costC + 
                       costE + costG + costS + eUseS + productA + productP + productS + 
                       Plant + GHG:Plant + costC:Plant + costE:Plant + costG:Plant + 
                       costS:Plant + eUseS:Plant + productA:Plant + productP:Plant + 
                       productS:Plant, data = data)
par(mfrow = c(1, 3))
plot(backmodel, which = c(1,2,4))
summary(backmodel)

stepforw <- stepAIC(reducedmodel, k=log(nrow(data)), scope=list(lower=null, upper=fullmodel), direction = 'forward', trace = FALSE)
summary(stepforw)
# formula = eI ~ saleA + saleS + costE + ePE + eUseS + productA + 
#         productP + Plant + costE:Plant + productA:Plant + productP:Plant + 
#         eUseS:Plant
# R-squared = 1
forwardmodel = lm(formula = eI ~ saleA + saleS + costE + ePE + eUseS + productA + 
                          productP + Plant + costE:Plant + productA:Plant + productP:Plant + 
                          eUseS:Plant, data = data)
par(mfrow = c(1, 3))
plot(forwardmodel, which = c(1,2,4))
summary(forwardmodel)

# lasso
library('lars')
attach(data)
Plant <- as.factor(Plant)
xfactors <- model.matrix(eI ~ (costE+productA+productP+eUseS)*Plant)[,-1]
x <- as.matrix(data.frame(saleA,saleS,costE,ePE,eUseS,productA,productP,xfactors))
predictors = scale(x)
eI.scaled = scale(eI)
object = lars(x = predictors, y = eI.scaled)
par(mfrow = c(1, 2))
plot(object)
plot.lars(object, xvar="df", plottype="Cp")
coef(object)
# Use the best Cp value to find best model from backmodel: 
best_step <- object$df[which.min(object$RSS)]
a <- summary(object)
coef(object, s=which.min(a$Cp), mode="step")
library(stargazer)
stargazer(coef(object, s=which.min(a$Cp), mode="step"), type = "text", title="Descriptive statistics", digits=1, flip=TRUE, out="table1.txt")
lassofit = lm(eI ~ saleA + saleS + costE + eUseS + productA + productP + costE:Plant + productA:Plant + productP:Plant + eUseS:Plant)
par(mfrow = c(1, 3))
plot(lassofit, which = c(1,2,4))
summary(lassofit)

# fit with significant variables
lassofit2 = lm(eI ~ saleS + costE + eUseS + costE:Plant + eUseS:Plant)
par(mfrow = c(1, 3))
plot(lassofit2, which = c(1,2,4))
summary(lassofit2) 

library('boot')
model = glm(eI ~ saleA + saleS + costE + eUseS + productA + productP + costE:Plant + productA:Plant + productP:Plant + eUseS:Plant, data = data)
par(mfrow = c(1, 3))
plot(model, which = c(1,2,4))
summary(model)
cv.err = cv.glm(data, model)
cv.err$delta
cv.err.6 = cv.glm(data, model, K = 6)
cv.err.6$delta[2]

