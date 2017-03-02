library(AER)
library("faraway")
library("car")
library("ggplot2")
library("gridExtra")
library("scatterplot3d")
library("rgl")
data("Journals")
# View(Journals)
## data and transformed variables
data("Journals")
journals <- Journals[, c("subs", "price")]
journals$citeprice <- Journals$price/Journals$citations
journals$age <- 2016 - Journals$foundingyear
journals$chars <- Journals$charpp*Journals$pages/10^6


summary(journals)
@## Stock and Watson (2007)

## Figure 8.9 (a) and (b)
plot(subs ~ citeprice, data = journals, pch = 19)
attach(journals)
cor(subs,citeprice)
# for low citeprice there are more subscriptions and there are few subs for high citeprice
plot(log(subs) ~ log(citeprice), data = journals, pch = 19)
cor(log(subs),log(citeprice))
fm1 <- lm(log(subs) ~ log(citeprice), data = journals)
abline(fm1)
summary(fm1)


mod <- fortify(fm1)
p1 <- qplot(.fitted, .resid, data = mod) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red", 
                                                                                   se = F)
qplot(subs, .resid, data = mod)
qplot(price, .resid, data = mod)
qplot(age, .resid, data = mod)
qplot(citeprice, .resid, data = mod)
qplot(pages,subs,data = Journals)

## Table 8.2, use HC1 for comparability with Stata 
fm2 <- lm(subs ~ citeprice + age + chars, data = log(journals))
summary(fm2)
shapiro.test(residuals(fm2))

fm3 <- lm(subs ~ citeprice + I(citeprice^2) + I(citeprice^3) +
            age + I(age * citeprice) + chars, data = log(journals))
summary(fm3)
shapiro.test(residuals(fm3))


fm4 <- lm(subs ~ citeprice + age + I(age * citeprice) + chars, data = log(journals))
summary(fm4)
shapiro.test(residuals(fm4))


fm5 <- lm(subs ~ citeprice  +
            age + I(age * citeprice) + chars, data = log(journals))

summary(fm5)
shapiro.test(residuals(fm5))

ggplot(fm5, aes(.fitted, z)) +
  geom_point() +
  geom_hline(yintercept=c(-2,0,2), color="dark blue", linetype="dashed") +
  ggtitle("Residual Plot")



## changes with respect to age
library("strucchange")
## Nyblom-Hansen test
scus <- gefp(subs ~ citeprice, data = log(journals), fit = lm, order.by = ~ age)
plot(scus, functional = meanL2BB)
## estimate breakpoint(s)
journals <- journals[order(journals$age),]
bp <- breakpoints(subs ~ citeprice, data = log(journals), h = 20)

plot(bp)
bp.age <- journals$age[bp$breakpoints]
## visualization
plot(subs ~ citeprice, data = log(journals), pch = 19, col = (age > log(bp.age)) + 1)
abline(coef(bp)[1,], col = 1)
abline(coef(bp)[2,], col = 2)
legend("bottomleft", legend = c("age > 18", "age < 18"), lty = 1, col = 2:1, bty = "n")





min.price <- 0
bin.glm <- glm(subs ~ price, data=journals, 
               family=binomial(link = "logit"))
summary(bin.glm)

qplot(data=journals,x=price,y=subs)
qplot(data=journals,x=price,y=subs,size=I(2))
p <- qplot(data=Journals,x=journals$price,y=journals$subs,colour=field,size=I(2))
p+geom_smooth(method = "lm", formula = y ~ x, colour = "red", se = F)
p+geom_smooth(method = "lm", formula = y ~ x, colour = "red", se = F) + geom_smooth(method = "loess", 
                                                                                    formula = y ~ x, colour = "green", se = F)
qplot(data=Journals,x=citations,y=subs)
qplot(data=journals,x=age,y=subs)
qplot(data=Journals,x=charpp,y=subs)


# <----------------------------------PART1:Model 1
pairs(~subs+citeprice+age+chars)

library("GGally")
ggpairs(journals,columns = c(1:3))


library(ggplot2)
fit <- lm(subs~.,data=journals)
summary(fit)
qplot(fitted.values(fit), subs, data=journals) +
  geom_abline(intercept=0, slope=1)


ggplot(fit, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept=0, color="dark blue", linetype="dashed") +
  ggtitle("Residual Plot")
# we see a pattern or a floor 
# there should not be any pattern it should be random

cor(fitted.values(fit), subs) #0.7071087 which is good
summary(fit)$r.square

x <- mean(residuals(fit))#Calculating the mean of residuals of model fit 
x
y <- sd(residuals(fit))#Calculating standard deviation of residuals of model fit
y
z <- (residuals(fit)-x)/y#calculating z score using formula

#plotting the residual plot of Z scores
ggplot(fit, aes(.fitted, z)) +
  geom_point() +
  geom_hline(yintercept=c(-2,0,2), color="dark blue", linetype="dashed") +
  ggtitle("Residual Plot")
# We have floor effect. And residuals are not normal.


# <------------------------------------part 2

# ---------Density plots-----------#

plota <- qplot(citeprice, data = journals, geom="density")
plota
# the variable citeprice is skewed

plotb <- qplot(age, data = journals, geom="density")
plotb
# age is also skewed


# -------------Normal QQ plots-----------#


plotc <- qplot(sample=citeprice, data = journals)+
          geom_abline(intercept = 0,slope = 1)
plotc

qplot(sample=residuals(fit), data = journals)
plot(fit)

# <----------Regressions---------#
library(MASS)
g2 <- rlm(subs~price+age+chars, psi = psi.huber, 
          journals)
g3 <- rlm(subs~price+age+chars, psi = psi.bisquare, 
          init = "lts", maxit = 100, journals)
g4 <- rlm(subs~price+age+chars, psi = psi.hampel, 
          init = "lts", maxit = 100, journals)
library(robustbase)
g5 <- ltsReg(subs~price+age+chars, data = journals)
# g6 <- ltsReg(subs~price+age+chars, data =journals, 
             # nsamp = "exact")
library(quantreg)
g7 <- rq(subs~price+age+chars, data = journals)
coefs <- compareCoefs(g, g2, g3, g4, g5, g7, se = FALSE)
colnames(coefs) <- c("OLS", "Huber", "Bisquare", "Hample", "LTS",  
                     "LAD")
coefs






# -------------------------
g <- lm(subs~.,data=journals)

summary(g)
shapiro.test(residuals(g))
car::durbinWatsonTest(residuals(g))
car::vif(g)
par(mfrow = c(1, 2))
plot(g)



library(car)
outlierTest(g)  # Bonferonni p-value for most extreme obs
qqPlot(g, main = "QQ Plot")  # qq plot for studentized resid 


# --------cooks distance
journ <- row.names(journals)
halfnorm(cooks.distance(g), 3, labs = journ, ylab = "Cook's distance")



library(car)
influencePlot(g1, .method=cooks.distance(g1), id.n=4)

# ---------------models
g1 <- lm(subs ~ price+foundingyear+charss+pages data = Journals)
g2 <- lm(subs ~ citeprice + age + chars, data = log(journals))

g3 <- lm(subs ~ citeprice + I(citeprice^2) + I(citeprice^3) +
           age + I(age * citeprice) + chars, data = log(journals))

g4 <- lm(subs ~ citeprice + age + I(age * citeprice) + chars, data = log(journals))
g5 <- lm(subs ~ citeprice  +
           age + I(age * citeprice) + chars, data = log(journals))
summary(g5)

# ------------------CVlm-------------
seed <- round(runif(1, min=0, max=100))
oldpar <- par(mfrow=c(2,3))
mse.g1 <- CVlm(data = journals, 
               form.lm=g1, 
               m=4, 
               seed=seed, 
               printit=F,
               main = "g1")
mse.g2 <- CVlm(data = journals, 
               form.lm=g2, 
               m=4, 
               seed=seed, 
               printit=F,
               main = "g2")
mse.g3 <- CVlm(data = journals, 
               form.lm=g3, 
               m=4, 
               seed=seed, 
               printit=F,
               main = "g3")
mse.g4 <- CVlm(data = journals, 
               form.lm=g4, 
               m=4, 
               seed=seed, 
               printit=F,
               main = "g4")
mse.g5 <- CVlm(data = journals, 
               form.lm=g5, 
               m=4, 
               seed=seed, 
               printit=F,
               main = "g5")
par(oldpar)




data.frame(mse.g1=attr(mse.g1, "ms"),
           mse.g2=attr(mse.g2, "ms"),
           mse.g3=attr(mse.g3, "ms"),
           mse.g4=attr(mse.g4, "ms"),
           mse.g5=attr(mse.g5, "ms"))


df <- data.frame( 
                 mse.g2=NULL, 
                 mse.g3=NULL,
                 mse.g4=NULL,
                 mse.g4=NULL)
for (i in 1:10) {
  seed <- round(runif(1, min=0, max=100))
  oldpar <- par(mfrow=c(2,3))
  
  mse.g2 <- CVlm(data = journals, 
                 form.lm=g2, 
                 m=4, 
                 seed=seed, 
                 printit=F,
                 main = "g2")
  mse.g3 <- CVlm(data = journals, 
                 form.lm=g3, 
                 m=4, 
                 seed=seed, 
                 printit=F,
                 main = "g3")
  mse.g4 <- CVlm(data = journals, 
                 form.lm=g4, 
                 m=4, 
                 seed=seed, 
                 printit=F,
                 main = "g4")
  mse.g5 <- CVlm(data = journals, 
                 form.lm=g5, 
                 m=4, 
                 seed=seed, 
                 printit=F,
                 main = "g5")
  par(oldpar)
  
  
  
  
  data.frame(
             mse.g2=attr(mse.g2, "ms"),
             mse.g3=attr(mse.g3, "ms"),
             mse.g4=attr(mse.g4, "ms"),
             mse.g5=attr(mse.g5, "ms"))
}
df
