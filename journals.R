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

# --------------------------------------------------SUMMARY-----
summary(Journals)

# -------------------------------------------------------Model g---
g <- lm(subs~.-field-title-publisher,data = Journals)
g1 <- step(g)
summary(g)
# ggpairs(Journals,c(3:9))

qplot(fitted.values(g), subs, data=Journals) +
  geom_abline(intercept=0, slope=1)

ggplot(g, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept=0, color="dark blue", linetype="dashed") +
  ggtitle("Residual Plot")


s <- scale(Journals$subs, center = TRUE, scale = TRUE)
boxplot(Journals$subs,ylab = "subscription",main = "Box Plot")

boxplot(Journals$subs)
s <- scale(Journals$subs, center = TRUE, scale = TRUE)
x <- (s-mean(s))/sd(s)

boxplot(Journals$subs,Journals$price,
        
        main = "Box Plot")
 qplot(field,subs, data=Journals, geom=c("boxplot"))
 boxplot(Journals$price,ylab = "Price",main = "Box Plot")
 boxplot(Journals$price,ylab = "Price",main = "Box Plot")
 
 
# --------------------------------------------------------TRANSFORMATION OF VARIABLES

 journals <- Journals[, c("subs", "price")]
 journals$citeprice <- Journals$price/Journals$citations
 journals$age <- 2000 - Journals$foundingyear
 journals$chars <- Journals$charpp*Journals$pages/10^6
 summary(journals)
library(GGally)
# ggpairs(journals)

plot(subs ~ citeprice, data = journals, pch = 19)
plot(log(subs) ~ log(citeprice), data = journals, pch = 19)
fm1 <- lm(log(subs) ~ log(citeprice), data = journals)
abline(fm1)



g2 <- lm(subs ~ citeprice + age + chars, data = log(journals))
summary(g2)
shapiro.test(residuals(g2))# it is not normally distributed

qplot(fitted.values(g2), subs, data=journals) +
  geom_abline(intercept=0, slope=1)

mod2 <- fortify(g2)
p2 <- qplot(.fitted, .resid, data = mod2) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red",  se = F)
p2

qqnorm(fitted.values(g2))



g3 <- lm(subs ~ citeprice + I(citeprice^2) + I(citeprice^3) +
            age + I(age * citeprice) + chars, data = log(journals))
summary(g3)
shapiro.test(residuals(g3))# it is not normally distributed

qplot(fitted.values(g3), subs, data=journals) +
  geom_abline(intercept=0, slope=1)

mod3 <- fortify(g3)
p3 <- qplot(.fitted, .resid, data = mod2) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red",  se = F)
p3

qplot(fitted.values(g3), subs, data=journals) +
  geom_abline(intercept=0, slope=1)


g4 <- step(g3)

mod4 <- fortify(g4)
p4 <- qplot(.fitted, .resid, data = mod4) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red",  se = F)
p4



qplot(fitted.values(g4), subs, data=journals) +
  geom_abline(intercept=0, slope=1)
attach(journals)
cor(fitted.values(g2), subs)
cor(fitted.values(g3), subs)

cor(fitted.values(g4), subs)



p1 <- qplot(sample = scale(.resid), data = mod2) + geom_abline(intercept = 0, 
                                                                slope = 1, color = "red") + labs(title = "Untransformed y", y = "Residuals")
p2 <- qplot(sample = scale(.resid), data = mod4) + geom_abline(intercept = 0, 
                                                                slope = 1, color = "red") + labs(title = "Sqrt-Tranformed y", y = "Residuals")
grid.arrange(p1, p2, nrow = 2)


p1 <- qplot(scale(.resid), data = mod2, geom = "blank") + geom_line(aes(y = ..density.., 
                                                                         colour = "Empirical"), stat = "density") + stat_function(fun = dnorm, aes(colour = "Normal")) + 
  geom_histogram(aes(y = ..density..), alpha = 0.4) + scale_colour_manual(name = "Density", 
                                                                          values = c("red", "blue")) + theme(legend.position = c(0.85, 0.85)) + labs(title = "Model g2", 
                                                                                                                                                     y = "Residuals")
p2 <- qplot(scale(.resid), data = mod4, geom = "blank") + geom_line(aes(y = ..density.., 
                                                                         colour = "Empirical"), stat = "density") + stat_function(fun = dnorm, aes(colour = "Normal")) + 
  geom_histogram(aes(y = ..density..), alpha = 0.4) + scale_colour_manual(name = "Density", 
                                                                          values = c("red", "blue")) + theme(legend.position = c(0.85, 0.85)) + labs(title = "Model g4", 
                                                                                                                                                     y = "Residuals")
grid.arrange(p1, p2, nrow = 2)



p1 <- qplot(sample = scale(.resid), data = mod3) + geom_abline(intercept = 0, 
                                                                slope = 1, color = "red") + labs(title = "Normal QQ-Plot", y = "Residuals Sqrt-transformed")
p2 <- qplot(sample = scale(.resid), data = mod4) + geom_abline(intercept = 0, 
                                                                 slope = 1, color = "red") + labs(title = "Normal QQ-Plot", y = "Residuals Box-Cox-Transform")
grid.arrange(p1, p2, nrow = 1)


vif(g3)

library("faraway")
influencePlot(g4)

summary(g4, corr=TRUE)$corr
summary(g4)
mod <- fortify(g4)
plot(ellipse(mod, c("citeprice", "age")), 
     type = "l", 
     xlim = c(-1,0),
     main = "Joint Confidence Region")
points(0,0)
points(coef(g4)["citeprice"], coef(g4)["age"], 
       pch=18)
abline(v=confint(g4)["pop15",], lty=2)
abline(h=confint(g4)["pop75",], lty=2)





shapiro.test(residuals(g4))# it is not normally distributed

qplot(fitted.values(g4), subs, data=journals) +
  geom_abline(intercept=0, slope=1)

mod4 <- fortify(g4)
p4 <- qplot(.fitted, .resid, data = mod4) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red",  se = F)
p4

summary(lm(abs(residuals(g4)) ~ fitted(g4)))
# Conclusion: The t-test does not reject constant error variance with a level of significance 10%, since the p-value, 0.3292, is greater than 0.05.
anova(g4,g3)
# Conclusion: The F-Ratio 0.2494 is small, therefore take Model g4 (the small model) since p-value 0.7795 is greater than 0.05



# ------------------CVlm-------------
library(DAAG)
seed <- round(runif(1, min=0, max=100))
oldpar <- par(mfrow=c(2,3))
mse.g <- CVlm(data = Journals, 
               form.lm=g, 
               m=4, 
               seed=seed, 
               printit=F,
               main = "g")
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

par(oldpar)




data.frame(
           mse.g2=attr(mse.g2, "ms"),
           mse.g3=attr(mse.g3, "ms"),
           mse.g4=attr(mse.g4, "ms")
           )
df <- data.frame( 
                 mse.g2=NULL, 
                 mse.g3=NULL,
                 mse.g4=NULL)


for (i in 1:10) {
  seed <- round(runif(1, min=0, max=100))
  oldpar <- par(mfrow=c(1,3))
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
  par(oldpar)
  df.temp <- data.frame(
                        mse.g2=attr(mse.g2, "ms"),
                        mse.g3=attr(mse.g3, "ms"),
                        mse.g4=attr(mse.g4, "ms")
                        )
  df <- rbind(df,df.temp)

}


df
# ---------------------------------------------BOX COX TRANSFORM

library(car)
(lambda <- powerTransform(g4))
lam <- lambda$lambda
glam <- lm(subs^lam ~ citeprice + age + I(age * citeprice) + chars, data = log(journals))
modlam <- fortify(glam)
p1 <- qplot(sample = scale(.resid), data = modg4) + geom_abline(intercept = 0, 
                                                                slope = 1, color = "red") + labs(title = "Normal QQ-Plot", y = "Residuals Sqrt-transformed")
p2 <- qplot(sample = scale(.resid), data = modlam) + geom_abline(intercept = 0, 
                                                                 slope = 1, color = "red") + labs(title = "Normal QQ-Plot", y = "Residuals Box-Cox-Transform")
grid.arrange(p1, p2, nrow = 1)
p4 <- qplot(.fitted, .resid, data = modlam) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red",  se = F)


library("faraway")
influencePlot(g4)


journ <- row.names(journals)
halfnorm(lm.influence(g4)$hat, labs = journ, ylab = "Leverages")


# ----------------------cooks distance   
cook <- cooks.distance(g4)
halfnorm(cook, 3, labs = journ, ylab = "Cook's distance")


g41 <- lm(subs ~ citeprice + age + I(age * citeprice) + chars, data = log(journals),subset = (cook < max(cook)))
compareCoefs(g4, g41)
summary(g41)
confint(g4)
confint(g41)
plot(ellipse(g3,c("log(age)", "log(chars)")), 
     type = "l", 
     xlim = c(-0.05,0.05),
     main = "Joint Confidence Region")
points(0,0)
points(coef(g4)["citeprice"], coef(g4)["age"], 
       pch=18)
abline(v=confint(g4)["citeprice",], lty=2)
abline(h=confint(g4)["age",], lty=2)