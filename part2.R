fish <- read.csv("fish.csv")
summary(fish)

fish$livebait <- as.factor(fish$livebait)
fish$camper <- as.factor(fish$camper)
summary(fish)

#
library(MASS)
library(countreg)
library(car)
library(statmod)
par( mfrow = c(3,2))
#

fish.glm <- glm(data = fish, count ~ livebait + camper + persons + child, family = poisson())
summary(fish.glm)
car::influenceIndexPlot(fish.glm)
car::influencePlot(fish.glm)

# we drop 89 and 138 
fish.new <- fish[-c(89, 138), ]
fish.new.glm <- glm(data = fish.new, count ~ livebait + camper + persons + child, family = poisson())
summary(fish.new.glm)
car::residualPlots(fish.new.glm)
car::marginalModelPlots(fish.new.glm)
car::influenceIndexPlot(fish.new.glm)

rootogram( object = fish.new.glm, plot = TRUE, style = "hanging")

# Zero-inflated poisson
library(pscl)

zero.poisson <- zeroinfl(data = fish.new, count ~ livebait + camper + persons + child | camper, dist = "poisson", link = "logit")

summary(zero.poisson)

rootogram( object = zero.poisson, plot = TRUE, style = "hanging")

# Negative binomial
NB.fish <- glm.nb(count ~ livebait + camper + persons + child, data = fish.new)

summary(NB.fish)

rootogram( object = NB.fish, plot = TRUE, style = "hanging")

# zero-inflated NB
zero.NB <- zeroinfl(data = fish.new, count ~ livebait + camper + persons + child | camper, dist = "negbin", link = "logit")

summary(zero.NB)

rootogram( object = zero.NB, plot = TRUE, style = "hanging")

library(pscl)
Hurdle <- pscl::hurdle(data = fish.new, count ~ livebait + camper + persons + child | camper, dist = "poisson", link = "logit")
rootogram( object = Hurdle, plot = TRUE, style = "hanging")

Hurdle.nb <- pscl::hurdle(data = fish.new, count ~ livebait + camper + persons + child | camper, dist = "negbin", link = "logit")
rootogram( object = Hurdle.nb, plot = TRUE, style = "hanging")



quasi.fish.glm <- glm(data = fish, count ~ livebait + camper + persons + child, family = quasipoisson())
summary(quasi.fish.glm)






