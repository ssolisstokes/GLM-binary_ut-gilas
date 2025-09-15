## Binary Generalized Linear Model Practice - elevation
## SSS 14 SEPT 2025

# load in libraries
library(ggplot2)
library(arm)
library(ggfortify)
library(grid)
library(readr)
library(dplyr)
library(AICcmodavg)
library(performance)
library(see)

install.packages("see")
# load in dataset
data_gilas <- read.csv("data_gmocc_all-ut.csv")

glimpse(data_gilas)


# copy code from example in lecture and create ggplot
ggplot(data_gilas, aes(elev, detection)) +
  geom_point(size=3) +
  geom_smooth(method="glm",
              fullrange = TRUE, #force fit line past limits of data
              method.args=list(family="binomial"(link="logit"))) +
  labs(title="GLM Detection of Gila Monsters (Utah)") +
  ylab ("Probability of Detection") +
  xlab ("Elevation (m)")


# create the model
model_gilas.elev <- glm(detection ~ elev, data = data_gilas, family=binomial)
model_gilas.elev 

# plot the residuals
x <- predict(model_gilas.elev)
y <- resid(model_gilas.elev)
binnedplot(x, y)

coef(model_gilas.elev)
confint(model_gilas.elev)

summary(model_gilas.elev)

performance::check_model(model_gilas.elev )
y
