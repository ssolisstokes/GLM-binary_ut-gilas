## Binary Generalized Linear Model Practice - air temp
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

# load in dataset
data_gilas <- read.csv("data_gmocc_all-ut.csv")

glimpse(data_gilas)

# copy code from example in lecture and create ggplot
ggplot(data_gilas, aes(air_temp, detection)) +
  geom_point(size=3) +
  geom_smooth(method="glm",
              fullrange = TRUE, #force fit line past limits of data
              method.args=list(family="binomial"(link="logit"))) +
  labs(title="GLM Detection of Gila Monsters (Utah)") +
  ylab ("Probability of Detection") +
  xlab ("Air Temperature")


# create the model
model_gilas.air_temp <- glm(detection ~ air_temp, data = data_gilas, family=binomial)
model_gilas.air_temp

# plot the residuals
x <- predict(model_gilas.air_temp)
y <- resid(model_gilas.air_temp)
binnedplot(x, y)

coef(model_gilas.air_temp)
confint(model_gilas.air_temp)

summary(model_gilas.air_temp)

performance::check_model(model_gilas.air_temp)
y
