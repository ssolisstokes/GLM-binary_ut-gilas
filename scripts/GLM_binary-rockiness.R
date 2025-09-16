## Binary Generalized Linear Model Practice - rockiness
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
plot.01 <- 
  ggplot(data_gilas, aes(rock_ind, detection)) +
  geom_jitter(size=3) +
  geom_smooth(method="glm",
              fullrange = TRUE, #force fit line past limits of data
              method.args=list(family="binomial"(link="logit"))) +
  labs(title="GLM Detection of Gila Monsters (Utah)") +
  ylab ("Probability of Detection") +
  xlab ("Rockiness Index")

# create the model
model_gilas.rockiness01 <- glm(detection ~ rock_ind, data = data_gilas, family=binomial)
model_gilas.rockiness01

# plot the residuals
x.01 <- predict(model_gilas.rockiness01)
y.01 <- resid(model_gilas.rockiness01) 
binned.01 <- binnedplot(x.01, y.01)

coef(model_gilas.rockiness01)
confint(model_gilas.rockiness01)

summary(model_gilas.rockiness01)

performance::check_model(model_gilas.rockiness01)
y

##### trial 2 with edited detection rockiness indexes 
# load in dataset
data_gilas.edited <- read.csv("data_gmocc_all-edited.csv")

# copy code from example in lecture and create ggplot
plot.02 <-
  ggplot(data_gilas.edited, aes(rock_ind, detection)) +
  geom_jitter(size=3) +
  geom_smooth(method="glm",
              fullrange = TRUE, #force fit line past limits of data
              method.args=list(family="binomial"(link="logit"))) +
  labs(title="GLM Detection of Gila Monsters (Utah)") +
  ylab ("Probability of Detection") +
  xlab ("Rockiness Index")

# create the model
model_gilas.rockiness02 <- glm(detection ~ rock_ind, data = data_gilas.edited, family=binomial)
model_gilas.rockiness02

# plot the residuals
x.02 <- predict(model_gilas.rockiness02)
y.02 <- resid(model_gilas.rockiness02)
binned.02 <- binnedplot(x.02, y.02)

coef(model_gilas.rockiness02)
confint(model_gilas.rockiness02)

performance::check_model(model_gilas.rockiness02)
y

### comparisons

plot.01
plot.02

binned.01
binned.02

coef(model_gilas.rockiness01)
coef(model_gilas.rockiness02)

confint(model_gilas.rockiness01)
confint(model_gilas.rockiness02)

summary(model_gilas.rockiness01)

summary(model_gilas.rockiness02)
