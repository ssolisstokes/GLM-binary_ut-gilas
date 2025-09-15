## Binary Generalized Linear Model Practice - ground temp
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

# load in dataset
data_gilas <- read.csv("data_gmocc_all-ut.csv")

glimpse(data_gilas)

# create the table we need for the model
# detection, rockiness, tran_id
gilas <-
  data_gilas %>%
  select(detection,gr_temp, tran_id)

# copy code from example in lecture and create ggplot
ggplot(gilas, aes(gr_temp, detection)) +
  geom_point(size=3) +
  geom_smooth(method="glm",
              fullrange = TRUE, #force fit line past limits of data
              method.args=list(family="binomial"(link="logit"))) +
  labs(title="GLM Detection of Gila Monsters (Utah)") +
  ylab ("Probability of Detection") +
  xlab ("Ground Temp")


# create the model
model_gilas.gr_temp <- glm(detection ~ gr_temp, data = gilas, family=binomial)
model_gilas.gr_temp

# plot the residuals
x <- predict(model_gilas.gr_temp)
y <- resid(model_gilas.gr_temp)
binnedplot(x, y)

coef(model_gilas.gr_temp)
confint(model_gilas.gr_temp)

summary(model_gilas.gr_temp)

performance::check_model(model_gilas.gr_temp)

y
