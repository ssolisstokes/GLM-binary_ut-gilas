## Binary Generalized Linear Model Practice - total prey presence
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


# summarise prey presence
data_gilas <- data_gilas %>%
  mutate(total_pp = pp_mam + pp_rep + pp_gbird)


# copy code from example in lecture and create ggplot
ggplot(data_gilas, aes(total_pp, detection)) +
  geom_point(size=3) +
  geom_smooth(method="glm",
              fullrange = TRUE, #force fit line past limits of data
              method.args=list(family="binomial"(link="logit"))) +
  labs(title="GLM Detection of Gila Monsters (Utah)") +
  ylab ("Probability of Detection") +
  xlab ("Prey Presence")


# create the model
model_gilas.total_pp <- glm(detection ~ total_pp, data = data_gilas, family=binomial)
model_gilas.total_pp

# plot the residuals
x <- predict(model_gilas.total_pp)
y <- resid(model_gilas.total_pp)
binnedplot(x, y)

coef(model_gilas.total_pp)
confint(model_gilas.total_pp)

summary(model_gilas.total_pp)

performance::check_model(model_gilas.total_pp)
y
