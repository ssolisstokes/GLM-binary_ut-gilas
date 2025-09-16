## Binary Generalized Linear Model Practice - final code for markdown
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
library(patchwork)


# load in dataset
gilas <- read.csv("data_gmocc_all-edited.csv")

# create the GLM
plot.glm <-
  ggplot(gilas, aes(rock_ind, detection)) +    
  geom_jitter(size=2, width = 0.25) +                                  # jitter helps to see all data points; width for spread     
  geom_smooth(method="glm",
              fullrange = TRUE,                                        # force fit line past limits of data
              method.args=list(family="binomial"(link="logit"))) +
  labs(title="Detection Probability::Substrate Texture - GLM Fit") +
  ylab ("Probability of Detection") +
  xlab ("Substrate Texture Index")

# create the LM
plot.lm <-
  ggplot(gilas, aes(rock_ind, detection)) +
  geom_jitter(size=2, width = 0.25) +
  geom_smooth(method = "lm",
              fullrange= TRUE) +
  labs(title = "Detection Probability::Substrate Texture - Linear Fit") +
  ylab("Probability of Detection") +
  xlab("Substrate Texture Index")

# use patchwork to see plots side by side
plot.lm + plot.glm


# run the model
model_rockiness <- glm(detection ~ rock_ind, data = gilas, family = binomial)
model_rockiness

# to plot on the logit scale, we have to create a new column in the dataframe 
  # remember cannot plot log(0) or log(1) and we're using binomial data
new_gilas <- data.frame(rock_ind = seq(0, 7, length.out = 100))
new_gilas$logit_fit <- predict(model_rockiness, newdata = new_gilas, type = "link")       #type = "link" returns predictions on the logit (log-odds) scale


#Adjust raw 0/1 data to avoid infinities (continuity correction)
gilas$detection_adj <- (gilas$detection + 0.5) / 2
gilas$logit_obs <- log(gilas$detection_adj / (1 - gilas$detection_adj))

# make another ggplot
ggplot() +
  geom_point(data = gilas, aes(x = rock_ind, y = logit_obs), color = "black", size = 3, alpha = 0.8) +
  geom_line(data = new_gilas, aes(x = rock_ind, y = logit_fit), color = "orange", linewidth = 1.2) +
  labs(
    title = "Substrate Model on Logit Scale",
    subtitle = "intercept = -13.57, slope = 1.76",
    x = "Substrate Texture Index",
    y = "Logit(Probability of Detection)")

# create binned residual plot
x <- predict(model_rockiness)
y <- resid(model_rockiness)
binnedplot(x,y)
  # my dataset it so small that it cannot create SEs

# interpret the results!

coef(model_rockiness)
  # interpret the slope  of the regression
  # divide the slope by 4 for an estimate of maximum predicted effect.
  # 1 unit of the index becomes "sandier" or "more granular"
  # 1.759/4 = 0.439 ; *100 = 44%
  # this means that as you "increase" up the index scale you have a 44% higher chance of detecting a Gila monster

confint(model_rockiness)
  # the 2.5% and 97.5% do not overlap 0; yay!

summary(model_rockiness)
# ensuring the residual deviance is less than the degrees of freedom
# useful also if comparing models (not right now) AICs
# reminder that P value is significant P < 0.001 
  # there is a very low chance that our results are from random chance, and we can be very confident that our results are based on the
    # interaction of substrate texture and detection


