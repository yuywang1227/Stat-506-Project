# set local directory
setwd("C:/Users/zhang/Desktop/fall 2019/git/506/project")

# load packages
library(tidyverse)
library(data.table)
library(foreign)
library(MASS)

## Data

# Import data
sleep_disorder <- setDT(read.xport("SLQ_I.XPT"))
tot_nutrition_d1 <-setDT(read.xport("DR1TOT_I.XPT"))
demo <- setDT(read.xport("DEMO_I.XPT"))

# to keep our codes consistent, I rename some of the variables
# Data cleaning
sleep_disorder = sleep_disorder[SLQ120 <= 4, .(respondent_ID = SEQN, sleep_hr = SLD012, sleepy_freq = factor(SLQ120))]

tot_nutrition = tot_nutrition_d1[, .(respondent_ID = SEQN, energy = DR1TKCAL, CHO = DR1TCARB)
                                ][, .(p_CHO = CHO*4/energy), by = respondent_ID
                                ][, carb_level := 0
                                ][p_CHO < 0.45, carb_level := 1
                                ][p_CHO > 0.65, carb_level := 2
                                ][, carb_level := factor(carb_level)
                                ]

demo = demo[RIDAGEYR >= 16, .(respondent_ID = SEQN, winter = factor(RIDEXMON), female = factor(RIAGENDR-1), age_yr = RIDAGEYR)]

# merge these three datasets
sleep = merge(sleep_disorder, tot_nutrition, by = "respondent_ID") %>%
  merge(. , demo, by = "respondent_ID") %>%
  na.omit(.)

sleep # the cleaned data


## Ordered logistic regression

# fit ordered logit model 
m <- polr(sleepy_freq ~ sleep_hr + carb_level + winter + female + age_yr, data = sleep, Hess = TRUE)

# view a summary of the model
summary(m)

# store the coefficientts
mcoef <- coef(summary(m))

# calculate p values
p <- pnorm(abs(mcoef[, "t value"]), lower.tail = FALSE) *2
mcoef = cbind(mcoef, "pvalue" = p)
mcoef

# 95% CI
(ci <- confint(m))
ci
# Interpretation: e.g. for carb_level, we would say that for a one unit increase in carb_level (i.e., going from 0 to 1), 
# we expect a 0.11 increase in the expected value of apply on the log odds scale, given all of the other variables in the model are held constant.

# odds ratios
exp(coef(m))

# odds ratios & CI
ci = exp(cbind(OR = coef(m), ci))
ci
