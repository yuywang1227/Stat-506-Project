# set local directory
setwd("C:/Users/zhang/Desktop/fall 2019/git/506/project")

# load packages
library(tidyverse)
library(data.table)
library(foreign)
library(MASS)

## Data

# Import data
sleep_disorder <- setDT(read.xport("sleep_disorder1516.XPT"))
tot_nutrition_d1 <-setDT(read.xport("nutrition_day1.XPT"))
demo <- setDT(read.xport("demo1516.XPT"))

# Data ckeaning
sleep_disorder = sleep_disorder[SLQ120 <= 4, .(respondent_ID = SEQN, sleep_hr = SLD012, sleepy_freq = factor(SLQ120))]

tot_nutrition = tot_nutrition_d1[, .(respondent_ID = SEQN, energy = DR1TKCAL, CHO = DR1TCARB)
                                ][, .(p_CHO = CHO*4/energy), by = respondent_ID
                                ][, CHO_level := 0
                                ][p_CHO < 0.45, CHO_level := 1
                                ][p_CHO > 0.65, CHO_level := 2
                                ][, CHO_level := factor(CHO_level)
                                ]

demo = demo[RIDAGEYR >= 16, .(respondent_ID = SEQN, six_month = factor(RIDEXMON), gender = factor(RIAGENDR), age = RIDAGEYR)]

# merge these three datasets
sleep = merge(sleep_disorder, tot_nutrition, by = "respondent_ID") %>%
  merge(. , demo, by = "respondent_ID") %>%
  na.omit(.)

sleep # the cleaned data


## Ordered logistic regression

# fit ordered logit model 
m <- polr(sleepy_freq ~ sleep_hr + CHO_level + six_month + gender + age, data = sleep, Hess = TRUE)

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
# Interpretation: e.g. for CHO_level, we would say that for a one unit increase in CHO_level (i.e., going from 0 to 1), 
# we expect a 0.11 increase in the expected value of apply on the log odds scale, given all of the other variables in the model are held constant.

# odds ratios
exp(coef(m))

# odds ratios & CI
ci = exp(cbind(OR = coef(m), ci))
ci
