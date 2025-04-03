# PROBLEM 1: Armfolding

## a. Load and Examine the data

# load the data
armfold <- read.csv("armfold.csv")

# Number of male and female students
table(armfold$Sex)

# sample proportion of males who folded left arm on top
# sample proportion of females who folded left arm on top 
prop.table(table(armfold$LonR_fold, armfold$Sex), margin = 2)


## b. Observed Difference Between Two Groups

# observed diff in proportions btw 2 groups (males-females)
prop_table = prop.table(table(armfold$Sex, armfold$LonR_fold), margin = 1)

p_male = prop_table["Male", "1"]
p_female = prop_table["Female", "1"]

obs_diff = p_male - p_female
obs_diff

## c.

# 95% confidence interval for diff in proportions (males-females)

# report result from R's built-in function (should be agree with hand-calculated value)

# Count of left-on-top for males and females
table_mf <- table(armfold$Sex, armfold$LonR_fold)

# Left-on-top counts
x <- c(table_mf["Male", "1"], table_mf["Female", "1"])
x

# Total counts
n <- c(sum(table_mf["Male", ]), sum(table_mf["Female", ]))
n

# CI for difference in proportions (male - female)
prop.test(x = x, n = n, correct = FALSE)

# formula for standard error for diff in proportions
# values you plugged into formula
# z value you used and WHY 

## d. 

## e.

## f.

## g.

## h.

## i.


# PROBLEM 2: Get out the vote

## PART A

# how much more likely are GOTV call recipients to have voted in 1998? 

# proportion of those receiving GOTV call who voted in 1998
# sample proportion of those NOT receiving GOTV call who voted in 1998
# large-sample 95% CI for diff in these two proportions: proportions of voting in 1998 (voted1998==1) for those who received GOTV call vs. those did NOT. 

# Load libraries
library(tidyverse)
library(mosaic)

# Load data
turnout <- read_csv("turnout.csv")

# Calculate proportions
mean(voted1998 ~ GOTV_call, data = turnout)

# Difference in proportions
diffmean(voted1998 ~ GOTV_call, data = turnout)

# Large-sample confidence interval (like we did with prop.test earlier)
x <- c(sum(turnout$GOTV_call == 1 & turnout$voted1998 == 1),
       sum(turnout$GOTV_call == 0 & turnout$voted1998 == 1))
n <- c(sum(turnout$GOTV_call == 1),
       sum(turnout$GOTV_call == 0))
prop.test(x = x, n = n, correct = FALSE)

## PART B

#proportion who voted in 1996, by GOTV_call
mean(voted1996 ~ GOTV_call, data = turnout)

#Mean AGE, by GOTV_call
mean(AGE ~ GOTV_call, data = turnout)

#Proportion registered with a major party, by GOTV_call
mean(MAJORPTY ~ GOTV_call, data = turnout)

# Difference in proportions for voted1996
prop.test(table(turnout$voted1996, turnout$GOTV_call)[2,], 
          colSums(table(turnout$voted1996, turnout$GOTV_call)), correct = FALSE)

# CI for AGE difference
t.test(AGE ~ GOTV_call, data = turnout)

# CI for MAJORPTY difference
prop.test(table(turnout$MAJORPTY, turnout$GOTV_call)[2,],
          colSums(table(turnout$MAJORPTY, turnout$GOTV_call)), correct = FALSE)

## PART C

library(MatchIt)
# match on confounders
matched <- matchit(GOTV_call ~ voted1996 + AGE + MAJORPTY,
                   data = turnout,
                   ratio = 5)

matched_data <- match.data(matched)

# balance for matched data
# proportion who voted in 1996.
mean(voted1996 ~ GOTV_call, data = matched_data)

# Mean AGE
mean(AGE ~ GOTV_call, data = matched_data)

# Proportion registered with major party
mean(MAJORPTY ~ GOTV_call, data = matched_data)

# CI for each confounder 1 : voted1996
prop.test(table(matched_data$voted1996, matched_data$GOTV_call)[2,], 
          colSums(table(matched_data$voted1996, matched_data$GOTV_call)), 
          correct = FALSE)

# CI for each confounder 2 : AGE
t.test(AGE ~ GOTV_call, data = matched_data)

# CI for each confounder 3 : MAJORPTY
prop.test(table(matched_data$MAJORPTY, matched_data$GOTV_call)[2,],
          colSums(table(matched_data$MAJORPTY, matched_data$GOTV_call)), 
          correct = FALSE)


# Estimate treatment effect ONLY using matched data
# repeating PART A w/ matched_data
# proportion who voted in 1998 w/ GOTV
mean(voted1998 ~ GOTV_call, data = matched_data)
# 95% CI for diff in voting
prop.test(table(matched_data$voted1998, matched_data$GOTV_call)[2,],
          colSums(table(matched_data$voted1998, matched_data$GOTV_call)), 
          correct = FALSE)
