# Load required packages through package manager "pacman" ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # for any additional wrangling needed
               Hmisc, # for correlation with p-values
               psych, # for data summary
               multcomp, reghelper, # for linear combinations
               rlang, interactions, # for any regions of significance
               prediction, # for predicted outcomes
               gtsummary, # for linear regression tables
               here, # for relative file paths
               table1) # for descriptive stats table
                
# Load functions for customised outputs from Prof Lesa Hoffman ----
source(here::here("Rscripts", "DrHoffman_Functions.R"))

# Import cleaned data set 
load(here::here("data", "pay_clean.RData"))

# T-test to compare mean base pay between Male and Female ----
t.test(BasePay ~ Gender, data = pay_clean)

Model1 <- lm(BasePay ~ Gender, data = pay_clean)
obj <- LMsummary(Model1)

# GLM with all conceptual variables except JobTitle ----
ModelFull <- lm(BasePay ~ Gender + Education + Dept + Seniority + Age + PerfEval + Bonus,
                data = pay_clean)
obj <- LMsummary(ModelFull)

# Assessing contribution of each variable in ModelFull ----

## Gender ----
ModelNoGender <- lm(BasePay ~ Education + Dept + Seniority + Age + PerfEval + Bonus,
                    data = pay_clean)
R2compare(ReducedModel = ModelNoGender, FullModel = ModelFull,
          PredName = "Gender", explain = T)

## Education ----
ModelNoEdu <- lm(BasePay ~ Gender + Dept + Seniority + Age + PerfEval + Bonus,
                 data = pay_clean)
R2compare(ReducedModel = ModelNoEdu, FullModel = ModelFull,
          PredName = "Education", explain = T)

## Dept ----
ModelNoDept <- lm(BasePay ~ Gender + Education + Seniority + Age + PerfEval + Bonus,
                  data = pay_clean)
R2compare(ReducedModel = ModelNoDept, FullModel = ModelFull,
          PredName = "Dept", explain = T)

## Seniority ----
ModelNoSenior <- lm(BasePay ~ Gender + Education + Dept + Age + PerfEval + Bonus,
                    data = pay_clean)
R2compare(ReducedModel = ModelNoSenior, FullModel = ModelFull,
          PredName = "Seniority", explain = T)

## Age ----
ModelNoAge <- lm(BasePay ~ Gender + Education + Dept + Seniority + PerfEval + Bonus,
                 data = pay_clean)
R2compare(ReducedModel = ModelNoAge, FullModel = ModelFull,
          PredName = "Age", explain = T)

## PerfEval ----
ModelNoEval <- lm(BasePay ~ Gender + Education + Dept + Seniority + Age + Bonus,
                  data = pay_clean)
R2compare(ReducedModel = ModelNoEval, FullModel = ModelFull,
          PredName = "PerfEval", explain = T)

## Bonus ----
ModelNoBonus <- lm(BasePay ~ Gender + Education + Dept + Seniority + Age + PerfEval,
                   data = pay_clean)
R2compare(ReducedModel = ModelNoBonus, FullModel = ModelFull,
          PredName = "Bonus", explain = T)

# PerfEval and Bonus may be removed for parsimony ----
Model2 <- lm(BasePay ~ Gender + Education + Dept + Seniority + Age,
             data = pay_clean)
obj <- LMsummary(Model2, effectsizes = T)

# Assessing contribution of each variable in Model2 ----

## Gender ----
Model2NoGender <- lm(BasePay ~ Education + Dept + Seniority + Age,
                    data = pay_clean)
R2compare(ReducedModel = Model2NoGender, FullModel = Model2,
          PredName = "Gender", explain = T)

## Education ----
Model2NoEdu <- lm(BasePay ~ Gender + Dept + Seniority + Age,
                 data = pay_clean)
R2compare(ReducedModel = Model2NoEdu, FullModel = Model2,
          PredName = "Education", explain = T)

## Dept ----
Model2NoDept <- lm(BasePay ~ Gender + Education + Seniority + Age,
                  data = pay_clean)
R2compare(ReducedModel = Model2NoDept, FullModel = Model2,
          PredName = "Dept", explain = T)

## Seniority ----
Model2NoSenior <- lm(BasePay ~ Gender + Education + Dept + Age,
                    data = pay_clean)
R2compare(ReducedModel = Model2NoSenior, FullModel = Model2,
          PredName = "Seniority", explain = T)

## Age ----
Model2NoAge <- lm(BasePay ~ Gender + Education + Dept + Seniority,
                 data = pay_clean)
R2compare(ReducedModel = Model2NoAge, FullModel = Model2,
          PredName = "Age", explain = T)

# Examining any interaction effects ----

## Frequency/Distribution ----
table1::table1(~ Gender | Education, overall = F,
               data = pay_clean)
table1::table1(~ Gender | Dept, overall = F,
               data = pay_clean)
table1::table1(~ Gender | Seniority, overall = F,
               data = pay_clean) 
boxplot(Age ~ Gender, data = pay_clean)

## Gender & Seniority ----
Model3 <- lm(BasePay ~ Gender + Education + Dept + Seniority + Age + Gender:Seniority,
             data = pay_clean)
obj <- LMsummary(Model3, effectsizes = T) # nope

## Gender & Age ----
Model4 <- lm(BasePay ~ Gender + Education + Dept + Seniority + Age + Gender:Age,
             data = pay_clean)
obj <- LMsummary(Model4, effectsizes = T) # nope

## Gender & Educ ----
Model5 <- lm(BasePay ~ Gender + Education + Dept + Seniority + Age + Gender:Education,
             data = pay_clean)
obj <- LMsummary(Model5, effectsizes = T) # nope

## Gender & Dept ----
Model6 <- lm(BasePay ~ Gender + Education + Dept + Seniority + Age + Gender:Dept,
             data = pay_clean)
obj <- LMsummary(Model6, effectsizes = T) # nope

## Age & Seniority ----
Model7 <- lm(BasePay ~ Gender + Education + Dept + Seniority + Age + Age:Seniority,
             data = pay_clean)
obj <- LMsummary(Model7, effectsizes = T) # nope

# Summary table for (tentative) final model ----
gtsummary::tbl_regression(Model2)
# incomplete table
