# Load necessary packages through package manager "pacman" ----

if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # for data wrangling
               readxl, # to import data from Excel files
               here, # to specify file path relative to project directory
               psych, # for descriptive stats
               naniar, # to assess missing data 
               table1, tableone) # for descriptive stats table

# Import data set ----

pay_v1 = read_xlsx(path = here::here("data", "PayV1.xlsx"), col_names = T)

# DATA CLEANING ----

# inspect raw data
str(pay_v1)
# BasePay is inputted as characters indicating possible data entry error

## Initial Missing Data ----
sapply(pay_v1, function(x) sum(is.na(x))) # number of NAs in each variable

## Invalid entries ----

# list all unique values under each categorical variable
unique(pay_v1$JobTitle)
unique(pay_v1$Gender) # "."
unique(pay_v1$Education)
unique(pay_v1$Dept) # "."
unique(pay_v1$Seniority)
unique(pay_v1$PerfEval) # "10"

# check each quantitative variable for non-digits in values
pay_v1 |>
  filter(str_detect(BasePay, "\\D") | is.na(BasePay)) # "100463P"
pay_v1 |>
  filter(str_detect(Age, "\\D") | is.na(BasePay))
pay_v1 |>
  filter(str_detect(Bonus, "\\D") | is.na(Bonus))

## Treat dot values as NAs ----
pay_v1 <- pay_v1 |> naniar::replace_with_na_all(condition = ~.x == ".")

# number of NAs
naniar::miss_var_summary(pay_v1) # in each variable
naniar::miss_case_summary(pay_v1) # in individual cases

# Little's missing completely at random test 
mcar_test(pay_v1) # p = .598, thus data is MCAR

## Remove all NAs & invalid entries ----
pay_clean <- pay_v1 |>
  drop_na() |> # 7 cases dropped
  filter_out(BasePay == "100463P" | PerfEval == "10") # 2 invalids

# DATA WRANGLING ----

## Change all variables into appropriate type based on codebook ----
str(pay_clean) # initial data structure

pay_clean = pay_clean |>
  mutate(JobTitle = as.factor(JobTitle),
         Gender = as.factor(Gender), # Female will be ref
         Education = factor(Education, levels = c("High School", "College", "Masters", "PhD")),
         Dept = as.factor(Dept), 
         Seniority = factor(Seniority, levels = c("1", "2", "3", "4", "5")),
         BasePay = as.numeric(BasePay),
         Age = as.numeric(Age),
         Bonus = as.numeric(Bonus), 
         PerfEval = factor(PerfEval, levels = c("1", "2", "3", "4", "5"))
         )

## Descriptive stats for quantitative variables ----
describe(pay_clean[, c("BasePay", "Age", "Bonus")])
# BasePay & Bonus are heavily skewed by absurd max values 
# max Age also does not make sense

## Examine outliers ----
par(mfrow = c(1, 3)) # for side-by-side boxplots
attach(pay_clean)
boxplot(BasePay)
boxplot(Age)
boxplot(Bonus)

# max and mean for each quant variables
cbind(max_BasePay = max(BasePay),
      max_Age = max(Age),
      max_Bonus = max(Bonus))
cbind(mean_BasePay = mean(BasePay),
      mean_Age = mean(Age),
      mean_Bonus = mean(Bonus))

# correct/remove outliers
pay_clean <-  pay_clean |>
  mutate(BasePay = replace_values(BasePay, 670890 ~ 67089)) |>
  filter_out(Age == max(Age) | Bonus == max(Bonus))

# re-check after correction/removal
attach(pay_clean)
boxplot(BasePay)
boxplot(Age)
boxplot(Bonus)

describe(pay_clean[, c("BasePay", "Age", "Bonus")]) # looks better now

# Table 1 Descriptive Statistics ----

## tableone package ----
table1 = tableone::CreateTableOne(vars = c("JobTitle",  "Age", "Education", "Dept", 
                                  "Seniority", "BasePay", "Bonus", "PerfEval"),
                         strata = "Gender", data = pay_clean, test = F, addOverall = T)
# kableone(table1) # html table
print(table1)

## table1 package ----

# set labels for each variable
pay_clean <- pay_clean |>
  mutate(JobTitle = setLabel(JobTitle, "Vị trí"),
         Gender = setLabel(Gender, "Giới tính"),
         Age = setLabel(Age, "Tuổi"),
         Education = setLabel(Education, "Học vấn"),
         Dept = setLabel(Dept, "Phòng ban"), 
         Seniority = setLabel(Seniority, "Thâm niên (bậc)"),
         BasePay = setLabel(BasePay, "Lương cơ bản (USD)"),
         Bonus = setLabel(Bonus, "Lương thưởng (USD)"),
         PerfEval = setLabel(PerfEval, "Đánh giá Hiệu suất"))

# set customised render for quantitative var
my.render.cont <- c("Trung bình (Lệch chuẩn)"=sprintf("%s (%s)", "MEAN", "SD"))

table1::table1(~ JobTitle + Age + Education + Dept + 
                 Seniority + BasePay + Bonus + PerfEval | Gender, 
               data = pay_clean, render.continuous = my.render.cont)
# html table in Viewer pane
# also gives median + min/max for quant variables

# Save cleaned data set to .RData file ----
save(pay_clean, file = here::here("data", "pay_clean.RData"))
