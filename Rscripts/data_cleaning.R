# Load necessary packages through package manager "pacman" ----

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse, # for data wrangling
  readxl, # to import data from Excel files
  here, # to specify file path relative to project directory
  psych, # for descriptive stats
  naniar, # to assess missing data
  table1,
  tableone # for table of descriptive stats
) # for descriptive stats table

# Import data set ----

pay_raw <- read_xlsx(path = here::here("data", "pay_raw.xlsx"), col_names = T)

# DATA CLEANING ----

# inspect raw data
str(pay_raw)
# BasePay is inputted as characters indicating possible data entry error

## Initial Missing Data ----
sapply(pay_raw, function(x) sum(is.na(x))) # number of NAs in each variable
naniar::miss_var_summary(pay_raw)

## Invalid entries ----

# list all unique values under each categorical variable
cat_vars <- pay_raw[, c(
  "JobTitle",
  "Gender",
  "Education",
  "Dept",
  "Seniority",
  "PerfEval"
)]
sapply(cat_vars, function(x) unique(x))

# check each quantitative variable for NA/non-digit entries
pay_raw |>
  filter(str_detect(BasePay, "\\D") | is.na(BasePay)) # "100463P" & NA
pay_raw |>
  filter(str_detect(Age, "\\D") | is.na(Age)) # "."
pay_raw |>
  filter(str_detect(Bonus, "\\D") | is.na(Bonus))

## treat dot values as NAs ----
pay_raw <- pay_raw |> naniar::replace_with_na_all(condition = ~ .x == ".")

# re-count NAs
naniar::miss_var_summary(pay_raw) # in each variable
naniar::miss_case_summary(pay_raw) # in individual cases

# Little's missing completely at random test
mcar_test(pay_raw) # p = .598, thus data is MCAR

## Remove all NAs & invalid entries ----
pay_cleanv1 <- pay_raw |>
  drop_na() |> # 7 cases dropped
  filter_out(BasePay == "100463P" | PerfEval == "10") # 2 invalids dropped

# DATA WRANGLING ----

## Change all variables into appropriate type based on codebook ----
str(pay_cleanv1) # initial data structure

pay_cleanv1 <- pay_cleanv1 |>
  mutate(
    JobTitle = as.factor(JobTitle),
    Gender = as.factor(Gender), # Female will be ref
    Education = factor(
      Education,
      levels = c("High School", "College", "Masters", "PhD")
    ),
    Dept = as.factor(Dept),
    Seniority = factor(Seniority, levels = c("1", "2", "3", "4", "5")),
    BasePay = as.numeric(BasePay),
    Age = as.numeric(Age),
    Bonus = as.numeric(Bonus),
    PerfEval = factor(PerfEval, levels = c("1", "2", "3", "4", "5"))
  )

str(pay_cleanv1) # re-check data structure OK

## Descriptive stats for quantitative variables ----
describe(pay_cleanv1[, c("BasePay", "Age", "Bonus")])
# BasePay & Bonus are heavily skewed by absurd max values
# max Age also does not make sense

## Examine outliers ----
par(mfrow = c(1, 3)) # for side-by-side boxplots
boxplot(pay_cleanv1$BasePay)
boxplot(pay_cleanv1$Age)
boxplot(pay_cleanv1$Bonus)

# max and mean for each quant variables
cbind(
    max_BasePay = max(pay_cleanv1$BasePay), 
    max_Age = max(pay_cleanv1$Age), 
    max_Bonus = max(pay_cleanv1$Bonus)
)
cbind(
  mean_BasePay = mean(pay_cleanv1$BasePay),
  mean_Age = mean(pay_cleanv1$Age),
  mean_Bonus = mean(pay_cleanv1$Bonus)
)

# correct or remove outliers
pay_cleanv1 <- pay_cleanv1 |>
  mutate(BasePay = replace_values(BasePay, 670890 ~ 67089)) |>
  # remove last 0 digit at the end from BasePay outlier
  filter_out(Age == max(Age) | Bonus == max(Bonus))
  # remove Age outlier and Bonus outlier

# re-check after correction/removal
boxplot(pay_cleanv1$BasePay)
boxplot(pay_cleanv1$Age)
boxplot(pay_cleanv1$Bonus)

describe(pay_cleanv1[, c("BasePay", "Age", "Bonus")]) # looks better now

# Save cleaned data set to .RData file ----
save(pay_cleanv1, file = here::here("data", "pay_cleanv1.RData"))

# Table 1 Descriptive Statistics ----

## tableone package ----
tableone::CreateTableOne(
  vars = c(
    "JobTitle",
    "Age",
    "Education",
    "Dept",
    "Seniority",
    "BasePay",
    "Bonus",
    "PerfEval"
  ),
  strata = "Gender",
  data = pay_cleanv1,
  test = F,
  addOverall = T
) |>
  print() |>
  kableone() # html

  ## table1 package ----

  # set labels for each variable
  pay_cleanv1 <- pay_cleanv1 |>
  mutate(
    JobTitle = setLabel(JobTitle, "Vị trí"),
    Gender = setLabel(Gender, "Giới tính"),
    Age = setLabel(Age, "Tuổi"),
    Education = setLabel(Education, "Học vấn"),
    Dept = setLabel(Dept, "Phòng ban"),
    Seniority = setLabel(Seniority, "Thâm niên (bậc)"),
    BasePay = setLabel(BasePay, "Lương cơ bản (USD)"),
    Bonus = setLabel(Bonus, "Lương thưởng (USD)"),
    PerfEval = setLabel(PerfEval, "Đánh giá Hiệu suất")
  )

# re-save data
save(pay_cleanv1, file = here::here("data", "pay_cleanv1.RData"))

# set customised render for quantitative var
my.render.cont <- c(
  "Trung bình (Lệch chuẩn)" = sprintf("%s (%s)", "MEAN", "SD")
)

table1::table1(
  ~ JobTitle +
    Age +
    Education +
    Dept +
    Seniority +
    BasePay +
    Bonus +
    PerfEval |
    Gender,
  data = pay_cleanv1,
  render.continuous = my.render.cont
)
# html table in Viewer pane
# default output gives median + min/max for quant variables
