# Load necessary packages through package manager "pacman" ----
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, here, psych, table1, tableone)

# Import data set ----
# use here::here() to specify file path relative to project directory
pay_v1 = read_xlsx(path = here::here("data", "PayV1.xlsx"), col_names = T)

# inspect raw data
str(pay_v1)
# BasePay is inputted as characters indicating possible data entry error

# Examine and remove NAs ----
sapply(pay_v1, function(x) sum(is.na(x))) # number of NAs in each variable
pay_clean = pay_v1 |> drop_na() # remove NAs

# Examine each variable for invalid entries ----
## Categorical variables ----
# JobTitle, Gender, Education, Dept, Seniority, PerfEval
# list all unique values under each of them
unique(pay_clean$JobTitle)
unique(pay_clean$Gender) # "."
unique(pay_clean$Education)
unique(pay_clean$Dept) # "."
unique(pay_clean$Seniority)
unique(pay_clean$PerfEval) # "10"

## Continuous/Quantitative variables ----
# Age, BasePay, Bonus
# check each variable for non-digits in values
sum(str_detect(pay_clean$BasePay, "\\D"))
str_subset(pay_clean$BasePay, "\\D") # letter

sum(str_detect(pay_clean$Age, "\\D"))
str_subset(pay_clean$Age, "\\D") # "."

sum(str_detect(pay_clean$Bonus, "\\D"))

# Remove all invalid entries ----
pay_clean = pay_clean |>
  filter_out(str_detect(BasePay, "\\D") | str_detect(Age, "\\D") | PerfEval == 10 | 
               str_detect(Gender, "\\.") | str_detect(Dept, "\\."))

# Change all variables into appropriate type based on codebook ----
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

# Descriptive stats for quantitative variables ----
describe(pay_clean[, c("BasePay", "Age", "Bonus")])
# BasePay & Bonus are heavily skewed by absurd max values 
# max Age also does not make sense

# Examine outliers ----
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

# remove outliers
pay_clean = pay_clean |>
  filter_out(BasePay == max(BasePay) | Age == max(Age) | Bonus == max(Bonus))

# re-check after removal

attach(pay_clean)
boxplot(BasePay)
boxplot(Age)
boxplot(Bonus)

describe(pay_clean[, c("BasePay", "Age", "Bonus")]) # looks better now

# Table 1 Descriptive Statistics ----
table1 = tableone::CreateTableOne(vars = c("JobTitle",  "Age", "Education", "Dept", 
                                  "Seniority", "BasePay", "Bonus", "PerfEval"),
                         strata = "Gender", data = pay_clean, test = F, addOverall = T)
# kableone(table1) # html table
print(table1)

table1::table1(~ JobTitle + Age + Education + Dept + 
                 Seniority + BasePay + Bonus + PerfEval | Gender, data = pay_clean)
# html table in Viewer pane
# also gives median + min/max for quant variables