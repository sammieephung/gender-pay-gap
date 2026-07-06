# Load required packages via package manager 'pacman' ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # for data wrangling + visualisation
               psych, # for descriptive stats when needed
               patchwork, # for side-by-side plots
               scales, # for scale units on plots
               ggrepel)
              
# Import cleaned data set ----
load(here::here("data", "pay_clean.RData"))

# Create new variable for TotalPay = BasePay + Bonus ----
pay_clean <- pay_clean |>
  mutate(TotalPay = BasePay + Bonus)
attr(pay_clean$TotalPay, "label") <- "Tổng lương (USD)"

# Descriptive stats
psych::describe(pay_clean)

# Histograms for BasePay & Bonus ----
p1 <- ggplot(pay_clean, aes(BasePay)) +
  geom_histogram(binwidth = 5000, col = "black") +
  scale_y_continuous(limits = c(0, 90), expand = c(0, 0)) +
  labs(title = "Phân bố Lương cơ bản") +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black"),
    axis.title.y = element_blank()
  )

p2 <- ggplot(pay_clean, aes(Bonus)) +
  geom_histogram(binwidth = 500, col = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  labs(title = "Phân bố Lương thưởng") +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black"),
    axis.title.y = element_blank()
  )

p1 + p2

# Bar plots for gender pay gap ----

## By Age group ----
# Create age group variable
pay_clean$AgeGroup = cut(pay_clean$Age, breaks = c(18, 25, 35, 45, 55, 65, Inf),
                         right = F, 
                         labels = c("Dưới 25 tuổi", "25-34 tuổi", "35-44 tuổi", 
                                    "45-54 tuổi", "55-64 tuổi", "Trên 65 tuổi"))
attr(pay_clean$AgeGroup, "label") <- "Nhóm tuổi"

# Compute pay gap 
meanbase_age <- pay_clean |>
  summarise(mean = mean(BasePay),
            .by = c(AgeGroup, Gender)) |>
  pivot_wider(names_from = Gender,
              values_from = mean) |>
  mutate(base_gap = (Male - Female) / Male)

meanbonus_age <- pay_clean |>
  summarise(mean = mean(Bonus),
            .by = c(AgeGroup, Gender)) |>
  pivot_wider(names_from = Gender,
              values_from = mean) |>
  mutate(bonus_gap = (Male - Female) / Male)

meantotal_age <- pay_clean |>
  summarise(mean = mean(TotalPay),
            .by = c(AgeGroup, Gender)) |>
  pivot_wider(names_from = Gender,
              values_from = mean) |>
  mutate(total_gap = (Male - Female) / Male)

# Create bar plots
p3 <- ggplot(meanbase_age, aes(x = AgeGroup, y = base_gap, label = percent(base_gap))) +
  geom_col() +
  geom_text(nudge_y = 0.01) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), 
                     limits = c(0, 0.2),
                     expand = c(0, 0),
                     labels = label_percent(),
                     name = "Chênh lệch (%)") +
  labs(title = "Chênh lệch về Lương cơ bản") +
  theme(
    panel.border = element_rect(colour = "black"),
    panel.background = element_blank()
  )

p4 <- ggplot(meanbonus_age, aes(x = AgeGroup, y = bonus_gap)) +
  geom_col() +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.05), 
                     limits = c(-0.2, 0.2),
                     expand = c(0, 0),
                     labels = label_percent(),
                     name = "Chênh lệch (%)") +
  geom_text(aes(y = bonus_gap + 0.02*sign(bonus_gap) ,
                label = percent(bonus_gap))) +
  labs(title = "Chênh lệch về Lương thưởng") +
  theme(
    panel.border = element_rect(colour = "black"),
    panel.background = element_blank()
  )

(p3 / p4) + plot_layout(axis_titles = "collect") | plot_spacer()

## By Education ----


## By Dept ----


## By Seniority ----


## By PerfEval