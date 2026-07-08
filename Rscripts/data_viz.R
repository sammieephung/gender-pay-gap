# Load required packages via package manager 'pacman' ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # for data wrangling + visualisation
               psych, # for descriptive stats when needed
               patchwork, # for side-by-side plots
               scales, # for scale units on plots
               gt) # for html tables 
              
# Import cleaned data set ----
load(here::here("data", "pay_clean.RData"))

# Create new variable for TotalPay = BasePay + Bonus 
# pay_clean <- pay_clean |>
#   mutate(TotalPay = BasePay + Bonus)
# attr(pay_clean$TotalPay, "label") <- "Tổng lương (USD)"

# Descriptive stats
psych::describe(pay_clean)

# Histograms for BasePay & Bonus ----
p1 <- ggplot(pay_clean, aes(BasePay)) +
  geom_histogram(binwidth = 5000, col = "black") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.title.y = element_blank())

p2 <- ggplot(pay_clean, aes(Bonus)) +
  geom_histogram(binwidth = 500, col = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.title.y = element_blank())

patchwork1 <- p1 + p2
patchwork1 + plot_annotation(title = "Phân bố Lương cơ bản và Lương thưởng")

# Overall Gender Pay Gap ----

## Box plots ----
p3 <- ggplot(pay_clean, aes(BasePay, Gender)) +
  # draw ticks for min/max at the ends of whisker
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot(fill = "lightgrey") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black")) 

p4 <- ggplot(pay_clean, aes(Bonus, Gender)) +
  # draw ticks for min/max at the ends of whisker
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot(fill = "lightgrey") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black")) 

# graph one plot on top of the other
# merge y-axis titles and add spacer for better aspect ratio
patchwork2 <- (p3 / p4) + plot_layout(axis_titles = "collect") | plot_spacer()
patchwork2 + plot_annotation(title = "Phân bố Lương cơ bản và Lương thưởng theo Giới tính")

## Table of of overall mean pay and pay gap (UNFINISHED) ----
pay_clean |>
  summarise(meanBase = mean(BasePay),
            meanBonus = mean(Bonus),
            .by = Gender) |> # no ungrouping needed
  # pivot wider to compute pay gaps from the columns
  pivot_wider(names_from = Gender,
              values_from = c(meanBase, meanBonus)) |>
  # compute pay gaps in new columns
  mutate(base_gap = (meanBase_Male - meanBase_Female) / meanBase_Male,
         bonus_gap = (meanBonus_Male - meanBonus_Female) / meanBonus_Male) |>
  # remove "mean" from column names to standardise and prepare for lengthening
  rename_with(\(x) gsub("mean", "", x) |> tolower()) |>
  # lengthen for all columns
  pivot_longer(everything()) |>
  # separate first column into two
  separate(col = name, into = c("pay_type", "x"), sep = "_") |>
  # pivot wider
  pivot_wider(names_from = x, values_from = value) |>
  # change pay_type levels for table, convert gap to %, round values to 2 decimal places
  mutate(pay_type = ifelse(pay_type == "base", "Cơ bản", "Thưởng"),
         gap = percent(gap, accuracy = 0.01),
         female = round(female, 2),
         male = round(male, 2)) |>
  # create html table
  gt() |>
  tab_header(title = "Trung bình Lương (USD) theo Giới tính và Chênh lệch (%)") |>
  tab_footnote(footnote = "Chênh lệch = (Trung bình Nam - Trung bình Nữ) / Trung bình Nam * 100%") |>
  tab_spanner(label = "Giới tính",
              columns = c(female, male)) |>
  cols_label(pay_type = "Loại lương",
             female = "Nữ",
             male = "Nam",
             gap = "Chênh lệch") |> 
  cols_align(align = "center") |>
  tab_style(style = "vertical-align:middle",
            locations = cells_column_labels(columns = c(pay_type, gap))) |>
  tab_style(style = cell_text(size = "smaller"),
            locations = cells_footnotes())

# Gender pay gap by ___ ----

## Age group ----
# Create age group variable
pay_clean$AgeGroup = cut(pay_clean$Age, breaks = c(18, 25, 35, 45, 55, 65, Inf),
                         right = F, 
                         labels = c("Dưới 25 tuổi", "25-34 tuổi", "35-44 tuổi", 
                                    "45-54 tuổi", "55-64 tuổi", "Trên 65 tuổi"))
attr(pay_clean$AgeGroup, "label") <- "Nhóm tuổi"


pay_clean |>
  summarise(mean = mean(BasePay),
            .by = c(AgeGroup, Gender)) |>
  pivot_wider(names_from = Gender,
              values_from = mean) |>
  # Compute pay gap 
  mutate(base_gap = (Male - Female) / Male) |>
  # Create bar plots
  ggplot(aes(x = AgeGroup, y = base_gap, label = percent(base_gap))) +
  geom_col() +
  geom_text(nudge_y = 0.005) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), 
                     limits = c(0, 0.2),
                     expand = c(0, 0),
                     labels = label_percent(),
                     name = "Chênh lệch (%)") +
  labs(title = "Chênh lệch về Lương cơ bản giữa Nam và Nữ theo Nhóm tuổi",
       subtitle = "Chênh lệch = (Lương TB của Nam - Lương TB của Nữ) / Lương TB của Nam * 100%") +
  theme(panel.border = element_rect(colour = "black"),
        panel.background = element_blank())

# meanbonus_age <- pay_clean |>
#   summarise(mean = mean(Bonus),
#             .by = c(AgeGroup, Gender)) |>
#   pivot_wider(names_from = Gender,
#               values_from = mean) |>
#   mutate(bonus_gap = (Male - Female) / Male)

# meantotal_age <- pay_clean |>
#   summarise(mean = mean(TotalPay),
#             .by = c(AgeGroup, Gender)) |>
#   pivot_wider(names_from = Gender,
#               values_from = mean) |>
#   mutate(total_gap = (Male - Female) / Male)

# ggplot(meanbonus_age, aes(x = AgeGroup, y = bonus_gap)) +
#   geom_col() +
#   scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.05), 
#                      limits = c(-0.2, 0.2),
#                      expand = c(0, 0),
#                      labels = label_percent(),
#                      name = "Chênh lệch (%)") +
#   geom_text(aes(y = bonus_gap + 0.02*sign(bonus_gap) ,
#                 label = percent(bonus_gap))) +
#   labs(title = "Chênh lệch về Lương thưởng") +
#   theme(
#     panel.border = element_rect(colour = "black"),
#     panel.background = element_blank()
#   )
# 
# patchwork3 <- (p5 / p6) + plot_layout(axis_titles = "collect") | plot_spacer()

## Education ----

pay_clean |>
  summarise(meanBase = mean(BasePay),
            .by = c(Education, Gender)) |>
  pivot_wider(id_cols = Education,
              names_from = Gender,
              values_from = meanBase) |>
  mutate(base_gap = (Male - Female) / Male) |> # compute pay gap 
  ggplot(aes(Education, base_gap, label = percent(base_gap, accuracy = 0.01))) +
  geom_col() + # graph bar plot
  geom_text(nudge_y = 0.005) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), 
                     limits = c(0, 0.2),
                     expand = c(0, 0),
                     labels = label_percent(),
                     name = "Chênh lệch (%)") +
  labs(title = "Chênh lệch về Lương cơ bản giữa Nam và Nữ theo Học vấn",
       subtitle = "Chênh lệch = (Lương TB của Nam - Lương TB của Nữ) / Lương TB của Nam * 100%") +
  theme(panel.border = element_rect(colour = "black"),
        panel.background = element_blank())

## Dept ----
pay_clean |>
  summarise(meanBase = mean(BasePay),
            .by = c(Dept, Gender)) |>
  pivot_wider(id_cols = Dept,
              names_from = Gender,
              values_from = meanBase) |>
  mutate(base_gap = (Male - Female) / Male) |> # compute pay gap 
  ggplot(aes(Dept, base_gap, label = percent(base_gap, accuracy = 0.01))) +
  geom_col() + # graph bar plot
  geom_text(nudge_y = 0.005) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), 
                     limits = c(0, 0.2),
                     expand = c(0, 0),
                     labels = label_percent(),
                     name = "Chênh lệch (%)") +
  labs(title = "Chênh lệch về Lương cơ bản giữa Nam và Nữ theo Phòng ban",
       subtitle = "Chênh lệch = (Lương TB của Nam - Lương TB của Nữ) / Lương TB của Nam * 100%") +
  theme(panel.border = element_rect(colour = "black"),
        panel.background = element_blank())

## Seniority ----
pay_clean |>
  summarise(meanBase = mean(BasePay),
            .by = c(Seniority, Gender)) |>
  pivot_wider(id_cols = Seniority,
              names_from = Gender,
              values_from = meanBase) |>
  mutate(base_gap = (Male - Female) / Male) |> # compute pay gap 
  ggplot(aes(Seniority, base_gap, label = percent(base_gap, accuracy = 0.01))) +
  geom_col() + # graph bar plot
  geom_text(nudge_y = 0.005) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), 
                     limits = c(0, 0.2),
                     expand = c(0, 0),
                     labels = label_percent(),
                     name = "Chênh lệch (%)") +
  labs(title = "Chênh lệch về Lương cơ bản giữa Nam và Nữ theo Bậc Thâm niên",
       subtitle = "Chênh lệch = (Lương TB của Nam - Lương TB của Nữ) / Lương TB của Nam * 100%") +
  theme(panel.border = element_rect(colour = "black"),
        panel.background = element_blank())

## PerfEval
pay_clean |>
  summarise(meanBase = mean(BasePay),
            .by = c(PerfEval, Gender)) |>
  pivot_wider(id_cols = PerfEval,
              names_from = Gender,
              values_from = meanBase) |>
  mutate(base_gap = (Male - Female) / Male) |> # compute pay gap 
  ggplot(aes(PerfEval, base_gap, label = percent(base_gap, accuracy = 0.01))) +
  geom_col() + # graph bar plot
  geom_text(nudge_y = 0.005) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), 
                     limits = c(0, 0.2),
                     expand = c(0, 0),
                     labels = label_percent(),
                     name = "Chênh lệch (%)") +
  labs(title = "Chênh lệch về Lương cơ bản giữa Nam và Nữ theo Đánh giá Hiệu suất",
       subtitle = "Chênh lệch = (Lương TB của Nam - Lương TB của Nữ) / Lương TB của Nam * 100%") +
  theme(panel.border = element_rect(colour = "black"),
        panel.background = element_blank())
