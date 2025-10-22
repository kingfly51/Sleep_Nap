###### 按工作周和节假周分割数据并重复分析 ######

# 工作周数据 (day_number 1-7)
work_week_data <- clean_data %>% filter(day_number <= 7)
# 节假周数据 (day_number 8-14)  
holiday_week_data <- clean_data %>% filter(day_number >= 8)

###### 工作周分析：预测第8天抑郁水平 ######
cat("=== 工作周分析 (Day 1-7) - 预测第8天抑郁 ===\n")

# 计算工作周午睡时长变异性
work_week_variability <- work_week_data %>%
  group_by(subject_id) %>%
  summarise(
    sd_nap_duration_work = sd(nap_duration, na.rm = TRUE),
    mean_nap_duration_work = mean(nap_duration, na.rm = TRUE),
    mean_sleep_duration_work = mean(sleep_duration, na.rm = TRUE),
    sd_sleep_duration_work = sd(sleep_duration, na.rm = TRUE)
  ) %>%
  ungroup()

# 获取第8天的抑郁数据
day8_depression <- clean_data %>%
  filter(day_number == 8) %>%
  select(subject_id, daily_dep) %>%
  rename(day8_dep = daily_dep)

# 合并工作周变异性数据和第8天抑郁
data_work_week <- data_new %>%
  select(subject_id, age, gender, education, nation, income, 
         pre_dep, pre_anx, pre_pss, pre_tls, post_dep, post_anx, post_pss, post_tls,
         pre_sleep_duration, pre_sleep_timing, pre_sleep_quality, pre_sleep_efficiency,
         pre_sleep_hypnotics, pre_sleep_disorder, pre_daytime_dysfunction) %>%
  left_join(work_week_variability, by = "subject_id") %>%
  left_join(day8_depression, by = "subject_id")

# 工作周回归分析：午睡变异性预测第8天抑郁
cat("\n=== 工作周回归分析 ===\n")
lm_work1 <- lm(day8_dep ~ mean_nap_duration_work + sd_nap_duration_work + 
                 mean_sleep_duration_work + sd_sleep_duration_work, 
               data = data_work_week)
summary(lm_work1)
cat("工作周模型1 - AIC:", AIC(lm_work1), "BIC:", BIC(lm_work1), "\n")

lm_work2 <- lm(day8_dep ~ age + gender + education + income + nation + 
                 mean_nap_duration_work + sd_nap_duration_work + 
                 mean_sleep_duration_work + sd_sleep_duration_work, 
               data = data_work_week)
summary(lm_work2)
cat("工作周模型2 - AIC:", AIC(lm_work2), "BIC:", BIC(lm_work2), "\n")

lm_work3 <- lm(day8_dep ~ age + gender + education + income + nation + pre_dep +
                 mean_nap_duration_work + sd_nap_duration_work + 
                 mean_sleep_duration_work + sd_sleep_duration_work, 
               data = data_work_week)
summary(lm_work3)
cat("工作周模型3 - AIC:", AIC(lm_work3), "BIC:", BIC(lm_work3), "\n")

lm_work4 <- lm(day8_dep ~ age + gender + education + income + nation + 
                 pre_dep + pre_anx + pre_pss + pre_tls +
                 mean_nap_duration_work + sd_nap_duration_work + 
                 mean_sleep_duration_work + sd_sleep_duration_work, 
               data = data_work_week)
summary(lm_work4)
cat("工作周模型4 - AIC:", AIC(lm_work4), "BIC:", BIC(lm_work4), "\n")

lm_work5 <- lm(day8_dep ~ age + gender + education + income + nation + 
                 pre_dep + pre_anx + pre_pss + pre_tls +
                 pre_sleep_duration + pre_sleep_timing + pre_sleep_quality + 
                 pre_sleep_efficiency + pre_sleep_hypnotics + pre_sleep_disorder + 
                 pre_daytime_dysfunction +
                 mean_nap_duration_work + sd_nap_duration_work + 
                 mean_sleep_duration_work + sd_sleep_duration_work, 
               data = data_work_week)
summary(lm_work5)
cat("工作周模型5 - AIC:", AIC(lm_work5), "BIC:", BIC(lm_work5), "\n")

# 确定分位数进行高低分组
quantile_work_low <- quantile(data_work_week$sd_nap_duration_work, probs = 0.27, na.rm = TRUE)
quantile_work_high <- quantile(data_work_week$sd_nap_duration_work, probs = 0.73, na.rm = TRUE)

cat("工作周分位数 - 低:", quantile_work_low, "高:", quantile_work_high, "\n")

# 分组
high_sdv_work <- data_work_week[data_work_week$sd_nap_duration_work > quantile_work_high, ]
low_sdv_work <- data_work_week[data_work_week$sd_nap_duration_work < quantile_work_low, ]
high_sdv_work <- na.omit(high_sdv_work)
low_sdv_work <- na.omit(low_sdv_work)

# 倾向得分匹配 - 工作周
high_sdv_work$group <- "high"
low_sdv_work$group <- "low"
combined_data_work <- rbind(high_sdv_work, low_sdv_work)
combined_data_work$group <- as.factor(combined_data_work$group)

matchit_result_work <- matchit(group ~ age + gender + education + nation + income +
                                 pre_dep + pre_anx + pre_pss + pre_tls + pre_sleep_duration + 
                                 pre_sleep_timing + pre_sleep_quality + pre_sleep_efficiency + 
                                 pre_sleep_hypnotics + pre_sleep_disorder + pre_daytime_dysfunction, 
                               data = combined_data_work,
                               method = "nearest",
                               distance = "glm",
                               ratio = 1,
                               caliper = 0.6)

# 获取匹配后数据
matched_data_work <- match.data(matchit_result_work)
high_sdv_matched_work <- matched_data_work[matched_data_work$group == "high", ]
low_sdv_matched_work <- matched_data_work[matched_data_work$group == "low", ]

# 生成比较表格 - 工作周（修正函数调用）
comparison_table_work <- create_comparison_table(high_sdv_matched_work, low_sdv_matched_work)
# 重命名列以包含day8_dep
names(comparison_table_work) <- c("Variables", "Low Nap duration variability", 
                                  "High Nap duration variability", "T/χ²", "P value")

# 手动添加day8_dep的比较结果
t_day8_dep <- t.test(high_sdv_matched_work$day8_dep, low_sdv_matched_work$day8_dep)
day8_dep_row <- data.frame(
  Variables = "Day 8 depression",
  `Low Nap duration variability` = paste0(round(mean(low_sdv_matched_work$day8_dep), 2), "±", 
                                          round(sd(low_sdv_matched_work$day8_dep), 2)),
  `High Nap duration variability` = paste0(round(mean(high_sdv_matched_work$day8_dep), 2), "±", 
                                           round(sd(high_sdv_matched_work$day8_dep), 2)),
  `T/χ²` = round(t_day8_dep$statistic, 3),
  `P value` = round(t_day8_dep$p.value, 3),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

comparison_table_work <- rbind(comparison_table_work, day8_dep_row)

cat("\n工作周匹配后基线比较:\n")
print(comparison_table_work)

# 计算效应量 - 工作周第8天抑郁
cohen_d_work_day8 <- cohens_d(high_sdv_matched_work$day8_dep, 
                              low_sdv_matched_work$day8_dep,
                              pooled_sd = TRUE)
cat("\n工作周午睡变异性对第8天抑郁的Cohen's d:", cohen_d_work_day8$Cohens_d, "\n")

###### 节假周分析：预测第15天抑郁水平 ######
cat("\n=== 节假周分析 (Day 8-14) - 预测第15天抑郁 ===\n")

# 计算节假周午睡时长变异性
holiday_week_variability <- holiday_week_data %>%
  group_by(subject_id) %>%
  summarise(
    sd_nap_duration_holiday = sd(nap_duration, na.rm = TRUE),
    mean_nap_duration_holiday = mean(nap_duration, na.rm = TRUE),
    mean_sleep_duration_holiday = mean(sleep_duration, na.rm = TRUE),
    sd_sleep_duration_holiday = sd(sleep_duration, na.rm = TRUE)
  ) %>%
  ungroup()

# 合并节假周变异性数据（使用post_dep作为第15天抑郁）
data_holiday_week <- data_new %>%
  select(subject_id, age, gender, education, nation, income, 
         pre_dep, pre_anx, pre_pss, pre_tls, post_dep, post_anx, post_pss, post_tls,
         pre_sleep_duration, pre_sleep_timing, pre_sleep_quality, pre_sleep_efficiency,
         pre_sleep_hypnotics, pre_sleep_disorder, pre_daytime_dysfunction) %>%
  left_join(holiday_week_variability, by = "subject_id")

# 节假周回归分析：午睡变异性预测第15天抑郁
cat("\n=== 节假周回归分析 ===\n")
lm_holiday1 <- lm(post_dep ~ mean_nap_duration_holiday + sd_nap_duration_holiday + 
                    mean_sleep_duration_holiday + sd_sleep_duration_holiday, 
                  data = data_holiday_week)
summary(lm_holiday1)
cat("节假周模型1 - AIC:", AIC(lm_holiday1), "BIC:", BIC(lm_holiday1), "\n")

lm_holiday2 <- lm(post_dep ~ age + gender + education + income + nation + 
                    mean_nap_duration_holiday + sd_nap_duration_holiday + 
                    mean_sleep_duration_holiday + sd_sleep_duration_holiday, 
                  data = data_holiday_week)
summary(lm_holiday2)
cat("节假周模型2 - AIC:", AIC(lm_holiday2), "BIC:", BIC(lm_holiday2), "\n")

lm_holiday3 <- lm(post_dep ~ age + gender + education + income + nation + pre_dep +
                    mean_nap_duration_holiday + sd_nap_duration_holiday + 
                    mean_sleep_duration_holiday + sd_sleep_duration_holiday, 
                  data = data_holiday_week)
summary(lm_holiday3)
cat("节假周模型3 - AIC:", AIC(lm_holiday3), "BIC:", BIC(lm_holiday3), "\n")

lm_holiday4 <- lm(post_dep ~ age + gender + education + income + nation + 
                    pre_dep + pre_anx + pre_pss + pre_tls +
                    mean_nap_duration_holiday + sd_nap_duration_holiday + 
                    mean_sleep_duration_holiday + sd_sleep_duration_holiday, 
                  data = data_holiday_week)
summary(lm_holiday4)
cat("节假周模型4 - AIC:", AIC(lm_holiday4), "BIC:", BIC(lm_holiday4), "\n")

lm_holiday5 <- lm(post_dep ~ age + gender + education + income + nation + 
                    pre_dep + pre_anx + pre_pss + pre_tls +
                    pre_sleep_duration + pre_sleep_timing + pre_sleep_quality + 
                    pre_sleep_efficiency + pre_sleep_hypnotics + pre_sleep_disorder + 
                    pre_daytime_dysfunction +
                    mean_nap_duration_holiday + sd_nap_duration_holiday + 
                    mean_sleep_duration_holiday + sd_sleep_duration_holiday, 
                  data = data_holiday_week)
summary(lm_holiday5)
cat("节假周模型5 - AIC:", AIC(lm_holiday5), "BIC:", BIC(lm_holiday5), "\n")

# 确定分位数
quantile_holiday_low <- quantile(data_holiday_week$sd_nap_duration_holiday, probs = 0.27, na.rm = TRUE)
quantile_holiday_high <- quantile(data_holiday_week$sd_nap_duration_holiday, probs = 0.73, na.rm = TRUE)

cat("节假周分位数 - 低:", quantile_holiday_low, "高:", quantile_holiday_high, "\n")

# 分组
high_sdv_holiday <- data_holiday_week[data_holiday_week$sd_nap_duration_holiday > quantile_holiday_high, ]
low_sdv_holiday <- data_holiday_week[data_holiday_week$sd_nap_duration_holiday < quantile_holiday_low, ]
high_sdv_holiday <- na.omit(high_sdv_holiday)
low_sdv_holiday <- na.omit(low_sdv_holiday)

# 倾向得分匹配 - 节假周
high_sdv_holiday$group <- "high"
low_sdv_holiday$group <- "low"
combined_data_holiday <- rbind(high_sdv_holiday, low_sdv_holiday)
combined_data_holiday$group <- as.factor(combined_data_holiday$group)

matchit_result_holiday <- matchit(group ~ age + gender + education + nation + income +
                                    pre_dep + pre_anx + pre_pss + pre_tls + pre_sleep_duration + 
                                    pre_sleep_timing + pre_sleep_quality + pre_sleep_efficiency + 
                                    pre_sleep_hypnotics + pre_sleep_disorder + pre_daytime_dysfunction, 
                                  data = combined_data_holiday,
                                  method = "nearest",
                                  distance = "glm",
                                  ratio = 1,
                                  caliper = 0.6)

# 获取匹配后数据
matched_data_holiday <- match.data(matchit_result_holiday)
high_sdv_matched_holiday <- matched_data_holiday[matched_data_holiday$group == "high", ]
low_sdv_matched_holiday <- matched_data_holiday[matched_data_holiday$group == "low", ]

# 生成比较表格 - 节假周
comparison_table_holiday <- create_comparison_table(high_sdv_matched_holiday, low_sdv_matched_holiday)
names(comparison_table_holiday) <- c("Variables", "Low Nap duration variability", 
                                     "High Nap duration variability", "T/χ²", "P value")

cat("\n节假周匹配后基线比较:\n")
print(comparison_table_holiday)

# 计算效应量 - 节假周第15天抑郁
cohen_d_holiday_day15 <- cohens_d(high_sdv_matched_holiday$post_dep, 
                                  low_sdv_matched_holiday$post_dep,
                                  pooled_sd = TRUE)
cat("\n节假周午睡变异性对第15天抑郁的Cohen's d:", cohen_d_holiday_day15$Cohens_d, "\n")

###### 3.4 按周的调节效应分析 ######

# 工作周调节效应分析
cat("\n=== 工作周调节效应分析 (Day 1-7) ===\n")
high_ids_work <- high_sdv_matched_work$subject_id
low_ids_work <- low_sdv_matched_work$subject_id

matched_work_week_data <- work_week_data %>%
  filter(subject_id %in% c(high_ids_work, low_ids_work)) %>%
  mutate(group = case_when(
    subject_id %in% high_ids_work ~ "high",
    subject_id %in% low_ids_work ~ "low"
  ))

# 工作周模型
model_work_week <- lmer(daily_dep ~ age + gender + education + nation + income + pre_dep + 
                          nap_duration + sleep_duration + day_number * group + 
                          (1 + day_number | subject_id), 
                        data = matched_work_week_data)
summary(model_work_week)

# 简单效应检验 - 工作周
emm_slopes_work <- emtrends(model_work_week, ~ group, var = "day_number")
cat("\n工作周简单效应检验:\n")
print(summary(emm_slopes_work))
print(pairs(emm_slopes_work))

# 节假周调节效应分析
cat("\n=== 节假周调节效应分析 (Day 8-14) ===\n")
high_ids_holiday <- high_sdv_matched_holiday$subject_id
low_ids_holiday <- low_sdv_matched_holiday$subject_id

matched_holiday_week_data <- holiday_week_data %>%
  filter(subject_id %in% c(high_ids_holiday, low_ids_holiday)) %>%
  mutate(group = case_when(
    subject_id %in% high_ids_holiday ~ "high",
    subject_id %in% low_ids_holiday ~ "low"
  ))

# 节假周模型
model_holiday_week <- lmer(daily_dep ~ age + gender + education + nation + income + pre_dep + 
                             nap_duration + sleep_duration + day_number * group + 
                             (1 + day_number | subject_id), 
                           data = matched_holiday_week_data)
summary(model_holiday_week)

# 简单效应检验 - 节假周
emm_slopes_holiday <- emtrends(model_holiday_week, ~ group, var = "day_number")
cat("\n节假周简单效应检验:\n")
print(summary(emm_slopes_holiday))
print(pairs(emm_slopes_holiday))

###### 结果汇总 ######
cat("\n=== 结果汇总 ===\n")
cat("工作周分析:\n")
cat("  - 午睡变异性对第8天抑郁的Cohen's d:", cohen_d_work_day8$Cohens_d, "\n")
cat("  - 工作周调节效应(day_number × group):", 
    ifelse(summary(model_work_week)$coefficients["day_number:grouphigh", "Pr(>|t|)"] < 0.05, "显著", "不显著"), "\n")

cat("节假周分析:\n")
cat("  - 午睡变异性对第15天抑郁的Cohen's d:", cohen_d_holiday_day15$Cohens_d, "\n")
cat("  - 节假周调节效应(day_number × group):", 
    ifelse(summary(model_holiday_week)$coefficients["day_number:grouphigh", "Pr(>|t|)"] < 0.05, "显著", "不显著"), "\n")

# 保存结果
write.csv(comparison_table_work, "work_week_matched_comparison.csv", row.names = FALSE)
write.csv(comparison_table_holiday, "holiday_week_matched_comparison.csv", row.names = FALSE)




###### 绘制图4A-F ######

library(ggplot2)
library(patchwork)
library(ggpubr)


# 设置主题
nature_theme <- theme_minimal() +
  theme(
    text = element_text(family = "sans", size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

###### 图4A: 工作周午睡变异性与第8天抑郁的散点图 ######
#nature_colors <- c("high" = "#1F77B4", "low" = "#FF7F0E")  # Blue and orange
p4A <- ggplot(data_work_week, aes(x = sd_nap_duration_work, y = day8_dep)) +
  geom_point(alpha = 0.6, color = "#1F77B4", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#D62728", fill = "lightpink") +
  labs(title = "",
       x = "Nap Duration Variability (SD, minutes)",
       y = "Day 8 Depression Score") +
  nature_theme +
  annotate("text", x = Inf, y = Inf, 
           label = paste("β = -0.002, p = 0.797"), 
           hjust = 1.1, vjust = 1.5, size = 3.5)

###### 图4B: 休息周午睡变异性与后测抑郁的散点图 ######
p4B <- ggplot(data_holiday_week, aes(x = sd_nap_duration_holiday, y = post_dep)) +
  geom_point(alpha = 0.6, color = "#2CA02C", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#FF7F0E", fill = "lightyellow") +
  labs(title = "",
       x = "Nap Duration Variability (SD, minutes)",
       y = "Post-test Depression Score") +
  nature_theme +
  annotate("text", x = Inf, y = Inf, 
           label = paste("β = 0.009, p = 0.055"), 
           hjust = 1.1, vjust = 1.5, size = 3.5)

###### 图4C: 工作周高低变异性组第8天抑郁的雨云图 ######
# 准备数据
work_raincloud_data <- rbind(
  data.frame(
    group = "Low Variability",
    depression = low_sdv_matched_work$day8_dep
  ),
  data.frame(
    group = "High Variability", 
    depression = high_sdv_matched_work$day8_dep
  )
)

p4C <- ggplot(work_raincloud_data, aes(x = group, y = depression, fill = group)) +
  geom_flat_violin(position = position_nudge(x = 0.1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = 0.5, color = NA) +
  geom_point(aes(x = as.numeric(factor(group)) - 0.15, color = group),
             position = position_jitter(width = 0.05), size = 1.5, alpha = 0.6) +
  geom_boxplot(aes(fill = group), outlier.shape = NA, alpha = 0.5, 
               width = 0.1, color = "black") +
  scale_fill_manual(values = c("Low Variability" = "#FF7F0E", "High Variability" = "#1F77B4")) +
  scale_color_manual(values = c("Low Variability" = "#FF7F0E", "High Variability" = "#1F77B4")) +
  labs(title = "",
       x = "Nap Variability Group",
       y = "Day 8 Depression Score") +
  nature_theme +
  theme(legend.position = "none") +
  stat_compare_means(comparisons = list(c("Low Variability", "High Variability")),
                     method = "t.test", label = "p.format") +
  annotate("text", x = 1.5, y = max(work_raincloud_data$depression) * 0.95,
           label = paste("t = -1.012, d = -0.202, p = 0.314"),
           size = 3.5)

###### 图4D: 休息周高低变异性组后测抑郁的雨云图 ######
holiday_raincloud_data <- rbind(
  data.frame(
    group = "Low Variability",
    depression = low_sdv_matched_holiday$post_dep
  ),
  data.frame(
    group = "High Variability",
    depression = high_sdv_matched_holiday$post_dep
  )
)

p4D <- ggplot(holiday_raincloud_data, aes(x = group, y = depression, fill = group)) +
  geom_flat_violin(position = position_nudge(x = 0.1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = 0.5, color = NA) +
  geom_point(aes(x = as.numeric(factor(group)) - 0.15, color = group),
             position = position_jitter(width = 0.05), size = 1.5, alpha = 0.6) +
  geom_boxplot(aes(fill = group), outlier.shape = NA, alpha = 0.5,
               width = 0.1, color = "black") +
  scale_fill_manual(values = c("Low Variability" = "#FF7F0E", "High Variability" = "#1F77B4")) +
  scale_color_manual(values = c("Low Variability" = "#FF7F0E", "High Variability" = "#1F77B4")) +
  labs(title = "",
       x = "Nap Variability Group",
       y = "Post-test Depression Score") +
  nature_theme +
  theme(legend.position = "none") +
  stat_compare_means(comparisons = list(c("Low Variability", "High Variability")),
                     method = "t.test", label = "p.format") +
  annotate("text", x = 1.5, y = max(holiday_raincloud_data$depression) * 0.95,
           label = paste("t = 2.396, d = 0.470, p = 0.019"),
           size = 3.5)

###### 图4E: 工作周时间趋势的交互作用 ######
# 准备预测数据
work_week_pred <- expand.grid(
  day_number = seq(1, 7, length.out = 50),
  group = c("low", "high"),
  stringsAsFactors = FALSE
)

# 添加其他协变量的均值
covariates_work <- data.frame(
  age = mean(matched_work_week_data$age, na.rm = TRUE),
  gender = mean(matched_work_week_data$gender, na.rm = TRUE),
  education = mean(matched_work_week_data$education, na.rm = TRUE),
  nation = mean(matched_work_week_data$nation, na.rm = TRUE),
  income = mean(matched_work_week_data$income, na.rm = TRUE),
  pre_dep = mean(matched_work_week_data$pre_dep, na.rm = TRUE),
  nap_duration = mean(matched_work_week_data$nap_duration, na.rm = TRUE),
  sleep_duration = mean(matched_work_week_data$sleep_duration, na.rm = TRUE)
)

work_week_pred <- cbind(work_week_pred, covariates_work)

# 获取预测值
work_week_pred$predicted_dep <- predict(model_work_week, newdata = work_week_pred, re.form = NA)

p4E <- ggplot(work_week_pred, aes(x = day_number, y = predicted_dep, color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = predicted_dep - 0.1, ymax = predicted_dep + 0.1), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values = c("low" = "#FF7F0E", "high" = "#1F77B4"),
                     labels = c("low" = "Low Variability", "high" = "High Variability")) +
  scale_fill_manual(values = c("low" = "#FF7F0E", "high" = "#1F77B4"),
                    labels = c("low" = "Low Variability", "high" = "High Variability")) +
  labs(title = "",
       x = "Day Number",
       y = "Predicted Depression Score",
       color = "Nap Variability", fill = "Nap Variability") +
  nature_theme +
  annotate("text", x = 1, y = max(work_week_pred$predicted_dep) * 0.95,
           label = "Interaction: β = -0.113, p = 0.096\nLow: β = -0.177***\nHigh: β = -0.064",
           hjust = 0, size = 3, color = "black")

###### 图4F: 休息周时间趋势的交互作用 ######
# 准备预测数据
holiday_week_pred <- expand.grid(
  day_number = seq(8, 14, length.out = 50),
  group = c("low", "high"),
  stringsAsFactors = FALSE
)

# 添加其他协变量的均值
covariates_holiday <- data.frame(
  age = mean(matched_holiday_week_data$age, na.rm = TRUE),
  gender = mean(matched_holiday_week_data$gender, na.rm = TRUE),
  education = mean(matched_holiday_week_data$education, na.rm = TRUE),
  nation = mean(matched_holiday_week_data$nation, na.rm = TRUE),
  income = mean(matched_holiday_week_data$income, na.rm = TRUE),
  pre_dep = mean(matched_holiday_week_data$pre_dep, na.rm = TRUE),
  nap_duration = mean(matched_holiday_week_data$nap_duration, na.rm = TRUE),
  sleep_duration = mean(matched_holiday_week_data$sleep_duration, na.rm = TRUE)
)

holiday_week_pred <- cbind(holiday_week_pred, covariates_holiday)

# 获取预测值
holiday_week_pred$predicted_dep <- predict(model_holiday_week, newdata = holiday_week_pred, re.form = NA)

p4F <- ggplot(holiday_week_pred, aes(x = day_number, y = predicted_dep, color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = predicted_dep - 0.1, ymax = predicted_dep + 0.1), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values = c("low" = "#FF7F0E", "high" = "#1F77B4"),
                     labels = c("low" = "Low Variability", "high" = "High Variability")) +
  scale_fill_manual(values = c("low" = "#FF7F0E", "high" = "#1F77B4"),
                    labels = c("low" = "Low Variability", "high" = "High Variability")) +
  labs(title = "",
       x = "Day Number",
       y = "Predicted Depression Score",
       color = "Nap Variability", fill = "Nap Variability") +
  nature_theme +
  annotate("text", x = 8, y = max(holiday_week_pred$predicted_dep) * 0.95,
           label = "Interaction: β = -0.145***\nLow: β = -0.069*\nHigh: β = 0.076*",
           hjust = 0, size = 3, color = "black")

###### 组合所有图形 ######
# 第一行：散点图
row1 <- p4A + p4B + plot_layout(ncol = 2)

# 第二行：雨云图
row2 <- p4C + p4D + plot_layout(ncol = 2)

# 第三行：时间趋势图
row3 <- p4E + p4F + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")

# 组合所有图形
figure4 <- row1 / row2 / row3 + 
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = "bold", size = 12))

# 显示图形
print(figure4)

# 保存图形
ggsave("Figure4_Work_Holiday_Week_Analysis.png", figure4, 
       width = 16, height = 18, dpi = 300, bg = "white")
ggsave("Figure4_Work_Holiday_Week_Analysis.pdf", figure4, 
       width = 16, height = 18, dpi = 300)

###### 单独保存每个子图 ######
ggsave("Figure4A_WorkWeek_Scatter.png", p4A, width = 8, height = 6, dpi = 300)
ggsave("Figure4B_HolidayWeek_Scatter.png", p4B, width = 8, height = 6, dpi = 300)
ggsave("Figure4C_WorkWeek_Raincloud.png", p4C, width = 8, height = 6, dpi = 300)
ggsave("Figure4D_HolidayWeek_Raincloud.png", p4D, width = 8, height = 6, dpi = 300)
ggsave("Figure4E_WorkWeek_TimeTrend.png", p4E, width = 8, height = 6, dpi = 300)
ggsave("Figure4F_HolidayWeek_TimeTrend.png", p4F, width = 8, height = 6, dpi = 300)

cat("\n图4A-F已生成并保存!\n")