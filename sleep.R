
###### 3.1 Basic information of the sample########
library(readxl)
data_orgin<-read_excel("晚测+晨测+前测+后测数据.xlsx")
#table(data_orgin$`41、你总共打盹或午睡了多长时间?(例如：82分钟)`)
data<-data_orgin
# 加载必要的包
library(tidyverse)
library(lubridate)
library(ggplot2)
library(zoo)
library(VIM)


clean_data <- data %>%
  separate(唯一序号, into = c("subject_id", "measure_num"), sep = "_", remove = FALSE) %>%
  mutate(day_number = as.numeric(str_extract(Night, "\\d+"))) %>%
  select(subject_id, day_number, 
         age = `4、 您的年龄是（周岁）: 此题目非常重要，请认真填写！`,
         gender = `3、 您的性别:`,
         nation = `5、 您的民族:`,
         education = `12、 您目前在读学段:`,
         income = `14、您当前每月的生活费是:（元）`,
         pre_tls1 = `54、请选择最符合您的表述—您感到被遗忘`,
         pre_tls2 = `54、您感到与他人隔绝`,
         pre_tls3 = `54、您觉得自己缺乏陪伴`,
         pre_sdv1 = `13、请考虑到您近两周的睡眠方式，您对以下陈述的同意程度如何?答案没有对错之分，不要在任何一个陈述上花费太多时间。 —我每晚大约在同一时间上床睡觉。`,
         pre_sdv2 = `13、我每天早上大约在同一时间醒来(醒来但未起床)。`,
         pre_sdv3 = `13、我每天早上大约在同一时间起床。`,
         pre_sdv4 = `13、我每晚的睡眠时长大致相同。`,
         pre_sdv5 = `13、我每晚夜里醒来的次数大致相同。`,
         pre_sdv6 = `13、我每晚夜里醒来后，醒着的时长大致相同。`,
         pre_sleep_quality = pre_sleep_quality,
         pre_sleep_timing = pre_sleep_timing,
         pre_sleep_duration = pre_sleep_duration,
         pre_sleep_efficiency = pre_sleep_efficiency,
         pre_sleep_hypnotics = pre_sleep_hypnotics,
         pre_sleep_disorder = pre_sleep_disorder,
         pre_daytime_dysfunction = pre_daytime_dysfunction,
         nap_duration = `41、你总共打盹或午睡了多长时间?(例如：82分钟)` ,
         sleep_duration = `33、您总共睡了多久?（不需要细致计算自己中间觉醒耗费的时间，直接估计睡了几个小时就可以）（例如:1.5小时，单位为小时）`,
         pre_dep1 = `51、请选择两周内最符合您的表述—做事情时缺乏兴趣和乐趣`,
         pre_dep2 = `51、情绪低落、抑郁或无望`,
         pre_anx1 = `52、请选择两周内最符合您的表述—感觉紧张，焦虑或急切`,
         pre_anx2 = `52、很难放松下来`,
         pre_anx3 = `52、感到似乎将有可怕的事情发生而害怕`,
         pre_pss1 = `53、请选择最符合您的表述—经常觉得自己无法控制生活中的重要事情`,
         pre_pss2 =`53、经常对自己处理个人问题的能力有自信`,
         pre_pss3 = `53、经常觉得事情进展顺利`,
         pre_pss4 = `53、经常感到困难堆积如山，以致无法克服`,
         pre_tls1 = `54、请选择最符合您的表述—您感到被遗忘`,
         pre_tls2 = `54、您感到与他人隔绝`,
         pre_tls3 =`54、您觉得自己缺乏陪伴`,
         daily_dep = `32、在今天的一天中，你平均感到有多抑郁?`,
         daily_anx = `31、在今天的一天中，你平均感到有多焦虑?`,
         daily_pss = `21、今天我感到压力很大。`,
         daily_tls = `33、我今天感到孤独。`,
         post_dep1 = `24、做事情时缺乏兴趣和乐趣`,
         post_dep2 = `24、情绪低落、抑郁或无望`,
         post_anx1 = `24、请选择两周内最符合您的表述 —感觉紧张，焦虑或急切`,
         post_anx2 = `24、很难放松下来`,
         post_anx3 = `24、感到似乎将有可怕的事情发生而害怕`,
         post_pss1 = `25、请选择最符合您的表述 —经常觉得自己无法控制生活中的重要事情`,
         post_pss2 = `25、经常对自己处理个人问题的能力有自信`,
         post_pss3 = `25、经常觉得事情进展顺利`,
         post_pss4 = `25、经常感到困难堆积如山，以致无法克服`,
         post_tls1 = `26、请选择最符合您的表述 —您感到被遗忘`,
         post_tls2 = `26、您感到与他人隔绝`,
         post_tls3 = `26、您觉得自己缺乏陪伴`
         )

clean_data <- within(clean_data,{
  gender[clean_data$gender=="男"]<-0
  gender[clean_data$gender=="女"]<-1
  
  nation[clean_data$nation=="NA"]<-NA
  nation[nation != "汉族" & !is.na(nation)] <- 1
  nation[clean_data$nation=="汉族"]<-0
  
  education[clean_data$education=="○大专"]<-0
  education[clean_data$education=="○本科"]<-1
  education[clean_data$education=="○硕士"]<-2
  education[clean_data$education=="○博士"]<-3
  
  income[clean_data$income<1001]<-0
  income[clean_data$income<2001 & clean_data$income>1000]<-1
  income[clean_data$income<3001 & clean_data$income>2000]<-2
  income[clean_data$income<4001 & clean_data$income>3000]<-3
  income[clean_data$income<5001 & clean_data$income>4000]<-4
  income[clean_data$income>5000]<-5
  
  pre_tls1[clean_data$pre_tls1=="几乎从不"]<-0
  pre_tls1[clean_data$pre_tls1=="有时"]<-1
  pre_tls1[clean_data$pre_tls1=="经常"]<-2
  pre_tls2[clean_data$pre_tls2=="几乎从不"]<-0
  pre_tls2[clean_data$pre_tls2=="有时"]<-1
  pre_tls2[clean_data$pre_tls2=="经常"]<-2
  pre_tls3[clean_data$pre_tls3=="几乎从不"]<-0
  pre_tls3[clean_data$pre_tls3=="有时"]<-1
  pre_tls3[clean_data$pre_tls3=="经常"]<-2
  
  pre_sdv1[clean_data$pre_sdv1=="从没有"] <- 0
  pre_sdv1[clean_data$pre_sdv1=="极少"] <- 1
  pre_sdv1[clean_data$pre_sdv1=="有时"] <- 2
  pre_sdv1[clean_data$pre_sdv1=="经常"] <- 3
  pre_sdv1[clean_data$pre_sdv1=="总是"] <- 4
  pre_sdv2[clean_data$pre_sdv2=="从没有"] <- 0
  pre_sdv2[clean_data$pre_sdv2=="极少"] <- 1
  pre_sdv2[clean_data$pre_sdv2=="有时"] <- 2
  pre_sdv2[clean_data$pre_sdv2=="经常"] <- 3
  pre_sdv2[clean_data$pre_sdv2=="总是"] <- 4
  pre_sdv3[clean_data$pre_sdv3=="从没有"] <- 0
  pre_sdv3[clean_data$pre_sdv3=="极少"] <- 1
  pre_sdv3[clean_data$pre_sdv3=="有时"] <- 2
  pre_sdv3[clean_data$pre_sdv3=="经常"] <- 3
  pre_sdv3[clean_data$pre_sdv3=="总是"] <- 4
  pre_sdv4[clean_data$pre_sdv4=="从没有"] <- 0
  pre_sdv4[clean_data$pre_sdv4=="极少"] <- 1
  pre_sdv4[clean_data$pre_sdv4=="有时"] <- 2
  pre_sdv4[clean_data$pre_sdv4=="经常"] <- 3
  pre_sdv4[clean_data$pre_sdv4=="总是"] <- 4
  pre_sdv5[clean_data$pre_sdv5=="从没有"] <- 0
  pre_sdv5[clean_data$pre_sdv5=="极少"] <- 1
  pre_sdv5[clean_data$pre_sdv5=="有时"] <- 2
  pre_sdv5[clean_data$pre_sdv5=="经常"] <- 3
  pre_sdv5[clean_data$pre_sdv5=="总是"] <- 4
  pre_sdv6[clean_data$pre_sdv6=="从没有"] <- 0
  pre_sdv6[clean_data$pre_sdv6=="极少"] <- 1
  pre_sdv6[clean_data$pre_sdv6=="有时"] <- 2
  pre_sdv6[clean_data$pre_sdv6=="经常"] <- 3
  pre_sdv6[clean_data$pre_sdv6=="总是"] <- 4
  
  
  pre_dep1[clean_data$pre_dep1=="完全不会"]<-0
  pre_dep1[clean_data$pre_dep1=="几天"]<-1
  pre_dep1[clean_data$pre_dep1=="一半以上的日子"]<-2
  pre_dep1[clean_data$pre_dep1=="几乎每天"]<-3
  pre_dep2[clean_data$pre_dep2=="完全不会"]<-0
  pre_dep2[clean_data$pre_dep2=="几天"]<-1
  pre_dep2[clean_data$pre_dep2=="一半以上的日子"]<-2
  pre_dep2[clean_data$pre_dep2=="几乎每天"]<-3
  
  pre_anx1[clean_data$pre_anx1=="完全不会"]<-0
  pre_anx1[clean_data$pre_anx1=="几天"]<-1
  pre_anx1[clean_data$pre_anx1=="一半以上的日子"]<-2
  pre_anx1[clean_data$pre_anx1=="几乎每天"]<-3
  pre_anx2[clean_data$pre_anx2=="完全不会"]<-0
  pre_anx2[clean_data$pre_anx2=="几天"]<-1
  pre_anx2[clean_data$pre_anx2=="一半以上的日子"]<-2
  pre_anx2[clean_data$pre_anx2=="几乎每天"]<-3
  pre_anx3[clean_data$pre_anx3=="完全不会"]<-0
  pre_anx3[clean_data$pre_anx3=="几天"]<-1
  pre_anx3[clean_data$pre_anx3=="一半以上的日子"]<-2
  pre_anx3[clean_data$pre_anx3=="几乎每天"]<-3
  
  pre_pss1[clean_data$pre_pss1=="从不"]<-0
  pre_pss1[clean_data$pre_pss1=="偶尔"]<-1
  pre_pss1[clean_data$pre_pss1=="有时"]<-2
  pre_pss1[clean_data$pre_pss1=="经常"]<-3
  pre_pss1[clean_data$pre_pss1=="总是"]<-4
  pre_pss2[clean_data$pre_pss2=="从不"]<-4
  pre_pss2[clean_data$pre_pss2=="偶尔"]<-3
  pre_pss2[clean_data$pre_pss2=="有时"]<-2
  pre_pss2[clean_data$pre_pss2=="经常"]<-1
  pre_pss2[clean_data$pre_pss2=="总是"]<-0
  pre_pss3[clean_data$pre_pss3=="从不"]<-4
  pre_pss3[clean_data$pre_pss3=="偶尔"]<-3
  pre_pss3[clean_data$pre_pss3=="有时"]<-2
  pre_pss3[clean_data$pre_pss3=="经常"]<-1
  pre_pss3[clean_data$pre_pss3=="总是"]<-0
  pre_pss4[clean_data$pre_pss4=="从不"]<-0
  pre_pss4[clean_data$pre_pss4=="偶尔"]<-1
  pre_pss4[clean_data$pre_pss4=="有时"]<-2
  pre_pss4[clean_data$pre_pss4=="经常"]<-3
  pre_pss4[clean_data$pre_pss4=="总是"]<-4
  
  
  post_tls1[clean_data$post_tls1=="几乎从不"]<-0
  post_tls1[clean_data$post_tls1=="有时"]<-1
  post_tls1[clean_data$post_tls1=="经常"]<-2
  post_tls2[clean_data$post_tls2=="几乎从不"]<-0
  post_tls2[clean_data$post_tls2=="有时"]<-1
  post_tls2[clean_data$post_tls2=="经常"]<-2
  post_tls3[clean_data$post_tls3=="几乎从不"]<-0
  post_tls3[clean_data$post_tls3=="有时"]<-1
  post_tls3[clean_data$post_tls3=="经常"]<-2
  
  post_dep1[clean_data$post_dep1=="完全不会"]<-0
  post_dep1[clean_data$post_dep1=="几天"]<-1
  post_dep1[clean_data$post_dep1=="一半以上的日子"]<-2
  post_dep1[clean_data$post_dep1=="几乎每天"]<-3
  post_dep2[clean_data$post_dep2=="完全不会"]<-0
  post_dep2[clean_data$post_dep2=="几天"]<-1
  post_dep2[clean_data$post_dep2=="一半以上的日子"]<-2
  post_dep2[clean_data$post_dep2=="几乎每天"]<-3
  
  post_anx1[clean_data$post_anx1=="完全不会"]<-0
  post_anx1[clean_data$post_anx1=="几天"]<-1
  post_anx1[clean_data$post_anx1=="一半以上的日子"]<-2
  post_anx1[clean_data$post_anx1=="几乎每天"]<-3
  post_anx2[clean_data$post_anx2=="完全不会"]<-0
  post_anx2[clean_data$post_anx2=="几天"]<-1
  post_anx2[clean_data$post_anx2=="一半以上的日子"]<-2
  post_anx2[clean_data$post_anx2=="几乎每天"]<-3
  post_anx3[clean_data$post_anx3=="完全不会"]<-0
  post_anx3[clean_data$post_anx3=="几天"]<-1
  post_anx3[clean_data$post_anx3=="一半以上的日子"]<-2
  post_anx3[clean_data$post_anx3=="几乎每天"]<-3
  
  post_pss1[clean_data$post_pss1=="从不"]<-0
  post_pss1[clean_data$post_pss1=="偶尔"]<-1
  post_pss1[clean_data$post_pss1=="有时"]<-2
  post_pss1[clean_data$post_pss1=="经常"]<-3
  post_pss1[clean_data$post_pss1=="总是"]<-4
  post_pss2[clean_data$post_pss2=="从不"]<-4
  post_pss2[clean_data$post_pss2=="偶尔"]<-3
  post_pss2[clean_data$post_pss2=="有时"]<-2
  post_pss2[clean_data$post_pss2=="经常"]<-2
  post_pss2[clean_data$post_pss2=="总是"]<-0
  post_pss3[clean_data$post_pss3=="从不"]<-4
  post_pss3[clean_data$post_pss3=="偶尔"]<-3
  post_pss3[clean_data$post_pss3=="有时"]<-2
  post_pss3[clean_data$post_pss3=="经常"]<-2
  post_pss3[clean_data$post_pss3=="总是"]<-0
  post_pss4[clean_data$post_pss4=="从不"]<-0
  post_pss4[clean_data$post_pss4=="偶尔"]<-1
  post_pss4[clean_data$post_pss4=="有时"]<-2
  post_pss4[clean_data$post_pss4=="经常"]<-3
  post_pss4[clean_data$post_pss4=="总是"]<-4
  
  
  post_tls1[clean_data$post_tls1=="几乎从不"]<-0
  post_tls1[clean_data$post_tls1=="有时"]<-1
  post_tls1[clean_data$post_tls1=="经常"]<-2
  post_tls2[clean_data$post_tls2=="几乎从不"]<-0
  post_tls2[clean_data$post_tls2=="有时"]<-1
  post_tls2[clean_data$post_tls2=="经常"]<-2
  post_tls3[clean_data$post_tls3=="几乎从不"]<-0
  post_tls3[clean_data$post_tls3=="有时"]<-1
  post_tls3[clean_data$post_tls3=="经常"]<-2
  
  daily_pss[clean_data$daily_pss=="非常不同意"] <-0
  daily_pss[clean_data$daily_pss=="不同意"] <-1
  daily_pss[clean_data$daily_pss=="比较不同意"] <-2
  daily_pss[clean_data$daily_pss=="一般"] <-3
  daily_pss[clean_data$daily_pss=="比较同意"] <-4
  daily_pss[clean_data$daily_pss=="同意"] <-5
  daily_pss[clean_data$daily_pss=="非常同意"] <-6
  
  daily_dep[clean_data$daily_dep=="极度"]<-10
  daily_dep[clean_data$daily_dep=="一点也不"]<-1
  
  daily_anx[clean_data$daily_anx=="极度"]<-10
  daily_anx[clean_data$daily_anx=="一点也不"]<-1
  
  daily_tls[clean_data$daily_tls=="极度"]<-10
  daily_tls[clean_data$daily_tls=="一点也不"]<-1
})

clean_data <- clean_data %>%
  mutate(across(2:50, as.numeric)) %>%
  select(1:50)

clean_data$pre_dep <- clean_data$pre_dep1+clean_data$pre_dep2
clean_data$pre_anx <- clean_data$pre_anx1+clean_data$pre_anx2+clean_data$pre_anx3
clean_data$pre_pss <- clean_data$pre_pss1+clean_data$pre_pss2+clean_data$pre_pss3+clean_data$pre_pss4
clean_data$pre_tls <-clean_data$pre_tls1+clean_data$pre_tls2+clean_data$pre_tls3

clean_data$pre_psqi <-clean_data$pre_sleep_duration+clean_data$pre_sleep_timing+clean_data$pre_sleep_quality+clean_data$pre_sleep_efficiency+clean_data$pre_sleep_hypnotics+clean_data$pre_sleep_disorder+clean_data$pre_daytime_dysfunction

clean_data$post_dep <- clean_data$post_dep1+clean_data$post_dep2
clean_data$post_anx <- clean_data$post_anx1+clean_data$post_anx2+clean_data$post_anx3
clean_data$post_pss <- clean_data$post_pss1+clean_data$post_pss2+clean_data$post_pss3+clean_data$post_pss4
clean_data$post_tls <- clean_data$post_tls1+clean_data$post_tls2+clean_data$post_tls3

clean_data$pre_sdv <-clean_data$pre_sdv1+clean_data$pre_sdv2+clean_data$pre_sdv3+clean_data$pre_sdv4+clean_data$pre_sdv5+clean_data$pre_sdv6


clean_data <- clean_data[complete.cases(clean_data$day_number),]

clean_data <- clean_data %>%
  filter(nap_duration < 360 & nap_duration >= 0 & 
           sleep_duration < 15 & sleep_duration >= 0)

clean_data <- clean_data %>%
  group_by(subject_id) %>%
  filter(n() >= 7) %>%
  ungroup()

clean_data <- clean_data %>%
  group_by(subject_id) %>%
  mutate(measurement_count = n()) %>%
  ungroup()

table(clean_data$nap_duration)
table(clean_data$measurement_count)


#####Figure 1
#writexl::write_xlsx(clean_data,"clean_data.xlsx")
daily_heatmap_data <- clean_data %>%
  group_by(subject_id) %>%
  mutate(subject_order = cur_group_id()) %>%
  ungroup() %>%
  arrange(subject_order, day_number) %>%
  mutate(subject_id = factor(subject_id, levels = unique(subject_id)))

nature_palette <- colorRampPalette(c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD"))(max(daily_heatmap_data$day_number))
ggplot(daily_heatmap_data, aes(x = day_number, y = subject_id, fill = as.factor(day_number))) +
  geom_tile(color = "white", size = 0.1) +  # 减小边框线条粗细
  labs(
    title = "Daily Measurement Pattern",
    subtitle = paste("Total subjects:", n_distinct(daily_heatmap_data$subject_id), 
                     "| Total days:", max(daily_heatmap_data$day_number)),
    x = "Measurement Days",
    y = "Subject ID",
    fill = "Day"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right",
    text = element_text(family = "sans")
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = nature_palette)


#####Sociodemographic information
#age
clean_data %>%
  group_by(subject_id) %>%
  summarise(age = first(age)) %>% 
  ungroup() %>%
  bruceR::Describe(age)
#gender
clean_data %>%
  group_by(subject_id) %>%
  summarise(gender = first(gender)) %>% 
  ungroup() %>%
  { table(.$gender) }
#education
clean_data %>%
  group_by(subject_id) %>%
  summarise(edu = first(education)) %>% 
  ungroup() %>%
  { table(.$edu) }
#nation
clean_data %>%
  group_by(subject_id) %>%
  summarise(nation = first(nation)) %>% 
  ungroup() %>%
  { table(.$nation) }
#income
clean_data %>%
  group_by(subject_id) %>%
  summarise(income = first(income)) %>% 
  ungroup() %>%
  { table(.$income) }

###### 计算量表信度 ######
library(psych)

# 1. 抑郁量表 (dep) - 前测和后测
# 前测抑郁
pre_dep_items <- clean_data %>%
  group_by(subject_id) %>%
  slice(1) %>%  # 取每个被试的第一条记录（前测数据）
  ungroup() %>%
  select(pre_dep1, pre_dep2) %>%
  mutate(across(everything(), as.numeric))  # 确保所有列都是数值型

psych::alpha(pre_dep_items)
# 后测抑郁
post_dep_items <- clean_data %>%
  group_by(subject_id) %>%
  slice(1) %>%  # 取每个被试的第一条记录
  ungroup() %>%
  select(post_dep1, post_dep2)

psych::alpha(post_dep_items)

# 2. 焦虑量表 (anx) - 前测和后测
# 前测焦虑
pre_anx_items <- clean_data %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(pre_anx1, pre_anx2, pre_anx3)

psych::alpha(pre_anx_items, check.keys = TRUE)

# 后测焦虑
post_anx_items <- clean_data %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(post_anx1, post_anx2, post_anx3)

psych::alpha(post_anx_items, check.keys = TRUE)

# 3. 压力量表 (pss) - 前测和后测
# 前测压力
pre_pss_items <- clean_data %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(pre_pss1, pre_pss2, pre_pss3, pre_pss4)

psych::alpha(pre_pss_items, check.keys = TRUE)

# 后测压力
post_pss_items <- clean_data %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(post_pss1, post_pss2, post_pss3, post_pss4)

psych::alpha(post_pss_items, check.keys = TRUE)

# 4. 孤独感量表 (tls) - 前测和后测
# 前测孤独感
pre_tls_items <- clean_data %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(pre_tls1, pre_tls2, pre_tls3)

psych::alpha(pre_tls_items, check.keys = TRUE)

# 后测孤独感
post_tls_items <- clean_data %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(post_tls1, post_tls2, post_tls3)

psych::alpha(post_tls_items, check.keys = TRUE)

# 5. 睡眠质量指数 (PSQI) - 前测
pre_psqi_items <- clean_data %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(pre_sleep_duration, pre_sleep_timing, pre_sleep_quality, 
         pre_sleep_efficiency, pre_sleep_hypnotics, pre_sleep_disorder, 
         pre_daytime_dysfunction)

psych::alpha(pre_psqi_items, check.keys = TRUE)

# 6. 睡眠规律性量表 (SDV) - 前测
pre_sdv_items <- clean_data %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(pre_sdv1, pre_sdv2, pre_sdv3, pre_sdv4, pre_sdv5, pre_sdv6)

psych::alpha(pre_sdv_items, check.keys = TRUE)

# 7. 日常测量信度 (daily measures) - 计算每个被试内部的一致性
# 日常抑郁、焦虑、压力、孤独感
daily_reliability <- clean_data %>%
  group_by(subject_id) %>%
  filter(n() >= 3) %>%  # 只保留至少有3次测量的被试
  summarise(
    daily_dep_alpha = ifelse(sd(daily_dep, na.rm = TRUE) > 0, 
                             cronbach.alpha(cbind(daily_dep), na.rm = TRUE)$alpha, NA),
    daily_anx_alpha = ifelse(sd(daily_anx, na.rm = TRUE) > 0, 
                             cronbach.alpha(cbind(daily_anx), na.rm = TRUE)$alpha, NA),
    daily_pss_alpha = ifelse(sd(daily_pss, na.rm = TRUE) > 0, 
                             cronbach.alpha(cbind(daily_pss), na.rm = TRUE)$alpha, NA),
    daily_tls_alpha = ifelse(sd(daily_tls, na.rm = TRUE) > 0, 
                             cronbach.alpha(cbind(daily_tls), na.rm = TRUE)$alpha, NA)
  ) %>%
  ungroup()

cat("日常测量信度描述性统计:\n")
bruceR::Describe(daily_reliability %>% select(-subject_id))

# 8. 汇总所有量表的信度结果
reliability_summary <- data.frame(
  量表 = c("前测抑郁", "后测抑郁", "前测焦虑", "后测焦虑", 
         "前测压力", "后测压力", "前测孤独感", "后测孤独感", 
         "前测PSQI", "前测睡眠规律性"),
  Cronbach_alpha = c(
    alpha(pre_dep_items, check.keys = TRUE)$total$raw_alpha,
    alpha(post_dep_items, check.keys = TRUE)$total$raw_alpha,
    alpha(pre_anx_items, check.keys = TRUE)$total$raw_alpha,
    alpha(post_anx_items, check.keys = TRUE)$total$raw_alpha,
    alpha(pre_pss_items, check.keys = TRUE)$total$raw_alpha,
    alpha(post_pss_items, check.keys = TRUE)$total$raw_alpha,
    alpha(pre_tls_items, check.keys = TRUE)$total$raw_alpha,
    alpha(post_tls_items, check.keys = TRUE)$total$raw_alpha,
    alpha(pre_psqi_items, check.keys = TRUE)$total$raw_alpha,
    alpha(pre_sdv_items, check.keys = TRUE)$total$raw_alpha
  )
)

print("各量表信度汇总:")
print(reliability_summary)

# 可视化信度结果
ggplot(reliability_summary, aes(x = reorder(量表, Cronbach_alpha), y = Cronbach_alpha)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = round(Cronbach_alpha, 3)), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(title = "各量表信度(Cronbach's α)汇总",
       x = "量表",
       y = "Cronbach's α系数") +
  theme_minimal() +
  ylim(0, 1) +
  theme(plot.title = element_text(hjust = 0.5))


######3.2 The effects of 14-day variability in nap duration and nighttime sleep duration on post-test depression#######
#这一步没作用，因为nap没有缺失
imputed_data <- clean_data %>%
  group_by(subject_id) %>%
  arrange(day_number) %>%
  mutate(nap_duration_imputed = na.approx(nap_duration, na.rm = FALSE)) %>%
  mutate(nap_duration_imputed = na.locf(nap_duration_imputed, na.rm = FALSE)) %>%
  mutate(nap_duration_imputed = na.locf(nap_duration_imputed, na.rm = FALSE, fromLast = TRUE)) %>%
  ungroup()

summary(imputed_data$nap_duration_imputed)

variability_metrics <- imputed_data %>%
  group_by(subject_id) %>%
  summarise(
    # 基本统计量
    mean_nap_duration = mean(nap_duration_imputed, na.rm = TRUE),
    sd_nap_duration = sd(nap_duration_imputed, na.rm = TRUE), # 标准差
    cv_nap_duration = sd_nap_duration / mean_nap_duration,    # 变异系数
    range_nap_duration = max(nap_duration_imputed, na.rm = TRUE) - min(nap_duration_imputed, na.rm = TRUE), # 全距
    # 日内标准差（如果需要计算其他变异性指标）
    iqr_nap_duration = IQR(nap_duration_imputed, na.rm = TRUE), # 四分位距
    # 均方连续差 (MSSD) - 测量相邻时间点的变化
    mssd = mean(diff(nap_duration_imputed)^2, na.rm = TRUE),
    rmssd = sqrt(mssd) , # 均方根连续差
    mean_sleep_duration = mean(sleep_duration),
    sd_sleep_duration = sd(sleep_duration)
  ) %>%
  ungroup()
# 查看变异性指标
print(variability_metrics)

data_new <- imputed_data %>%
  group_by(subject_id) %>%
  summarise(
    age =mean(age),
    gender = mean(gender),
    education = mean(education),
    nation = mean(nation),
    income = mean(income),
    pre_dep = mean(pre_dep),
    pre_anx = mean(pre_anx),
    pre_pss = mean(pre_pss),
    pre_tls = mean(pre_tls),
    pre_psqi = mean(pre_psqi),
    pre_sleep_duration=mean(pre_sleep_duration),
    pre_sleep_timing=mean(pre_sleep_timing),
    pre_sleep_quality=mean(pre_sleep_quality),
    pre_sleep_efficiency=mean(pre_sleep_efficiency),
    pre_sleep_hypnotics=mean(pre_sleep_hypnotics),
    pre_sleep_disorder=mean(pre_sleep_disorder),
    pre_daytime_dysfunction=mean(pre_daytime_dysfunction),
    post_dep = mean(post_dep),
    post_anx = mean(post_anx),
    post_pss = mean(post_pss),
    post_tls = mean(post_tls),
    pre_sdv = mean(pre_sdv)) 

data_new<-cbind(data_new,variability_metrics,by="subject_id")


lm1<-lm(post_dep~mean_nap_duration+sd_nap_duration+mean_sleep_duration+sd_sleep_duration,data_new)
summary(lm1)
BIC(lm1)
AIC(lm1)

lm2<-lm(post_dep~age + gender + education+ income+ nation+ mean_nap_duration+sd_nap_duration+mean_sleep_duration+sd_sleep_duration,data_new)
summary(lm2)
BIC(lm2)
AIC(lm2)

lm3<-lm(post_dep~age + gender + education+ income+ nation+ pre_dep+mean_nap_duration+sd_nap_duration+mean_sleep_duration+sd_sleep_duration,data_new)
summary(lm3)
BIC(lm3)
AIC(lm3)

lm4<-lm(post_dep~age + gender + education+ income+ nation+ pre_dep+ pre_anx+ pre_pss+pre_tls+mean_nap_duration+sd_nap_duration+mean_sleep_duration+sd_sleep_duration,data_new)
summary(lm4)
BIC(lm4)
AIC(lm4)

lm5<-lm(post_dep~age + gender + education+ income+ nation+ pre_dep+ pre_anx+ pre_pss+pre_tls+
          pre_sleep_duration+pre_sleep_timing+pre_sleep_quality+pre_sleep_efficiency+pre_sleep_hypnotics+pre_sleep_disorder+pre_daytime_dysfunction
          +mean_nap_duration+sd_nap_duration+mean_sleep_duration+sd_sleep_duration,data_new)
summary(lm5)
BIC(lm5)
AIC(lm5)

######3.3 Higher nap duration variability influenced depression, but not anxiety, stress, or loneliness#########

summary(data_new$sd_nap_duration)
data_new <- na.omit(data_new)
quantile(data_new$sd_nap_duration, probs = 0.27,na.rm=TRUE)# 17.74564
quantile(data_new$sd_nap_duration, probs = 0.73,na.rm=TRUE)# 39.56311

high_sdv <- data_new[data_new$sd_nap_duration>40.07837,]
low_sdv <- data_new[data_new$sd_nap_duration<18.77773,]
high_sdv <- na.omit(high_sdv)
low_sdv<-na.omit(low_sdv)

##### 进行倾向得分匹配
high_sdv$group <- "high"
low_sdv$group <- "low"
# 合并数据
combined_data <- rbind(high_sdv, low_sdv)
combined_data$group <- as.factor(combined_data$group)
library(MatchIt)
matchit_result_advanced <- matchit(group ~ age + gender + education + nation + income +
                                     pre_dep + pre_anx + pre_pss + pre_tls + pre_sleep_duration + 
                                     pre_sleep_timing + pre_sleep_quality + pre_sleep_efficiency + 
                                     pre_sleep_hypnotics + pre_sleep_disorder + pre_daytime_dysfunction + 
                                      pre_daytime_dysfunction, 
                                   data = combined_data,
                                   method = "nearest",
                                   distance = "glm",
                                   ratio = 1,
                                   caliper = 0.6)

# 获取高级匹配后的数据
matched_data_advanced <- match.data(matchit_result_advanced)

# 重新分割数据
high_sdv_matched_adv <- matched_data_advanced[matched_data_advanced$group == "high", ]
low_sdv_matched_adv <- matched_data_advanced[matched_data_advanced$group == "low", ]

# 创建结果表格的函数
create_comparison_table <- function(high_data, low_data) {
  # 初始化结果数据框
  results <- data.frame(
    Variables = character(),
    Low_Group = character(),
    High_Group = character(),
    Statistic = numeric(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # 添加行的函数
  add_row_to_results <- function(var_name, low_val, high_val, stat, p_val) {
    results <<- rbind(results, data.frame(
      Variables = var_name,
      Low_Group = low_val,
      High_Group = high_val,
      Statistic = round(stat, 3),
      P_Value = round(p_val, 3),
      stringsAsFactors = FALSE
    ))
  }
  
  # 年龄
  t_age <- t.test(high_data$age, low_data$age)
  add_row_to_results("Age", 
                     paste0(round(mean(low_data$age), 2), "±", round(sd(low_data$age), 2)),
                     paste0(round(mean(high_data$age), 2), "±", round(sd(high_data$age), 2)),
                     t_age$statistic, t_age$p.value)
  
  # 性别（卡方检验）
  gender_table <- table(
    Group = c(rep("Low", nrow(low_data)), rep("High", nrow(high_data))),
    Gender = c(low_data$gender, high_data$gender)
  )
  chi_gender <- chisq.test(gender_table)
  gender_low <- paste0(sum(low_data$gender == 0), ":", sum(low_data$gender == 1))
  gender_high <- paste0(sum(high_data$gender == 0), ":", sum(high_data$gender == 1))
  add_row_to_results("Gender", gender_low, gender_high, 
                     chi_gender$statistic, chi_gender$p.value)
  
  # 教育
  t_edu <- t.test(high_data$education, low_data$education)
  add_row_to_results("Education", 
                     paste0(round(mean(low_data$education), 2), "±", round(sd(low_data$education), 2)),
                     paste0(round(mean(high_data$education), 2), "±", round(sd(high_data$education), 2)),
                     t_edu$statistic, t_edu$p.value)
  
  # 收入
  t_income <- t.test(high_data$income, low_data$income)
  add_row_to_results("Income", 
                     paste0(round(mean(low_data$income), 2), "±", round(sd(low_data$income), 2)),
                     paste0(round(mean(high_data$income), 2), "±", round(sd(high_data$income), 2)),
                     t_income$statistic, t_income$p.value)
  
  # 民族
  nation_table <- table(
    Group = c(rep("Low", nrow(low_data)), rep("High", nrow(high_data))),
    Nation = c(low_data$nation, high_data$nation)
  )
  chi_nation <- chisq.test(nation_table)
  nation_low <- paste0(sum(low_data$nation == 0), ":", sum(low_data$nation == 1))
  nation_high <- paste0(sum(high_data$nation == 0), ":", sum(high_data$nation == 1))
  add_row_to_results("Nation", nation_low, nation_high, 
                     chi_nation$statistic, chi_nation$p.value)
  
  # 基线抑郁
  t_pre_dep <- t.test(high_data$pre_dep, low_data$pre_dep)
  add_row_to_results("Baseline depression", 
                     paste0(round(mean(low_data$pre_dep), 2), "±", round(sd(low_data$pre_dep), 2)),
                     paste0(round(mean(high_data$pre_dep), 2), "±", round(sd(high_data$pre_dep), 2)),
                     t_pre_dep$statistic, t_pre_dep$p.value)
  
  # 基线焦虑
  t_pre_anx <- t.test(high_data$pre_anx, low_data$pre_anx)
  add_row_to_results("Baseline anxiety", 
                     paste0(round(mean(low_data$pre_anx), 2), "±", round(sd(low_data$pre_anx), 2)),
                     paste0(round(mean(high_data$pre_anx), 2), "±", round(sd(high_data$pre_anx), 2)),
                     t_pre_anx$statistic, t_pre_anx$p.value)
  
  # 基线压力
  t_pre_pss <- t.test(high_data$pre_pss, low_data$pre_pss)
  add_row_to_results("Baseline pressure", 
                     paste0(round(mean(low_data$pre_pss), 2), "±", round(sd(low_data$pre_pss), 2)),
                     paste0(round(mean(high_data$pre_pss), 2), "±", round(sd(high_data$pre_pss), 2)),
                     t_pre_pss$statistic, t_pre_pss$p.value)
  
  # 基线孤独感
  t_pre_tls <- t.test(high_data$pre_tls, low_data$pre_tls)
  add_row_to_results("Baseline loneliness", 
                     paste0(round(mean(low_data$pre_tls), 2), "±", round(sd(low_data$pre_tls), 2)),
                     paste0(round(mean(high_data$pre_tls), 2), "±", round(sd(high_data$pre_tls), 2)),
                     t_pre_tls$statistic, t_pre_tls$p.value)
  
  # 睡眠相关变量
  sleep_vars <- c(
    "pre_sleep_duration", "pre_sleep_timing", "pre_sleep_quality", 
    "pre_sleep_efficiency", "pre_sleep_hypnotics", "pre_sleep_disorder", 
    "pre_daytime_dysfunction"
  )
  
  sleep_names <- c(
    "Baseline sleep duration", "Baseline sleep timing", "Baseline sleep quality",
    "Baseline sleep efficiency", "Baseline sleep hypnotics", "Baseline sleep disorder",
    "Baseline daytime dysfunction"
  )
  
  for (i in 1:length(sleep_vars)) {
    var <- sleep_vars[i]
    name <- sleep_names[i]
    
    t_test <- t.test(high_data[[var]], low_data[[var]])
    add_row_to_results(name, 
                       paste0(round(mean(low_data[[var]]), 2), "±", round(sd(low_data[[var]]), 2)),
                       paste0(round(mean(high_data[[var]]), 2), "±", round(sd(high_data[[var]]), 2)),
                       t_test$statistic, t_test$p.value)
  }
  
  # 后测变量
  post_vars <- c("post_dep", "post_anx", "post_pss", "post_tls")
  post_names <- c("Post-test depression", "Post-test anxiety", 
                  "Post-test pressure", "Post-test loneliness")
  
  for (i in 1:length(post_vars)) {
    var <- post_vars[i]
    name <- post_names[i]
    
    t_test <- t.test(high_data[[var]], low_data[[var]])
    add_row_to_results(name, 
                       paste0(round(mean(low_data[[var]]), 2), "±", round(sd(low_data[[var]]), 2)),
                       paste0(round(mean(high_data[[var]]), 2), "±", round(sd(high_data[[var]]), 2)),
                       t_test$statistic, t_test$p.value)
  }
  
  return(results)
}

# 生成表格
comparison_table <- create_comparison_table(high_sdv_matched_adv, low_sdv_matched_adv)

# 重命名列名以匹配要求的格式
names(comparison_table) <- c("Variables", "Low Nap duration variability", 
                             "High Nap duration variability", "T/χ²", "P value")

# 打印表格
print(comparison_table)

# 可选：将表格保存为CSV文件
write.csv(comparison_table, "matched_comparison_table.csv", row.names = FALSE)

# 在控制台中以更美观的格式显示
cat("\n匹配后的基线比较表格:\n")
cat("==================================================================================\n")
cat(sprintf("%-30s %-25s %-25s %-8s %-8s\n", 
            "Variables", "Low Group", "High Group", "T/χ²", "P value"))
cat("----------------------------------------------------------------------------------\n")
for (i in 1:nrow(comparison_table)) {
  cat(sprintf("%-30s %-25s %-25s %-8.3f %-8.3f\n",
              comparison_table$Variables[i],
              comparison_table$`Low Nap duration variability`[i],
              comparison_table$`High Nap duration variability`[i],
              comparison_table$`T/χ²`[i],
              comparison_table$`P value`[i]))
}
cat("==================================================================================\n")

# 检验匹配效果
cat("\n倾向得分匹配效果检验:\n")
summary(matchit_result_advanced)



##### 计算post_dep的Cohen's d
library(effectsize)
cohen_d_post_dep <- cohens_d(high_sdv_matched_adv$post_dep, 
                             low_sdv_matched_adv$post_dep,
                             pooled_sd = TRUE)
print(cohen_d_post_dep)


#####绘制雨云图


source("geom_flat_violin.R")
library(cowplot) #一种整洁的theme
library(readr)
library(tidyverse)
library(ggplot2)
library(gglayer)
library(ggpubr)

#构建分组
high_group<-high_sdv_matched_adv
low_group<-low_sdv_matched_adv
low_group$sd_nap_duration[low_group$sd_nap_duration<=max(low_group$sd_nap_duration)]<-"Low"
high_group$sd_nap_duration[high_group$sd_nap_duration>=min(high_group$sd_nap_duration)]<-"High"
sdv_group<-rbind(low_group,high_group)
sdv_group$sd_nap_duration<-as.factor(sdv_group$sd_nap_duration)

# dep的云雨图
ggplot(sdv_group, aes(x = sd_nap_duration, y = post_dep, fill = sd_nap_duration)) +
  geom_flat_violin(aes(fill = sd_nap_duration),position = position_nudge(x = .1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(sd_nap_duration)-.15, y = post_dep, colour = sd_nap_duration),position =
               position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = sd_nap_duration, y = post_dep, fill = sd_nap_duration),outlier.shape = NA, alpha
               = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+theme_cowplot()+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Nap duration Variability", y = "Post-test depression")+theme_cowplot()+
  theme(legend.position = "none")+ #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = ..p.format..), method = "t.test", 
                     comparisons = list(c("High", "Low")))  

source("geom_flat_violin.R")
library(cowplot)
library(readr)
library(tidyverse)
library(ggplot2)
library(gglayer)
library(ggpubr)
library(patchwork) # 新增patchwork包用于图形组合

# 构建分组
high_group <- high_sdv_matched_adv
low_group <- low_sdv_matched_adv
low_group$sd_nap_duration[low_group$sd_nap_duration <= max(low_group$sd_nap_duration)] <- "Low"
high_group$sd_nap_duration[high_group$sd_nap_duration >= min(high_group$sd_nap_duration)] <- "High"
sdv_group <- rbind(low_group, high_group)
sdv_group$sd_nap_duration <- as.factor(sdv_group$sd_nap_duration)

# 创建绘制雨云图的函数
create_raincloud_plot <- function(data, y_var, y_label) {
  ggplot(data, aes(x = sd_nap_duration, y = .data[[y_var]], fill = sd_nap_duration)) +
    geom_flat_violin(aes(fill = sd_nap_duration),
                     position = position_nudge(x = .1, y = 0),
                     adjust = 1.5, trim = FALSE, alpha = .5, colour = NA) +
    geom_point(aes(x = as.numeric(sd_nap_duration) - .15, y = .data[[y_var]], colour = sd_nap_duration),
               position = position_jitter(width = .05), size = 1, shape = 20) +
    geom_boxplot(aes(x = sd_nap_duration, y = .data[[y_var]], fill = sd_nap_duration),
                 outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    labs(x = "Nap duration Variability", y = y_label) +
    theme_cowplot() +
    theme(legend.position = "none") +
    stat_compare_means(aes(label = ..p.format..), method = "t.test", 
                       comparisons = list(c("High", "Low")))
}

# 创建四个子图
plot_dep <- create_raincloud_plot(sdv_group, "post_dep", "Post-test depression")
plot_anx <- create_raincloud_plot(sdv_group, "post_anx", "Post-test anxiety")
plot_pss <- create_raincloud_plot(sdv_group, "post_pss", "Post-test stress")
plot_tls <- create_raincloud_plot(sdv_group, "post_tls", "Post-test total score")

# 组合图形 - 2x2布局
combined_plot <- (plot_dep + plot_anx) / (plot_pss + plot_tls) +
  plot_annotation(tag_levels = 'A') # 添加子图标签A, B, C, D

# 显示组合图形
combined_plot













#######3.4The Moderating Effect of Nap Duration Variability on the Change in Depression Over Time #######
library(dplyr)
# 提取所有需要的subject_id
high_ids <- high_sdv_matched_adv$subject_id
low_ids <- low_sdv_matched_adv$subject_id

# 从clean_data中筛选并添加组别
matched_clean_data <- clean_data %>%
  filter(subject_id %in% c(high_ids, low_ids)) %>%
  mutate(group = case_when(
    subject_id %in% high_ids ~ "high",
    subject_id %in% low_ids ~ "low"
  ))
matched_clean_data$holiday<-matched_clean_data$day_number
matched_clean_data <-within(matched_clean_data,{
  holiday[matched_clean_data$holiday<=7]<-0
  holiday[matched_clean_data$holiday>=8]<-1
})

model1 <- lmer(daily_dep~ age+gender+education+nation+income+holiday+
                 day_number * group + (1| subject_id), data = matched_clean_data)
summary(model1)

model2 <- lmer(daily_dep~ age+gender+education+nation+income+holiday+pre_dep+
                 day_number * group + (1| subject_id), data = matched_clean_data)
summary(model2)

model3 <- lmer(daily_dep~ age+gender+education+nation+income+holiday+pre_dep+nap_duration+sleep_duration+
                 day_number * group + (1+ day_number | subject_id), data = matched_clean_data)
summary(model3)

model4 <- lmer(daily_dep~ age+gender+education+nation+income+holiday+pre_dep+nap_duration+sleep_duration+
                 day_number * group + (1+ day_number +nap_duration+sleep_duration | subject_id), data = matched_clean_data)
summary(model4)

model5 <- lmer(daily_dep~ age+gender+education+nation+income+pre_dep+pre_anx+pre_pss+pre_tls+nap_duration+sleep_duration+holiday+
                 day_number * group + (1+ day_number +nap_duration+sleep_duration | subject_id), data = matched_clean_data)
summary(model5)

anova(model1,model2,model3,model4,model5)
#简单效应检验
emm_slopes <- emtrends(model4, ~ group, var = "day_number")
summary(emm_slopes)  # 各组的斜率估计
pairs(emm_slopes)    # 组间斜率差异检验

##figure 3
library(ggplot2)
nature_colors <- c("high" = "#1F77B4", "low" = "#FF7F0E")  # Blue and orange
ggplot(matched_clean_data, aes(x = day_number, y = daily_dep, group = subject_id, color = group)) +
  geom_line(alpha = 0.4, size = 0.6) +  # Adjust line thickness and transparency
  stat_smooth(aes(group = group, color = group), 
              method = "lm", se = TRUE, size = 1.2) +
  labs(title = "",
       x = "Day Number", y = "Depression Score",
       color = "Nap Variability") +
  theme_minimal() +
  scale_color_manual(values = nature_colors) +
  theme(
    text = element_text(family = "sans", size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )


####### 3.5 Stability test #######
######3.5.1 Repeat the content of section 3.2#####
# 加载必要的包
library(tidyverse)
library(mice)

# 首先为每个被试创建完整的14天序列
complete_days <- clean_data %>%
  distinct(subject_id) %>%
  expand(subject_id, day_number = 1:14)

# 将原始数据与完整序列合并，补全缺失的天数
complete_data <- complete_days %>%
  left_join(clean_data, by = c("subject_id", "day_number"))

# 准备用于多重插补的数据
# 创建宽格式数据，每一天的变量作为单独列
wide_data <- complete_data %>%
  pivot_wider(
    id_cols = subject_id,
    names_from = day_number,
    values_from = c(nap_duration, sleep_duration),
    names_glue = "{.value}_day{day_number}"
  )

# 合并个体特征变量
individual_data <- complete_data %>%
  group_by(subject_id) %>%
  summarise(
    age = first(na.omit(age)),
    gender = first(na.omit(gender)),
    education = first(na.omit(education)),
    nation = first(na.omit(nation)),
    income = first(na.omit(income)),
    pre_dep = first(na.omit(pre_dep)),
    pre_anx = first(na.omit(pre_anx)),
    pre_pss = first(na.omit(pre_pss)),
    pre_tls = first(na.omit(pre_tls)),
    pre_psqi = first(na.omit(pre_psqi)),
    pre_sleep_duration = first(na.omit(pre_sleep_duration)),
    pre_sleep_timing = first(na.omit(pre_sleep_timing)),
    pre_sleep_quality = first(na.omit(pre_sleep_quality)),
    pre_sleep_efficiency = first(na.omit(pre_sleep_efficiency)),
    pre_sleep_hypnotics = first(na.omit(pre_sleep_hypnotics)),
    pre_sleep_disorder = first(na.omit(pre_sleep_disorder)),
    pre_daytime_dysfunction = first(na.omit(pre_daytime_dysfunction)),
    post_dep = first(na.omit(post_dep)),
    post_anx = first(na.omit(post_anx)),
    post_pss = first(na.omit(post_pss)),
    post_tls = first(na.omit(post_tls)),
    pre_sdv = first(na.omit(pre_sdv))
  ) %>%
  ungroup()

# 合并所有数据用于多重插补
data_for_imputation <- individual_data %>%
  left_join(wide_data, by = "subject_id")

# 检查缺失情况
cat("插补前缺失值情况：\n")
print(colSums(is.na(data_for_imputation)))

# 进行多重插补
imp <- mice(data_for_imputation, 
            m = 5,           # 生成5个插补数据集
            maxit = 10,      # 最大迭代次数
            seed = 123,
            print = FALSE)

# 查看插补模型
summary(imp)

# 使用第一个插补数据集
imputed_data_wide <- complete(imp, 1)

# 将宽格式数据转换回长格式
imputed_data_long <- imputed_data_wide %>%
  pivot_longer(
    cols = contains("_day"),
    names_to = c(".value", "day_number"),
    names_pattern = "(.*)_day(.*)"
  ) %>%
  mutate(day_number = as.numeric(day_number)) %>%
  arrange(subject_id, day_number)

# 计算变异性指标
variability_metrics <- imputed_data_long %>%
  group_by(subject_id) %>%
  summarise(
    # 午睡时长变异性
    mean_nap_duration = mean(nap_duration, na.rm = TRUE),
    sd_nap_duration = sd(nap_duration, na.rm = TRUE),
    cv_nap_duration = sd_nap_duration / mean_nap_duration,
    range_nap_duration = max(nap_duration, na.rm = TRUE) - min(nap_duration, na.rm = TRUE),
    iqr_nap_duration = IQR(nap_duration, na.rm = TRUE),
    mssd = mean(diff(nap_duration)^2, na.rm = TRUE),
    rmssd = sqrt(mssd),
    
    # 睡眠时长变异性
    mean_sleep_duration = mean(sleep_duration, na.rm = TRUE),
    sd_sleep_duration = sd(sleep_duration, na.rm = TRUE),
    
    # 其他变异性指标
    cv_sleep_duration = sd_sleep_duration / mean_sleep_duration,
    range_sleep_duration = max(sleep_duration, na.rm = TRUE) - min(sleep_duration, na.rm = TRUE),
    iqr_sleep_duration = IQR(sleep_duration, na.rm = TRUE)
  ) %>%
  ungroup()

# 创建最终分析数据集 - 保留所有需要的列
data_new <- imputed_data_wide %>%
  # 保留所有个体特征变量
  select(
    subject_id, age, gender, education, nation, income,
    pre_dep, pre_anx, pre_pss, pre_tls, pre_psqi, pre_sdv,
    pre_sleep_duration, pre_sleep_timing, pre_sleep_quality, 
    pre_sleep_efficiency, pre_sleep_hypnotics, pre_sleep_disorder, 
    pre_daytime_dysfunction,
    post_dep, post_anx, post_pss, post_tls
  ) %>%
  # 合并变异性指标
  left_join(variability_metrics, by = "subject_id")

# 检查最终数据的缺失情况和列名
cat("\n最终数据集的缺失值情况：\n")
print(colSums(is.na(data_new)))

cat("\n最终数据集的列名：\n")
print(names(data_new))

cat("\n最终数据集的维度：\n")
print(dim(data_new))

# 显示数据结构
cat("\n最终数据集的结构：\n")
str(data_new)

# 回归分析
cat("\n=== 回归分析结果 ===\n")

lm1 <- lm(post_dep ~ mean_nap_duration + sd_nap_duration + mean_sleep_duration + sd_sleep_duration, data_new)
summary(lm1)
cat("模型1 - BIC:", BIC(lm1), "AIC:", AIC(lm1), "\n\n")

lm2 <- lm(post_dep ~ age + gender + education + income + nation + mean_nap_duration + sd_nap_duration + mean_sleep_duration + sd_sleep_duration, data_new)
summary(lm2)
cat("模型2 - BIC:", BIC(lm2), "AIC:", AIC(lm2), "\n\n")

lm3 <- lm(post_dep ~ age + gender + education + income + nation + pre_dep + mean_nap_duration + sd_nap_duration + mean_sleep_duration + sd_sleep_duration, data_new)
summary(lm3)
cat("模型3 - BIC:", BIC(lm3), "AIC:", AIC(lm3), "\n\n")

lm4 <- lm(post_dep ~ age + gender + education + income + nation + pre_dep + pre_anx + pre_pss + pre_tls + mean_nap_duration + sd_nap_duration + mean_sleep_duration + sd_sleep_duration, data_new)
summary(lm4)
cat("模型4 - BIC:", BIC(lm4), "AIC:", AIC(lm4), "\n\n")

lm5 <- lm(post_dep ~ age + gender + education + income + nation + pre_dep + pre_anx + pre_pss + pre_tls +
            pre_sleep_duration + pre_sleep_timing + pre_sleep_quality + pre_sleep_efficiency + 
            pre_sleep_hypnotics + pre_sleep_disorder + pre_daytime_dysfunction +
            mean_nap_duration + sd_nap_duration + mean_sleep_duration + sd_sleep_duration, data_new)
summary(lm5)
cat("模型5 - BIC:", BIC(lm5), "AIC:", AIC(lm5), "\n")

######3.5.2 Repeat the content of section 3.3#####


summary(data_new$sd_nap_duration)
quantile(data_new$sd_nap_duration, probs = 0.27,na.rm=TRUE)
quantile(data_new$sd_nap_duration, probs = 0.73,na.rm=TRUE)

high_sdv <- data_new[data_new$sd_nap_duration>40.28421,]
low_sdv <- data_new[data_new$sd_nap_duration<18.3319,]
high_sdv <- na.omit(high_sdv)
low_sdv<-na.omit(low_sdv)

##### 进行倾向得分匹配
high_sdv$group <- "high"
low_sdv$group <- "low"
# 合并数据
combined_data <- rbind(high_sdv, low_sdv)
combined_data$group <- as.factor(combined_data$group)
library(MatchIt)
matchit_result_advanced <- matchit(group ~ age + gender + education + nation + income +
                                     pre_dep + pre_anx + pre_pss + pre_tls + pre_sleep_duration + 
                                     pre_sleep_timing + pre_sleep_quality + pre_sleep_efficiency + 
                                     pre_sleep_hypnotics + pre_sleep_disorder + pre_daytime_dysfunction + 
                                     pre_daytime_dysfunction, 
                                   data = combined_data,
                                   method = "nearest",
                                   distance = "glm",
                                   ratio = 1,
                                   caliper = 0.7)

# 获取高级匹配后的数据
matched_data_advanced <- match.data(matchit_result_advanced)

# 重新分割数据
high_sdv_matched_adv <- matched_data_advanced[matched_data_advanced$group == "high", ]
low_sdv_matched_adv <- matched_data_advanced[matched_data_advanced$group == "low", ]

# 创建结果表格的函数
create_comparison_table <- function(high_data, low_data) {
  # 初始化结果数据框
  results <- data.frame(
    Variables = character(),
    Low_Group = character(),
    High_Group = character(),
    Statistic = numeric(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # 添加行的函数
  add_row_to_results <- function(var_name, low_val, high_val, stat, p_val) {
    results <<- rbind(results, data.frame(
      Variables = var_name,
      Low_Group = low_val,
      High_Group = high_val,
      Statistic = round(stat, 3),
      P_Value = round(p_val, 3),
      stringsAsFactors = FALSE
    ))
  }
  
  # 年龄
  t_age <- t.test(high_data$age, low_data$age)
  add_row_to_results("Age", 
                     paste0(round(mean(low_data$age), 2), "±", round(sd(low_data$age), 2)),
                     paste0(round(mean(high_data$age), 2), "±", round(sd(high_data$age), 2)),
                     t_age$statistic, t_age$p.value)
  
  # 性别（卡方检验）
  gender_table <- table(
    Group = c(rep("Low", nrow(low_data)), rep("High", nrow(high_data))),
    Gender = c(low_data$gender, high_data$gender)
  )
  chi_gender <- chisq.test(gender_table)
  gender_low <- paste0(sum(low_data$gender == 0), ":", sum(low_data$gender == 1))
  gender_high <- paste0(sum(high_data$gender == 0), ":", sum(high_data$gender == 1))
  add_row_to_results("Gender", gender_low, gender_high, 
                     chi_gender$statistic, chi_gender$p.value)
  
  # 教育
  t_edu <- t.test(high_data$education, low_data$education)
  add_row_to_results("Education", 
                     paste0(round(mean(low_data$education), 2), "±", round(sd(low_data$education), 2)),
                     paste0(round(mean(high_data$education), 2), "±", round(sd(high_data$education), 2)),
                     t_edu$statistic, t_edu$p.value)
  
  # 收入
  t_income <- t.test(high_data$income, low_data$income)
  add_row_to_results("Income", 
                     paste0(round(mean(low_data$income), 2), "±", round(sd(low_data$income), 2)),
                     paste0(round(mean(high_data$income), 2), "±", round(sd(high_data$income), 2)),
                     t_income$statistic, t_income$p.value)
  
  # 民族
  nation_table <- table(
    Group = c(rep("Low", nrow(low_data)), rep("High", nrow(high_data))),
    Nation = c(low_data$nation, high_data$nation)
  )
  chi_nation <- chisq.test(nation_table)
  nation_low <- paste0(sum(low_data$nation == 0), ":", sum(low_data$nation == 1))
  nation_high <- paste0(sum(high_data$nation == 0), ":", sum(high_data$nation == 1))
  add_row_to_results("Nation", nation_low, nation_high, 
                     chi_nation$statistic, chi_nation$p.value)
  
  # 基线抑郁
  t_pre_dep <- t.test(high_data$pre_dep, low_data$pre_dep)
  add_row_to_results("Baseline depression", 
                     paste0(round(mean(low_data$pre_dep), 2), "±", round(sd(low_data$pre_dep), 2)),
                     paste0(round(mean(high_data$pre_dep), 2), "±", round(sd(high_data$pre_dep), 2)),
                     t_pre_dep$statistic, t_pre_dep$p.value)
  
  # 基线焦虑
  t_pre_anx <- t.test(high_data$pre_anx, low_data$pre_anx)
  add_row_to_results("Baseline anxiety", 
                     paste0(round(mean(low_data$pre_anx), 2), "±", round(sd(low_data$pre_anx), 2)),
                     paste0(round(mean(high_data$pre_anx), 2), "±", round(sd(high_data$pre_anx), 2)),
                     t_pre_anx$statistic, t_pre_anx$p.value)
  
  # 基线压力
  t_pre_pss <- t.test(high_data$pre_pss, low_data$pre_pss)
  add_row_to_results("Baseline pressure", 
                     paste0(round(mean(low_data$pre_pss), 2), "±", round(sd(low_data$pre_pss), 2)),
                     paste0(round(mean(high_data$pre_pss), 2), "±", round(sd(high_data$pre_pss), 2)),
                     t_pre_pss$statistic, t_pre_pss$p.value)
  
  # 基线孤独感
  t_pre_tls <- t.test(high_data$pre_tls, low_data$pre_tls)
  add_row_to_results("Baseline loneliness", 
                     paste0(round(mean(low_data$pre_tls), 2), "±", round(sd(low_data$pre_tls), 2)),
                     paste0(round(mean(high_data$pre_tls), 2), "±", round(sd(high_data$pre_tls), 2)),
                     t_pre_tls$statistic, t_pre_tls$p.value)
  
  # 睡眠相关变量
  sleep_vars <- c(
    "pre_sleep_duration", "pre_sleep_timing", "pre_sleep_quality", 
    "pre_sleep_efficiency", "pre_sleep_hypnotics", "pre_sleep_disorder", 
    "pre_daytime_dysfunction"
  )
  
  sleep_names <- c(
    "Baseline sleep duration", "Baseline sleep timing", "Baseline sleep quality",
    "Baseline sleep efficiency", "Baseline sleep hypnotics", "Baseline sleep disorder",
    "Baseline daytime dysfunction"
  )
  
  for (i in 1:length(sleep_vars)) {
    var <- sleep_vars[i]
    name <- sleep_names[i]
    
    t_test <- t.test(high_data[[var]], low_data[[var]])
    add_row_to_results(name, 
                       paste0(round(mean(low_data[[var]]), 2), "±", round(sd(low_data[[var]]), 2)),
                       paste0(round(mean(high_data[[var]]), 2), "±", round(sd(high_data[[var]]), 2)),
                       t_test$statistic, t_test$p.value)
  }
  
  # 后测变量
  post_vars <- c("post_dep", "post_anx", "post_pss", "post_tls")
  post_names <- c("Post-test depression", "Post-test anxiety", 
                  "Post-test pressure", "Post-test loneliness")
  
  for (i in 1:length(post_vars)) {
    var <- post_vars[i]
    name <- post_names[i]
    
    t_test <- t.test(high_data[[var]], low_data[[var]])
    add_row_to_results(name, 
                       paste0(round(mean(low_data[[var]]), 2), "±", round(sd(low_data[[var]]), 2)),
                       paste0(round(mean(high_data[[var]]), 2), "±", round(sd(high_data[[var]]), 2)),
                       t_test$statistic, t_test$p.value)
  }
  
  return(results)
}

# 生成表格
comparison_table <- create_comparison_table(high_sdv_matched_adv, low_sdv_matched_adv)

# 重命名列名以匹配要求的格式
names(comparison_table) <- c("Variables", "Low Nap duration variability", 
                             "High Nap duration variability", "T/χ²", "P value")

# 打印表格
print(comparison_table)

library(effectsize)
cohen_d_post_dep <- cohens_d(high_sdv_matched_adv$post_dep, 
                             low_sdv_matched_adv$post_dep,
                             pooled_sd = TRUE)
print(cohen_d_post_dep)
######3.5.3 Repeat the content of section 3.4#####

library(dplyr)
# 提取所有需要的subject_id
high_ids <- high_sdv_matched_adv$subject_id
low_ids <- low_sdv_matched_adv$subject_id

# 从clean_data中筛选并添加组别
matched_clean_data <- clean_data %>%
  filter(subject_id %in% c(high_ids, low_ids)) %>%
  mutate(group = case_when(
    subject_id %in% high_ids ~ "high",
    subject_id %in% low_ids ~ "low"
  ))
matched_clean_data$holiday<-matched_clean_data$day_number
matched_clean_data <-within(matched_clean_data,{
  holiday[matched_clean_data$holiday<=7]<-0
  holiday[matched_clean_data$holiday>=8]<-1
})

model1 <- lmer(daily_dep~ age+gender+education+nation+income+holiday+
                 day_number * group + (1| subject_id), data = matched_clean_data)
summary(model1)

model2 <- lmer(daily_dep~ age+gender+education+nation+income+holiday+pre_dep+
                 day_number * group + (1| subject_id), data = matched_clean_data)
summary(model2)

model3 <- lmer(daily_dep~ age+gender+education+nation+income+holiday+pre_dep+nap_duration+sleep_duration+
                 day_number * group + (1+ day_number | subject_id), data = matched_clean_data)
summary(model3)

model4 <- lmer(daily_dep~ age+gender+education+nation+income+holiday+pre_dep+nap_duration+sleep_duration+
                 day_number * group + (1+ day_number +nap_duration+sleep_duration | subject_id), data = matched_clean_data)
summary(model4)

model5 <- lmer(daily_dep~ age+gender+education+nation+income+pre_dep+pre_anx+pre_pss+pre_tls+nap_duration+sleep_duration+holiday+
                 day_number * group + (1+ day_number +nap_duration+sleep_duration | subject_id), data = matched_clean_data)
summary(model5)

anova(model1,model2,model3,model4,model5)
#简单效应检验
emm_slopes <- emtrends(model4, ~ group, var = "day_number")
summary(emm_slopes)  # 各组的斜率估计
pairs(emm_slopes)    # 组间斜率差异检验

##figure 3
library(ggplot2)
nature_colors <- c("high" = "#1F77B4", "low" = "#FF7F0E")  # Blue and orange
ggplot(matched_clean_data, aes(x = day_number, y = daily_dep, group = subject_id, color = group)) +
  geom_line(alpha = 0.4, size = 0.6) +  # Adjust line thickness and transparency
  stat_smooth(aes(group = group, color = group), 
              method = "lm", se = TRUE, size = 1.2) +
  labs(title = "",
       x = "Day Number", y = "Depression Score",
       color = "Nap Variability") +
  theme_minimal() +
  scale_color_manual(values = nature_colors) +
  theme(
    text = element_text(family = "sans", size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )





