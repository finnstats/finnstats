library(xlsx)
library(rstatix)
library(reshape)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(plyr)
library(datarium)

data<-data("selfesteem", package = "datarium")
data<-selfesteem


data<-read.xlsx("D:/RStudio/Website/data.xlsx",sheetName="Sheet1")
data <- data %>%
  gather(key = "time", value = "score", T0, T1, T2) %>%
  convert_as_factor(id, Treatment,time)
head(data, 3)


summary<-data %>%
  group_by(Treatment,time) %>%
  get_summary_stats(score, type = "mean_sd")
data.frame(summary)

bxp <- ggboxplot(data, x = "time", y = "score", add = "point")
bxp

ggqqplot(data, "score", facet.by = "time")

ggqqplot(data, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ Treatment, labeller = "label_both")

outlier<-data %>%
  group_by(Treatment,time) %>%
  identify_outliers(score)
data.frame(outlier)

normality<-data %>%
  group_by(Treatment,time) %>%
  shapiro_test(score)
data.frame(normality)

res<-anova_test(data=data,dv=score,wid=id,within=time)
get_anova_table(res)

pair<-data %>%
  pairwise_t_test(
    value~time,paired=TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pair)
########################################################################
res.aov <- anova_test(
  data = data, dv = score, wid = id,
  within = c(Treatment, time)
)
get_anova_table(res.aov)


# Effect of treatment at each time point
one.way <- data %>%
  group_by(time) %>%
  anova_test(dv = score, wid = id, within = Treatment) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way
data.frame(one.way)


# Pairwise comparisons between treatment groups at each time point
pwc <- data %>%
  group_by(time) %>%
  pairwise_t_test(
    score ~ Treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc)

# Effect of time at each level of treatment
one.way2 <- data %>%
  group_by(Treatment) %>%
  anova_test(dv = score, wid = id, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
data.frame(one.way2)
# Pairwise comparisons between time points
pwc2 <- data %>%
  group_by(Treatment) %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc2)

# comparisons for treatment variable
data %>%
  pairwise_t_test(
    score ~ Treatment, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
# comparisons for time variable
data %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )


# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )




