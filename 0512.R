good_report <- c(20.8, 18.7, 19.9, 20.6, 22.0, 23.4, 22.8, 24.9, 22.2, 20.3, 24.9, 22.3, 27.0, 20.4, 22.2, 24.0, 21.2, 22.1, 22.0, 22.7)
bad_report <- c(18.0, 19.0, 19.2, 18.8, 18.4, 19.0, 18.5, 16.1, 16.8, 14.0, 17.0, 13.6, 17.5, 19.9, 20.2, 18.8, 18.0, 23.2, 18.2, 19.4)
no_report <- c(19.9, 16.0, 15.0, 20.1, 19.3, 19.2, 18.0, 19.2, 21.2, 18.8, 18.5, 19.3, 19.3, 19.4, 10.8, 19.1, 19.7, 19.8, 21.3, 20.6)

library(dplyr)
library(ggpubr)

df <- data.frame(value = c(good_report, bad_report, no_report),
                 group = rep(c("good_report", "bad_report", "no_report"), each = 20))

# 일원분산분석(one-way ANOVA) 수행
anova_result <- aov(value ~ group, data = df)

# 분산분석 결과 요약
summary(anova_result)

# 분산분석 결과 시각화
ggboxplot(df, x = "group", y = "value", 
          color = "group", palette = "rainbow",
          ylab = "Value", xlab = "Group", 
          title = "Comparison of Means by Group") +
  stat_compare_means(method = "anova")

