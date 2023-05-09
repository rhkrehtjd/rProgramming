library(ggplot2)

x <- mpg$displ; y <- mpg$hwy
df <- data.frame(x, y)

fit <- lm(y ~ x, data = df) 
cooksd <- cooks.distance(fit) # 쿡 거리 이용
outliers <- as.numeric(names(cooksd)[(cooksd > 4 * mean(cooksd))])
df_outliers <- df[outliers, ] # 이상점만 
df_outliers_5 <- df_outliers[df_outliers$x > 5, ] # 배기량이 높은데도 연비가 좋은 경우만

ggplot() + 
  geom_point(data = df, aes(x = x, y = y), color = "black") +
  geom_point(data = df_outliers_5, aes(x = x, y = y), color = "red")



library(gridExtra)

p1 = ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy),cex=3.5) +
  geom_smooth(mapping = aes(x = displ, y = hwy), se = FALSE, lwd = 2)

p2 = ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy),cex=3.5) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group= drv), se = FALSE, lwd = 2)

p3 = ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = drv),cex=3.5) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color= drv), se = FALSE, lwd = 2)

p4 = ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = drv),cex=3.5) +
  geom_smooth(mapping = aes(x = displ, y = hwy), se = FALSE, lwd = 2)

p5 = ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = drv),cex=3.5) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group=drv, linetype = drv), se = FALSE, lwd = 2)

p6 = ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), size = 7, shape = 21, fill = "white", color = "white")+
  geom_point(mapping = aes(x = displ, y = hwy, color = drv),size = 3)
                                     
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)




ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~class, nrow = 2)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))


a=readLines("https://apod.nasa.gov/htmltest/gifcity/sqrt2.1mil")
a=a[-c(1:30,(length(a)-5):length(a))]
a=paste(a, collapse = "")
a=substr(a, 5, nchar(a))
nchar(a)
