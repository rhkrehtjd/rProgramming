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


digits <- as.character(0:9)
result <- c()
count <- 1

while (count < nchar(a)) {
  
  used_digits <- c()
  
  for (i in count:nchar(a)) {
    digit <- substr(a, i, i)
    if (digit %in% digits && !(digit %in% used_digits)) {
      used_digits <- c(used_digits, digit)
    }
    if (length(used_digits) == length(digits)) {
      break
    }
    count <- count + 1
  }
  
  result <- c(result, count)
  count <- count + 1
}

print(result)
length(result)
diff = c(18,diff(result))
length(diff)
hist(diff)
median(diff)
mean(diff)
length(which(diff==10))
result[which(diff==10)][[1]]-9
substr(a, result[which(diff==10)][[1]]-9, result[which(diff==10)][[1]])






total_result <- 0

for (n in 10:59) {
  result <- 0
  for (j in 0:9) {
    result <- result + (-1)^j * choose(9, j) * (9-j)^(n-1)
  }
  result <- result * 10^(1-n)
  total_result <- total_result + result
}

print(total_result)


results <-  vector("numeric", length = 50)

for (n in 10:59) {
  result <- 0
  for (j in 0:9) {
    result <- result + (-1)^j * choose(9, j) * (9-j)^(n-1)
  }
  result <- result * 10^(1-n)
  results[n-9] <- result
}

print(results)
sum(results)
freq = results / sum(results)
barplot(freq)

barplot(prop.table(table(diff[diff>=10 & diff <=59])))


mat = rbind(prop.table(table(diff[diff>=10 & diff <=59])),freq)
barplot(mat,beside=T,col=c("red","blue"),
        main="n = 10:59")

which(abs(mat[1,] - mat[2,])==max(abs(mat[1,] - mat[2,])))
abs(mat[1,] - mat[2,])[17]
mean(mat[2,]*(10:59))
sum(mat[2,])
