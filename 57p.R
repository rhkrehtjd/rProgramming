library(readxl)
df = read_excel("lotto.xlsx",skip=3, col_names=FALSE)
colnames(df)[1:13] = c("year","num","date","pop1","money1","pop2","money2",
                       "pop3","money3","pop4","money4","pop5","money5")
df[, c("money1","money2","money3","money4","money5")] <- 
  apply(df[,c("money1","money2","money3","money4","money5")], 2, function(x) as.numeric(gsub("[^[:digit:]]", "", x)))

# 1)
sum_1059=df[df$num == 1059,]
((sum_1059$pop4 * sum_1059$money4) + (sum_1059$pop5 * sum_1059$money5) + (sum_1059$pop3 * sum_1059$money3 * 8))*2

sum(df$pop4 * df$money4 + df$pop5 * df$money5 + df$pop3 * df$money3 * 8) *2

# 2)
df_1=df[df$num == 1,]
((df_1$pop4 * df_1$money4) + (df_1$pop5 * df_1$money5) + (df_1$pop3 * df_1$money3 * 8))*2 /2000

((sum_1059$pop4 * sum_1059$money4) + (sum_1059$pop5 * sum_1059$money5) + (sum_1059$pop3 * sum_1059$money3 * 8))*2/1000

# 3)
sum(df$pop1)
sum(df$pop2)
sum(df$pop3)

# 4)
df = df[,c(1:13)]
df$total <- 2*(df$pop4*df$money4 + df$pop5*df$money5 + df$pop3*df$money3*8)
df$game <- ifelse(df$num <= 87, df$total/2000, df$total/1000)

df
par(mfrow=c(1,2))
hist(df$game/10000, xlim=c(min(df$game),max(df$game)), breaks = 100)
hist(df$total / 100000000, xlim=c(min(df$total),max(df$total)), breaks = 100)


# (5)
par(mfrow=c(2,2))

hist(df$pop1,xlim=c(min(df$pop1),max(df$pop1)),breaks=50)
hist(df$pop1 / df$game,  xlim=c(min(df$pop1 / df$game),max(df$pop1 / df$game)),breaks = 100)
abline(v=1/8145060 ,col = "red")

price <- vector(mode = "numeric", length = nrow(df))
price[df$num <= 87] <- 2000
price[df$num > 87] <- 1000

df <- cbind(df, price)

df$expectation_1st = df$total / (df$price * 8145060)


sd <- sd(df$expectation_1st)
t = qt(0.025, 1058)
df$upper_bound <- df$expectation_1st  - t * sd
df$lower_bound <- df$expectation_1st  + t * sd
df$col <- "red"
df$col[which(df$pop1 >= df$lower_bound & df$pop1 <= df$upper_bound)] <- "blue"


plot(df$pop1~df$expectation_1st,cex = 0.5,pch=19,bg="black")
abline(a=0,b=1, col="red")
plot(df$pop1~df$num,col = df$col,cex = 0.5,pch=19)
lines(df$num,df$upper_bound, col="blue")
lines(df$num,df$lower_bound, col="blue")
lines(df$expectation_1st~df$num,col="yellow")
