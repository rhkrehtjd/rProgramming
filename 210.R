my_unique <- function(x) {
  result <- c() # 중복 제거된 결과를 저장할 벡터
  for (i in x) {
    if (!(i %in% result)) { # 중복되는 값이 없으면 결과 벡터에 추가
      result <- c(result, i)
    }
  }
  return(result)
}

set.seed(1)
rn <- sample(1:10, 10, replace=TRUE)
my_unique(rn)

lego_url <- "https://raw.githubusercontent.com/kshan226/data/main/lego.csv"
df = read.csv(lego_url, header = TRUE)

number <- substr(df$item_url, nchar(df$item_url)-5, nchar(df$item_url))
number <- gsub("-", "", number)
number <- gsub("/", "", number)
number <- gsub("_", "", number)
number = as.numeric(number)
sum(number)

url <- "https://raw.githubusercontent.com/kshan226/lifespan/main/queen.csv"
df = read.csv(url, header = TRUE)

year_of_birth<- substr(df[,5], 1, 4)
death  = substr(df[,5], nchar(df[1,5])-4, nchar(df[1,5])-1)
year_of_birth = as.numeric(year_of_birth);death = as.numeric(death)
lifespan = death-year_of_birth +1

plot(lifespan~ year_of_birth,main="lifespan of Queen")
abline(lm(lifespan~year_of_birth),col="red",lwd = 2.4)
mean(lifespan)

url <- "https://raw.githubusercontent.com/kshan226/data/main/lotto1064.csv"
df = read.csv(url, header = TRUE)
lotto = df[-c(1,2),c(2,4:13)]
colnames(lotto) = c("num","pop1","money1","pop2","money2",
                    "pop3","money3","pop4","money4","pop5","money5") 
lotto[, c("money1","money2","money3","money4","money5")] <- 
  apply(lotto[,c("money1","money2","money3","money4","money5")], 2, function(x) as.numeric(gsub("[^[:digit:]]", "", x)))
lotto$pop1_cumsum =  cumsum(lotto$pop1)

lotto[,c("pop1","pop2","pop3","pop4","pop5")]=lapply(lotto[,c("pop1","pop2","pop3","pop4","pop5")], 
                                                     function(x) as.numeric(gsub(",", "", x)))
lotto$num = as.numeric(lotto$num)
lotto$total <- 2*(lotto$pop4*lotto$money4 + 
                    lotto$pop5*lotto$money5 + lotto$pop3*lotto$money3*8) 
lotto$game <- ifelse(lotto$num <= 87, lotto$total/2000, lotto$total/1000)
lotto$game_cumsum =  cumsum(lotto$game)

plot(lotto$pop1_cumsum/lotto$game_cumsum~ rev(lotto$num),cex=0.2,
     xlab = "ROUND", ylab = "Relative Frequency",
     main = "Cumulative relative frequency of the First rank")
abline(h = 1/8145060,col="red", lwd = 2.4)
arrows(300,1.3e-07,200,1/8145060)
text(400,1.33e-07,expression(paste(frac(1, 8145060))*" = "*"1.228e-07"),col="blue")


freq = lotto$pop1_cumsum/lotto$game_cumsum
freq[1064]
