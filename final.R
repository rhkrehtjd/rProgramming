
a=readLines("https://apod.nasa.gov/htmltest/gifcity/sqrt2.1mil")
length(a) # 첫번째 빈칸
a=a[-c(1:30,(length(a)-5):length(a))] # 불필요 라인 제거
a=paste(a, collapse = "")
a=substr(a, 5, nchar(a)) # 맨 앞 문자 4개 제외
substr(a, 1, 3) # 다섯번째 빈칸
substr(a, nchar(a)-2, nchar(a)) # 여섯번째 빈칸
nchar(a) # 일곱번째 빈칸


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
  count <- count + 1}

print(result)
result = result[-length(result)]


diff = c(18,diff(result))
diff[1] # 여덟번째 빈칸
result[2] #아홉번째 빈칸
diff[2] # 열번째 빈칸

median(diff) # 11 빈칸
mean(diff) # 12 빈칸


hist(diff,breaks=100)


length(which(diff==10)) # 13 빈칸
result[which(diff==10)][[1]]-9 # 14 빈칸
substr(a, result[which(diff==10)][[1]]-9, result[which(diff==10)][[1]])# 15 빈칸


results <-  vector("numeric")
for (n in 10:59) {
  result <- 0
  for (j in 0:9) {
    result <- result + (-1)^j * choose(9, j) * (9-j)^(n-1)
  }
  result <- result * 10^(1-n)
  results[n-9] <- result
}
sum(results) # 18번째 빈칸


result <- 0
for (j in 0:9) {
  result <- result + (-1)^j * choose(9, j) * (9-j)^(10-1)
}
result* 10^(1-10) #17번째 빈칸


mat = rbind(prop.table(table(diff[diff>=10 & diff <=59])),freq[1:50])
barplot(mat,beside=T,col=c("red","blue"),
        main="n = 10:59")

# 18번째 빈칸
which(abs(mat[1,] - mat[2,])==max(abs(mat[1,] - mat[2,])))

# 19번째 빈칸
result <- 0
for (j in 0:9) {
  result <- result + (-1)^j * choose(9, j) * (9-j)^(26-1)
}
result* 10^(1-26)-prop.table(table(diff))[18]



a=0
for(i in 1:10){a = a + 1/i}
round(10*a,2) # 마지막 빈칸

