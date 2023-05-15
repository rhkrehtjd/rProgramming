library(readr)
data = read_file("pi.txt")

# 첫번째 빈칸
data = substr(data,3,10^7 +2)
substr(data, start = nchar(data)-5, stop = nchar(data))

# 2,3,4,5 빈칸
digits <- c(as.character(0:9), letters[1:6])
result <- c()
used_digits <- rep(FALSE, length(digits))

for (i in 1:nchar(data)) {
  digit <- substr(data, i, i)
  if (digit %in% digits && !used_digits[digit == digits]) {
    used_digits[digit == digits] <- TRUE
  }
  if (identical(used_digits, rep(TRUE, length(digits)))) {
    result <- c(result, i)
    used_digits <- rep(FALSE, length(digits))
  }
}

result[1] # 두번째 빈칸
result[2] # 세번째 빈칸
result[3] # 네번째 빈칸
length(result) # 다섯번째 빈칸

diff = c(81,diff(result))
min(diff) # 여섯번째 빈칸
sum(diff==18) # 일곱번째 빈칸
max(diff) # 여덟번째 빈칸
sum(diff==211) # 아홉번째 빈칸


a=0
for(i in 1:16){a = a + 1/i}
round(16*a,2) # 열번째 빈칸



plotting = function(len){
  data = substr(data, start = 3, stop = (10^len)+2)
  
  digits <- c(as.character(0:9), letters[1:6])
  result <- c()
  used_digits <- rep(FALSE, length(digits))
  
  for (i in 1:nchar(data)) {
    digit <- substr(data, i, i)
    if (digit %in% digits && !used_digits[digit == digits]) {
      used_digits[digit == digits] <- TRUE
    }
    if (identical(used_digits, rep(TRUE, length(digits)))) {
      result <- c(result, i)
      used_digits <- rep(FALSE, length(digits))
    }
  }
  
  diff = c(81,diff(result))
  
  results <-  vector()
  for (n in 16:263) {
    result <- 0
    for (j in 0:15) {
      result <- result + (-1)^j * choose(15, j) * (15-j)^(n-1)
    }
    result <- result * 16^(1-n)
    results[n-15] <- result
  }
  
  prop = prop.table(table(diff))
  
  plot(prop,xlim=c(0,as.numeric(names(prop)[length(prop)])),
       ylim = c(0,max(prop)+0.02),
       xaxt='n',yaxt='n',bty='n',
       xlab = "length",ylab="Density")
  lines(c(rep(0,15),results),col="red",lwd=2)
  if(len==4){axis(1,at =seq(0,as.numeric(names(prop)[length(prop)]),20))
    axis(2,at = seq(0,0.04,0.01))
    text(as.numeric(names(prop)[length(prop)])/2, max(prop)+0.01,
         labels= paste( 10^len, "cut\nmean = ", round(mean(diff),2), sep = ""),cex=1.2)}
  else if(len==5){axis(1,at =seq(0,as.numeric(names(prop)[length(prop)]),50))
    axis(2,at = seq(0,0.03,0.01))
    text(as.numeric(names(prop)[length(prop)])/2, max(prop)+0.01,
         labels= paste( 10^len, "cut\nmean = ", round(mean(diff),2), sep = ""),cex=1.2)}
  else if(len==6){axis(1,at =seq(0,as.numeric(names(prop)[length(prop)]),50))
    axis(2,at = seq(0,0.03,0.01))
    text(as.numeric(names(prop)[length(prop)])/2, max(prop)+0.01,
         labels= paste( 10^len, "cut\nmean = ", round(mean(diff),2), sep = ""),cex=1.2)}
  else if(len==7){axis(1,at =seq(0,as.numeric(names(prop)[length(prop)]),50))
    axis(2,at = seq(0,0.03,0.01))
    text(as.numeric(names(prop)[length(prop)])/2, max(prop)+0.01,
         labels= paste( 10^len, "cut\nmean = ", round(mean(diff),2), sep = ""),cex=1.2)}
  
}


par(mfrow=c(2,2),mar=c(5,5,5,2))
plotting(4)
plotting(5)
plotting(6)
plotting(7)





