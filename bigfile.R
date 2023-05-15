
# 첫번째 빈칸
substr(data, start = nchar(data)-5, stop = nchar(data))

diff = c(81,diff(result))
min(diff)
sum(diff==18)
max(diff)
sum(diff==211)



library(readr)
data = read_file("pi.txt")

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
plot(prop,xlim=c(1,length(prop)),xaxt='n',yaxt='n',bty='n')
lines(c(rep(0,15),results),col="red",lwd=2)}




par(mfrow=c(2,2),mar=c(1,1,1,1))
plotting(4)
plotting(5)
plotting(6)
plotting(7)

a=0
for(i in 1:16){a = a + 1/i}
16*a
       



