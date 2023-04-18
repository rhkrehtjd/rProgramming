par(mfrow=c(2,2))
par(bg = "white", col.axis = "white", col.lab = "white", col.main = "white", 
    col.sub = "white", xaxt = "s", yaxt = "s", bty = "n")

x <- seq(-4, 4, length=100)
y <- dnorm(x, mean=0, sd=1)
set.seed(1)  # 난수 발생 초기치 설정
sample_data <- round(rnorm(10, mean=0.5, sd=1), 1)  # N(0.5, 1)에서 10개 추출하여 소숫점 둘째 자리에서 반올림


plot(x, y, type="l", lwd=2, col="#FFFFCC", xaxt='n', yaxt='n', xlab="", ylab="")
polygon(c(x, rev(x)), c(y, rep(0, length(y))), col="#FFFFCC", border=NA)
lines(x, y, col="black", lwd=1)
segments(0, 0, 0, max(y), col="black", lwd=2)
abline(h=0, lty=1, col="black")
stripchart(sample_data, add=TRUE,pch=16, col="Blue",method="stack",at=0.0004)
stripchart(sum(sample_data)/10, add=TRUE,pch=24,bg="red", col="red",method="stack",at=0.0004)

result = t.test(sample_data, conf.level = 0.95)
conf_interval = result$conf.int

axis(side = 1, at = seq(-4, 4, by=2), labels =seq(-4, 4, by=2),cex.axis=1.2,lwd=1.4,line=3, col.axis="black")

############################################################################

sample_data <- round(rnorm(10, mean=0.5, sd=1), 1) 

plot(x, y, type="l", lwd=2, col="#FFFFCC", xaxt='n', yaxt='n', xlab="", ylab="")
polygon(c(x, rev(x)), c(y, rep(0, length(y))), col="#FFFFCC", border=NA)
lines(x, y, col="black", lwd=1)
segments(0, 0, 0, max(y), col="black", lwd=2)
abline(h=0, lty=1, col="black")
stripchart(sample_data, add=TRUE,pch=16, col="Blue",method="stack",at=0.0004)
stripchart(sum(sample_data)/10, add=TRUE,pch=24,bg="red", col="red",method="stack",at=0.0004)

result = t.test(sample_data, conf.level = 0.95)
conf_interval = result$conf.int

axis(side = 1, at = seq(-4, 4, by=2), labels =seq(-4, 4, by=2),cex.axis=1.2,lwd=1.4,line=3, col.axis="black")

############################################################################

sample_data <- round(rnorm(10, mean=0.5, sd=1), 1) 

plot(x, y, type="l", lwd=2, col="#FFFFCC", xaxt='n', yaxt='n', xlab="", ylab="")
polygon(c(x, rev(x)), c(y, rep(0, length(y))), col="#FFFFCC", border=NA)
lines(x, y, col="black", lwd=1)
segments(0, 0, 0, max(y), col="black", lwd=2)
abline(h=0, lty=1, col="black")
stripchart(sample_data, add=TRUE,pch=16, col="Blue",method="stack",at=0.0004)
stripchart(sum(sample_data)/10, add=TRUE,pch=24,bg="red", col="red",method="stack",at=0.0004)

result = t.test(sample_data, conf.level = 0.95)
conf_interval = result$conf.int


x_range <- c(conf_interval[1], conf_interval[2])
axis(side = 1, at = seq(-4, 4, by=2), labels =seq(-4, 4, by=2),cex.axis=1.2,lwd=1.4,line=3, col.axis="black")

############################################################################

sample_data <- round(rnorm(10, mean=0.5, sd=1), 1) 

plot(x, y, type="l", lwd=2, col="#FFFFCC", xaxt='n', yaxt='n', xlab="", ylab="")
polygon(c(x, rev(x)), c(y, rep(0, length(y))), col="#FFFFCC", border=NA)
lines(x, y, col="black", lwd=1)
segments(0, 0, 0, max(y), col="black", lwd=2)
abline(h=0, lty=1, col="black")
stripchart(sample_data, add=TRUE,pch=16, col="Blue",method="stack",at=0.0004)
stripchart(sum(sample_data)/10, add=TRUE,pch=24,bg="red", col="red",method="stack",at=0.0004)

result = t.test(sample_data, conf.level = 0.95)
conf_interval = result$conf.int


x_range <- c(conf_interval[1], conf_interval[2])
axis(side = 1, at = seq(-4, 4, by=2), labels =seq(-4, 4, by=2),cex.axis=1.2,lwd=1.4,line=3, col.axis="black")

