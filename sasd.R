set.seed(1)

# 수평선 그리기
par(bg = "white", col.axis = "white", col.lab = "white", col.main = "white", 
    col.sub = "white", xaxt = "n", yaxt = "n", bty = "n")
plot(x = c(0, 10), y = c(0, 10), type = "n")
for (i in 0:10) {
  lines(x = c(0, 10), y = rep(i, 2), lty = 1)
}

# 바늘 떨어뜨리기
hits <- 0
for (i in 1:1000) {
  x1 <- runif(1, 0, 10) # 바늘의 중간 x좌표
  y1 <- runif(1, 0, 10) # 바늘의 중간 y좌표
  angle <- runif(1, 0, 2*pi)
  
  x2 <- x1 + 0.3 * cos(angle + pi)
  y2 <- y1 + 0.3 * sin(angle + pi)
  
  x3 <- x1 + 0.3 * cos(angle)
  y3 <- y1 + 0.3 * sin(angle)
  
  if (floor(y2) == floor(y3) & ceiling(y2) == ceiling(y3)) {
    col <- "blue"
  } else {
    col <- "red"
    hits <- hits + 1
  }
  
  lines(x = c(x2, x3), y = c(y2, y3), col = col)
}

# pi 추정하기
pi_estimate <- 2 * 0.6 * 1000 / hits
pi_estimate
