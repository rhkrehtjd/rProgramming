#(1)#############################################################################
power_of_test = function(x_,mu_0 ,mu_1,n){ # 첫 번째 그림 그리는 함수 정의
  par(mfrow = c(3,1)) ;par(bg = "white", col.axis = "white", col.lab = "white",
                           col.main = "white", col.sub = "white", xaxt = "n", 
                           yaxt = "n" , bty = "n") # 흰 백지 생성
  sample = x_ # 표본 추출
  sample_mean = round(sample, 1) # 둘째자리에서 반올림
  
  x = seq(-15, 20, length.out = 100) # x축 범위 설정
  y1 = dnorm(x, mu_1, 5) # 평균 3, 표준편차 5인 정규분포 값
  plot(x, y1, type = "l", lwd = 2, xlim = c(-15, 20),ylim=c(-0.02,max(y1)+0.08)) 
  stripchart(sample, add=TRUE,at=0.004,cex=2, pch=16,col="red") # 4개의 표본 
  stripchart(mean(sample), add=TRUE,at=-0.005,cex=1.8, pch=24,col="blue",bg="blue") # 표본의 평균 파란 세모로 표시
  lines(x = c(-15, 20), y = c(0, 0), col = "black", lwd = 2) # x축 그리기
  arrows(3,0.02,mean(sample),0,length = 0.11) # 표본평균 가르킬 화살표
  segments(3, 0, 3, -0.009, lwd = 2) # 아래쪽으로 향할 mu_1의 눈금
  text(3, -0.017, expression(mu * "=3"),cex=1.3) # mu_1 text 표시
  text(-7, max(y1), "X ~ N(3,5)",cex=1.4) # 정규분포 text 표시
  text(3, 0.03, cex=1.3,substitute(paste(bar(x), " = ", mean),list(mean=round(mean(sample),1)))) # 표본평균 text 표시
  text(3, max(y1)+0.07,expression(H[0]*" : "*mu*" <= "*0*" vs "*H[a]*" : "*mu*" > "*0*"\n"),cex=1.6) # 제목 text
  text(3, max(y1)+0.04, cex=1.27,substitute(paste( "n = ",n),list(n=n)))
  
  x = seq(-6, 12, length.out = 100)
  y2 = dnorm(x, 3, 5/sqrt(n))
  alpha = qnorm(0.95, mean = 0, sd = 5/sqrt(n))
  plot(x, y2, type = "l", lwd = 2, xlim = c(-15, 20),ylim=c(-0.03,max(y2)+0.05))
  polygon(c(alpha, x[x>=alpha], tail(x[x>=alpha], 1)), c(0, y2[x>=alpha], 0), col="#FF00004D", border=NA) # 색칠
  segments(alpha, 0, alpha,dnorm(alpha, 3, 5/sqrt(n)),col="black", lwd = 2)
  lines(x = c(-6, 12), y = c(0, 0), col = "black", lwd = 2)
  segments(3, 0, 3, -0.009, lwd = 2)
  text(3, -0.026, expression(mu * "=3"),cex=1.4)
  text(-7, max(y2)-0.039, cex=1.5,substitute(paste(bar(x), " ~ N(3,", frac(5,sqrt(n)),")"),list(n=n))) # 표본평균 text 표시
  stripchart(mean(sample), add=TRUE,at=0.002,cex=1.6, pch=24,col="blue",bg="blue")
  arrows(8, max(y2)-0.08,5,max(y2)-0.15,length=0.1)
  text(12, max(y2)-0.039, cex=1.3, substitute(paste("1-", beta, "=", prob), 
            list(prob = format(pnorm(alpha, mean = 3, sd = 5/sqrt(n), 
            lower.tail = FALSE), digits = 2))))
  
  
  
  x = seq(-9, 9, length.out = 100)
  y2 = dnorm(x, mu_0, 5/sqrt(n))
  plot(x, y2, type = "l", lwd = 2, xaxt="s", xlim = c(-15, 20), ylim = c(-0.05, max(y2) + 0.05))
  polygon(c(alpha, x[x>=alpha], tail(x[x>=alpha], 1)), c(0, y2[x>=alpha], 0), col="#FF00004D", border=NA)
  segments(alpha, 0, alpha,dnorm(alpha, 0, 5/sqrt(n)),col="black", lwd = 2)
  lines(x = c(-9, 9), y = c(0, 0), col = "black", lwd = 2)
  text(alpha, -0.015, round(alpha,2) ,cex=1)
  axis(side = 1, at = seq(-15, 20, by = 5),label = seq(-15, 20, by = 5))
  text(0, 0.02, expression(mu * "=0"),cex=1)
  segments(0, 0, 0, 0.009, lwd = 2)
  text(-7, max(y2)-0.039, cex=1.5,substitute(paste("N(0,", frac(5,sqrt(n)),")"),list(n=n)))
  stripchart(mean(sample), add=TRUE,at=0.002,cex=1.7, pch=24,col="blue",bg="blue")
  arrows(7, max(y2)-0.08,4.8,max(y2)-0.15,length=0.1)
  if(mean(sample) < alpha){
    text(10, max(y2)-0.01, expression("Do not reject "*H[0]), cex=1.3)}else{
      text(8, max(y2)-0.01, expression("Reject "*H[0]), cex=1.3)
    }
  text(8.5, max(y2)-0.05, expression(alpha*"=0.05"), cex=1.3)}

set.seed(12)
power_of_test(rnorm(4, 3, 5), 0, 3, 4)
power_of_test(rnorm(9, 3, 5), 0, 3, 9)
power_of_test(rnorm(25, 3, 5), 0, 3, 25)
power_of_test(rnorm(36, 3, 5), 0, 3, 36)

