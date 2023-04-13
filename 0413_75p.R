library(beeswarm); set.seed(1235)
random_a = c(rnorm(25,172,6)); random_b = c(rnorm(25,167,6))
group_a = rep("a",25);group_b = rep("b",25)

random_a_and_b = c(random_a, random_b) # 두 그룹 난수
group_a_and_b = c(group_a,group_b) # "a","b" 라벨링

############################################################################

rslt = aov(random_a_and_b ~ group_a_and_b) # one-way ANOVA
summary(rslt) # 유의  -> 사후분석 수행 > 신뢰구간 구하기

# 신뢰구간 구하는 과정
rslt_list = unlist(summary(rslt)) # 분석 결과를 벡터로 저장
qt_val = qtukey(df = rslt_list[2], nmeans = 2,p = 0.95 )/sqrt(2) # 임계값
sd = sqrt(rslt_list[6]) # 변동성
n = 25 # 샘플 크기
margin_error = qt_val*sd*sqrt(1/n) # 표준오차
sample_mean = aggregate(random_a_and_b, list(group_a_and_b), mean) # 각 그룹 평균
moes = c(sample_mean$x-margin_error, sample_mean$x+margin_error) # 신뢰구간
############################################################################

par(mar = c(5,5,2,5)) # 여백 조정
plot(c(150,190),axes=FALSE, xaxt="n", type="n",
     xlim=c(0.3, 2.5), ylab="height" ) # 캔버스 생성
y_range=c(150,190)
beeswarm(random_a_and_b~group_a_and_b,side=-1,
         col = 1:length(n), add=TRUE,ylim=y_range,  
         method="center",xlab="",main = '') # 표본 plotting
axis(1, 1:2, labels=c("A","B")) # 2개의 집단 표시
axis(2) # 세로축 생성


x_adj = c(1,2) + 0.245 # beeswarm옆에 그리기 위해 값 조정
arrows(x_adj[1],moes[1],x_adj[1],moes[3],lwd=1.4, length=0.14,angle=90, code=3) # 집단 a의 신뢰구간
arrows(x_adj[2],moes[2],x_adj[2],moes[4],lwd=1.4, length=0.14,angle=90, code=3) # 집단 b의 신뢰구간
points(x_adj, sample_mean$x,col=2,pch=20) # 집단 a ,b 평균
lines(c(x_adj[2], 3), c(sample_mean$x[2], sample_mean$x[2]), lty=3)

par(new=TRUE) # 위에 겹쳐 그리기
diff=sample_mean$x[1]-sample_mean$x[2] # 평균 차이

test=t.test(random_a_and_b~group_a_and_b, 
            conf.level=0.95, var.equal=TRUE) # 평균차이 유의미한가?

pvalue=round(test$p.value,3)
y_range = c(150,190)
plot(0.97, diff, axes=FALSE, xlab=NA, ylab=NA,
     type="n", xlim=c(0,1),
     ylim=y_range-sample_mean$x[2],
     main=paste("95% confidence interval\np-value=", pvalue))
axis(side=4, at=seq(-10,10,5)) # 평균 차에 대한 축 추가

points(0.97, diff, pch=18, col="blue") # 평균 차이
arrows(0.97, test$conf.int[1], 0.97,test$conf.int[2],
       ,length=0.15,angle=30, code=3)  # 평균 차이의 신뢰구간



