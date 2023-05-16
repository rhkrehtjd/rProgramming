x1 <- c(28, 47, 39, 25, 34, 29, 38, 23, 52, 37, 35, 39, 38, 32, 25)
x2 <- c(146, 169, 160, 156, 161, 168, 154, 153, 160, 152, 155, 154, 157, 162, 142)
x3 <- c(34, 57, 38, 28, 37, 50, 54, 40, 62, 39, 46, 54, 57, 53, 32)
y <- c(22, 36, 24, 22, 27, 29, 26, 23, 31, 25, 23, 27, 31, 25, 23)
df = data.frame(x1,x2,x3,y)

# 1-(1)
model_1 = lm(y~x3)
summary(model_1)

# 이상치 확인
resuduals = model_1$residuals
hist(residuals)
# 오른쪽 꼬리에 동떨어진 점들을 볼 수 있음

# 내적으로 표준화된 잔차
s_residuals = rstandard(model_1)
hist(s_residuals)
# 절댓값이 2이상인 부분이 이상치일 가능성이 있음

# 외적으로 표준화된 잔차
s_residual_i = rstudent(model_1)
hist(s_residual_i)

s_residual_i[which.max(s_residual_i)]

# 기각역
qt(0.975,nrow(df)-1-2)<s_residual_i[which.max(s_residual_i)]
# 이상치라고 할 수 있다. 

# 산점도로 확인
plot(y~x3, pch=20, cex=2,col="darkorange")
text(x3[2],y[2],"2",pos=2) #
abline(model_1,col="steelblue", lwd=2)

# 또 다른 이상점의 유무 확인
s_residual_i[which(abs(s_residual_i)>qt(0.975,nrow(df)-1-2))]
# 없음


# 영향점?
influence(model_1)
influence.measures(model_1)

which(abs(dffits(model_1)) > 2*sqrt((1+1)/(nrow(df)-1-1)))
# 두번째 관측치
which(cooks.distance(model_1) > qf(0.5,2,nrow(df)-1-1))
# 없음

which(abs(covratio(model_1) -1 ) > 3*(1+1)/nrow(df))
# 2,4,9,15번째 관측치

summary(influence.measures(model_1))


# 1-(2)
model_2 = lm(y~.,data=df)
summary(model_2)

# 이상치 확인
resuduals = model_2$residuals
hist(residuals)
# 오른쪽 꼬리에 동떨어진 점들을 볼 수 있음

# 내적으로 표준화된 잔차
s_residuals = rstandard(model_2)
hist(s_residuals)
# 절댓값이 2이상인 부분이 이상치일 가능성이 있음

# 외적으로 표준화된 잔차
s_residual_i = rstudent(model_2)
hist(s_residual_i)

s_residual_i[which.max(s_residual_i)]

# 기각역
qt(0.975,nrow(df)-3-2)<s_residual_i[which.max(s_residual_i)]
# 이상치라고 할 수 있다. 


# 또 다른 이상점의 유무 확인
s_residual_i[which(abs(s_residual_i)>qt(0.975,nrow(df)-3-2))]
# 없음


# 영향점?
influence(model_2)
influence.measures(model_2)

which(abs(dffits(model_2)) > 2*sqrt((3+1)/(nrow(df)-3-1)))
# 2,15번째 관측치
which(cooks.distance(model_2) > qf(0.5,4,nrow(df)-3-1))
# 없음

which(abs(covratio(model_2) -1 ) > 3*(3+1)/nrow(df))
# 1,4,6,8,9,10번째 관측치
summary(influence.measures(model_2))



# 2-(1)
df <- data.frame(x1 = c(80, 80, 75, 62, 62, 62, 62, 62, 58, 58, 58, 58, 58, 58, 50, 50, 50, 50, 50, 56, 70),
                 x2 = c(27, 27, 25, 24, 22, 23, 24, 24, 23, 18, 18, 17, 18, 19, 18, 18, 19, 19, 20, 20, 20),
                 x3 = c(89, 88, 90, 87, 87, 87, 93, 93, 87, 90, 89, 88, 82, 93, 89, 86, 72, 79, 80, 82, 91),
                 y = c(42, 37, 37, 28, 18, 18, 19, 20, 15, 14, 14, 13, 11, 12, 8, 7, 8, 8, 9, 15, 15))


df




