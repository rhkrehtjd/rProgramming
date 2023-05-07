df = read.csv("king_josun.csv",header=TRUE)
colnames(df) = c("order","name","start","end",
                 "start_age","lasting",
                 "wife","wifes","son","daughter",
                 "birth","death","lifetime") # 변수명 변경
df$order <- as.numeric(gsub("대", "", df$order)) # numeric으로 변경

# 의미없는 변수(name)나
# 즉위년도로 대체할 수 있는 변수(order, end, birth, death)는 제거
exclude_col = which(colnames(df)%in%c("order","name","end","birth","death"))
df = df[-c(exclude_col)] 
head(df)

# 시간이 흐름에 따라 수명은 증가하는가?
cor(df$start, df$lifetime) # 0.02 => 두 변수는 상관관계가 없다.
plot(df$start, df$lifetime) # 산점도 그리기
abline(lm(lifetime~ start,data=df),col="red") # 회귀직선 그리기, 기울기가 0에 근접한다. 
# 시간이 흐름에 따라 수명은 증가했다고 할 수 없다. 
# 해당 기간동안 의학이 발전했다고 할 수 없을 것이다. 

# 시간의 흐름과 수명간에 매우 약한 상관관계가 확인됨에 따라
# 의학발전이 없었다고 생각해볼 수 있다. 
# 그렇다면 더딘 의학발전의 또 다른 방증은 무엇이 있을까
# 시간의 흐름에 따라 왕의 자녀 수에 대해 분석해보자


# 시간이 흐름에 따라 자녀수를 plotting해보자
# 5명이하의 자녀를 낳았을 땐 빨간색으로 표시해보자
# 이때 아내의 수도 고려해보자
# 아내의 수가 충분함에도 자녀수가 적다는 건 의학이 도움되지 못했다는 방증이 될 수 있다. 
plot(1:nrow(df),df$son + df$daughter,
     col = ifelse(df$son+df$daughter <=5,"red","black"))

under_5 = ifelse(df$son+df$daughter <=5,TRUE,FALSE) # 다섯명이하인 경우는 TRUE
under_5_wifes = df[under_5,"wife"] + df[under_5,"wifes"] # 다섯명이하인 경우 아내의 수
plot(under_5_wifes, df[under_5,"son"] + df[under_5,"daughter"])
abline(lm( df[under_5,"son"] + df[under_5,"daughter"] ~ under_5_wifes))
# 회귀직선의 기울기가 0.1이다.

up_5_wifes = df[!under_5,"wife"] + df[!under_5,"wifes"]
cor(df[under_5,"son"] + df[under_5,"daughter"] , under_5_wifes)
cor(up_5_wifes, df[!under_5,"son"]+df[!under_5,"daughter"])
# 자녀수가 6명이상일 땐 아내 수와 자녀 수의 상관계수가 0.8로 매우 강한 양의 상관관계
# 를 가짐을 알 수 있다. 
# 하지만 자녀수가 5명이하일 땐 아내 수와 자녀수의 상관계수는 약 0.1로 상관관계가 매우
# 약해짐을 알 수 있다. 
# 즉, 자녀수가 5명이하인 경우에선 자녀의 수~아내의 수엔 상관관계가 약하며 다른 데 원인이
# 있을 수 있음을 생각해볼 수 있다. 
# 또는 의학이 발전하지 못해 해당 문제를 해결하지 못했다고 생각할 수도 있겠다. 
