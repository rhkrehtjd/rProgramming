df = read.csv("king_josun.csv",header=TRUE)

colnames(df) = c("order","name","start","end",
                 "start_age","lasting",
                 "wife","wifes","son","daughter",
                 "birth","death","lifetime") # 변수명 변경
df$sum_child = df$son + df$daughter # 아들 + 딸
df$sum_wife = df$wife + df$wifes # 왕비 + 후궁
df$order <- as.numeric(gsub("대", "", df$order)) # 순서 numeric으로 변경
df = df[!colnames(df)=="name"] # 왕들의 이름은 제외

exclude_col = which(colnames(df)%in%c("start","end","birth","death"))
df = df[-c(exclude_col)] # order와 start, end, birth, death는 동일 변수라 봐도 무방, 제외

###############

cor_df <- cor(df) # 각 변수의 선형관계 확인
attention = c() # 유의한 변수 관계 저장할 벡터
for (i in 1:ncol(cor_df)){
  for (j in 1:nrow(cor_df)){
    if( (abs(cor_df[j,i]) >= 0.3 ) & (abs(cor_df[j,i]) <= 0.8) ) {
      attention = c(attention,c(rownames(cor_df)[i],rownames(cor_df)[j]))
    }
  }} 
# 상관계수의 절댓값이 0.3이상 0.8이하인 것만 저장
# 절댓값이 0.3이하인 것은 상관관계가 약하다고 판단하고 제외
# 절댓값이 0.8이상인 것은 당연한 결과라 판단하고 제외
col1 <- attention[seq(1, length(attention), 2)] # 상관계수의 첫 번째 변수를 저장할 column
col2 <- attention[seq(2, length(attention), 2)] # 상관계수의 두 번째 변수를 저장할 column

mat <- matrix(nrow = length(attention)/2, ncol = 2) # 빈 matrix에 생성
mat[,1] <- col1; mat[,2] <- col2 # matrix에 저장하자

# 각 변수와 상관관계가 있는 변수 모아 저장
df_order = df[,c("order",mat[mat[,1]=="order",][,2])] 
df_start_age = df[,c("start_age",mat[mat[,1]=="start_age",][,2])]
df_lasting = df[,c("lasting",mat[mat[,1]=="lasting",][,2])]
df_wife = df[,c("wife","lasting")]
df_wifes = df[,c("wifes",mat[mat[,1]=="wifes",][,2])]
df_son= df[,c("son",mat[mat[,1]=="son",][,2])]
df_daughter = df[,c("daughter",mat[mat[,1]=="daughter",][,2])]
df_lifetime = df[,c("lifetime",mat[mat[,1]=="lifetime",][,2])]
df_sum_child = df[,c("sum_child",mat[mat[,1]=="sum_child",][,2])]
df_sum_wife = df[,c("sum_wife",mat[mat[,1]=="sum_wife",][,2])]

# 유의한 상관계수를 가진 변수들을 산점도로 살펴보자
# 대각원소 기준 위에만 그려서 확인

pairs(df_order,panel = panel.smooth,lower.panel = function(x,y){NULL})
# 즉위 순서와 즉위 나이, 후궁, 아들 수, 딸 수, 자녀 수는 음의 상관관계 재위기간은 양의 상관관계
pairs(df_start_age,panel = panel.smooth,lower.panel = function(x,y){NULL})
# 즉위 나이와 수명은 양의 상관관계, 재위기간은 음의 상관관계
pairs(df_lasting,panel = panel.smooth,lower.panel = function(x,y){NULL})
# 재위 기간과 수명, 자녀 수 양의 상관관계
pairs(df_wife,panel = panel.smooth,lower.panel = function(x,y){NULL})
# 왕비 수와 재위기간은 양의 상관관계
pairs(df_wifes,panel = panel.smooth,lower.panel = function(x,y){NULL})
# 후궁 수와 아들 수 , 딸 수는 양의 상관관계
pairs(df_son,panel = panel.smooth,lower.panel = function(x,y){NULL})
# 아들 수와 딸 수, 후궁+왕비 수는 양의 상관관계
pairs(df_daughter,panel = panel.smooth,lower.panel = function(x,y){NULL})
# 딸 수와 재위기간, 후궁 수, 아들 수, 수명, 후궁+왕비 수는 양의 상관관계
pairs(df_lifetime,panel = panel.smooth,lower.panel = function(x,y){NULL})
pairs(df_sum_child,panel = panel.smooth,lower.panel = function(x,y){NULL})
pairs(df_sum_wife,panel = panel.smooth,lower.panel = function(x,y){NULL})

# 종합해보자
# 즉위 순서가 뒤로 갈수록 즉위나이,자녀 수, 후궁 수가 감소한다. 
# 즉위 나이, 자녀 수가 많을 수록 수명이 길다. 
# 후궁 수가 많을 수록 딸의 수가 증가한다. 멋대로 해석해보자면 아들을 선호하여, 아들을 낳을 때까지 후궁을 늘리나?


coplot(df$daughter~df$lasting|df$wifes,panel=panel.smooth)
# 후궁 수가 증가할수록 딸의 수와 재위기간의 상관관계는 강해지는가? 


# 다중공선성 진단
model = lm(lifetime ~ start_age + lasting + sum_child, df) 
summary(model)
vif_model <- vif(model)
vif_model # 큰 이상 없다

plot(model)

