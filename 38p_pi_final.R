data = readLines("https://www.angio.net/pi/digits/pi1000000.txt") # 데이터 불러오기
data = substr(data,3, nchar(data)) # 앞에 "3."부분 버리고 indexing

by_2 = substring(data, seq(1, nchar(data), 2), 
                 seq(2, nchar(data), 2))  # 2개씩 자르기

df2=data.frame(table(by_2)) # 두 개씩 자른 것의 빈도표를 data.frame으로 저장

df2[df2$Freq == min(df2$Freq),] # 빈도수가 가장 적게 나온 값은?

df2[df2$Freq == max(df2$Freq),] # 빈도수가 가장 많이 나온 값은?


data = readLines("https://www.angio.net/pi/digits/pi1000000.txt")  # 데이터 불러오기
data = substr(data,3, nchar(data)-1) # 3개씩 자르기 위해서 3부터 999999까지만 indexing

by_3 <- substring(data, seq(1, nchar(data)-4 , by = 3),
                  seq(3,nchar(data)-1 , by = 3)) # 3개씩 자르기

df3=data.frame(table(by_3)) # by_3의 빈도수 계산

df3[df3$Freq == min(df3$Freq),] # 가장 적은 빈도수를 가진 숫자는?

df3[df3$Freq == max(df3$Freq),] # 가장 많은 빈도수를 가진 숫자는?

for (i in 0:4){
  rl <- rle(strsplit(data, "")[[1]]) 
  # rle 내장 함수 이용하여, 연속된 개수와 그 때의 숫자를 lengths와 values에 각각 저장
  num_1 = rl$lengths[which(rl$values == i)] 
  # rl의 values가 0일 때의 길이를 num_1에 저장
  # 그 다음 르핑에선 rl의 values가 2일 때의 길이를 num_1에 저장, 해당 과정을 4까지 반복
  print(max(num_1))}# 0부터 4까지 각각 가장 많이 나온 빈도수 print

# rle 내장 함수 이용하여, 연속된 개수와 그 때의 숫자를 lengths와 values에 각각 저장
rle_x <- rle(strsplit(data, "")[[1]]) 
# rle_x의 values가 3인 경우의 길이를 다 뽑은 후 그때의 max를 추출
max_length <- max(rle_x$lengths[rle_x$values == "3"])
# cussum 함수 이용하여 연속된 숫자들의 개수를 누적 합 후,
# 기존 rle_x$values == "3"& rle_x$lengths == max_length를 만족하는 조건의 index
# 이때 index는 cuscum 함수의 특성상 3이 가장 길게 나온 마지막 위치임
last_position <- cumsum(rle_x$lengths)[rle_x$values == "3"
                                       & rle_x$lengths == max_length]
start_position = last_position - max_length + 1 # 마지막 위치 - 연속된 길이 + 1 = 시작 위치
start_position


########################################

par(mfrow=c(3,2),mar=c(6,6,3,3))
data = readLines("https://www.angio.net/pi/digits/pi1000000.txt")
data = substr(data,3, nchar(data)) # "3."부분 버리기

by_1 = substring(data, seq(1, nchar(data), 1), 
                 seq(1, nchar(data), 1)) # 1개씩 잘라서 저장

df1=data.frame(table(by_1)) # 빈도수 계산
a=data.frame(table(df1$Freq)) # 빈도수의 빈도 계산

a_=rep(0,length(seq(min(df1$Freq),max(df1$Freq), 1))) # 가능한 빈도수의 범위만큼의 0으로 이루어진 벡터 생성
# a$Var1, 즉 가능한 빈도수 부분의 위치만 벡터 a_에 해당 빈도수를 저장
# 히스토그램에 해당 빈도수의 빈도는 빈도로 나타내고 나머지는 0으로 나타내기 위한 과정임
a_[which( seq(min(df1$Freq),max(df1$Freq), 1) %in% a$Var1)]=a$Freq 

expectation_freq = 10^6 * (1/10)

barplot(a_, xlim = c(1, length(a_)), ylab = "Frequency",xlab = "Frequency In pi",main="Single digit(0~9)")
axis(side = 1, at = seq(52, length(a_)-30, length.out=4),line=0.2,labels = format(seq(99600, 100200, by = 200),scientific = FALSE))
barplot(a_, xlim = c(1, length(a_)), ylab = "Frequency",xlab = "Frequency In pi / Expectation_Freq",main="Single digit(0~9)")
axis(side = 1, at = seq(52, length(a_)-30, length.out=4),line=0.2, labels = format(seq(99600, 100200, by = 200)/expectation_freq,scientific = FALSE))

########################################

a=data.frame(table(df2$Freq)) # 위에서 진행했던 과정과 동일 과정
a_=rep(0,length(seq(min(df2$Freq),max(df2$Freq), 1)))
a_[which( seq(min(df2$Freq),max(df2$Freq), 1) %in% a$Var1)]=a$Freq

expectation_freq = 10^6 * (1/2) * (1/100)

round(c(1.23,1.23),1)

barplot(a_, xlim = c(1, length(a_)), ylab = "Frequency",xlab = "Frequency In pi",main="Double digit(00~99)")
axis(side = 1, at = seq(3, length(a_), length.out=8),line=0.2, labels = format(seq(4850, 5200, by = 50),scientific = FALSE))
barplot(a_, xlim = c(1, length(a_)), ylab = "Frequency",xlab = "Frequency In pi / Expectation_Freq",main="Double_digit(00~99)")
axis(side = 1, at = seq(3, length(a_), length.out=8),line=0.2,labels = format(seq(4850, 5200, by = 50)/expectation_freq,scientific = FALSE))

########################################

a=data.frame(table(df3$Freq)) # 위에서 진행했던 과정과 동일 과정
a_=rep(0,length(seq(min(df3$Freq),max(df3$Freq), 1)))
a_[which( seq(min(df3$Freq),max(df3$Freq), 1) %in% a$Var1)]=a$Freq

expectation_freq = (10^6 -1)*(1/3)*(1/10^3)

barplot(a_, xlim = c(1, length(a_)), ylab = "Frequency",xlab = "Frequency In pi",main="Triple digit(000~999)")
axis(side = 1, at = seq(14, length(a_), length.out=6),line=0.2,labels = format(seq(280, 380, by = 20),scientific = FALSE))
barplot(a_, xlim = c(1, length(a_)), ylab = "Frequency",xlab = "Frequency In pi / Expectation_Freq",main="Triple digit(000~999)")
axis(side = 1, at = seq(14, length(a_), length.out=4),line=0.2, labels = format(round(seq(280, 380, length.out= 4)/expectation_freq,1),scientific = FALSE))
