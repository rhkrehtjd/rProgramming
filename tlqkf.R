data = readLines("https://www.angio.net/pi/digits/pi1000000.txt")
data = substr(data,3, nchar(data))

by_2 = substring(data, seq(1, nchar(data), 2), 
                 seq(2, nchar(data), 2))

df2=data.frame(table(by_2))

df2[df2$Freq == min(df2$Freq),]

df2[df2$Freq == max(df2$Freq),]

data = readLines("https://www.angio.net/pi/digits/pi1000000.txt")
data = substr(data,3, nchar(data)-1)

by_3 <- substring(data, seq(1, nchar(data)-4 , by = 3),
                  seq(3,nchar(data)-1 , by = 3))

df3=data.frame(table(by_3))

df3[df3$Freq == min(df3$Freq),]

df3[df3$Freq == max(df3$Freq),]

for (i in 0:4){
  rl <- rle(strsplit(data, "")[[1]])
  num_1 = rl$lengths[which(rl$values == i)]
  print(max(num_1))}

rle_x <- rle(strsplit(data, "")[[1]])
max_length <- max(rle_x$lengths[rle_x$values == "3"])
max_position <- max(cumsum(rle_x$lengths)[rle_x$values == "3"
                                          & rle_x$lengths == max_length])
start_position = max_position - max_length + 1
start_position


data = readLines("https://www.angio.net/pi/digits/pi1000000.txt")
data = substr(data,3, nchar(data))

by_1 = substring(data, seq(1, nchar(data), 1), 
                 seq(1, nchar(data), 1))

df1=data.frame(table(by_1))



par(mfrow=c(3,2),mar=c(6,6,3,3))

########################################

x <- seq(min(df1$Freq),max(df1$Freq), 1)
a=data.frame(table(df1$Freq))
a_=rep(0,length(seq(min(df1$Freq),max(df1$Freq), 1)))
a_[which( seq(min(df1$Freq),max(df1$Freq), 1) %in% a$Var1)]=a$Freq

barplot(a_, xlim = c(0, length(a_)+1), ylab = "Frequency",main="뭐야")
axis(side = 1, at = seq(52, length(a_)-159, length.out=4),line=0.2,labels = format(seq(99600, 100200, by = 200),scientific = FALSE))
barplot(a_, xlim = c(0, length(a_)+1), ylab = "Frequency")
axis(side = 1, at = seq(52, length(a_)-159, length.out=4),line=0.2, labels = format(seq(99600, 100200, by = 200),scientific = FALSE))

########################################

x <- seq(min(df2$Freq),max(df2$Freq), 1)
a=data.frame(table(df2$Freq))
a_=rep(0,length(seq(min(df2$Freq),max(df2$Freq), 1)))
a_[which( seq(min(df2$Freq),max(df2$Freq), 1) %in% a$Var1)]=a$Freq

barplot(a_, xlim = c(0, length(a_)+1), ylab = "Frequency")
axis(side = 1, at = seq(3, length(a_)-7, length.out=8),line=0.2, labels = format(seq(4850, 5200, by = 50),scientific = FALSE))
barplot(a_, xlim = c(0, length(a_)+1), ylab = "Frequency")
axis(side = 1, at = seq(3, length(a_)-7, length.out=8),line=0.2,labels = format(seq(4850, 5200, by = 50),scientific = FALSE))

########################################

x <- seq(min(df3$Freq),max(df3$Freq), 1)
a=data.frame(table(df3$Freq))
a_=rep(0,length(seq(min(df3$Freq),max(df3$Freq), 1)))
a_[which( seq(min(df3$Freq),max(df3$Freq), 1) %in% a$Var1)]=a$Freq

barplot(a_, xlim = c(0, length(a_)+1), ylab = "Frequency")
axis(side = 1, at = seq(14, length(a_)-13, length.out=6),line=0.2,labels = format(seq(280, 380, by = 20),scientific = FALSE))
barplot(a_, xlim = c(0, length(a_)+1), ylab = "Frequency")
axis(side = 1, at = seq(14, length(a_)-13, length.out=6),line=0.2, labels = format(seq(280, 380, by = 20),scientific = FALSE))

