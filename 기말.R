library(rvest)
library(bit64)
df <- data.frame()

for (i in 1:1071) {
  page <- read_html(paste0(i, ".txt"))
  
  round <- html_text(html_nodes(page, "h4 strong"))
  round <- as.integer(gsub("[^0-9]+", "", round))
  
  drawing_day <- html_text(html_nodes(page, "p.desc"))
  drawing_day <- format(as.Date(gsub("[^0-9]+", "", drawing_day), format = "%Y%m%d"), "%Y-%m-%d")
  
  td_elements <- html_nodes(page, "td")
  td_text <- html_text(td_elements)
  prize <- td_text[c(2, 3, 4, 8, 9, 10, 13, 14, 15, 18, 19, 20, 23, 24, 25)]
  
  span_win <- html_nodes(page, "div.num.win span")
  span_win_text <- html_text(span_win)
  span_win_text <- as.integer(span_win_text)
  
  span_bonus <- html_nodes(page, "div.num.bonus span")
  span_bonus <- html_text(span_bonus)
  span_bonus <- as.integer(span_bonus)
  
  li <- html_nodes(page, "ul.list_text_common li")[2]
  li_text <- html_text(li)
  numbers <- gsub("[^0-9]", "", li_text)
  sales <- as.integer64(numbers)
  
  row <- data.frame(round, drawing_day, prize[1], prize[2], prize[3], prize[4], prize[5], prize[6], 
                    prize[7], prize[8], prize[9], prize[10], prize[11], prize[12], prize[13], 
                    prize[14], prize[15], span_win_text[1], span_win_text[2], span_win_text[3], 
                    span_win_text[4], span_win_text[5], span_win_text[6], span_bonus, sales)
  
  df <- rbind(df, row)
}
df[, c(3, 5, 6, 8, 9, 11, 12, 14, 15, 17)] <- lapply(df[, c(3, 5, 6, 8, 9, 11, 12, 14, 15, 17)], function(x) as.integer64(gsub("[^0-9]", "", x)))
df[, c(4, 7, 10, 13, 16)] <- lapply(df[, c(4, 7, 10, 13, 16)], function(x) as.integer(gsub(",", "", x)))

colnames(df) <- c("round", "drawing_day", "prize_sum_1", "games_1", "prize_1", "prize_sum_2", "games_2", "prize_2", "prize_sum_3", "games_3", "prize_3", "prize_sum_4", "games_4", "prize_4", "prize_sum_5", "games_5", "prize_5", "lotto_1", "lotto_2", "lotto_3", "lotto_4", "lotto_5", "lotto_6", "lotto_7", "sales")


sum(df$sales)

sum(df[,c("prize_sum_1")]) +sum(df[,c("prize_sum_2")])+sum(df[,c("prize_sum_3")])+
  sum(df[,c("prize_sum_4")])+sum(df[,c("prize_sum_5")])



results <- c()  # 결과를 저장할 빈 벡터 생성

for (i in 1:1071) {
  filename <- paste0(i, ".txt")  # 파일명 생성 (예: "1.txt", "2.txt", ...)
  page <- read_html(filename)  # HTML 페이지 읽어오기
  
  rowspan_cell <- page %>% html_nodes(xpath = "//td[@rowspan='5']") %>% html_text()  # rowspan이 적용된 셀 내용 추출
  
  if (length(rowspan_cell) > 0) {
    result <- gsub("[\r\n\t ]+", "", rowspan_cell)  # 공백과 줄바꿈 문자 제거
    result <- gsub("^.*?\t+", "", result)  # 첫 번째 탭 문자 이전의 내용 제거
    result <- gsub("\n+", "", result)  # 줄바꿈 문자 제거
    results <- c(results, result)  # 결과를 벡터에 추가
  }
}

df$note = results
df$note <- substring(df$note, 3)
df$auto = sub(".*자동(\\d+).*", "\\1", df$note)
df$auto = as.integer(df$auto)

sum(df$auto,na.rm=TRUE)


df$semi = sub(".*반자동(\\d+).*", "\\1", df$note)
sum(df$semi,na.rm=TRUE)
