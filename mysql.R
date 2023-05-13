library(readxl)
library(dplyr)
library(RMySQL)

df = read.csv("ttrain.csv",encoding = "UTF-8")
df = df[,-1]
colnames(df) = c("dept","arr","category","cases")
df[, 1:2] <- lapply(df[, 1:2], function(x) format(x, scientific = FALSE))
df$dept <- substr(df$dept, 1, 5)
df$arr <- substr(df$arr, 1, 5)
df[,1] = as.numeric(df[,1]);df[,2] = as.numeric(df[,2])

code <- read_excel("code.xlsx", sheet = 1)
colnames(code) = "code"
new_vec <- substr(code$"code", 12, 28)

new_v <- strsplit(new_vec, "\\|") %>% unlist() %>% trimws()
code = data.frame(col1 = new_v[seq(1, length(new_v), by = 2)],
                 col2 = new_v[seq(2, length(new_v), by = 2)])
df$dept_region <- code$col2[match(df$dept, code$col1)]
df$arr_region <- code$col2[match(df$arr, code$col1)]



dept_region_subset <- df[grepl('전주시', df$dept_region),]
arr_region_subset <- df[grepl('전주시', df$arr_region),]


read.csv("result.csv")


con <- dbConnect(
  MySQL(), 
  user="root", 
  password="264433",
  host="localhost",
  client.flag=CLIENT_MULTI_RESULTS)

dbSendQuery(con, "SET NAMES utf8;") 
dbSendQuery(con, "SET CHARACTER SET utf8;") 
dbSendQuery(con, "SET character_set_connection=utf8;")
dbSendQuery(con, "CREATE DATABASE LOGISTICS;")
dbSendQuery(con, "USE LOGISTICS")
dbSendQuery(con, "CREATE TABLE `logistics` (
            `arr` BIGINT, 
            `dept` BIGINT,
            `category` VARCHAR(15) CHARACTER SET utf8,
            `cases` BIGINT,
            `arr_region` VARCHAR(15) CHARACTER SET utf8,
            `dept_region` VARCHAR(15)) CHARACTER SET utf8")
con <- dbConnect(MySQL(), user = "root", 
                 password = "264433", 
                 dbname = "LOGISTICS",
                 host = "localhost",
                 charset = "utf8")
dbGetQuery(con, "SET GLOBAL local_infile = true;")
dbWriteTable(con, "logistics", df, append = TRUE, row.names=FALSE)

result <- dbGetQuery(con, "SELECT * FROM logistics")


result
