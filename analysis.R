

library(lubridate)
library(rvest)
library(stringr)
library(quantmod)
library(writexl)
library(googlesheets4)

code <- c("00639", "006205", "00633L", 
          "00634R", "00643", "00636", 
          "00655L", "00656R", "00703", 
          "0061", "00637L", "00638R", 
          "006206", "00739", "00752", 
          "00753L", "006207", "^TWII")

name <- c("富邦深100", "富邦上証(新臺幣)", "富邦上証正2", 
          "富邦上証反1", "群益深証中小(新臺幣)", "國泰中國A50(新臺幣)", 
          "國泰中國A50正2", "國泰中國A50反1", "台新MSCI中國", 
          "元大寶滬深", "元大滬深300正2", "元大滬深300反1	", 
          "元大上證50", "元大MSCI A股", "中信中國50", 
          "中信中國50正2", "復華滬深(原簡稱:FH滬深)", "大盤")

fee <- c(1.09, 1.09, 1.22, 
         1.22, 1.08, 1.05, 
         1.16, 1.13, 1.1, 
         0.4, 1.22, 1.2, 
         1.09, 0.8, 1.17, 
         1.19, 0.85, 0)

now <- Sys.Date()
ym <- str_c(str_sub(now, 1, 4), str_sub(now, 6, 7), "01")

riskFreeURL <- read_html("https://www.cbc.gov.tw/tw/cp-371-1040-5C7A0-1.html")
riskFree <- as.numeric(riskFreeURL %>% html_nodes("tr:nth-child(5) td:nth-child(2)") %>% html_text())

gs4_deauth()
gs4_auth(path = "core-parsec-377012-bf54595b7c83.json")
history <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1moEt9ePoBL4xw3pqY5-R-e_FROnjCbWYIaWz5bkQ7vQ/edit?usp=sharing", sheet = "Sheet01")

tableURL <- read_html(str_glue("https://www.twse.com.tw/rwd/zh/afterTrading/STOCK_DAY?date={ym}&stockNo=0061&response=html"))
table <- tableURL %>% html_nodes("td") %>% html_text()
date <- table[1+((length(table)/9)-1)*9]
if(date != history[length(history[,1]),1]){
  close <- c()
  for(i in code[1:17]){
    tableURL <- read_html(str_glue("https://www.twse.com.tw/rwd/zh/afterTrading/STOCK_DAY?date={ym}&stockNo={i}&response=html"))
    table <- tableURL %>% html_nodes("td") %>% html_text()
    c <- table[7+((length(table)/9)-1)*9]
    close <- c(close, c)
  }
  tableURL <- read_html(str_glue("https://www.twse.com.tw/rwd/zh/afterTrading/FMTQIK?date={ym}&response=html"))
  table <- tableURL %>% html_nodes("td") %>% html_text()
  c <- table[5+((length(table)/6)-1)*6]
  c <- str_replace_all(c, ",", "")
  close <- c(close, c)
  history <- rbind(history, c(date, close))
}

return <- data.frame(1:120)
for(i in 2:19){
  rate <- c()
  for(j in 0:119){
    r <- round(((as.numeric(history[length(history[,1])-j, i]) - as.numeric(history[length(history[,1])-j-120, i]))/as.numeric(history[length(history[,1])-j-120, i]))*100, 2)
    rate <- c(rate, r-as.numeric(fee[i-1]))
  }
  return <- cbind(return, rate)
}

sharpeRatio <- c()
aAll <- c()
bAll <- c()
for(i in 2:19){
  sr <- round((mean(return[,i])-riskFree)/sd(return[,i]), 2)
  sharpeRatio <- c(sharpeRatio, sr)
  regression <- lm(return[,i]~return[,19], data = return)
  p1 <- summary(regression)$coefficients[1,4]
  p2 <- summary(regression)$coefficients[2,4]
  a <- round(as.numeric(summary(regression)$coefficients[1,1]), 2)
  b <- round(as.numeric(summary(regression)$coefficients[2,1]), 2)
  if(p1 > 0.050){
    a <- 0
  }
  if(p2 > 0.05){
    b <- 0
  }
  aAll <- c(aAll, a)
  bAll <- c(bAll, b)
}

result <- data.frame(code[1:17], name[1:17], fee[1:17], sharpeRatio[1:17], aAll[1:17], bAll[1:17])
names(result) <- c("Code", "Name", "Fee%", "SharpeRatio", "Alpha", "Beta")
result <- result[order(result$SharpeRatio, decreasing = TRUE),]

write_sheet(data = history, ss = "https://docs.google.com/spreadsheets/d/1moEt9ePoBL4xw3pqY5-R-e_FROnjCbWYIaWz5bkQ7vQ/edit?usp=sharing", sheet = "Sheet01")
write_sheet(data = result, ss = "https://docs.google.com/spreadsheets/d/1moEt9ePoBL4xw3pqY5-R-e_FROnjCbWYIaWz5bkQ7vQ/edit?usp=sharing", sheet = "Sheet02")
