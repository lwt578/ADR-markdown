library(tidyverse)
library(lubridate)
library(openxlsx)
library(flextable)
library(scales)



df <- read.xlsx("不良反应明细2022.xlsx")
df$不良反应结果 <- parse_factor(df$不良反应结果,
                          levels = c("痊愈","好转","未好转","不详","有后遗症","死亡")) 
df$日期 <- ymd(df$日期)

df2 <- df%>%
  filter(between(month(日期),7,9))
c <- nrow(df2)

n <- df2%>% 
  count(患者性别,name = "数量")
n<- mutate(n,占比= percent(数量/c,0.1))

min <- min(df2$年龄)
max <- max(df2$年龄)

age <- df2$年龄
age_break <- c(0,14,44,65,Inf)
age_distribution <- data.frame(table(cut(age,age_break)))%>%
  mutate(prob= percent(Freq/c,0.1))

colnames(age_distribution) <- c("年龄","例数","占比(%)")
age_distribution[,1]<- c("0-14岁","15-44岁","45-65岁","65岁以上")

set_flextable_defaults(text.align = "center")

tb1 <- flextable(age_distribution,cwidth = 1)
tb1 <- align(tb1,align = "center",part = "all")
tb1


# - -----------------------------------------------------------------------


drug <- df2%>% 
  count(药物种类,name = "数量",sort = TRUE)%>%
  mutate(占比= percent(数量/c,0.1))

ab <- df2%>%
  filter(药物种类=="抗菌药物")%>% 
  count(药品,name = "数量",sort = TRUE)
ab <-mutate(ab,占比= percent(数量/sum(ab$数量),0.1))


tb21 <- flextable(drug,cwidth = 2)
tb21 <- align(tb21,align = "center",part = "all")%>%
  autofit()


tb22 <- flextable(ab,cwidth = 2.2)%>%
  autofit()
tb22 <- align(tb22,align = "center",part = "all")


tb21
tb22


# - -----------------------------------------------------------------------

dosage <- df2%>% 
  count(给药途径,name = "数量",sort = TRUE)%>%
  mutate(占比= percent(数量/c,0.1))


tb23 <- flextable(dosage,cwidth = 2)%>%
  add_header_lines(values = "表2.3 给药途径分布情况")
tb23 <- align(tb23,align = "center",part = "all")
tb23 <- autofit(tb23)

tb23


# - -----------------------------------------------------------------------

adr <-str_split(df2$不良反应,pattern = "、")%>%
  unlist()%>%
  table()%>%
  data.frame()%>%
  arrange(desc(Freq))
colnames(adr) <- c("不良反应名称","数量")

severe <- nrow(filter(df2,报告类型=="严重"))
new <- nrow(filter(df2,新的=="是"))

class <- df2%>% 
  count(累及器官系统,name = "数量",sort = TRUE)%>% 
  mutate(占比= percent(数量/c,0.1))

outcome <- df2%>% 
  count(不良反应结果,name = "数量",.drop=FALSE)

tb3 <- flextable(class,cwidth = 2.3)%>%
  add_header_lines(values = "表3 不良反应累及器官系统")
tb3 <- align(tb3,align = "center",part = "all")
tb3 <- autofit(tb3)
tb3

