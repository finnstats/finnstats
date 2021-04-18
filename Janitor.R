library(janitor)
library(xlsx)
library(dplyr)

data<-read.csv("D:/RStudio/Website/FinData.csv",1)
colnames(data)

clean<-clean_names(data)
colnames(clean)
tabyl(clean,employee_status)
clean %>% tabyl(employee_status) %>% adorn_pct_formatting(digits =2,affix_sign=TRUE)

clean %>% tabyl(employee_status, full_time) %>% adorn_totals()

clean %>% tabyl(employee_status, full_time) %>% adorn_totals(where = "col")

clean %>% tabyl(employee_status, full_time) %>% adorn_totals(where = c("row","col"))


clean %>% tabyl(employee_status, full_time) %>% 
adorn_totals("row") %>%
adorn_percentages("row") %>%
adorn_pct_formatting() %>%
adorn_ns("front") 

clean %>% tabyl(employee_status,full_time,subject)

clean_x<-clean %>% remove_empty(whic=c("rows"))

clean %>% get_dupes(first_name,certification)

excel_numeric_to_date(41103)
