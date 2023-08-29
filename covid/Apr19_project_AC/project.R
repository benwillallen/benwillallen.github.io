#___________________________________
## STAT405/605 Project
## Author: Anthony Cai (tc72)
## Created: Mar 10, 2022
## Modified: Apr 18, 2022
#___________________________________

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyquant)

# Provides helpful functions and sample data to use in R
library(tidyverse) 
library(forecast)
library(tsibble)
library(knitr)
options(warn=-1)

#*************************************************
# Data Cleaning ####
mydf <- read.csv("COVID19casedata.csv")
mydf <- mydf[,-c(4:5,10:11)]
mydf <- na.omit(mydf)

mydf21 <- subset(mydf, case_month > "2020-12")
mydf20 <- subset(mydf, case_month <= "2020-12")

# case by state
case_by_state <- data.frame(table(mydf$res_state))
colnames(case_by_state) <- c('State', 
                             'Case')

# California data
mydf_ca <- subset(mydf, res_state == "CA")
ca_monthly <- data.frame(table(mydf_ca$case_month))
ca_monthly <- data.frame(ca_monthly, 
                         cm = cummean(ca_monthly$Freq),
                         cs = cumsum(ca_monthly$Freq),
                         state = "CA")
colnames(ca_monthly) <- c('Month', 
                          'Case', 
                          'Cumulative_Mean',
                          'Cumulative_Sum',
                          'State')
# New York data
mydf_ny <- subset(mydf, res_state == "NY")
ny_monthly <- data.frame(table(mydf_ny$case_month))
ny_monthly <- data.frame(ny_monthly, 
                         cm = cummean(ny_monthly$Freq),
                         cs = cumsum(ny_monthly$Freq),
                         state = "NY")
colnames(ny_monthly) <- c('Month', 
                          'Case', 
                          'Cumulative_Mean',
                          'Cumulative_Sum',
                          'State')
# Florida data
mydf_fl <- subset(mydf, res_state == "FL")
fl_monthly <- data.frame(table(mydf_fl$case_month))
fl_monthly <- data.frame(fl_monthly, 
                         cm = cummean(fl_monthly$Freq),
                         cs = cumsum(fl_monthly$Freq),
                         state = "FL")
colnames(fl_monthly) <- c('Month', 
                          'Case', 
                          'Cumulative_Mean',
                          'Cumulative_Sum',
                          'State')
# Illinois data
mydf_il <- subset(mydf, res_state == "IL")
il_monthly <- data.frame(table(mydf_il$case_month))
il_monthly <- data.frame(il_monthly, 
                         cm = cummean(il_monthly$Freq),
                         cs = cumsum(il_monthly$Freq),
                         state = "IL")
colnames(il_monthly) <- c('Month', 
                          'Case', 
                          'Cumulative_Mean',
                          'Cumulative_Sum',
                          'State')
# Arizona data
mydf_az <- subset(mydf, res_state == "AZ")
az_monthly <- data.frame(table(mydf_az$case_month))
az_monthly <- data.frame(az_monthly, 
                         cm = cummean(az_monthly$Freq),
                         cs = cumsum(az_monthly$Freq),
                         state = "AZ")
colnames(az_monthly) <- c('Month', 
                          'Case', 
                          'Cumulative_Mean',
                          'Cumulative_Sum',
                          'State')
# Pennsylvania data
mydf_pa <- subset(mydf, res_state == "PA")
pa_monthly <- data.frame(table(mydf_pa$case_month))
pa_monthly <- data.frame(pa_monthly, 
                         cm = cummean(pa_monthly$Freq),
                         cs = cumsum(pa_monthly$Freq),
                         state = "PA")
colnames(pa_monthly) <- c('Month', 
                          'Case', 
                          'Cumulative_Mean',
                          'Cumulative_Sum',
                          'State')
# Hawaii data
mydf_hi <- subset(mydf, res_state == "HI")
hi_monthly <- data.frame(table(mydf_hi$case_month))
hi_monthly <- data.frame(hi_monthly, 
                         cm = cummean(hi_monthly$Freq),
                         cs = cumsum(hi_monthly$Freq),
                         state = "HI")
colnames(hi_monthly) <- c('Month', 
                          'Case', 
                          'Cumulative_Mean',
                          'Cumulative_Sum',
                          'State')
# DC data
mydf_dc <- subset(mydf, res_state == "DC")
dc_monthly <- data.frame(table(mydf_dc$case_month))
dc_monthly <- data.frame(dc_monthly, 
                         cm = cummean(dc_monthly$Freq),
                         cs = cumsum(dc_monthly$Freq),
                         state = "DC")
colnames(dc_monthly) <- c('Month', 
                          'Case', 
                          'Cumulative_Mean',
                          'Cumulative_Sum',
                          'State')
# Ohio data
mydf_oh <- subset(mydf, res_state == "OH")
oh_monthly <- data.frame(table(mydf_oh$case_month))
oh_monthly <- data.frame(oh_monthly, 
                         cm = cummean(oh_monthly$Freq),
                         cs = cumsum(oh_monthly$Freq),
                         state = "OH")
colnames(oh_monthly) <- c('Month', 
                          'Case', 
                          'Cumulative_Mean',
                          'Cumulative_Sum',
                          'State')
# New Jersy data
mydf_nj <- subset(mydf, res_state == "NJ")
nj_monthly <- data.frame(table(mydf_nj$case_month))
nj_monthly <- data.frame(nj_monthly, 
                         cm = cummean(nj_monthly$Freq),
                         cs = cumsum(nj_monthly$Freq),
                         state = "NJ")
colnames(nj_monthly) <- c('Month', 
                          'Case', 
                          'Cumulative_Mean',
                          'Cumulative_Sum',
                          'State')
# Massachusetts data
mydf_ma <- subset(mydf, res_state == "MA")
ma_monthly <- data.frame(table(mydf_ma$case_month))
ma_monthly <- data.frame(ma_monthly, 
                         cm = cummean(ma_monthly$Freq),
                         cs = cumsum(ma_monthly$Freq),
                         state = "MA")
colnames(ma_monthly) <- c('Month', 
                          'Case', 
                          'Cumulative_Mean',
                          'Cumulative_Sum',
                          'State')
# US data
us_monthly <- data.frame(table(mydf$case_month))
us_monthly <- data.frame(us_monthly, 
                         cm = cummean(us_monthly$Freq),
                         cs = cumsum(us_monthly$Freq),
                         state = "US")
colnames(us_monthly) <- c('Month', 
                          'Case', 
                          'Cumulative_Mean',
                          'Cumulative_Sum',
                          'State')
#*************************************************

#*************************************************
# Visualization ####
## gg1 ####
gg1 <- mydf %>%
  ggplot(aes(x=age_group, fill=age_group)) +
  geom_bar() +
  facet_wrap(~race, scale = "free") + 
  labs(x = "Age group", y = "Case count")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.background = element_rect(
          color="black", fill="aliceblue", size=1.5, linetype="solid"
        )) 

## gg2 ####
gg2 <- case_by_state %>%
  ggplot(mapping = aes(x = reorder(State, Case), Case)) + 
  theme_light() +
  geom_bar(stat = "identity", fill="cornflowerblue") +
  labs(x = "State", y = "Case count") +
  coord_flip()

## gg3 ####
bind <- rbind(ca_monthly,
              ny_monthly,
              fl_monthly, 
              il_monthly, 
              az_monthly,
              pa_monthly,
              oh_monthly,
              nj_monthly,
              ma_monthly,
              dc_monthly,
              hi_monthly)
gg3 <- 
  ggplot() +
  theme_light() + 
  geom_line(aes(x=Month, y=Cumulative_Sum, color='California', group=1),
            bind %>% filter(State=='CA'), size = 1.2) +
  geom_line(aes(x=Month, y=Cumulative_Sum, color='New York', group=1),
            bind %>% filter(State=='NY'), size = 1.2) +
  geom_line(aes(x=Month, y=Cumulative_Sum, color='Florida', group=1),
            bind %>% filter(State=='FL'), size = 1.2) +
  geom_line(aes(x=Month, y=Cumulative_Sum, color='Illinois', group=1),
            bind %>% filter(State=='IL'), size = 1.2)  +
  geom_line(aes(x=Month, y=Cumulative_Sum, color='Arizona', group=1),
            bind %>% filter(State=='AZ'), size = 1.2) +
  geom_line(aes(x=Month, y=Cumulative_Sum, color='Pennsylvania', group=1),
            bind %>% filter(State=='PA'), size = 1.2) +
  geom_line(aes(x=Month, y=Cumulative_Sum, color='Ohio', group=1),
            bind %>% filter(State=='OH'), size = 1.2) +
  geom_line(aes(x=Month, y=Cumulative_Sum, color='New Jersy', group=1),
            bind %>% filter(State=='NJ'), size = 1.2) +
  geom_line(aes(x=Month, y=Cumulative_Sum, color='Massachusetts', group=1),
            bind %>% filter(State=='MA'), size = 1.2) +
  geom_line(aes(x=Month, y=Cumulative_Sum, color='District of Columbia', group=1),
            bind %>% filter(State=='DC'), size = 1.2) +
  geom_line(aes(x=Month, y=Cumulative_Sum, color='Hawaii', group=1),
            bind %>% filter(State=='HI'), size = 1.2) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Cumulative Data") +
  labs(color = "State") 

## gg4-15 #####
gg4 <- ca_monthly %>% 
  ggplot(aes(x=Month, y = Case)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1.3) +
  autolayer(ma(ca_monthly$Case, order=3, centre=FALSE), series="3-MA", size = 1) +
  autolayer(ma(ca_monthly$Case, order=5, centre=FALSE), series="5-MA", size = 1) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30 , hjust = 1)) +
  ggtitle("California") + 
  labs(color = "Series")
gg5 <- ny_monthly %>% 
  ggplot(aes(x=Month, y = Case)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1.3) +
  autolayer(ma(ny_monthly$Case, order=3, centre=FALSE), series="3-MA", size = 1) +
  autolayer(ma(ny_monthly$Case, order=5, centre=FALSE), series="5-MA", size = 1) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("New York") + 
  labs(color = "Series") 
gg6 <- fl_monthly %>% 
  ggplot(aes(x=Month, y = Case)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1.3) +
  autolayer(ma(il_monthly$Case, order=3, centre=FALSE), series="3-MA", size = 1) +
  autolayer(ma(il_monthly$Case, order=5, centre=FALSE), series="5-MA", size = 1) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Florida") + 
  labs(color = "Series") 
gg7 <- il_monthly %>% 
  ggplot(aes(x=Month, y = Case)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1.3) +
  autolayer(ma(il_monthly$Case, order=3, centre=FALSE), series="3-MA", size = 1) +
  autolayer(ma(il_monthly$Case, order=5, centre=FALSE), series="5-MA", size = 1) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30 , hjust = 1)) +
  ggtitle("Illinois") + 
  labs(color = "Series") 
gg8 <- az_monthly %>% 
  ggplot(aes(x=Month, y = Case)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1.3) +
  autolayer(ma(az_monthly$Case, order=3, centre=FALSE), series="3-MA", size = 1) +
  autolayer(ma(az_monthly$Case, order=5, centre=FALSE), series="5-MA", size = 1) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30 , hjust = 1)) +
  ggtitle("Arizona") + 
  labs(color = "Series") 
gg9 <- pa_monthly %>% 
  ggplot(aes(x=Month, y = Case)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1.3) +
  autolayer(ma(pa_monthly$Case, order=3, centre=FALSE), series="3-MA", size = 1) +
  autolayer(ma(pa_monthly$Case, order=5, centre=FALSE), series="5-MA", size = 1) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30 , hjust = 1)) +
  ggtitle("Pennsylvania") + 
  labs(color = "Series") 
gg10 <- oh_monthly %>% 
  ggplot(aes(x=Month, y = Case)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1.3) +
  autolayer(ma(oh_monthly$Case, order=3, centre=FALSE), series="3-MA", size = 1) +
  autolayer(ma(oh_monthly$Case, order=5, centre=FALSE), series="5-MA", size = 1) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30 , hjust = 1)) +
  ggtitle("Ohio") + 
  labs(color = "Series") 
gg11 <- nj_monthly %>% 
  ggplot(aes(x=Month, y = Case)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1.3) +
  autolayer(ma(nj_monthly$Case, order=3, centre=FALSE), series="3-MA", size = 1) +
  autolayer(ma(nj_monthly$Case, order=5, centre=FALSE), series="5-MA", size = 1) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30 , hjust = 1)) +
  ggtitle("New Jersy") + 
  labs(color = "Series") 
gg12 <- hi_monthly %>% 
  ggplot(aes(x=Month, y = Case)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1.3) +
  autolayer(ma(hi_monthly$Case, order=3, centre=FALSE), series="3-MA", size = 1) +
  autolayer(ma(hi_monthly$Case, order=5, centre=FALSE), series="5-MA", size = 1) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30 , hjust = 1)) +
  ggtitle("Hawaii") + 
  labs(color = "Series") 
gg13 <- dc_monthly %>% 
  ggplot(aes(x=Month, y = Case)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1.3) +
  autolayer(ma(dc_monthly$Case, order=3, centre=FALSE), series="3-MA", size = 1) +
  autolayer(ma(dc_monthly$Case, order=5, centre=FALSE), series="5-MA", size = 1) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30 , hjust = 1)) +
  ggtitle("District of Columbia") + 
  labs(color = "Series") 
gg14 <- ma_monthly %>% 
  ggplot(aes(x=Month, y = Case)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1.3) +
  autolayer(ma(ma_monthly$Case, order=3, centre=FALSE), series="3-MA", size = 1) +
  autolayer(ma(ma_monthly$Case, order=5, centre=FALSE), series="5-MA", size = 1) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30 , hjust = 1)) +
  ggtitle("Massachusetts") + 
  labs(color = "Series")
gg15 <- us_monthly %>% 
  ggplot(aes(x=Month, y = Case)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "#99CC00") +
  geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1.3) +
  autolayer(ma(us_monthly$Case, order=3, centre=FALSE), series="3-MA", size = 1) +
  autolayer(ma(us_monthly$Case, order=5, centre=FALSE), series="5-MA", size = 1) +
  labs(x = "Month", y = "Case count") +
  theme(axis.text.x = element_text(angle = 30 , hjust = 1)) +
  ggtitle("United States") + 
  labs(color = "Series")

## pie plot ####
par(mfrow = c(2,2))
par(mar=c(1.25, 0.75, 1.25, 0.75))
# pie1
case_by_age <- data.frame(table(mydf$age_group))
colnames(case_by_age) <- c('Age', 
                           'Case')
pct <- round(case_by_age$Case/sum(case_by_age$Case)*100)
Age <- paste(case_by_age$Age, pct) 
Age <- paste(Age,"%") 
pie(case_by_age$Case,labels = Age, font=1,
    main = "Age", radius = 0.75)
# pie2
case_by_sex <- data.frame(table(mydf$sex))
case_by_sex <- case_by_sex[-3,]
colnames(case_by_sex) <- c('Sex', 
                           'Case')
pct <- round(case_by_sex$Case/sum(case_by_sex$Case)*100)
Sex <- paste(case_by_sex$Sex, pct) 
Sex <- paste(Sex,"%",sep="") 
pie(case_by_sex$Case,labels = Sex,
    font=1,
    main = "Gender",
    radius = 0.75)
# pie3
case_by_sym<- data.frame(table(mydf$symptom_status))
case_by_sym <- case_by_sym[-3,]
colnames(case_by_sym) <- c('Symptom', 
                           'Case')
pct <- round(case_by_sym$Case/sum(case_by_sym$Case)*100)
Sym <- paste(case_by_sym$Symptom, pct) 
Sym <- paste(Sym,"%") 
pie(case_by_sym$Case,labels = Sym, font=1,
    main = "Symptom", radius = 0.75)
# pie4
case_by_cur<- data.frame(table(mydf$current_status))
colnames(case_by_cur) <- c('Status', 
                           'Case')
pct <- round(case_by_cur$Case/sum(case_by_cur$Case)*100)
Cur <- paste(case_by_cur$Status, pct) 
Cur <- paste(Cur,"%") 
pie(case_by_cur$Case,labels = Cur, font=1,
    main = "Status", radius = 0.75)
#*************************************************

#*************************************************
# SQL ####
library(RSQL) #Generate and Process 'SQL' Queries in R
library(RSQLite) #Can create an in-memory SQL database

# Build the placeholder
mydb <- dbConnect(drv = RSQLite::SQLite(),
                  dbname = ":memory:")

dbWriteTable(conn = mydb, 
             name = "covid19_data",
             value = mydf
)

dbWriteTable(conn = mydb, 
             name = "state_monthly_case",
             value = bind
)

# dbRemoveTable(mydb, name = "Covid-19 Data")

dbListTables(mydb)
dbListFields(mydb, "covid19_data")

## query1 ####
# Count number of cases from California by race
query <- paste0("SELECT race, 
                          count(*) as case_count,
                          count(*) * 100.0/ sum(count(*)) over () as case_percent
                  FROM covid19_data
                  WHERE res_state = 'CA'
                  GROUP BY race;")
res <- DBI::dbGetQuery(mydb, statement = query); res

## query2 ####
# Create a function to access the state monthly data
find_monthly_case <- function(state) {
  query <- paste0("
    SELECT res_state, case_month, count(*) as case_count
    FROM covid19_data
    WHERE res_state = '", state, "'
    GROUP BY case_month
    ORDER BY case_month;"
  )
  res <<- DBI::dbGetQuery(mydb, statement = query); 
  print(res)
}

find_monthly_case("HI")
# find_monthly_case("DC")

## query3 ####
# find the maximum case, month of each state and order by the case number
query <- paste0("SELECT res_state, 
                          case_month as maxcase_month, 
                          MAX(case_count) as maxcase_count
                  FROM (SELECT *, count(*) as case_count
                        FROM covid19_data
                        GROUP BY res_state, case_month)
                  GROUP BY res_state
                  ORDER BY maxcase_count DESC;")
res <- DBI::dbGetQuery(mydb, query)
head(res)
tail(res)
#*************************************************

#*************************************************
# String ####
library(stringr)

min(nchar(mydf$ethnicity)) 
max(nchar(mydf$ethnicity)) 
mean(nchar(mydf$ethnicity)) 
median(nchar(mydf$ethnicity)) 

# find length of strings in each column
# age
age_list <- t(rbind(unique(mydf$age_group),
                    nchar(unique(mydf$age_group))))
age_len <- data.frame(matrix(unlist(age_list), 
                             nrow=length(age_list)/2, 
                             byrow=FALSE))
colnames(age_len) <- c("String_Name", "Length")

# race
race_list <- t(rbind(unique(mydf$race),
                     nchar(unique(mydf$race))))
race_len <- data.frame(matrix(unlist(race_list), 
                              nrow=length(race_list)/2, 
                              byrow=FALSE))
colnames(race_len) <- c("String_Name", "Length")

# ethnicity
ethnicity_list <- t(rbind(unique(mydf$ethnicity),
                          nchar(unique(mydf$ethnicity))))
ethnicity_len <- data.frame(matrix(unlist(ethnicity_list), 
                             nrow=length(ethnicity_list)/2, 
                             byrow=FALSE))
colnames(ethnicity_len) <- c("String_Name", "Length")

# process
process_list <- t(rbind(unique(mydf$process),
                        nchar(unique(mydf$process))))
process_len <- data.frame(matrix(unlist(process_list),
                                 nrow=length(process_list)/2, 
                                 byrow=FALSE))
colnames(process_len) <- c("String_Name", "Length")

# current status
status_list <- t(rbind(unique(mydf$current_status),
                       nchar(unique(mydf$current_status))))
status_len <- data.frame(matrix(unlist(status_list), 
                                nrow=length(status_list)/2, 
                                byrow=FALSE))
colnames(status_len) <- c("String_Name", "Length")

# symptom status
symp_list <- t(rbind(unique(mydf$symptom_status),
                     nchar(unique(mydf$symptom_status))))
symp_len <- data.frame(matrix(unlist(symp_list), 
                              nrow=length(symp_list)/2, 
                              byrow=FALSE))
colnames(symp_len) <- c("String_Name", "Length")

# aggregate results
str_len <- rbind(age_len, race_len, ethnicity_len, process_len, status_len, symp_len)
str_len$Length <- as.integer(str_len$Length)
str_len_sort <- str_len %>% 
  arrange(desc(Length)) %>% 
  distinct(); 
head(str_len_sort)
tail(str_len_sort)

# replace strings
mydf$age_group <- str_replace_all(mydf$age_group, 
                                  "0 - 17 years",
                                  "0 to 17 years"
                                  )
mydf$current_status <- str_replace_all(mydf$current_status, 
                                  "Laboratory-confirmed case",
                                  "Lab-confirmed"
                                  )
mydf$current_status <- str_replace_all(mydf$current_status, 
                                       "Probable Case",
                                       "Probable"
                                       )
mydf$symptom_status <- str_replace_all(mydf$symptom_status, 
                               "nul",
                               "Null"
                               )
mydf$icu_yn <- str_replace_all(mydf$icu_yn, 
                                       "nul",
                                       "Null"
                               )

head(mydf)
tail(mydf)
#*************************************************

#*************************************************
# Advanced Works ####
## dv1 ####
bind <- rbind(fl_monthly, 
              il_monthly, 
              az_monthly,
              pa_monthly,
              oh_monthly,
              nj_monthly,
              ma_monthly,
              hi_monthly)
dv1 <- 
  ggplot(bind, aes(x=State, y=Case, fill=State)) + 
  theme_bw() +
  geom_violin()

## dv2 ####
# stacked area chart
library(viridis)
library(hrbrthemes)
bind <- rbind(ca_monthly,
              ny_monthly,
              fl_monthly, 
              il_monthly, 
              az_monthly,
              pa_monthly,
              oh_monthly,
              nj_monthly,
              ma_monthly,
              dc_monthly,
              hi_monthly)
bind$State <- factor(bind$State, 
                     levels=c("CA","NY","FL","IL","AZ","PA","OH","NJ","MA","DC","HI"))
Month_Over_Time <- as.numeric(bind$Month)
dv2 <-
  ggplot(bind, aes(x=Month_Over_Time, y=Case, fill=State)) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  geom_area()

## dv3 ####
# hexbin choropleth map
library(rgeos)
library(geojsonio)
library(RColorBrewer)

case_by_50state <- case_by_state
case_by_50state$State <- sort(case_by_50state$State)
case_by_50state$State <- state.name[match(case_by_50state$State,state.abb)]
case_by_50state$State[8] <- "District of Columbia"

case_by_50state <- na.omit(case_by_50state)
row.names(case_by_50state) <- 1:nrow(case_by_50state)

spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

library(broom)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Prepare binning
case_by_50state$bin <- cut(case_by_50state$Case, 
                           breaks=c(0,
                                    50000,
                                    100000,
                                    300000,
                                    1000000,
                                    2000000,
                                    Inf), 
                          labels=c("<50", 
                                   "50-100", 
                                   "100-300", 
                                   "300-1000", 
                                   "1000-2000", 
                                   "2000+"), 
                          include.lowest = TRUE)

# Merge geospatial and numerical information
spdf_fortified <- spdf_fortified %>%
  left_join(. , case_by_50state, by=c("id"="State")) 

# Prepare a color scale coming from the viridis color palette
my_palette <- rev(rocket(8))[c(-1,-8)]

dv3 <-
  ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes(fill = bin, x = long, y = lat, group = group), 
               size=0, 
               alpha=0.9) +
  geom_text(data=centers, 
            aes(x=x, y=y, label=id), 
            color="white", 
            size=3, 
            alpha=0.6) +
  theme_void() +
  scale_fill_manual( 
    values=my_palette, 
    name="Range of Case Number (in thousands)", 
    guide = guide_legend( keyheight = unit(3, units = "mm"), 
                          keywidth=unit(12, units = "mm"), 
                          label.position = "bottom", 
                          title.position = 'top', 
                          nrow=1)) +
  ggtitle( "Covid-19 Trend in United States of America" ) +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#cfe2f3", color = NA), 
    panel.background = element_rect(fill = "#cfe2f3", color = NA), 
    legend.background = element_rect(fill = "#cfe2f3", color = NA),
    plot.title = element_text(size= 24, 
                              hjust=0.5, 
                              color = "#22211d", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

## grid view ####
library(gridExtra)
library(grid)
lay <- rbind(c(1,1,1,2,2),
             c(1,1,1,2,2),
             c(3,3,3,2,2),
             c(3,3,3,2,2))
grid.arrange(gg1,gg2,gg3,layout_matrix = lay)

lay <- rbind(1,2,3,3)
grid.arrange(dv1,dv2,dv3,layout_matrix = lay)

## functional programming ####
### ggplots for states ####
library(usdata)
func_monthly <- function(state) {
  mydf_state <- subset(mydf, res_state == paste0(state))
  state_monthly <- data.frame(table(mydf_state$case_month))
  state_monthly <- data.frame(state_monthly, 
                           cm = cummean(state_monthly$Freq),
                           cs = cumsum(state_monthly$Freq),
                           state = paste0(state))
  colnames(state_monthly) <- c('Month', 
                            'Case', 
                            'Cumulative_Mean',
                            'Cumulative_Sum',
                            'State')
  return(state_monthly)
}

state_abb <- sort(unique(mydf$res_state))
bind_all <- data.frame()
for (i in 1:length(state_abb)){
  bind_all <- rbind(bind_all,func_monthly(state_abb[i]))
}

gg <- list()
state_abb <- state_abb[-20]

for (i in 1:length(state_abb)){
  state_name <- abbr2state(state_abb[i])
  if (state_abb[i] == "GU"){
    state_name <- "Guam"
  }
  if (state_abb[i] == "PR"){
    state_name <- "Puerto Rico"
  }
  if (state_abb[i] == "VI"){
    state_name <- "Virgin Islands"
  }
  gg[[i]] <- local({ 
    subset(bind_all, State == state_abb[i]) %>% 
    ggplot(aes(x=Month, y = Case)) +
    theme_bw() +
    geom_bar(stat = "identity", fill = "darkgoldenrod1") +
    geom_line(aes(x=Month, y=Cumulative_Mean, group=1, col = "Cumulative_Mean"), size = 1) +
    autolayer(ma(subset(bind_all, State == state_abb[i])$Case, order=3, centre=FALSE), 
              series="3-MA", 
              size = 1) +
    autolayer(ma(subset(bind_all, State == state_abb[i])$Case, order=5, centre=FALSE), 
              series="5-MA", 
              size = 1) +
    labs(x = "Month", y = "Case count") +
    theme(axis.text.x = element_text(angle = 30 , hjust = 1)) +
    ggtitle(state_name) + 
    labs(color = "Series")
  })
}

marrangeGrob(gg, ncol=3, nrow=4, byrow = TRUE)

### stacked plot  ####
state_abb_sort <- as.character(case_by_state[order(-case_by_state$Case),]$State)
bind <- bind_all
bind$State <- factor(bind$State, 
                     levels=state_abb_sort)
Month_Over_Time <- as.Date(paste(bind$Month,"-01",sep=""))
stacked_plot <-
  ggplot(bind, aes(x=Month_Over_Time, y=Case, fill=State)) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  geom_area() +
  theme(plot.background = element_rect(fill = "#F5F4EF", color = NA),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  xlab("Month")

### box plot ####
library(colorspace)

i <- 1
k <- 1
boxpt <- list()
while (i <= 54 ) {
  j <- i+4
  if(j > 54){
    j <- 54
  }
  boxpt[[k]] <- local({ 
    ggplot(subset(bind_all, bind_all$State == c(state_abb_sort[i:j])), 
           aes(x = State, y = Case, color = State)) +
    geom_boxplot(aes(color = State, fill = after_scale(desaturate(lighten(color, .6), .6))),
                 size = 1) +
    geom_point()+
    scale_color_brewer(palette = "Dark2", guide = "none") +
    labs(x = "State", y = "Case") +
    coord_flip() +
    theme_light()  
  })
  j <- j+1
  i <- i+5
  k <- k+1
}
marrangeGrob(boxpt, ncol=2, nrow=3, byrow = TRUE)

### alternatives ####
g <-
  ggplot(subset(bind_all, bind_all$State == c(state_abb_sort[2:7])), 
         aes(x = State, y = Case)) +
  scale_color_brewer(palette = "Dark2", guide = "none")
g + 
  theme_light() + 
  geom_boxplot()+
  geom_point()

g + 
  theme_light() + 
  geom_violin(size = 1, alpha = .5) +
  coord_flip()

### interactive ####
library(plotly)
us_monthly$Month <- as.Date(paste(us_monthly$Month,"-01",sep=""))
p <- us_monthly %>%
  ggplot(aes(x=Month, y=Case)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("U.S. Covid-19 Case") +
  theme_ipsum()
p <- ggplotly(p)

library(xts)
library(lubridate)
library(dygraphs)
don <- xts(x = us_monthly$Case, order.by = us_monthly$Month)
p2 <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

