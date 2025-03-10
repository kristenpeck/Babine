
# this is to analyze the camera and non-camera data from 2022 during the trial year
# Focal species is sockeye 

#added to after 2023 season ending

# Author: Kristen started 24-May-2023

library(tidyverse)
library(readxl)
library(dplyr)

pertrap <- read_excel("MASTER2022_Babine fence shifts_July15-Nov16-copy.xlsx", 
           sheet="PerTrapDataEntry", skip=4, 
           col_names = c("Date","Trap","num.traps","gate.start.time","gate.end.time",
                         "start.time","end.time","staff",
                         "lg.SK","jk.SK","CO","PK","lg.CH","jk.CH",
                         "ST","BTDV","RBCT","MW","SU","Lamprey","NPM",
                         "KO","comment","QA")) %>% 
  mutate(gate.start.time.hr = substr(as.character(gate.start.time), 12, 13),
         gate.start.time.min = substr(gate.start.time, 15, 16),
         gate.start.time = ymd_hm(paste0(Date,"_",gate.start.time.hr,"-",gate.start.time.min))) %>% 
  mutate(gate.end.time.hr = substr(as.character(gate.end.time), 12, 13),
         gate.end.time.min = substr(gate.end.time, 15, 16),
         gate.end.time = ymd_hm(paste0(Date,"_",gate.end.time.hr,"-",gate.end.time.min))) %>% 
  mutate(start.time.hr = substr(as.character(start.time), 12, 13),
         start.time.min = substr(start.time, 15, 16),
         start.time = ymd_hm(paste0(Date,"_",start.time.hr,"-",start.time.min))) %>% 
  mutate(end.time.hr = substr(as.character(end.time), 12, 13),
         end.time.min = substr(end.time, 15, 16),
         end.time = ymd_hm(paste0(Date,"_",end.time.hr,"-",end.time.min))) %>% 
  select(-c(gate.start.time.hr,gate.start.time.min,
            gate.end.time.hr,gate.end.time.min,
            start.time.hr,start.time.min,
            end.time.hr,end.time.min))



str(pertrap)

unique(pertrap$Trap)
range(pertrap$Date)


#truncate dates to what we have full data for

pertrap.summer <- pertrap %>% 
  filter(Date >= ymd("2022-07-16") & Date <= ymd("2022-11-25")) %>% 
  mutate(gate.duration = difftime(gate.end.time,gate.start.time, units="hours")) %>% 
  mutate(duration = difftime(end.time,start.time, units="hours")) %>% 
  mutate(duration.trap = duration/num.traps)

unique(pertrap.summer$Trap)

pertrap.summer.daily.cam <- pertrap.summer %>% 
  mutate(camera = ifelse(Trap %in% "3-cam",T,F)) %>% 
  group_by(Date,camera) %>% 
  summarize(duration = sum(duration, na.rm=T), 
            CO.daily = sum(CO, na.rm=T))

pertrap.summer.daily.staff <- pertrap.summer %>% 
  mutate(camera = ifelse(Trap %in% "3-cam",T,F)) %>% 
  filter(camera %in% F) %>% 
  group_by(Date,staff) %>% 
  summarize(duration = sum(duration, na.rm=T), 
            CO.daily = sum(CO, na.rm=T))

# check CO rates between cam and man

ggplot(pertrap.summer.daily.cam[pertrap.summer.daily.cam$Date >= ymd("2022-08-10"),])+
  geom_bar(aes(x=as_date(Date), y=CO.daily, fill=camera), stat="identity")

ggplot(pertrap.summer.daily.staff[pertrap.summer.daily.staff$Date >= ymd("2022-08-10"),])+
  geom_bar(aes(x=as_date(Date), y=CO.daily, fill=staff), stat="identity")


#this doesnt quite work yet
ggplot(pertrap.summer)+
  geom_bar(aes(x=Date, y=gate.duration), stat = "identity")
range(pertrap.summer$Date)

#Does the camera coho rate match the magnitude and timing of the 
#manual chutes?

pertrap.summer



# 2023 in-summer camera
# trying to sort out how many were missed early season due to wildfire delay


#errors in here
pershift <- read_excel("BabineFence_2023-preliminaryALL.xlsx", 
                      sheet="PerShiftDataEntry", skip=2, 
                      col_names = c("Date","Trap","start.time","end.time",
                                    "staff",
                                    "lg.SK","jk.SK","CO","PK","lg.CH","jk.CH",
                                    "ST","BTDV","RBCT","MW","SU","Lamprey","NPM",
                                    "KO","comment","QA")) %>% 
  mutate(start.time.hr = substr(as.character(start.time), 12, 13),
         start.time.min = substr(start.time, 15, 16),
         start.time = ymd_hm(paste0(Date,"_",start.time.hr,"-",start.time.min))) %>% 
  mutate(end.time.hr = substr(as.character(end.time), 12, 13),
         end.time.min = substr(end.time, 15, 16),
         end.time = ymd_hm(paste0(Date,"_",end.time.hr,"-",end.time.min))) %>% 
  select(-c(start.time.hr,start.time.min,
            end.time.hr,end.time.min))


daily.count <- read_excel("BabineFence_2023-preliminaryALL.xlsx", 
                       sheet="Daily Summary", skip=4, 
                       col_names = c("Date",
                                     "lg.SK","jk.SK","CO","PK","lg.CH","jk.CH",
                                     "ST","BTDV"))
daily.count.raw <- 
  read_excel("BabineFence_2023-preliminaryALL.xlsx", 
             sheet="Daily Summary", range = "A3:AA145",
             col_typ = c("date","numeric","numeric","numeric","numeric",
                         "numeric","numeric","numeric","numeric",
                         "numeric","numeric","numeric","numeric",
                         "numeric","numeric","numeric","numeric",
                         "numeric",
                         "date","numeric","numeric","numeric",
                         "date","numeric","numeric","numeric",
                         "text"))
str(daily.count.raw)  
names(daily.count.raw) <- c("date","lg.SK","jk.SK","CO",
                             "PK","lg.CH","jk.CH","ST","BTDV",
                             "morts.SK","morts.CO","morts.PK","morts.CH","morts.Other",
                             "FSC.SK","Sale.jk.SK", "Sale.lg.SK",
                             "TotalSK2fence","env.time1",
                             "watertemp1","waterlevel1","airtemp1",
                             "env.time2",	"watertemp2",
                             "waterlevel2","airtemp2",	"comments") 


daily.count <- daily.count.raw %>% 
  
  group_by(date) %>% 
  mutate(lg.SK = as.integer(sum(lg.SK,FSC.SK,Sale.lg.SK,morts.SK, na.rm=T))) %>% 
  mutate(jk.SK = as.integer(sum(jk.SK,Sale.jk.SK, na.rm=T))) %>%
  mutate(CO = as.integer(CO), PK = as.integer(PK)) %>% 
  mutate(date = as_date(date)) %>% 
  select(c(date,lg.SK,jk.SK,CO,PK,lg.CH,jk.CH,ST,BTDV))

str(daily.count)

daily.count.SK  <- daily.count %>% 
  filter(date >= ymd("2023-07-20")) %>% 
  group_by(date) %>% 
  mutate(SK = sum(lg.SK, jk.SK)) %>% 
  ungroup() %>% 
  mutate(cumul.SK = cumsum(SK)) %>% 
  select(date, SK, cumul.SK) 

daily.count.SK %>% 
  summarize( tot.SK = sum(SK))

ggplot(daily.count)+
  geom_line(aes(x=date, y=lg.SK), col="blue")+
  geom_line(aes(x=date, y=jk.SK), col="red")

ggplot(daily.count.SK)+
  geom_line(aes(x=date, y=cumul.SK))






