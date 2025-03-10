
#LBN data cleanup (2022)

library(readxl)
library(tidyverse)
library(lubridate)

#extract babine data by shift

pershift <- read_excel("MASTER2022_Babine fence shifts_July15-Oct12.xlsx",
           sheet = "PerTrapDataEntry")

#who counted the most sockeye?
perperson <- pershift %>% 
  mutate(total.salmon = sum(lg.SK,jk.SK,CO,PK,lg.CH,jk.CH,
                           na.rm=T)) %>% 
  group_by(staffname) %>% 
  summarize(start.date = min(as_date(date)),end.date = max(as_date(date)),
            days = length(unique(date)),sum.lg.SK = sum(lg.SK, na.rm=T),
            sum.jk.SK = sum(jk.SK, na.rm=T),
            sum.PK = sum(PK, na.rm=T),sum.CO = sum(CO, na.rm=T),
            sum.lg.CH = sum(lg.CH, na.rm=T),sum.jk.CH = sum(jk.CH, na.rm=T)) %>% 
  mutate(prop.jk.SK = round(sum.jk.SK/sum.lg.SK,3)) %>% 
  arrange(desc(sum.lg.SK)) 
 


camera <- pershift %>% 
  filter(trap %in% "3-cam") %>% 
  mutate(hour = hour(starttime), min = 0, 
         date.time = ymd_hm(paste(date, hour, min)))



# Cam QA

#has every hour been reviewed? 
date.frame <- tibble(date = rep(as_date(ymd("2022-07-15"):
                                          ymd("2022-10-12")),24)) %>% 
  arrange(date)

time <- tibble(hour = rep(00:23,90), min= 0) 

time.frame <- cbind(date.frame, time) %>% 
  mutate(date.time = ymd_hm(paste(date,hour,min)))

check <- time.frame %>% 
  left_join(camera, by="date.time") %>% 
  filter(date.x >= ymd("2022-07-29")) #this was when started counting by hour

#summarize as totals per day
cam.summary <- camera %>% 
  group_by(date) %>% 
  summarize(sum.lg.SK = sum(lg.SK, na.rm=T),sum.jk.SK = sum(jk.SK, na.rm=T))

ggplot()+
  geom_line(data=cam.summary, aes(x=date, y=sum.lg.SK), col="blue")+
  geom_line(data=cam.summary, aes(x=date, y=sum.jk.SK), col="green")


#extract manual data
manual <- pershift %>% 
  filter(!(trap %in% "3-cam"))


#summarize as totals per day
manual.summary <- manual %>% 
  group_by(date) %>% 
  summarize(sum.lg.SK = sum(lg.SK, na.rm=T),sum.jk.SK = sum(jk.SK, na.rm=T))

ggplot()+
  geom_line(data=manual.summary, aes(x=date, y=sum.lg.SK), col="blue")+
  geom_line(data=manual.summary, aes(x=date, y=sum.jk.SK), col="green")






# 
# dates.babine <- excel_sheets("2022_Babine_Fence_Daily_Counts.xlsx")
# dates.babine <- dates.babine[43:84]
# #
# # #cleaner dates, check that they match
#  date.range <- as_date(c(ymd("2021-09-01"):ymd("2021-10-12")))
#  data.frame(dates.babine,date.range)
# 
# 
#  summer.babine <- data.frame(date=NA, trap="1-2,4-7",
#                              START = NA, STOP=NA,
#                              STAFFNAME = NA, lg.SK = NA,jk.SK =NA,
#                               lg.CO=NA,jk.CO=NA,
#                               PK=NA, lg.CH=NA,jk.CH=NA, ST=NA,
#                               BTDV=NA,crew="LBN")
# 
#   for (i in 1:length(dates.babine)){
#      tmp <- read_excel("2022_Babine_Fence_Daily_Counts.xlsx",
#               sheet = dates.babine[i],range = "A6:K34",col_names = F) %>% 
#        mutate()
#      rbind(summer.babine,tmp)
#       }
#    head(summer.babine)
#  # 
#  #  write_csv(summer.babine, "Daily.tally.BABINE.csv")
#  
#  
#  
#  
#  
# summary <- read_excel("2022_Babine_Fence_Daily_Counts.xlsx", 
#                       sheet = "Daily Summary",skip = 2)

# names(summary) <- c("date","lg.SK","jk.SK","CO","PK","lg.CH",
#                     "jk.CH","ST","char","FSC.SK","Comm.jk.SK",
#                     "Comm.lg.SK","time.open","time.closed",
#                     "")






















# #take out slashes and commas and make seperate rows xXxX DIDN'T WORK
# eg <- read_excel("2022_Babine_Fence_Daily_Counts.xlsx", 
#            sheet="July 29", range = c("A6:K13"),
#            col_names = c("start.time","stop.time","staff.name",
#                          "lg.SK","jk.SK","CO","jk.CO","PK",
#                          "lg.CH","jk.CH", "ST"))
# str(eg)
# eg$staff.name
# 
# 
# #rbind(eg[grep("/",eg$staff.name),],eg[grep("/",eg$staff.name),])
# 
# slashes <- eg[grep("/",eg$staff.name),] 
# %>% 
#   filter(staff.name %in% eg[grep("/",eg$staff.name),"staff.name"]) %>% 
#   mutate(staff1 = sub("/.","",staff.name))
# 
# [sub("/.*", "",eg$staff.name),"staff.name"]
# 
# 
# class(sub("/.*", "",eg$staff.name))
# sub(".*/", "",eg$staff.name)
# 
# sub("/.*", "",eg$staff.name[grep("/",eg$staff.name)])
# sub(".*/", "",eg$staff.name[grep("/",eg$staff.name)])
# 
# sub(",.*", "",eg$lg.SK)
# sub(".*,", "",eg$lg.SK)
# 







