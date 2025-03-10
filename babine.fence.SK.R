
# Babine Fence 

# This script is used to clean, QA and analyse the Babine fish fence data
#for Coho extension/Holtby expansion see babine.fence.CO.R and fishcam repo



library(plyr)
library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(lubridate)


#### ggplot themes ####
theme_babine4 <- function(base_size = 14) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      # L'ensemble de la figure
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      # Zone où se situe le graphique
      #panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Les axes
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text.x = element_text(size = rel(0.80), face = "bold", 
                                 angle=60, hjust = 1.2, vjust=1.2),
      axis.text.y = element_text(size = rel(0.80), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      # La légende
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      # Les étiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.75), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}

theme_babine5 <- function(base_size = 14) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      # L'ensemble de la figure
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      # Zone où se situe le graphique
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Les axes
      axis.title = element_text(size = rel(1.2), face = "bold"),
      axis.text.x = element_text(size = rel(1.2), face = "bold", 
                                 angle=60, hjust = 1, vjust=1),
      axis.text.y = element_text(size = rel(1.2), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      # La légende
      legend.position = c(0.9,0.75),
      legend.title = element_text(size = rel(1.2), face = "bold"),
      legend.text = element_text(size = rel(1), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      # Les étiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}



### QAd FENCE DATA ####


##### Compile QA'd fence data (2014 to 2024 (Missing 2020 raw data)) ####

ct <- c("date", "numeric","numeric","numeric","numeric","numeric",
        "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
        "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")

fence2014 <- read_excel("./fencedata/BabineFence_2014-daily reporting_FINAL.xlsx", 
                        sheet="Daily Summary",range = "A4:V84",
                        col_names = c("date","lgSK","jkSK","CO","PK","lgCH","jkCH",
                                      "ST","Char","SK.morts","CO.morts","PK.morts",
                                      "CH.morts","Other.morts","FSC.SK","FSC.CO",
                                      "FSC.PK","FSC.CH", "FSC.ST",
                                      "DemojkSK","DemoLgSK","DemoPK"),
                        col_types = ct) 
fence2015 <- read_excel("./fencedata/BabineFence_2015-daily reporting_FINAL.xlsx", 
                        sheet="Daily Summary",range = "A4:V81",
                        col_names = c("date","lgSK","jkSK","CO","PK","lgCH","jkCH",
                                      "ST","Char","SK.morts","CO.morts","PK.morts",
                                      "CH.morts","Other.morts","FSC.SK","FSC.CO",
                                      "FSC.PK","FSC.CH", "FSC.ST",
                                      "DemojkSK","DemoLgSK","DemoPK"),
                        col_types = ct)
fence2016 <- read_excel("./fencedata/BabineFence_2016-daily reporting_FINAL.xlsx", 
                        sheet="Daily Summary",range = "A4:V97",
                        col_names = c("date","lgSK","jkSK","CO","PK","lgCH","jkCH",
                                      "ST","Char","SK.morts","CO.morts","PK.morts",
                                      "CH.morts","Other.morts","FSC.SK","FSC.CO",
                                      "FSC.PK","FSC.CH", "FSC.ST",
                                      "DemojkSK","DemoLgSK","DemoPK"),
                        col_types = ct)
fence2017 <- read_excel("./fencedata/BabineFence_2017-daily reporting_FINAL.xlsx", 
                        sheet="Daily Summary",range = "A4:V93",
                        col_names = c("date","lgSK","jkSK","CO","PK","lgCH","jkCH",
                                      "ST","Char","SK.morts","CO.morts","PK.morts",
                                      "CH.morts","Other.morts","FSC.SK","FSC.CO",
                                      "FSC.PK","FSC.CH", "FSC.ST",
                                      "DemojkSK","DemoLgSK","DemoPK"),
                        col_types = ct)
fence2018 <- read_excel("./fencedata/BabineFence_2018-daily reporting_FINAL.xlsx", 
                        sheet="Daily Summary",range = "A4:V82",
                        col_names = c("date","lgSK","jkSK","CO","PK","lgCH","jkCH",
                                      "ST","Char","SK.morts","CO.morts","PK.morts",
                                      "CH.morts","Other.morts","FSC.SK","FSC.CO",
                                      "FSC.PK","FSC.CH", "FSC.ST",
                                      "DemojkSK","DemoLgSK","DemoPK"),
                        col_types = ct)
fence2019 <- read_excel("./fencedata/BabineFence_2019-daily reporting_FINAL.xlsx", 
                        sheet="Daily Summary",range = "A4:V86",
                        col_names = c("date","lgSK","jkSK","CO","PK","lgCH","jkCH",
                                      "ST","Char","SK.morts","CO.morts","PK.morts",
                                      "CH.morts","Other.morts","FSC.SK","FSC.CO",
                                      "FSC.PK","FSC.CH", "FSC.ST",
                                      "DemojkSK","DemoLgSK","DemoPK"),
                        col_types = ct)

#note that we did not have the raw data sheets for 2020, so could not QA
#the sp. totals here are assumed to be be total run like in most older data:
fence2020 <- read_excel("./fencedata/Babine Adult Weir Count-sourceStockdrive_copy9-Mar-2025.xls",
                        sheet = "2010-2022", range = "FQ13:FX110",
                        col_names = c("date","lgSK","jkSK","PK","CO","lgCH","jkCH",
                                      "ST"))
fence2021 <- read_excel("./fencedata/BabineFence_2021-daily reporting_FINAL.xlsx", 
                        sheet="Daily Summary",range = "A4:V138",
                        col_names = c("date","lgSK","jkSK","CO","PK","lgCH","jkCH",
                                      "ST","Char","SK.morts","CO.morts","PK.morts",
                                      "CH.morts","Other.morts","FSC.SK","FSC.CO",
                                      "FSC.PK","FSC.CH", "FSC.ST",
                                      "DemojkSK","DemoLgSK","DemoPK"),
                        col_types = ct)
fence2022 <- read_excel("./fencedata/BabineFence_2022-daily reporting_FINAL.xlsx", 
                        sheet="Daily Summary",range = "A4:V128",
                        col_names = c("date","lgSK","jkSK","CO","PK","lgCH","jkCH",
                                      "ST","Char","SK.morts","CO.morts","PK.morts",
                                      "CH.morts","Other.morts","FSC.SK","FSC.CO",
                                      "FSC.PK","FSC.CH", "FSC.ST",
                                      "DemojkSK","DemoLgSK","DemoPK"),
                        col_types = ct)
fence2023 <- read_excel("./fencedata/BabineFence_2023-daily reporting_FINAL.xlsx", 
                        sheet="Daily Summary",range = "A14:V145",
                        col_names = c("date","lgSK","jkSK","CO","PK","lgCH","jkCH",
                                      "ST","Char","SK.morts","CO.morts","PK.morts",
                                      "CH.morts","Other.morts","FSC.SK","FSC.CO",
                                      "FSC.PK","FSC.CH", "FSC.ST",
                                      "DemojkSK","DemoLgSK","DemoPK"),
                        col_types = ct)
fence2024 <- read_excel("./fencedata/BabineFence_2024-daily reporting_FINAL.xlsx", 
                        sheet="Daily Summary",range = "A4:V140",
                        col_names = c("date","lgSK","jkSK","CO","PK","lgCH","jkCH",
                                      "ST","Char","SK.morts","CO.morts","PK.morts",
                                      "CH.morts","Other.morts","FSC.SK","FSC.CO",
                                      "FSC.PK","FSC.CH", "FSC.ST",
                                      "DemojkSK","DemoLgSK","DemoPK"),
                        col_types = ct)

fence20142024 <- rbind(fence2014, fence2015, fence2016, fence2017, fence2018, fence2019, 
                       fence2021, fence2022, fence2023, fence2024) %>% #excludes 2020
  mutate(year = year(date), fyear = as.factor(year), yday = yday(date)) 



#### HISTORIC FENCE DATA ####


yr.names <- as.character(c(1946, 1947,1949:1963,1965:2013)) #do not have cdaily counts from 1948 or 1964

fencehistoric.raw.lgSK <- read_excel("./fencedata/Babine Adult Weir Count-sourceStockdrive_copy9-Mar-2025.xls",
           sheet = "RAW LG SOCKEYE DATA", range = "A395:BO569", col_names = c("date.int",yr.names)) %>% 
  mutate(day = substr(date.int, 9,10),
         month = substr(date.int, 6,7)) %>% 
  select(-c(date.int)) %>% 
  pivot_longer(cols = !c(day,month),
               names_to = "year",
               values_to = "lgSK") %>% 
  mutate(date = ymd(paste0(year,"-",month,"-",day)),
         yday = yday(date),
         year = as.numeric(year)) %>% 
  select(year,date,yday,lgSK)

fencehistoric.raw.jkSK <- read_excel("./fencedata/Babine Adult Weir Count-sourceStockdrive_copy9-Mar-2025.xls",
            sheet = "JK SOCKEYE DATA", range = "A396:BO570", col_names = c("date.int",yr.names)) %>% 
  mutate(day = substr(date.int, 9,10),
         month = substr(date.int, 6,7)) %>% 
  select(-c(date.int)) %>% 
  pivot_longer(cols = !c(day,month),
               names_to = "year",
               values_to = "jkSK") %>% 
  mutate(date = ymd(paste0(year,"-",month,"-",day)),
         yday = yday(date),
         year = as.numeric(year)) %>% 
  select(year,date,yday,jkSK)

fencehistoric.raw.PK <- read_excel("./fencedata/Babine Adult Weir Count-sourceStockdrive_copy9-Mar-2025.xls",
            sheet = "PINK DATA", range = "A396:BO570", col_names = c("date.int",yr.names)) %>% 
  mutate(day = substr(date.int, 9,10),
         month = substr(date.int, 6,7)) %>% 
  select(-c(date.int)) %>% 
  pivot_longer(cols = !c(day,month),
               names_to = "year",
               values_to = "PK") %>% 
  mutate(date = ymd(paste0(year,"-",month,"-",day)),
         yday = yday(date),
         year = as.numeric(year)) %>% 
  select(year,date,yday,PK)

fencehistoric <- fencehistoric.raw.lgSK %>% 
  left_join(fencehistoric.raw.jkSK) %>% 
  left_join(fencehistoric.raw.PK) %>% 
  filter(!is.na(lgSK)) %>% #this filters out dates before the program started/after it ended
  arrange(date)

fence2020 <- fence2020 %>%
  filter(date >= ymd("2020-07-16")) %>% #start date
  mutate(yday = yday(date),
         year = year(date)) %>% 
  select(year,date,yday,lgSK,jkSK,PK)

fence.all <- fence20142024 %>% 
  mutate(date = as_date(date),
          lgSK = lgSK+SK.morts+FSC.SK+DemoLgSK,
         jkSK =jkSK+DemojkSK,
         PK = PK+PK.morts+FSC.PK+DemoPK) %>% 
  select(year,date,yday,lgSK,jkSK,PK) %>% 
  rbind(fencehistoric, fence2020) %>% 
  arrange(date)

#export csv of all observed daily counts
#write_csv(fence.all, "Babine.fence.daily.obs.lgSK-jkSK-PK1946-2024.csv")

fence.all.long <- fence.all %>% 
  pivot_longer(cols = !c(year,date,yday),
               names_to = "species",
               values_to = "total")

fence.observed.yrly <- fence.all.long %>% 
  group_by(year,species) %>% 
  summarize(total.obs = sum(total, na.rm=T),
            first.day = as.Date(first(date)),
            first.yday = yday(first.day),
            last.day = as.Date(last(date)))

y <- fence.observed.yrly[fence.observed.yrly$species %in%"PK",]

#plot the totals by species over the years
ggplot(fence.observed.yrly[fence.observed.yrly$year %in% c(2010:2024),])+
  geom_line(aes(x=year, y=total.obs, col=species))+
  scale_x_continuous(breaks = seq(2010,2024,1))

#plot the start date. Note in recent years the goal is to install by the second monday in July.
# the 2nd Monday is appr. :
yday(dmy("11-Jul-2016"))#193
yday(dmy("10-Jul-2017"))#191
yday(dmy("09-Jul-2018"))#190
yday(dmy("08-Jul-2019"))#189
yday(dmy("13-Jul-2020"))#195
yday(dmy("12-Jul-2021"))#193
yday(dmy("11-Jul-2022"))#192
yday(dmy("10-Jul-2023"))#191
yday(dmy("08-Jul-2024"))#190
# appr 192 

ggplot(fence.observed.yrly)+
  geom_point(aes(x=year, y=first.yday))+
  geom_hline(aes(yintercept = 192))

as_date(210, origin = ymd("2025-01-01")) #appr. end of July

#which years started later than late July (julian 210)?

fence.timing <- fence.all %>% 
  group_by(year) %>% 
  summarize(first.day = as.Date(first(date)),
            first.yday = yday(first.day),
            last.day = as.Date(last(date))) %>% 
  mutate(late = ifelse(first.yday >= 210, T, F))

fence.timing[fence.timing$late %in% T,]


#### visualize run timing SOCKEYE####

#visualize timing of the run thru season (borrowed from my babine.fence.CO.R)
yr.select <- 2000:2024

fence.lgSK <- fence.all %>% 
  select(year,date, yday,lgSK) %>% 
  left_join(fence.observed.yrly[fence.observed.yrly$species %in% "lgSK",c("year","total.obs")], by="year") %>% 
  group_by(year) %>% 
  mutate(cumulsum = cumsum(lgSK),daily.prop = lgSK/total.obs, 
         cumulprop = round(cumulsum/total.obs,4))

fence.jkSK <- fence.all %>% 
  select(year,date, yday,jkSK) %>% 
  left_join(fence.observed.yrly[fence.observed.yrly$species %in% "jkSK",c("year","total.obs")], by="year") %>% 
  group_by(year) %>% 
  mutate(cumulsum = cumsum(jkSK),daily.prop = jkSK/total.obs, 
         cumulprop = round(cumulsum/total.obs,4))

#setup
x <- data.frame(year=yr.select,start=as_date(NA),first.quart = as_date(NA), 
                median = as_date(NA),third.quart = as_date(NA), end = as_date(NA))
#proportion of the run 
props <- c(0.1,0.25,0.5,0.75,0.9,1)

#this is fast, but could turn into function if too laggy
for (i in 1:length(yr.select)){
  data <- fence.lgSK %>% 
    filter(year %in% yr.select[i])
  x[i,"start"] <- data[first(which(abs(data$cumulprop-props[1])==min(abs(data$cumulprop-props[1])))),"date"]
  x[i,"first.quart"] <- data[first(which(abs(data$cumulprop-props[2])==min(abs(data$cumulprop-props[2])))),"date"]
  x[i,"median"] <- data[first(which(abs(data$cumulprop-props[3])==min(abs(data$cumulprop-props[3])))),"date"]
  x[i,"third.quart"] <- data[first(which(abs(data$cumulprop-props[4])==min(abs(data$cumulprop-props[4])))),"date"]
  x[i,"end"] <- data[first(which(abs(data$cumulprop-props[5])==min(abs(data$cumulprop-props[5])))),"date"]
}

date.of.operation <- fence.lgSK %>% 
  filter(year %in% yr.select) %>% 
  group_by(year) %>% 
  arrange(date) %>% 
  summarize(first.day = date[1], last.day = tail(date,1)) %>% 
  mutate(last.julian = yday(last.day))


timing.long <- x %>%
  pivot_longer(!year, names_to="phase", values_to = "date") %>% 
  mutate(yday = yday(date)) 

summary(lm(yday~year, data = timing.long[timing.long$phase %in% "first.quart",]))
summary(lm(yday~year, data = timing.long[timing.long$phase %in% "median",]))
#from the above: long term trends (from 1950) are pretty flat, but from 2000 
#   onwards shows a non-significant later returning trend. Should take into account late starts/migration 
#   from extended freshets (see Stiff et al 2015)

ggplot(data=timing.long)+
  geom_point(aes(x=year,y=yday, col=phase))+
  geom_line(aes(x=year,y=yday, col=phase))+
  geom_smooth(aes(x=year,y=yday, col=phase),method = "lm")

tmp <- fence.lgSK %>% 
  filter(year %in% yr.select) %>% 
  left_join(x, by="year")  %>% 
  # mutate(period = ifelse(Date < start, "start",
  #                        ifelse(Date >= start & Date <= first.quart, "first.quart",
  #                               ifelse(Date >= first.quart & Date <= median, "median",
  #                                      ifelse(Date >= median & Date <= third.quart, "third.quart",
  #                                             ifelse(Date >= third.quart & Date <= end, "end",
  #                                                    ifelse(Date >= end, "final", NA))))))) %>%
  mutate(period = ifelse(date < first.quart, "first quarter",
                         ifelse(date >= first.quart & date < third.quart, "middle half",
                                ifelse(date >= third.quart, "last quarter", NA)))) %>%
  mutate(periodf = factor(period, levels=c("first quarter","middle half","last quarter"),
                          ordered=T)) %>% 
  mutate(fake.date = as_date(yday, origin = ymd("2025-01-01")))
tmp1 <- tmp %>%
  filter(year %in% c(1950:1989))
tmp2 <- tmp %>%
  filter(year %in% c(1990:1999))
tmp3 <- tmp %>%
  filter(year %in% c(2000:2024))

plot.dist.timing <- ggplot(data=tmp3)+
  geom_ribbon(aes(x=fake.date, ymin=0, ymax=lgSK, fill=periodf))+
  geom_line(aes(x=fake.date, y=lgSK), col="black")+
  facet_wrap(~year,scales = "free_y")+
  labs(x="Date",y="Daily lg SK",fill="")+
  theme_babine4()
plot.dist.timing

 # ggsave(plot=plot.dist.timing, filename = "lgSk_runtiming2000-2024.png",
 #        device="png",width=10, height=6)



#calc ave. daily proportion of the run for a period of time
ave.daily.prop.lgSK9324 <- fence.lgSK %>% 
  filter(year >= 1993) %>% 
  group_by(yday) %>% 
  summarize(ave.daily.prop.lgSK = mean(daily.prop, na.rm=T)) %>% 
  mutate(year.cat = "1993-2024",  
         fake.date= as_date(yday, origin = ymd("2025-01-01")))
#ave.daily.prop.jkSK = mean(daily.prop.jkSK, na.rm=T))
 

#### interpolation using run timing ####

#interpolate for six days in 2018 wildfire (julian days 235 to 240 need filling):
#calc daily proportion of the run for a period of time
ggplot(ave.daily.prop.lgSK9324)+
  geom_line(aes(x=yday, y=ave.daily.prop.lgSK))+
  geom_line(data=fence.lgSK[fence.lgSK$year %in% 2018,], aes(x=yday,y=daily.prop),col="red")
 
ggplot(fence.lgSK[fence.lgSK$year %in% 2018,])+
  geom_density(aes(x=lgSK), fill="blue", alpha=0.5)

#look at smoothed kde instead of daily ave proportion
ggplot(ave.daily.prop.lgSK9324)+
  geom_density(aes(x=ave.daily.prop.lgSK))


ave.daily.prop.lgSK9317 <- fence.lgSK %>% 
  filter(year %in% 1993:2017) %>% 
  group_by(yday) %>% 
  summarize(ave.daily.prop.lgSK = mean(daily.prop, na.rm=T)) %>% 
  mutate(year.cat = "1993-2017")

ave.daily.prop.lgSK.10yr <- fence.lgSK %>% 
  filter(year %in% c(2013:2017,2019:2023)) %>% 
  group_by(yday) %>% 
  summarize(ave.daily.prop.lgSK = mean(daily.prop, na.rm=T)) %>% 
  mutate(year.cat = "2013:2017,2019:2023")



infill.fence2018 <- fence.lgSK %>% 
  left_join(ave.daily.prop.lgSK.10yr,by="yday") %>% 
  mutate(lgSK = ifelse(year %in% 2018 & yday %in% 235:240, 
                         total.obs*ave.daily.prop.lgSK,lgSK),
         infill = ifelse(year %in% 2018 & yday %in% 235:240,T,NA)) %>% 
  filter(year %in%2018)
infill.fence2018 %>% 
  filter(year %in% 2018) %>% 
  summarize(total.obs = sum(lgSK))

ggplot()+
  geom_line(data=infill.fence2018, aes(x=yday,y=lgSK))+
  geom_point(data=infill.fence2018, aes(x=yday,y=lgSK, col=infill))

#interpolate for 2011:

#interpolate for late start in 2011 (julian days 195 to 222 need predicting):
#calc daily proportion of the run for a period of time
ggplot(ave.daily.prop.lgSK9324)+
  geom_line(aes(x=yday, y=ave.daily.prop.lgSK))+
  geom_line(data=fence.lgSK[fence.lgSK$year %in% 2011,], aes(x=yday,y=daily.prop),col="red")

ave.daily.prop.lgSK9310 <- fence.lgSK %>% 
  filter(year %in% 1993:2010) %>% 
  group_by(yday) %>% 
  summarize(ave.daily.prop.lgSK = mean(daily.prop, na.rm=T)) %>% 
  mutate(year.cat = "1993-2010")

ave.daily.prop.lgSK.10yr <- fence.lgSK %>% 
  filter(year %in% c(2006:2010,2012:2016)) %>% 
  group_by(yday) %>% 
  summarize(ave.daily.prop.lgSK = mean(daily.prop, na.rm=T)) %>% 
  mutate(year.cat = "2006:2010,2012:2016")

infill.fence <- fence.lgSK %>% 
  right_join(ave.daily.prop.lgSK.10yr,by="yday") %>% 
  mutate(lgSK = ifelse(year %in% 2011 & yday %in% 195:222, 
                       total.obs*ave.daily.prop.lgSK,lgSK),
         infill = ifelse(year %in% 2011 & yday %in% 195:222,T,F))
infill.fence %>% 
  filter(year %in% 2011) %>% 
  summarize(total.obs = sum(lgSK))


#interpolation using Fulton/Pinkut channel/river/surplus regression (1992, 2007)

cols <- read_excel("./fencedata/Tablesupdates_2025_updateKP-copy7-Mar-2025.xlsx", sheet = "Wood95",
                   range = "A12:AV12", .name_repair = "universal")
noquote(names(cols))
wood95 <- read_excel("./fencedata/Tablesupdates_2025_updateKP-copy7-Mar-2025.xlsx", sheet = "Wood95",
                     skip = 14, col_names = names(cols)) %>% 
  select(year = ...3,lgSK = Babine.Fence.Count..adults.,
         BLDPeffective.sp = Fulton...Pinkut.Effective.Esc.,
         babine.fence.catch=Catch.at.Babine.Fence...Above,
         BLDPreturn = Pinkut...Fulton.Total.Adult.Visual.Counts,
         BLDPsurplus = Fulton...Pinkut.Estimated.Surplus,
         BLDPsurplus.calcd = Potential.Surplus..corrected.) %>% 
  mutate(BLDPtotal = BLDPreturn+BLDPsurplus)
wood95

(lm.pre1991 <- summary(lm(lgSK~BLDPtotal, data=wood95[wood95$year %in% c(1950:1991,1993),])))
ggplot(wood95[wood95$year  %in% c(1950:1991,1993),])+
  geom_smooth(aes(x=BLDPtotal, y=lgSK), method = "lm",se = T)

#regression formula for back-calculating 1992:
lm.pre1991$coeff[1]+lm.pre1991$coeff[2]*1233785 
# nope, that's not it.. it is actually more of a regression on the calc'd surplus to the visual surplus

(lm.surplus19501993 <- summary(lm(BLDPsurplus.calcd~BLDPsurplus, 
                                  data=wood95[wood95$year %in% c(1950:1991,1993),])))
#regression formula for back-calculating 1992:
lm.surplus19501993$coeff[1]+lm.surplus19501993$coeff[2]*400000 #1992 visual surplus
# this gets pretty close to what was presented in the paper (681364). Possibly there is a transcription error somewhere


# so if this was used for 2007, maybe they used an updated group of years?
(lm.surplus19502010 <- summary(lm(BLDPsurplus.calcd~BLDPsurplus, 
                                  data=wood95[wood95$year %in% c(1950:1991,1993:2006,2008:2010),])))
#regression formula for back-calculating 1992:
lm.surplus19502010$coeff[1]+lm.surplus19502010$coeff[2]*116490 #2007 visual surplus
# mmm... doesn't quite get there. Possibly they didn't actually use this method for 2007


#### Environmental Conditions #### 

# Babine R at outlet of Nilkitkwa L: 08EC013 (just d/s of fence)
# Babine R at Fort Babine: 08EC001 (at Babine lake outlet)
# X Skeena R at Glen Vowell: 08EB003 * big time gap between 1985 and 2022
# Skeena R above Babine: 08EB005
# Skeena R at Usk: 08EF001

library(tidyhydat)
#download_hydat()


#####Co-Management Plan ####
# Babine River hydro station just d/s of the fence:

hy_stations(station_number = "08EC013")

bab.flows.real.raw <- realtime_dd(station_number = "08EC013")
bab.flows.hist.raw <- hy_daily_flows(station_number = "08EC013")

range(bab.flows.real.raw$Date)
range(bab.flows.hist.raw$Date)

###### temperature ####

temp_08EC013.raw <- read_excel("Water_Temp.Telemetry@08EC013.EntireRecord.xlsx", 
                               sheet= "Water_Temp.Telemetry@08EC013.En",skip = 14) %>% 
  mutate(Value = ifelse(Value %in% 99999, NA, Value)) %>% 
  mutate(date = as_date(`Timestamp (UTC-08:00)`)) %>% 
  mutate(yday = yday(date),year = year(date),
         fake.date = as_date(yday-1,origin = ymd("2024-01-01")))

temp_08EC013 <- read_excel("Water_Temp.Telemetry@08EC013.EntireRecord.xlsx", 
                           sheet= "Water_Temp.Telemetry@08EC013.En",skip = 14) %>% 
  mutate(Value = ifelse(Value %in% 99999, NA, Value)) %>% 
  mutate(date = as_date(`Timestamp (UTC-08:00)`)) %>% 
  group_by(date) %>% 
  summarize(min.daily.deg = min(Value, na.rm=T),
            mean.daily.deg = mean(Value, na.rm=T),
            max.daily.deg = max(Value, na.rm=T)) %>% 
  mutate(yday = yday(date),year = year(date),
         fake.date = as_date(yday-1,origin = ymd("2024-01-01")))
str(temp_08EC013)

#max temps
plot.temp_08EC013 <- ggplot(temp_08EC013[temp_08EC013$year>=2016,])+
  geom_line(aes(x=fake.date, y=max.daily.deg)) + 
  geom_hline(aes(yintercept = 18), col="gray50", linetype="dashed")+
  geom_hline(aes(yintercept = 20), col="gray50", linetype="dotted")+
  scale_x_date(limits = c(ymd("2024-06-15"),ymd("2024-09-30")),
               date_breaks = "1 week", date_labels = "%b-%d")+
  scale_y_continuous(limits = c(10,23), breaks = seq(10,22,2))+
  facet_wrap(~year) +
  labs(x="Date",y="Water Temperature (deg. C)",
       title="Max. daily water temperature (June-Sept.), Stn 08EC013")+
  theme(axis.text.x = element_text(hjust=1, angle=45))
plot.temp_08EC013

# ggsave(filename = "plot.temp_08EC013byyr.png",plot = plot.temp_08EC013, 
#        device = "png", width = 8, height = 6)

#min temps
plot.temp_08EC013min <- ggplot(temp_08EC013[temp_08EC013$year>=2016,])+
  geom_line(aes(x=fake.date, y=min.daily.deg)) + 
  geom_hline(aes(yintercept = 18), col="gray50", linetype="dashed")+
  geom_hline(aes(yintercept = 20), col="gray50", linetype="dotted")+
  scale_x_date(limits = c(ymd("2024-06-15"),ymd("2024-09-30")),
               date_breaks = "1 week", date_labels = "%b-%d")+
  scale_y_continuous(limits = c(10,23), breaks = seq(10,22,2))+
  facet_wrap(~year) +
  labs(x="Date",y="Water Temperature (deg. C)",
       title="Min. daily water temperature (June-Sept.), Stn 08EC013")+
  theme(axis.text.x = element_text(hjust=1, angle=45))
plot.temp_08EC013min

#max and min
#min temps
plot.temp_08EC013minmax <- ggplot(temp_08EC013[temp_08EC013$year>=2016,])+
  geom_line(aes(x=fake.date, y=min.daily.deg), col="gray50") + 
  geom_line(aes(x=fake.date, y=max.daily.deg), col="gray25") + 
  geom_hline(aes(yintercept = 18), col="gray50", linetype="dashed")+
  geom_hline(aes(yintercept = 20), col="gray50", linetype="dotted")+
  scale_x_date(limits = c(ymd("2024-06-15"),ymd("2024-09-30")),
               date_breaks = "1 week", date_labels = "%b-%d")+
  scale_y_continuous(limits = c(10,23), breaks = seq(10,22,2))+
  facet_wrap(~year) +
  labs(x="Date",y="Water Temperature (deg. C)",
       title="Min. & Max. daily water temperature (June-Sept.), Stn 08EC013")+
  theme(axis.text.x = element_text(hjust=1, angle=45))
plot.temp_08EC013minmax

#ggsave(filename = "plot.temp_08EC013minmax2016-2024.png",
#       plot = plot.temp_08EC013minmax, 
#               device = "png", width = 8, height = 6)

plot.temp_08EC013minmax2024 <- ggplot(temp_08EC013[temp_08EC013$year>=2024,])+
  geom_line(aes(x=fake.date, y=min.daily.deg), col="gray50") + 
  geom_line(aes(x=fake.date, y=max.daily.deg), col="gray25") + 
  geom_hline(aes(yintercept = 18), col="gray50", linetype="dashed")+
  geom_hline(aes(yintercept = 20), col="gray50", linetype="dotted")+
  geom_vline(aes(xintercept = ymd("2024-08-03")), col="red")+
  scale_x_date(limits = c(ymd("2024-06-15"),ymd("2024-09-30")),
               date_breaks = "1 week", date_labels = "%b-%d")+
  scale_y_continuous(limits = c(10,23), breaks = seq(10,22,2))+
  facet_wrap(~year) +
  labs(x="Date",y="Water Temperature (deg. C)",
       title="Min. & Max. daily water temperature (June-Sept.), Stn 08EC013")+
  theme(axis.text.x = element_text(hjust=1, angle=45))+
  theme_babine4()
plot.temp_08EC013minmax2024

ggsave(plot = plot.temp_08EC013minmax2024, 
       filename = "plot.temp_08EC013minmax2024_2.png", width=7, height=5)



ggplot(temp_08EC013[temp_08EC013$year>=2016,])+
  geom_line(aes(x=fake.date, y=max.daily.deg, col=as_factor(year))) +
  geom_hline(aes(yintercept = 18), linetype="dashed")+
  geom_hline(aes(yintercept = 20), linetype="dotted")+
  scale_x_date(limits = c(ymd("2024-06-01"),ymd("2024-09-30")),
               date_breaks = "1 week", date_labels = "%b-%d")+
  scale_y_continuous(limits = c(8,23), breaks = seq(8,22,2))+
  labs(x="Date",y="Water Temperature (deg. C)",col="",
       title="Max. daily water temperature (June-Sept.), Stn 08EC013")+
  theme(axis.text.x = element_text(hjust=1, angle=45))

# ggsave(filename = "plot.temp_08EC013.png",plot = plot.temp_08EC013, 
#        device = "png", width = 8, height = 6)



#Find the average temperature during main Sockeye migration

# when do most Sockeye pass through the Babine River section?

babine.dailylgSK <- read_excel("Babine Adult Weir Count-1990-2022copy.xls", 
                               sheet="archived90-22LGSK") %>% 
  gather("year","lgSK",-DATE,na.rm = T) %>% 
  mutate(month = substr(DATE,6,7),day = substr(DATE,9,10),
         year = as.numeric(year), 
         date = ymd(paste0(year,"-",month,"-",day)),
         yday = yday(date),
         fake.date = as_date(yday(date), origin = "2024-01-01")) %>% 
  select(-c(DATE,month,day))
str(babine.dailylgSK)

babine.dailylgSK.1322 <- babine.dailylgSK %>% 
  filter(year %in% c(2013:2022), fake.date <= ymd("2024-10-10")) %>% 
  group_by(year) %>% 
  mutate(cumsumSK = cumsum(lgSK), totalSK = sum(lgSK), 
         prop.cumSK=cumsumSK/totalSK)

ggplot(babine.dailylgSK.1422)+
  geom_line(aes(x=fake.date, y=prop.cumSK, col=as.factor(year)))+
  scale_y_continuous(breaks = seq(0,1,.1))+
  scale_x_date(date_breaks = "1 week")+
  theme_babine4()

#from 2014-2022, the approximate first 10% of sockeye past by Aug 1st, 
# 90% pass by Sept 10th

ave.temp.by.day <- temp_08EC013.raw %>% 
  filter(fake.date %in% seq(ymd("2024-08-01"),ymd("2024-09-10"),1)) %>% 
  group_by(fake.date) %>% 
  summarize(min.temp = min(Value, na.rm=T),
            mean.temp = mean(Value, na.rm=T),
            max.temp = mean(Value, na.rm=T)) %>% 
  mutate(mean.plus1.5 = mean.temp+1.5, mean.minus1.5 = mean.temp-1.5)

ave.temp.1522 <- temp_08EC013.raw %>% 
  filter(fake.date %in% seq(ymd("2024-08-01"),ymd("2024-09-10"),1)) %>%
  summarize(mean.temp = mean(Value, na.rm=T),
            sd.temp = sd(Value, na.rm=T))
ave.temp.1522


plot.ave.temp.SKseason1522 <- ggplot(ave.temp.by.day)+
  geom_line(aes(x=fake.date, y=mean.temp))+
  geom_line(aes(x=fake.date, y=mean.plus1.5), linetype="dotted")+
  geom_line(aes(x=fake.date, y=mean.minus1.5), linetype="dotted")+
  geom_text(aes(x=ymd("2024-08-01"),y=14,
                label = paste0("Aug1-Sep10 Mean = ",
                               round(ave.temp.1522$mean.temp,2),
                               " deg C"), hjust=0))+
  scale_y_continuous(breaks = seq(10,21,1))+
  theme_babine4()+
  scale_x_date(date_breaks = "1 week", date_labels = "%b-%d")+
  labs(x="Date",y="Water Temp. (deg C)", 
       title = "Ave. daily water temperature +/- 1.5 deg. C (2015-2022 08EC013)")
plot.ave.temp.SKseason1522

# ggsave(plot = plot.ave.temp.SKseason1522, filename = "plot.ave.temp.SKseason1522.png",
#        device = "png", width=8, height=6)


# Flows

year.select <- c(1976:2024)

hydro.babine <- hy_daily_flows(station_number = c("08EC013")) %>% 
  mutate(Year = year(Date), julian = yday(Date)) %>% 
  filter(Year %in% year.select) %>% 
  #filter(julian<=278 & julian >= 213) %>% 
  mutate(fake.date = as_date(julian-1,origin="2024-01-01")) %>% 
  mutate(fyear = factor(Year, order = T))  %>% 
  select(-"Symbol")
#filter(julian %in% c(yday(ymd("2021-aug-01")),yday(ymd("2021-dec-01"))) ) %>% 
#filter(!is.na(year))

# bab.flows.real.raw <- realtime_dd(station_number = "08EC013") %>% 
#   mutate(Date = as_date(Date)) %>% 
#   select(STATION_NUMBER,Date, Parameter, Value) %>% 
#   group_by(STATION_NUMBER, Date, Parameter) %>% 
#   summarize(Value = mean(Value, na.rm=T)) %>% 
#   filter(Parameter %in% "Flow") %>% 
#   mutate(Year = year(Date), julian = yday(Date),fake.date = as_date(julian,origin="2023-01-01"),
#          fyear = factor(Year, order = T))


bab.flows.real.raw <- read_csv("08EC013_QR_20241130T0035.csv",skip = 10,
                               col_types = c("c","?","?","?","?"),
                               col_names = c("Date","Parameter","Value","Approval","Qualifier")) %>% 
  mutate(Date = ymd(substr(Date,1,10)), Parameter = "Flow") %>% 
  group_by(Date, Parameter) %>% 
  summarize(ave.flow = mean(Value, na.rm=T)) %>% #ave daily flow 
  select(Date,Parameter, Value=ave.flow) %>% 
  mutate(Year = year(Date), fyear = as.factor(Year),
         julian = yday(Date),fake.date = as_date(julian-1,origin="2024-01-01"),
         STATION_NUMBER = "08EC013", decade = "2020s")


setdiff(names(hydro.babine),names(bab.flows.real.raw))
names(hydro.babine)
names(bab.flows.real.raw)

hydro.babine <- rbind(hydro.babine, bab.flows.real.raw) %>% 
  mutate(decade = ifelse(Year %in% c(1940:1949), "1940s",
                         ifelse(Year %in% c(1950:1959), "1950s",
                                ifelse(Year %in% c(1960:1969), "1960s",
                                       ifelse(Year %in% c(1970:1979), "1970s",
                                              ifelse(Year %in% c(1980:1989), "1980s",
                                                     ifelse(Year %in% c(1990:1999), "1990s",
                                                            ifelse(Year %in% c(2000:2009), "2000s",
                                                                   ifelse(Year %in% c(2010:2019), "2010s", 
                                                                          ifelse(Year %in% c(2020:2029), "2020s", NA))))))))))






# Find the average levels from 2013-2023

level.babine.raw <- read_csv("08EC013_HG_20241121T1708.csv", skip = 10,
                             col_types = c("c","?","?","?","?"),
                             col_names = c("Date","Parameter","Value","Approval","Qualifier")) %>% 
  mutate(Date = ymd(substr(Date,1,10)), Parameter = "Level") %>% 
  mutate(Year = year(Date), julian = yday(Date),fake.date = as_date(julian-1,origin="2024-01-01"),
         fyear = factor(Year, order = T), STATION_NUMBER = "08EC013")

level.babine <- read_csv("08EC013_HG_20241121T1708.csv", skip = 10,
                         col_types = c("c","?","?","?","?"),
                         col_names = c("Date","Parameter","Value","Approval","Qualifier")) %>% 
  mutate(Date = ymd(substr(Date,1,10)), Parameter = "Level") %>% 
  group_by(Date, Parameter) %>% 
  summarize(ave.level = mean(Value, na.rm=T)) %>% 
  select(Date,Parameter, Value=ave.level) %>% 
  mutate(Year = year(Date), julian = yday(Date),fake.date = as_date(julian-1,origin="2024-01-01"),
         fyear = factor(Year), STATION_NUMBER = "08EC013")

historic.lvl.babine <- hy_daily_levels(station_number = c("08EC013")) %>% 
  mutate(Year = year(Date), julian = yday(Date)) %>% 
  filter(Year %in% year.select) %>% 
  #filter(julian<=278 & julian >= 213) %>% 
  mutate(fake.date = as_date(julian-1,origin="2024-01-01")) %>% 
  mutate(fyear = factor(Year, order = T))  %>% 
  select(-"Symbol")

levels.babine <- rbind(historic.lvl.babine, level.babine) %>% 
  mutate(decade = ifelse(Year %in% c(1940:1949), "1940s",
                         ifelse(Year %in% c(1950:1959), "1950s",
                                ifelse(Year %in% c(1960:1969), "1960s",
                                       ifelse(Year %in% c(1970:1979), "1970s",
                                              ifelse(Year %in% c(1980:1989), "1980s",
                                                     ifelse(Year %in% c(1990:1999), "1990s",
                                                            ifelse(Year %in% c(2000:2009), "2000s",
                                                                   ifelse(Year %in% c(2010:2019), "2010s", 
                                                                          ifelse(Year %in% c(2020:2029), "2020s", NA))))))))))





plot.lvl_08EC013 <- ggplot(levels.babine[levels.babine$Year>=2016,])+
  geom_line(aes(x=fake.date, y=Value)) + 
  geom_hline(aes(yintercept = 0.35), col="gray50", linetype="dashed")+
  geom_hline(aes(yintercept = 0.2), col="gray25", linetype="dotted")+
  scale_x_date(limits = c(ymd("2024-06-15"),ymd("2024-09-30")),
               date_breaks = "1 week", date_labels = "%b-%d")+
  scale_y_continuous(limits = c(0,1.4), breaks = seq(0,1.4,0.2))+
  facet_wrap(~Year) +
  labs(x="Date",y="Water Level (m)",
       title="Mean daily water level (June-Sept.), Stn 08EC013")+
  theme(axis.text.x = element_text(hjust=1, angle=45))
plot.lvl_08EC013

# ggsave(plot = plot.lvl_08EC013, filename = "plot.ave.lvl.1624.png",
#        device = "png", width=8, height=6)

plot.lvl_08EC013_2024 <- ggplot(levels.babine[levels.babine$Year>=2024,])+
  geom_line(aes(x=fake.date, y=Value)) + 
  geom_hline(aes(yintercept = 0.35), col="gray50", linetype="dashed")+
  geom_hline(aes(yintercept = 0.2), col="gray25", linetype="dotted")+
  #geom_vline(aes(xintercept = ymd("2024-08-03")), col="red")+
  scale_x_date(limits = c(ymd("2024-06-15"),ymd("2024-09-30")),
               date_breaks = "1 week", date_labels = "%b-%d")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2))+
  facet_wrap(~Year) +
  labs(x="Date",y="Water Level (m)",
       title="Mean daily water level (June-Sept.), Stn 08EC013")+
  theme(axis.text.x = element_text(hjust=1, angle=45))+
  theme_babine4()
plot.lvl_08EC013_2024

ggsave(plot= plot.lvl_08EC013_2024, filename = "plot.lvl_08EC013_2024_2.png",
       width=7, height=5)



## Ave levels 2014-2024

ave.daily.levels <- levels.babine %>% 
  filter(Year %in% c(2000:2023)) %>% 
  group_by(fake.date) %>% 
  summarize(min.daily = min(Value, na.rm=T),
            mean.daily = mean(Value, na.rm=T),
            max.daily = max(Value, na.rm=T),
            sd.daily = sd(Value, na.rm=T),
            quant25 = quantile(Value,probs = 0.25, na.rm=T),
            quant50 = quantile(Value,probs = 0.5, na.rm=T),
            quant75 = quantile(Value,probs = 0.75, na.rm=T))

lvl.ave.0023 <- levels.babine %>% 
  filter(Year %in% c(2000:2023), 
         fake.date %in% seq(ymd("2024-08-01"),ymd("2024-09-10"),1)) %>% 
  summarize(min = min(Value, na.rm=T),
            mean = mean(Value, na.rm=T),
            max = max(Value, na.rm=T),
            sd = sd(Value, na.rm=T),
            quant25 = quantile(Value,probs = 0.25, na.rm=T),
            quant50 = quantile(Value,probs = 0.5, na.rm=T),
            quant75 = quantile(Value,probs = 0.75, na.rm=T))
lvl.ave.0023

plot.ave.level.SKseason0023 <- ggplot(ave.daily.levels)+
  geom_line(aes(x=fake.date, y=mean.daily))+
  geom_line(aes(x=fake.date, y=mean.daily+sd.daily), linetype="dotted")+
  geom_line(aes(x=fake.date, y=mean.daily-sd.daily), linetype="dotted")+
  geom_text(aes(x=ymd("2024-08-01"),y=.2,
                label = paste0("Aug1-Sep10 Mean = ",
                               round(lvl.ave.0023$mean,2),
                               " m"), hjust=0))+
  scale_y_continuous(limits = c(0,1),breaks = seq(0,1,.2))+
  theme_babine4()+
  scale_x_date(limits = c(ymd("2024-08-01"),ymd("2024-09-10")),
               date_breaks = "1 week", date_labels = "%b-%d")+
  labs(x="Date",y="Water Level (m)", 
       title = "Ave. daily water level +/- 1 SD (2000-2023 08EC013)")
plot.ave.level.SKseason0023

# ggsave(plot=plot.ave.level.SKseason1424, filename = "plot.ave.level.SKseason1424.png",
#        device="png", width=8, height=6)


plot.ave.level.SKseason0023_2024 <- ggplot(ave.daily.levels)+
  geom_line(aes(x=fake.date, y=mean.daily), size=1.5)+
  geom_line(aes(x=fake.date, y=mean.daily+sd.daily), linetype="dotted")+
  geom_line(aes(x=fake.date, y=mean.daily-sd.daily), linetype="dotted")+
  geom_line(data = levels.babine[levels.babine$Year>=2024,],
            aes(x=fake.date, y=Value), col="red", size=1.5)+
  geom_text(aes(x=ymd("2024-08-01"),y=.2,
                label = "2024", hjust=0), col="red")+
  scale_y_continuous(limits = c(0,1),breaks = seq(0,1,.2))+
  theme_babine4()+
  scale_x_date(limits = c(ymd("2024-08-01"),ymd("2024-09-10")),
               date_breaks = "1 week", date_labels = "%b-%d")+
  labs(x="Date",y="Water Level (m)", 
       title = "Ave. daily water level +/- 1 SD (2000-2023 08EC013)")
plot.ave.level.SKseason0023_2024

ggsave(plot=plot.ave.level.SKseason0023_2024, 
       filename = "plot.ave.level.SKseason0023_2024.png", width=7, 
       height=5)


# find years with flows > 120 cms (Stiff et al 2015) in July

str(hydro.babine)

high.flows.bab <- hydro.babine %>% 
  mutate(month = month(Date)) %>% 
  filter(month %in% c(7:10)) %>% 
  filter(Value >= 120)

#which years had cms > than 120 since start of time series:
(high.water.yrs <- unique(high.flows.bab$Year))
length(high.water.yrs)

all.years<- unique(hydro.babine$Year)

low.water.yrs <- setdiff(all.years,high.water.yrs)


#Out of all years:
length(unique(hydro.babine$Year))

ggplot(high.flows.bab)+
  geom_line(aes(x=fake.date, y=Value, col=fyear))




##### 2024 flows compared to past years ####

decadal.flows <- hydro.babine %>% 
  filter(Year %in% c(1970:2022)) %>% 
  group_by(decade, fake.date) %>% 
  summarize(daily.min = min(Value), daily.max = max(Value))



bab.flows.real.raw23 <- bab.flows.real.raw %>% 
  filter(Year %in% 2023)
bab.flows.real.raw24 <- bab.flows.real.raw %>% 
  filter(Year %in% 2024)

ggsave(plot = ggplot()+
         geom_ribbon(data = decadal.flows, aes(x=fake.date, ymin=daily.min,
                                               ymax = daily.max, fill=decade), alpha = 0.5)+
         geom_line(data=bab.flows.real.raw23,aes(x=fake.date, y=Value), col="red", linewidth=1)+
         geom_text(aes(x=ymd("2024-08-31"), y=10,label = "2023"), col="red", size=5)+
         geom_line(data=bab.flows.real.raw24,aes(x=fake.date, y=Value), col="purple", linewidth=1.5)+
         geom_text(aes(x=ymd("2024-08-01"), y=15,label = "2024"), col="purple", size=5)+
         scale_x_date(limits = c(limits = c(ymd("2024-07-10"), ymd("2024-09-30"))),
                      date_labels = "%b-%d", date_breaks = "2 weeks")+
         scale_y_continuous(breaks = seq(0,220,20), limits = c(0,220))+
         theme_babine5()+
         labs(title="08EC013 (station d/s of Babine fence)", x="date",y="discharge (cms)", 
              fill="min-max \nby decade"),
       filename = "2023+2024flowsBabine.png",device="png",width = 10,height=6)

historic <- hydro.babine %>% 
  filter(Year %in% c(2015:2023)) %>% 
  group_by(fyear)

ggsave(plot=ggplot()+
         geom_line(data=historic,aes(x=fake.date, y=Value, group=fyear, col=fyear),  size=1.25, alpha=0.6)+
         #geom_line(data=bab.flows.real.raw23,aes(x=fake.date, y=Value), col="black",  size=1.25)+
         geom_line(data=bab.flows.real.raw24,aes(x=fake.date, y=Value), col="red",  size=1.25)+
         scale_x_date(limits = c(limits = c(ymd("2024-06-08"), ymd("2024-09-30"))),
                      date_labels = "%b-%d", date_breaks = "2 weeks")+
         geom_text(aes(x=ymd("2024-08-01"), y=15,label = "2024"), col="red", size=5)+
         scale_y_continuous(limits = c(0,250),breaks = seq(0,250,20))+
         labs(title="08EC013 (station d/s of Babine fence)", x="date",y="discharge (cms)", 
              col="year (2015 onward)")+
         theme(axis.text.x = element_text(hjust=1, angle=45)),
       filename = "2024flowsBabine2.png",device="png",width = 10,height=6)


#Skeena.flows.real.raw <- realtime_dd(station_number = "08EB005")
Skeena.flows.hist.raw <- hy_daily_flows(station_number = "08EB005")

#range(Skeena.flows.real.raw$Date)
range(Skeena.flows.hist.raw$Date)

hydro.skeena <- hy_daily_flows(station_number = c("08EB005")) %>% 
  mutate(Year = year(Date), julian = yday(Date)) %>% 
  filter(Year %in% year.select) %>% 
  #filter(julian<=278 & julian >= 213) %>% 
  mutate(fake.date = as_date(julian,origin="2021-01-01")) %>% 
  mutate(fyear = factor(Year, order = T))  
#filter(julian %in% c(yday(ymd("2021-aug-01")),yday(ymd("2021-dec-01"))) ) %>% 
#filter(!is.na(year))

yday(as_date("2021-oct-31"))

#compare flows at diff stations
ggplot()+
  geom_line(data=hydro.babine,aes(x=fake.date, y=Value), col="green", size=1) + 
  geom_line(data=hydro.skeena,aes(x=fake.date, y=Value), col="blue", size=1) + 
  scale_x_date(date_labels = "%b-%d")+
  facet_wrap(~Year)




#start date of fence for last 10 years and flows:
SK.LG.raw <- read_excel("Babine Adult Coho_expansion from SD.xlsx", sheet="LG SOCKEYE DATA",
                        col_names = T,range = "A394:BL569")

names(SK.LG.raw) <- c("Date",1946:1947,1949:1963,1965:2010)

SK.LG <- SK.LG.raw %>% 
  gather("Year","Count",-Date) %>% 
  mutate(Year= as.numeric(Year),day=day(Date),month=month(Date),julian=yday(Date),
         Date=ymd(paste(Year,month,day)),fake.date=as_date(julian,origin="2021-01-01")) %>% 
  filter(!is.na(Count))

start.date1946_2010 <- SK.LG %>% 
  group_by(Year) %>% 
  summarise(first = Date[1])
#write_csv(x = start.date1946_2010, file = "Babine.start.date1946_2010.csv")

start.date <- read_csv("Babine.start.date1946_2021.csv") %>% 
  mutate(day=substr(first.date,1,2), month = month(first.date), 
         date = dmy(paste(day,month,Year)),julian = yday(date),
         fake.date=as_date(julian,origin="2021-01-01")) %>% 
  filter(Year %in% year.select)

flow.at.start <- start.date %>% 
  left_join(hydro.babine, by=c("date"="Date","Year")) %>% 
  select(Year,first.date,Value) %>% 
  mutate(julian = yday(first.date), fake.date=as_date(julian,origin="2021-01-01"))

range(hydro.babine$Date)
ggplot()+
  geom_line(data=hydro.babine,aes(x=fake.date, y=Value), col="green", size=1) + 
  geom_vline(data=start.date, aes(xintercept=fake.date),col="red")+
  geom_hline(data=start.date, aes(yintercept=133),col="blue")+
  scale_x_date(date_labels = "%b-%d", limits = c(ymd("2021-07-01"),ymd("2021-08-31")))+
  labs(x="date",y="discharge (cms)")+
  facet_wrap(~Year)

ggplot(data=flow.at.start)+
  geom_point(aes(x=Year, y=julian, col=Value))

#plot together - don't really correlate at all, so take out hydro

babine.coho.2010s <- babine.all %>% 
  filter(Year %in% year.select)



ggplot()+
  geom_line(data = babine.coho.2010s,aes(x=as_date(fake.date),y=Count,
                                         col=as.factor(Year)), 
            size=2)+
  geom_line(data=hydro.skeena, aes(x=fake.date, y=Value), size=1) +
  facet_wrap(~Year)+
  scale_x_date(limits = c(as_date("2021-Jul-01"),as_date("2021-oct-31")),
               date_breaks= "1 week", date_labels = "%d%b")+
  labs(title="Babine Fence Coho",x="appr.date")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=6),title = element_text(size=6),
        legend.box = "vertical")






 #### SOCKEYE ####

##### Harvest vs. counting timing - 

pershift <- read_excel("BabineFence_2023-daily reporting MASTER_copy30-nov-2023.xlsx",
                       sheet = "PerShiftDataEntry",range = "A4:V1000", 
                       col_names = c("date","trap","trp_num","start_time","end_time","staffname",
                                     "lgSK","jkSK",
                                     "CO","PK","lgCH","jkCH","STHD","Char","RB","MW",
                                     "SU","Lamprey","NPM","KO","Comments","QA"),
                       col_types = c("date","text","guess","date","date","text","guess","guess",
                                     "guess","guess","guess","guess","guess","guess",
                                     "guess","guess","guess","guess","guess","guess",
                                     "guess","guess")) %>% 
  mutate(duration_hrs = ifelse(is.na(end_time)&trap %in% "CamPan23",1440,end_time-start_time)/60) %>% 
  mutate(trap_hrs = trp_num*duration_hrs) %>% 
  rowwise %>% 
  mutate(SKpertrap = round(sum(lgSK,jkSK, na.rm=T)/trp_num),
         SKpertrapperhour = SKpertrap/duration_hrs,
         SKpertraphr = round(sum(lgSK,jkSK, na.rm=T)/trap_hrs)) 

pershift.daily <- pershift %>% 
  group_by(trap, date) %>% 
  summarize(mean.SK.perhrpertrap = mean(SKpertrapperhour))

ggplot(pershift.daily)+
  geom_line(aes(x=date,y=mean.SK.perhrpertrap, col=trap))

ggplot(pershift.daily)+
  geom_bar(aes(x=date,y=mean.SK.perhr, fill=trap), position = "stack",
           stat = "identity")


daily <- read_excel("BabineFence_2023-daily reporting withTotalSK-13-Aug-2023.xlsx",
                    sheet = "Daily Summary", range="A3:AA132",
                    col_types = c("date","guess","guess","guess","guess","guess",
                                  "guess","guess","guess","guess","guess","guess",
                                  "guess","guess","guess","guess","guess", "guess",
                                  "guess","guess","guess","guess","guess", "guess",
                                  "guess","guess","guess"))

str(daily)




#### Historic Sockeye - compare this year's run of SK to ave  ####

babine.dailylgSK <- read_excel("Babine Adult Weir Count-1990-2023copy.xls", 
                               sheet="archived90-22LGSK") %>% 
  gather("year","lgSK",-DATE,na.rm = T) %>% 
  mutate(month = substr(DATE,6,7),day = substr(DATE,9,10),
         year = as.numeric(year), 
         date = ymd(paste0(year,"-",month,"-",day))) %>% 
  select(-c(DATE,month,day))


babine.dailyjkSK <- read_excel("Babine Adult Weir Count-1990-2023copy.xls", 
                               sheet="archived90-22JKSK") %>% 
  gather("year","jkSK",-DATE,na.rm = T) %>% 
  mutate(month = substr(DATE,6,7),day = substr(DATE,9,10),
         year = as.numeric(year), 
         date = ymd(paste0(year,"-",month,"-",day))) %>% 
  select(-c(DATE,month,day))


babine.dailySK <- babine.dailylgSK %>% 
  left_join(babine.dailyjkSK, by = c("date","year")) %>% 
  mutate(fyear = as.factor(year), julian = yday(date),
         fake.date = as_date(julian-1, origin = ymd("2024-01-01"))) %>% 
  #filter(year %in% low.water.yrs) %>% 
  #filter(year >= 1993) %>% 
  group_by(year) %>% 
  mutate(cumul.lgSK = cumsum(lgSK),cumul.jkSK = cumsum(jkSK)) %>% 
  mutate(total.lgSK = sum(lgSK), total.jkSK = sum(jkSK)) %>% 
  mutate(daily.prop.lgSK = lgSK/total.lgSK, daily.prop.jkSK = jkSK/total.jkSK)

#using charmaine's data instead (total return):
babine.dailySK <- read_csv("Babine_daily_fromCCH_30-Nov2023.csv") %>% 
  gather("year","lgSK",-DATE,na.rm = T) %>% 
  mutate(month = substr(DATE,4,6),day = substr(DATE,1,2),year = as.numeric(year),
         date = ymd(paste0(year,"-",month,"-",day)), 
         fyear = as.factor(year), 
         julian = yday(date),
         fake.date = as_date(julian-1, origin = ymd("2024-01-01"))) %>% 
  select(-c(month,day)) %>% 
  #filter(year %in% low.water.yrs) %>% 
  filter(year >= 1993) %>% 
  group_by(year) %>% 
  mutate(cumul.lgSK = cumsum(lgSK)) %>% 
  mutate(total.lgSK = sum(lgSK)) %>% 
  mutate(daily.prop.lgSK = lgSK/total.lgSK)




(annual.totals.SK <- babine.dailySK %>% 
    group_by(year) %>% 
    summarize(yrly.total.lgSK = sum(lgSK)))#, 
#yrly.total.jkSK = sum(jkSK)))

ave.daily.prop <- babine.dailySK %>% 
  group_by(julian) %>% 
  summarize(ave.daily.prop.lgSK = mean(daily.prop.lgSK, na.rm=T)) %>% 
  mutate(year = "1993-2023",  
         fake.date= as_date(julian-1, origin = ymd("2024-01-01")))
#ave.daily.prop.jkSK = mean(daily.prop.jkSK, na.rm=T))

unique(babine.dailySK$year)

ggplot(ave.daily.prop)+
  geom_line(aes(x=julian, y=ave.daily.prop.lgSK), col="blue")+
  #geom_line(aes(x=julian, y=ave.daily.prop.jkSK), col="red")+
  labs(x="date",y="ave. daily proportion \n(blue=large SK, red=jack SK)")




# SKruntiming <- babineSK.2024 %>% 
#   mutate(year = as.character(year)) %>% 
#   select(year,julian,ave.daily.prop.lgSK=prop.daily23) %>% 
#   rbind(ave.daily.prop) %>% 
#   mutate(fake.date = as_date(julian-1, origin = ymd("2023-01-01")))

# (plot.SKruntiming <- ggplot(SKruntiming)+
#   geom_line(aes(x=fake.date, y=ave.daily.prop.lgSK, col=year), linewidth = 1)+
#   scale_x_date(date_breaks = "2 weeks", date_labels = "%b - %d")+
#   labs(x="Date",y="Babine Fence: Ave. Daily Proportion",col="Year")+
#   theme_babine5())
# 
# ggsave(plot.SKruntiming,filename = "plot.SKruntiming2023.png",
#        width = 9,height = 6,device = "png")

plot.SKruntiming2024 <- ggplot()+
  geom_line(data=ave.daily.prop, aes(x=fake.date, y=ave.daily.prop.lgSK), 
            col="grey50", size=1.5)+
  geom_line(data=babineSK.2024, aes(x=fake.date, y=prop.daily), 
            col="black", size=1.5)+
  scale_x_date(limits=c(ymd("2024-07-01"),ymd("2024-10-31")), 
               date_breaks = "2 weeks", date_labels = "%b - %d")+
  labs(x="Date",y="Babine Fence: Ave. Daily Proportion",col="Year")+
  theme_babine5()
plot.SKruntiming2024

ggsave(plot.SKruntiming2024,filename = "plot.SKruntiming2024.png",
       width = 9,height = 6,device = "png")




#### PINKS #### 

# fence20142024 %>% 
#   filter(!is.na(DemoPK)) %>% 
#   group_by(year) %>% 
#   summarize(tot.PKdemo = sum(DemoPK)) #ask Karlena if this is actually a demo PK fishery or mislabeled FSC?
#sounds like these may have been permitted bycatch on the sockeye fishery



babine.pink.recent.long <- fence20142024 %>% 
  mutate(PK = PK+FSC.PK+DemoPK) %>% 
  select(year,date,yday,PK) 

babine.pink.long <- fencehistoric.raw.PK %>% 
  rbind(babine.pink.recent.long) %>% 
  mutate(cycle = ifelse(year %%2 == 0, "even","odd")) %>% 
  arrange(date)
babine.pink.long


babine.pink.recent.long <- fence20142024 %>% 
  mutate(PK = PK+FSC.PK+DemoPK) %>% 
  select(year,date,yday,PK) 

babine.pink.long <- babine.pink.historic %>% 
  mutate(date = as.Date(date.int, origin = "1899-12-30"),
         day = substr(date, 9,10),
         month = substr(date, 6,7)) %>% 
  select(-c(date.int,date)) %>% 
  pivot_longer(cols = !c(day,month),
               names_to = "year",
               values_to = "PK") %>% 
  mutate(date = ymd(paste0(year,"-",month,"-",day)),
         yday = yday(date),
         year = as.numeric(year)) %>% 
  select(year,date,yday,PK) %>% 
  rbind(babine.pink.recent.long) %>% 
  mutate(cycle = ifelse(year %%2 == 0, "even","odd")) %>% 
  arrange(date)
babine.pink.long

# ggplot(babine.pink.long)+
#   geom_line(aes(x=date,y=PK,col=year))+
#   facet_wrap(~year, scale = "free_x")+
#   theme(legend.position = "")
ggplot(babine.pink.long %>% 
         group_by(year,cycle) %>% 
         summarize(tot.PK = sum(PK, na.rm=T)) %>% 
         ungroup())+
  geom_point(aes(year, tot.PK))+
  geom_line(aes(year, tot.PK))+
  # scale_x_continuous(breaks=seq(min(babine.pink.long$year),
  #                               max(babine.pink.long$year),10))+
  scale_y_continuous(breaks=seq(0,550000,50000))+
  facet_wrap(~cycle)

#export pink data:
# not ready for that yet - interpolate like the sockeye data for missing values?


# #read in charmaine's data (total return): (skip - same as historic info)
# babine.dailySK <- read_csv("Babine_daily_fromCCH_30-Nov2023.csv") %>% 
#   gather("year","totlgSK",-DATE,na.rm = T) %>% 
#   mutate(month = substr(DATE,4,6),day = substr(DATE,1,2),year = as.numeric(year),
#          date = ymd(paste0(year,"-",month,"-",day)), 
#          fyear = as.factor(year), 
#          julian = yday(date),
#          fake.date = as_date(julian-1, origin = ymd("2024-01-01"))) %>% 
#   select(-c(month,day)) %>% 
#   #filter(year %in% low.water.yrs) %>% 
#    filter(year >= 2014) #%>% 
#   # group_by(year) %>% 
#   # mutate(cumul.lgSK = cumsum(totlgSK)) %>% 
#   # mutate(total.lgSK = sum(totlgSK)) %>% 
#   # mutate(daily.prop.lgSK = totlgSK/total.lgSK)
# 
ggplot()+
  geom_line(data = babine.dailySK,aes(x=julian, y=totlgSK), col="black", linewidth=2)+
  geom_line(data = fence20142024,aes(x=yday, y=lgSK, col=fyear))+
  facet_wrap(~fyear)

ggplot()+
  geom_line(data = fence20142024,aes(x=date, y=lgSK, col=fyear))+
  facet_wrap(~fyear, scales = "free_x")

(annual.totals.SK <- fence20142024 %>% 
    group_by(year) %>% 
    summarize(yrly.total.lgSK = sum(lgSK)))#, 
#yrly.total.jkSK = sum(jkSK)))









# rm(list = c("fence2014", "fence2015", "fence2016", "fence2017", "fence2018", "fence2019", 
#             "fence2021", "fence2022", "fence2023", "fence2024"))





# Mixed-Stock Genetic analysis thru fence ####

# this code is adapted from L. Warkentin's chinook Tyee data
# found in skeena-chinook/scripts/skeena-tyee-test-fishery-genetics-2023.R

# read long format data, multiple lines per fish, each row for collection / population
# (finer than CU sometimes) with a probability for that fish
# d <- read_xlsx("PID20230115_Babine_Fence_FSC(23)_sc50_2024-05-08_NF_week.xlsx", 
#                sheet="collections_ids") # KP we don't have this



# read in long format data, multiple lines per fish, each for for a timing group with 
# probability for that fish being assigned to a given timing group
d_cu <- read_xlsx("PID20230115_Babine_Fence_FSC(23)_sc50_2024-05-08_NF_week.xlsx", 
                  sheet="repunits_ids")
str(d_cu)
# read in wide format data, with rep units and top baseline collections it corresponds to
d_cu_wide <- read_xlsx("PID20230115_Babine_Fence_FSC(23)_sc50_2024-05-08_NF_week.xlsx", 
                       sheet="repunits_table_ids")
str(d_cu_wide)

# read in extraction sheet, with one line per fish, with date sampled 
es <- read_xlsx("PID20230115_Babine_Fence_FSC(23)_sc50_2024-05-08_NF_week.xlsx",
                sheet="extraction_sheet") %>% 
  mutate(catch_date = ymd(as.Date(as.numeric(CatchDate..YYYY.MM.DD.), origin = "1899-12-30")),
         catch_julian_date = as.integer(CatchJulDate), isoweek = isoweek(catch_date)) %>% 
  select(indiv, CatchYear,catch_date,catch_julian_date,isoweek,Fish,Vial)

# Read in key of CU names
cu_key <- read_xlsx("PID20230115_Babine_Fence_FSC(23)_sc50_2024-05-08_NF_week.xlsx",
                    sheet="baseline_collections")

# read in collection data
MGL_template <- read_excel("MGL-Mixed-stock tissue Babine River Fence Sockeye-2023.xlsx",
                           sheet="template",skip=17)
str(MGL_template)


# merge long format results with CU names
dfc <- d_cu %>% 
  left_join(es, by = "indiv")


sum.by.week <- dfc %>% 
  filter(rank %in% 1) %>% 
  group_by(isoweek) %>% 
  summarize(n.early = length(which(repunit %in% "Babine_Early")),
            n.mid = length(which(repunit %in% "Babine_Mid")),
            n.late = length(which(repunit %in% "Babine_Late")),
            perc.early = n.early/(n.early+n.mid+n.late),
            perc.mid = n.mid/(n.early+n.mid+n.late),
            perc.late = n.late/(n.early+n.mid+n.late))
sum.by.date <- dfc %>% 
  filter(rank %in% 1) %>% 
  group_by(catch_date) %>% 
  summarize(n.early = length(which(repunit %in% "Babine_Early")),
            n.mid = length(which(repunit %in% "Babine_Mid")),
            n.late = length(which(repunit %in% "Babine_Late")),
            perc.early = n.early/(n.early+n.mid+n.late),
            perc.mid = n.mid/(n.early+n.mid+n.late),
            perc.late = n.late/(n.early+n.mid+n.late))
sum.by.date

sum.by.date.long <- sum.by.date %>% 
  select(catch_date, perc.early,perc.mid,perc.late) %>% 
  pivot_longer(-catch_date) %>% 
  mutate(nameF = factor(name, levels=c("perc.early","perc.mid","perc.late")))

sum.by.week.long <- sum.by.week %>% 
  select(isoweek, perc.early,perc.mid,perc.late) %>% 
  pivot_longer(-isoweek) %>% 
  mutate(nameF = factor(name, levels=c("perc.early","perc.mid","perc.late")))

ggplot(sum.by.date.long)+
  geom_bar(aes(x=catch_date, y=value, 
               fill= nameF), 
           stat="identity") +
  scale_x_date(date_breaks = "1 week",date_labels = "%b-%d")+
  labs(title = "Rank 1 assignment",x="catch date",y="percent",
       fill="")

ggplot(sum.by.week.long)+
  geom_bar(aes(x=isoweek, y=value, 
               fill= nameF), 
           stat="identity") +
  #scale_x_date(date_breaks = "1 week",date_labels = "%b-%d")+
  labs(title = "Rank 1 assignment",x="catch week",y="percent",
       fill="")




#filter for probability > 85%

#how many indivs total:
length(unique(dfc$indiv))

#how many indivs assign over 85%:
dfc.over85 <- dfc %>% 
  filter(rep_pofz > 85) 

(tot.over85 <- dfc.over85  %>% 
    summarize(n.over85 = length(unique(indiv))) )

sum.by.date85 <- dfc.over85 %>% 
  group_by(catch_date) %>% 
  summarize(n.early = length(which(repunit %in% "Babine_Early")),
            n.mid = length(which(repunit %in% "Babine_Mid")),
            n.late = length(which(repunit %in% "Babine_Late")),
            perc.early = n.early/(n.early+n.mid+n.late),
            perc.mid = n.mid/(n.early+n.mid+n.late),
            perc.late = n.late/(n.early+n.mid+n.late))
sum.by.date85

sum.by.week85 <- dfc.over85 %>% 
  group_by(isoweek) %>% 
  summarize(n.early = length(which(repunit %in% "Babine_Early")),
            n.mid = length(which(repunit %in% "Babine_Mid")),
            n.late = length(which(repunit %in% "Babine_Late")),
            perc.early = n.early/(n.early+n.mid+n.late),
            perc.mid = n.mid/(n.early+n.mid+n.late),
            perc.late = n.late/(n.early+n.mid+n.late))
sum.by.week85

sum.by.date.long85 <- sum.by.date85 %>% 
  select(catch_date, perc.early,perc.mid,perc.late) %>% 
  pivot_longer(-catch_date) %>% 
  mutate(nameF = factor(name, levels=c("perc.early","perc.mid","perc.late")))

sum.by.week.long85 <- sum.by.week85 %>% 
  select(isoweek, perc.early,perc.mid,perc.late) %>% 
  pivot_longer(-isoweek) %>% 
  mutate(nameF = factor(name, levels=c("perc.early","perc.mid","perc.late")))


ggplot(sum.by.date.long85)+
  geom_bar(aes(x=catch_date, y=value, fill=nameF), stat="identity") +
  scale_x_date(date_breaks = "1 week",date_labels = "%b-%d")+
  labs(title = "Over 85% assignment",x="catch date",y="percent",
       fill="")
ggplot(sum.by.week.long85)+
  geom_bar(aes(x=isoweek, y=value, fill=nameF), stat="identity") +
  #scale_x_date(date_breaks = "1 week",date_labels = "%b-%d")+
  labs(title = "Over 85% assignment",x="catch week",y="percent",
       fill="")


# look at assignment to baseline collections

dfc_wide <- d_cu_wide %>% 
  select(indiv,top_collection,associated_collection_prob) %>% 
  left_join(es, by="indiv")

#how many indivs assign over 85%:
dfc_wide.over85 <- dfc_wide %>% 
  filter(associated_collection_prob > 85) 

sum.by.week_wide85 <- dfc_wide.over85 %>% 
  group_by(isoweek) %>% 
  summarize(n.pinkut = length(which(top_collection %in% "Pinkut_Cr")),
            n.morrison = length(which(top_collection %in% "Morrison_Cr")),
            n.4mile = length(which(top_collection %in% "Four_Mile_Cr")),
            n.fulton = length(which(top_collection %in% "Fulton_R")),
            n.pierre = length(which(top_collection %in% "Pierre_Cr")),
            n.tahlolower = length(which(top_collection %in% "Tahlo_Cr_Lower")),
            n.upperbabine = length(which(top_collection %in% "Upper_Babine_R")),
            total = sum(n.pinkut,n.morrison,n.4mile,n.fulton,n.pierre,
                        n.tahlolower,n.upperbabine, na.rm = T)) %>% 
  mutate(perc.pinkut = n.pinkut/total,
         perc.morrison = n.morrison/total,
         perc.4mile = n.4mile/total,
         perc.fulton = n.fulton/total,
         perc.pierre = n.pierre/total,
         perc.tahlolower = n.tahlolower/total,
         perc.upperbabine = n.upperbabine/total)
sum.by.week_wide85

sum.by.week.wide85 <- sum.by.week_wide85 %>% 
  select(isoweek, perc.pinkut,perc.morrison,perc.4mile,
         perc.fulton, perc.pierre,perc.tahlolower,perc.upperbabine) %>% 
  pivot_longer(-isoweek)

ggplot(sum.by.week.wide85)+
  geom_bar(aes(x=isoweek, y=value, fill=name), stat="identity") +
  #scale_x_date(date_breaks = "1 week",date_labels = "%b-%d")+
  labs(title = "Over 85% assignment",x="catch week",y="percent",
       fill="")

# multiply this up to the run size during each week

weekly.SK2023 <- fence2023 %>% 
  mutate(isoweek = isoweek(date),year = year(date), tot.SK = lgSK+jkSK) %>% 
  select(isoweek, year, date, tot.SK) %>% 
  group_by(isoweek, year) %>% 
  summarize(tot.SK = sum(tot.SK)) %>% 
  full_join(sum.by.week.long85) %>% 
  mutate(SK.by.timing.group = tot.SK*value)


ggplot(weekly.SK2023)+
  geom_line(aes(x=isoweek, y=tot.SK))+
  geom_bar(aes(x=isoweek, y=SK.by.timing.group, fill=nameF), stat="identity")+
  scale_y_continuous(breaks = seq(0,max(weekly.SK2023$tot.SK),50000))+
  scale_x_continuous(limits = c(29,45))

# total.SK2023 <- weekly.SK2023 %>%
#   group_by(year) %>%
#   summarize(from.wk = min(isoweek),
#             to.wk= max(isoweek),
#             total.early = ifelse(nameF %in% "perc.early", sum(SK.by.timing.group),NA),
#             total.mid = ifelse(nameF %in% "perc.mid", sum(SK.by.timing.group),NA),
#             total.late = ifelse(nameF %in% "perc.late", sum(SK.by.timing.group),NA))

total.SK2023 <- weekly.SK2023 %>%
  select(isoweek, name,SK.by.timing.group) %>% 
  pivot_wider(names_from = name, 
              values_from = SK.by.timing.group) %>% 
  ungroup() %>% 
  summarize(total.early = sum(perc.early),
            total.mid = sum(perc.mid),
            total.late = sum(perc.late))
total.SK2023





