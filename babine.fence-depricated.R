
# This script is used to clean, QA and analyse the Babine fish fence data
# Initially this was aimed at the Coho extension and the Holtby expansion
# Since then has expanded to environmental data, the harvest fisheries and more

# Author: Kristen P., DFO 
# Created 2 Sept 2021
# last updated March 2024


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



#### Babine fence COHO ####

#newer coho data #Babine 1946-2024:
babine194624 <- read_excel("Babine Coho Daily 1946-2024.xlsx", sheet="Coho",
                          col_names = T ) %>% 
  gather("Year","Count",-Date) %>% 
  mutate(Year= as.numeric(Year),day=day(Date),month=month(Date),julian=yday(Date),
         Date=ymd(paste(Year,month,day)),fake.date=as_date(julian-1,origin="2024-01-01")) %>% 
  filter(!is.na(Count))

#get total counts by year
(total.counts.CO <- babine194624 %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(total.CO = sum(Count)))

#calc cumulative proportion of the daily run by year, add columns
babine.all <- ddply(babine194624, "Year", summarize, Date=Date, Count=Count, 
      cumulsum = cumsum(Count)) %>% 
  left_join(total.counts.CO) %>% 
  mutate(daily.prop = Count/total.CO, 
         cumulprop = round(cumulsum/total.CO,2),
         day=day(Date),month=month(Date),julian=yday(Date),
        fake.date=as_date(julian-1,origin="2024-01-01"),
        decade = ifelse(Year %in% c(1940:1949), "1940s",
                        ifelse(Year %in% c(1950:1959), "1950s",
                               ifelse(Year %in% c(1960:1969), "1960s",
                                      ifelse(Year %in% c(1970:1979), "1970s",
                                             ifelse(Year %in% c(1980:1989), "1980s",
                                                    ifelse(Year %in% c(1990:1999), "1990s",
                                                           ifelse(Year %in% c(2000:2009), "2000s",
                                                                  ifelse(Year %in% c(2010:2019), "2010s", 
                                                                         ifelse(Year %in% c(2020:2029), "2020s", NA)))))))))) %>% 
  filter(!is.na(Count))
unique(babine.all[,c("Year","decade")])
  


#pick extension years based on date of operation - mid-Oct cutoff
yday(ymd("2024-10-15")) #this excludes 1950, which operated until 288

(extension.yrs <- as.vector(unique(babine.all[which(babine.all$julian >= 289),"Year"])))
length(extension.yrs)



#filter df for yrs running past mid-Oct
babine.extensions1 <- babine.all %>% 
  filter(Year %in% extension.yrs)

(extension.by.decade <- babine.extensions1 %>% 
  filter(!(decade %in% "2020s")) %>% 
  group_by(fake.date,decade) %>% 
  summarize(mn=mean(Count, na.rm=T),sd=sd(Count, na.rm=T), 
            n=length(Count),
            se=sd(Count, na.rm=T)/sqrt(length(Count))) %>% 
    mutate(CI.lower = ifelse(mn-1.96*se < 0,0,mn-1.96*se),
           CI.upper = mn+1.96*se))


#"base years" based on Holtby

base.yrs <- c(1950,1952,1957,1976,1977,1979,1985,1989,1995,1996,1998)

# base.yrs = c(1950,1952,1957,1976,1977,1979,1985,1989,1995,1996,1998,
#              1999,2000,2001,2003,2004) #these additional three years added post-holtby report

#filter df for base years
babine.extensions2 <- babine.all %>% 
  filter(Year %in% base.yrs)

# #current year:
 babine2024 <- babine.all %>% 
   filter(Year %in% 2024)
          
 
#plot years together:              
plot.decadal.coho <- ggplot()+
  geom_ribbon(data=extension.by.decade,
              aes(x=fake.date, ymin=CI.lower, ymax=CI.upper, fill=decade),
              alpha = 0.5)+
  geom_line(data=babine2024,aes(x=Date,y=Count),col="black",size=1)+
  scale_x_date(limits = c(as_date("2024-Aug-10"),as_date("2024-dec-05")),
               date_breaks= "1 week", date_labels = "%d-%b")+
  labs(x="Date",y="Daily Coho Count", fill="")+
  theme_babine4()
plot.decadal.coho

 # ggsave(plot = plot.decadal.coho,filename = "plotdecadalcoho.png", device="png",
 #        width = 6, height = 5)

#plot years together - holtby:              
ggplot()+
  geom_line(data=babine.extensions2,
            aes(x=fake.date,y=Count,col=as.factor(Year)),size=1)+
  geom_line(data=babine2024,aes(x=Date,y=Count),col="black",size=1.5)+
  scale_x_date(limits = c(as_date("2024-Aug-10"),as_date("2024-nov-22")),
               date_breaks= "1 week", date_labels = "%d%b")+
  labs(title="2024 compared to Base Years (Holtby)")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),title = element_text(size=6),
        legend.box = "vertical",
        plot.title=element_text(size=10))
  

##### Timing COHO ####

###### box plots of coho timing by year ####

#proportion of the run 
props <- c(0.1,0.25,0.5,0.75,0.9,1)

# closest<-function(data,value){
#   x <- data[which(abs(data$cumulprop-value)==min(abs(data$cumulprop-value))),]
#   x$date}
# 


x <- data.frame(Year=extension.yrs,start=as_date(NA),first.quart = as_date(NA), 
                median = as_date(NA),third.quart = as_date(NA), end = as_date(NA))

for (i in 1:length(extension.yrs)){
  data <- babine.extensions1 %>% 
    filter(Year %in% extension.yrs[i])
  x[i,"start"] <- data[first(which(abs(data$cumulprop-props[1])==min(abs(data$cumulprop-props[1])))),"Date"]
  x[i,"first.quart"] <- data[first(which(abs(data$cumulprop-props[2])==min(abs(data$cumulprop-props[2])))),"Date"]
  x[i,"median"] <- data[first(which(abs(data$cumulprop-props[3])==min(abs(data$cumulprop-props[3])))),"Date"]
  x[i,"third.quart"] <- data[first(which(abs(data$cumulprop-props[4])==min(abs(data$cumulprop-props[4])))),"Date"]
  x[i,"end"] <- data[first(which(abs(data$cumulprop-props[5])==min(abs(data$cumulprop-props[5])))),"Date"]
}

date.of.operation <- babine.extensions1 %>% 
  filter(Year %in% extension.yrs) %>% 
  group_by(Year) %>% 
  arrange(Date) %>% 
  summarize(first.day = Date[1], last.day = tail(Date,1)) %>% 
  mutate(last.julian = yday(last.day))


timing.extensionyrs <- x %>%
  pivot_longer(!Year, names_to="phase", values_to = "Date") %>% 
  mutate(julian = yday(Date)) 

tmp <- babine.extensions1 %>% 
  left_join(x, by="Year")  %>% 
  # mutate(period = ifelse(Date < start, "start",
  #                        ifelse(Date >= start & Date <= first.quart, "first.quart",
  #                               ifelse(Date >= first.quart & Date <= median, "median",
  #                                      ifelse(Date >= median & Date <= third.quart, "third.quart",
  #                                             ifelse(Date >= third.quart & Date <= end, "end",
  #                                                    ifelse(Date >= end, "final", NA))))))) %>%
  mutate(period = ifelse(Date < first.quart, "first quarter",
                         ifelse(Date >= first.quart & Date < third.quart, "middle half",
                             ifelse(Date >= third.quart, "last quarter", NA)))) %>%
  mutate(periodf = factor(period, levels=c("first quarter","middle half","last quarter"),
                          ordered=T))
tmp1 <- tmp %>% 
  filter(Year %in% c(1950:1989))
tmp2 <- tmp %>% 
  filter(Year %in% c(1990:1999))
tmp3 <- tmp %>% 
  filter(Year %in% c(2000:2024))

plot.dist.timing1 <- ggplot(data=tmp1)+
  geom_ribbon(aes(x=fake.date, ymin=0, ymax=Count, fill=periodf))+
  geom_line(aes(x=fake.date, y=Count), col="black")+
  geom_vline(aes(xintercept=(ymd("2024-10-01"))), linetype="dashed")+
  facet_wrap(~Year,scales = "free_y")+
  labs(x="Date",y="Daily Coho",fill="")+
  theme_babine4()
plot.dist.timing1
 # ggsave(plot=plot.dist.timing1, filename = "plot.dist.timing1.png",
 #        device="png",width=6, height=6)

plot.dist.timing2 <- ggplot(data=tmp2)+
  geom_ribbon(aes(x=fake.date, ymin=0, ymax=Count, fill=periodf))+
  geom_line(aes(x=fake.date, y=Count), col="black")+
  geom_vline(aes(xintercept=(ymd("2024-10-01"))), linetype="dashed")+
  facet_wrap(~Year,scales = "free_y")+
  labs(x="Date",y="Daily Coho",fill="")+
  theme_babine4()
plot.dist.timing2
 # ggsave(plot=plot.dist.timing2, filename = "plot.dist.timing2.png",
 #        device="png",width=6, height=6)

plot.dist.timing3 <- ggplot(data=tmp3)+
  geom_ribbon(aes(x=fake.date, ymin=0, ymax=Count, fill=periodf))+
  geom_line(aes(x=fake.date, y=Count), col="black")+
  geom_vline(aes(xintercept=(ymd("2024-10-01"))), linetype="dashed")+
  facet_wrap(~Year,scales = "free_y")+
  labs(x="Date",y="Daily Coho",fill="")+
  theme_babine4()
plot.dist.timing3
 # ggsave(plot=plot.dist.timing3, filename = "plot.dist.timing3.png",
 #        device="png",width=6, height=6)

plot.timing.ext <- ggplot()+
  geom_boxplot(data=timing.extensionyrs,aes(x=Year, y=julian, group=Year),
               fill="gray50")+
  geom_point(data=date.of.operation, 
             aes(x=Year, y=last.julian), shape="-", size=2)+
  geom_text(aes(x=base.yrs,y=min(timing.extensionyrs$julian),label="*"))+
  scale_x_continuous(breaks=seq(1950,2024,2), labels = seq(1950,2024,2))+
  labs(x="Year", y="Julian Day")+
  theme_babine4()
  # theme(axis.text.x = element_text(angle=45, hjust=1),
  #          legend.position = "none")
plot.timing.ext
# ggsave(plot = plot.timing.ext, filename = "plot.timing.ext.png",
#        device="png", width=6, height=4)



###### regressions testing changes over time: #####

#for start of season, use all years
years.of.operation <- unique(babine.all$Year)

y <- data.frame(Year=years.of.operation,start=as_date(NA),first.quart = as_date(NA), 
                median = as_date(NA),third.quart = as_date(NA), end = as_date(NA),
                stop = as_date(NA))

for (i in 1:length(years.of.operation)){
  data <- babine.all %>% 
    filter(Year %in% years.of.operation[i])
  y[i,"start"] <- data[first(which(abs(data$cumulprop-props[1])==min(abs(data$cumulprop-props[1])))),"Date"]
  y[i,"first.quart"] <- data[first(which(abs(data$cumulprop-props[2])==min(abs(data$cumulprop-props[2])))),"Date"]
  y[i,"median"] <- data[first(which(abs(data$cumulprop-props[3])==min(abs(data$cumulprop-props[3])))),"Date"]
  y[i,"third.quart"] <- data[first(which(abs(data$cumulprop-props[4])==min(abs(data$cumulprop-props[4])))),"Date"]
  y[i,"end"] <- data[first(which(abs(data$cumulprop-props[5])==min(abs(data$cumulprop-props[5])))),"Date"]
  y[i,"stop"] <- data[first(which(abs(data$cumulprop-props[6])==min(abs(data$cumulprop-props[6])))),"Date"]
  }

timing.extensionyrs.all <- y %>%
  pivot_longer(!Year, names_to="phase", values_to = "Date") %>% 
  mutate(julian = yday(Date)) 

#date when first 10% past the fence
start <- timing.extensionyrs.all %>% 
  filter(phase %in% "start")

start.lm <- lm(data = start, julian~Year)
(start.sum <- summary(start.lm))



plot.start.timing <- ggplot(start)+
  geom_point(aes(x=as.integer(Year), y=julian))+
  geom_text(aes(x=1960,y=max(julian-5), label = 
                  paste("R-squ =",round(start.sum$r.squared,2),", p =",
                        round(start.sum$coeff[2,4], 2))))+
  geom_smooth(aes(x=as.integer(Year), y=julian), method="lm")+
  labs(title = "Start of Run", x="Year", y="Julian Day")+
  scale_x_continuous(breaks=seq(1946,2024,2), labels = seq(1946,2024,2))+
  #theme(axis.text.x = element_text(angle=45, hjust=1))+
  theme_babine4()
plot.start.timing
# ggsave(plot=plot.start.timing, filename = "plot.start.timing.png",
#        device="png", width=6, height=4)


#date when half past the fence
median <- timing.extensionyrs %>% 
  filter(phase %in% "median")

median.lm <- lm(data = median, julian~as.numeric(Year))
(median.sum <- summary(median.lm))


plot.med.timing <- ggplot(median)+
  geom_point(aes(x=as.integer(Year), y=julian))+
  geom_text(aes(x=1960,y=max(julian-5), label = 
                  paste("R-squ =",round(median.sum$r.squared,2),", p =",
                        round(median.sum$coeff[2,4], 2))))+
  geom_smooth(aes(x=as.integer(Year), y=julian), method="lm")+
  labs(title = "Median of Run", x="Year", y="Julian Day")+
  scale_x_continuous(breaks=seq(1950,2024,2), labels = seq(1950,2024,2))+
  #theme(axis.text.x = element_text(angle=45, hjust=1))+
  theme_babine4()
plot.med.timing
# ggsave(plot=plot.med.timing, filename="plot.med.timing.png", device="png",
#        width=6, height = 4)

# date when 90% of run has passed
end <- timing.extensionyrs %>% 
  filter(phase %in% "end")

(end.sum <- summary(lm(data = end, julian~as.numeric(Year))))

plot.end.timing <- ggplot(end)+
  geom_point(aes(x=as.integer(Year), y=julian))+
  geom_text(aes(x=1960,y=max(julian-5), label = 
                  paste("R-squ =",round(end.sum$r.squared,2),", p =",
                        round(end.sum$coeff[2,4], 2))))+
  geom_smooth(aes(x=as.integer(Year), y=julian), method="lm")+
  labs(title = "End of Run",x="Year", y="Julian Day")+
  scale_x_continuous(breaks=seq(1950,2024,2), labels = seq(1950,2024,2))+
  theme_babine4()
  #theme(axis.text.x = element_text(angle=45, hjust=1))
plot.end.timing
# ggsave(plot=plot.end.timing, filename = "plot.end.timing.png",
#        device="png", width=6, height=4)


#followup question - is end later because the fence ran later?
# may not be all that informative if so. Start and Median more important.
# But since they both show this trend, is more believable. The
# fence possibly ran late BECAUSE there were still coho... tough
# to say now?

# plot date when fence stopped running

stop <- timing.extensionyrs.all %>% 
  filter(phase %in% "stop")

stop.lm <- lm(data = stop, julian~Year)
(stop.sum <- summary(stop.lm))



plot.stop.timing <- ggplot(stop)+
  geom_point(aes(x=as.integer(Year), y=julian))+
  geom_text(aes(x=1960,y=max(julian-5), label = 
                  paste("R-squ =",round(stop.sum$r.squared,2),", p =",
                        round(stop.sum$coeff[2,4], 2))))+
  geom_smooth(aes(x=as.integer(Year), y=julian), method="lm")+
  labs(title = "stop of Run", x="Year", y="Julian Day")+
  scale_x_continuous(breaks=seq(1946,2024,2), labels = seq(1946,2024,2))+
  #theme(axis.text.x = element_text(angle=45, hjust=1))+
  theme_babine4()
plot.stop.timing






##### Coho expansion - Holtby's method ####

#different base year cut-offs for expansion (see spreadsheet)
base.yrs <- c(1950,1952, 1957, 1976, 1977, 1979, 1985,
              1989, 1995, 1996, 1998)

#1950 1952 1957 1976 1977 1979 1985 1989 1995 1996 1998 (pre-1998)
#1998 1999 2000 2001 (2002)
#1998 1999 2000 2001 2003 2004 (2005-2020)



# calculate ave daily proportion of run for base years. Holtby method
# apparently split pre-1992 and post-1992, and post 1998 (see spreadsheet)


unique(babine.extensions2$Year)

#can assign a cut-off day for days to make 
# averages from (currently all days)
cut.off.holtby <- yday(ymd("2024-12-31"))

# summarize total count before cut-off day
total.base.byyr <- babine.extensions2 %>% 
  group_by(Year) %>% 
  filter(julian <= cut.off.holtby) %>% #in Holtby, appear to cut the timing to Oct 13th for the total run
  summarize(cutoff.total = sum(Count), last.julian = tail(julian,1))

#join cut-off dates back to df of base years
base.yrs.df <- babine.extensions2 %>% 
  left_join(total.base.byyr) %>% 
  filter(julian <= last.julian) %>% 
  mutate(holtby.daily.prop = Count/cutoff.total, 
         prepost = ifelse(Year <= 1992, "pre","post")) %>% 
  group_by(Year) %>% 
  mutate(holtby.cumul.prop = cumsum(holtby.daily.prop))
head(base.yrs.df)


# averages daily proportion in each year

unique(base.yrs.df$Year)

ave.daily.base.yrs <- base.yrs.df %>% 
  group_by(julian, fake.date, prepost) %>% 
  summarize(ave.daily.prop = round(mean(holtby.cumul.prop),2),
            sd.daily.prop = round(sd(holtby.cumul.prop),2)) 

#visualize
ggplot()+
  geom_line(data=base.yrs.df, aes(x=fake.date,y=cumulprop,
                                   col=as.character(Year)))+
  geom_line(data=ave.daily.base.yrs,aes(x=fake.date,y=ave.daily.prop,
                                        linetype=prepost))+
  labs(x="Date",y="cumulative proportion")

#expand non-base years using base year proportions
    #base years from Holtby report
base.yrs 
non.base.yrs <- setdiff(as.numeric(unique(babine.all$Year)),base.yrs)
#non.base.yrs <- setdiff(non.base.yrs,1965) #removed 1965 since they estimate differently - look into this again

#non base year df
babine.nonbase <- babine.all %>% 
  filter(Year %in% non.base.yrs) 

total.nonbase.byyr <- babine.nonbase%>% 
  arrange(Year, julian) %>% 
  group_by(Year) %>% 
  summarize(total = unique(total.CO), last.julian = tail(julian,1),
            fake.date = tail(fake.date,1))  %>% 
  #mutate(prepost = ifelse(Year <= 1992, "pre",
  #                        ifelse(Year > 1992&Year <= 1998,"post","new"))) %>% 
  mutate(prepost = ifelse(Year <= 1992, "pre","post"))                      


#only expand counts on years when counts did not go to cut.off.holtby
# expanded.yrs <- total.nonbase.byyr %>% 
#   filter(last.julian <= cut.off.holtby) %>% 
#   left_join(ave.daily.base.yrs, by= "fake.date") %>% 
#   mutate(est.total = total/ave.daily.prop) %>% 
#   select(-c(julian, ave.daily.prop, sd.daily.prop))

pre.expanded.yrs <- total.nonbase.byyr %>%
  filter(last.julian <= cut.off.holtby, prepost %in% "pre") %>%
  left_join(ave.daily.base.yrs, by= c("fake.date", "prepost")) %>%
  mutate(est.total = total/ave.daily.prop) %>%
  select(-c(julian, ave.daily.prop, sd.daily.prop, prepost))

post.expanded.yrs <- total.nonbase.byyr %>%
  filter(last.julian <= cut.off.holtby, prepost %in% "post") %>%
  left_join(ave.daily.base.yrs, by= c("fake.date", "prepost")) %>%
  mutate(est.total = total/ave.daily.prop) %>%
  select(-c(julian, ave.daily.prop, sd.daily.prop, prepost))

# new.expanded.yrs <- total.nonbase.byyr %>%
#   filter(last.julian <= cut.off.holtby, prepost %in% "new") %>%
#   left_join(ave.daily.base.yrs, by= c("fake.date", "prepost")) %>%
#   mutate(est.total = total/ave.daily.prop) %>%
#   select(-c(julian, ave.daily.prop, sd.daily.prop, prepost))

non.expanded.yrs <- total.nonbase.byyr %>% 
  filter(last.julian > cut.off.holtby) %>% 
  mutate(est.total = total )


base.yrs.noexpansion <- babine.extensions2 %>% 
  group_by(Year) %>% 
  #filter(fake.date < ymd("2021-10-15")) %>% #in Holtby, appear to cut the timing to Oct 14th for the total run
  summarize(total = sum(Count), last.julian = tail(julian,1),
            fake.date = tail(fake.date,1)) %>% 
  mutate(est.total = total)



#put back together: (took out new.expanded.yrs)
total.byyr <- rbind(pre.expanded.yrs,post.expanded.yrs,
                    non.expanded.yrs,base.yrs.noexpansion) %>% 
  mutate(est.total = round(est.total,0),
         additional = est.total-total, Year=as.numeric(Year)) %>% 
  arrange(Year) %>% 
  select(Year, total.counted = total, last.day.counted = last.julian,
         fake.date, estimated.total = est.total, 
         added.by.expansion = additional)
total.byyr
unique(total.byyr$Year)

#write_csv(total.byyr, "coho.totalsbyyr.csv")


# second2021 <- total.byyr %>% 
#   filter(Year > 2021.1) %>% 
#   mutate(Year = 2022, Year.lab = "2021 Actual")
  

plot.holtby.exp <- ggplot(total.byyr)+
  geom_col(aes(x=Year, y=estimated.total), fill="grey50") + 
  geom_col(aes(x=Year, y=total.counted), fill="grey15") +
   geom_hline(aes(yintercept=1200))+
   geom_hline(aes(yintercept=11500),linetype="dashed")+ #LRP from Holtby at 11500
 # geom_vline(aes(xintercept=c(1992)), linetype="dotted")+
  scale_x_continuous(limits = c(1999,2025),breaks = seq(2000,2024,2))+
  scale_y_continuous(limits = c(0,24000), breaks = seq(0,24000,4000))+
  labs(y="Coho Count (grey) and \nExpansion (black)",x="")+
  theme_babine4()
plot.holtby.exp  

ggsave(plot=plot.holtby.exp, filename = "plot.holtby.exp_RW_PSR2024.png",
       width= 6, height = 4)
        



##### The non-Holtby expansion ####

#(extension.yrs <- as.vector(unique(babine.all[which(babine.all$julian >= 289),"Year"])))

extension.yrs
non.extension.yrs <- setdiff(unique(babine.all$Year),extension.yrs)

str(babine.extensions1)

#calc average daily cumulative prop of run for extension years
ave.daily.ext.yrs <- babine.extensions1 %>% 
  group_by(julian, fake.date) %>% 
  summarize(ave.daily.prop = round(mean(cumulprop),2),
            sd.daily.prop = round(sd(cumulprop),2),
            n.daily.prop = length(unique(Year)) )

# extrapolate non-extension years

non.expansion.df <- babine.all %>% 
  filter(Year %in% non.extension.yrs) %>% 
  arrange(Date) %>% 
  group_by(Year) %>% 
  summarize(total = unique(total.CO), last.julian = tail(julian,1),
            fake.date = tail(fake.date,1)) %>% 
  left_join(ave.daily.ext.yrs) %>% 
  mutate(total.est = total/ave.daily.prop, additional = total.est-total) %>% 
  select(Year,total,total.est,additional)
  
  #
non.expansion.df[,c("total.est","additional")]

expansion.df <- babine.extensions1 %>% 
  group_by(Year) %>% 
  summarize(total = unique(total.CO),total.est = unique(total.CO), 
            additional = 0)


expansions.all <- rbind(non.expansion.df, expansion.df)


plot.extension.exp <- ggplot(expansions.all)+
  geom_col(aes(x=Year, y=total.est), fill="grey10")+
  geom_col(aes(x=Year, y=total), fill="grey50")+
  geom_hline(aes(yintercept=1200))+
  geom_hline(aes(yintercept=11500),linetype="dashed")+
  scale_x_continuous(limits = c(1945,2022),breaks = seq(1946,2022,4))+
  scale_y_continuous(limits = c(0,60000), labels = NULL)+
  labs(y="",x="")+
  theme_babine4()
plot.extension.exp

#compare plots
library(gridExtra)

expansions.plot <- grid.arrange(plot.holtby.exp, plot.extension.exp,
                               nrow = 1,widths = c(2,1.7))


# ggsave(plot = expansions.plot, filename="expansions.plot.compare.png", 
#        device="png", width=7, height=4)

#

##### Leave-one-out expansion ####

runs <- vector(mode="list", length = length(extension.yrs))
names(runs) <- extension.yrs

#loop:

for (i in 1:length(extension.yrs)){

yrs.select <- extension.yrs[which(extension.yrs != extension.yrs[i])]

ave.daily.ext.yrs <- babine.extensions1 %>% 
  filter(Year %in% yrs.select) %>% 
  group_by(julian, fake.date) %>% 
  summarize(ave.daily.prop = round(mean(cumulprop),2),
            sd.daily.prop = round(sd(cumulprop),2),
            n.daily.prop = length(unique(Year)) )

runs[[i]] <- babine.extensions1 %>% 
  filter(Year %in% extension.yrs[i]) %>% 
  arrange(Date) %>% 
  group_by(Year) %>% 
  summarize(truncated = cumulsum[julian == 274],
            trunc.julian = 274,
            total.counted = unique(total.CO), 
            last.julian = tail(julian,1),
            last.fake.date = tail(fake.date,1)) %>% 
  left_join(ave.daily.ext.yrs, by=c("trunc.julian"="julian" )) %>% 
  mutate(trunc.est = truncated/ave.daily.prop, 
         plus20 = total.counted+total.counted*.20,
         minus20 = total.counted-total.counted*.20,
         perc.diff = trunc.est/total.counted) %>% 
  select(Year,truncated,total.counted,trunc.est,perc.diff,
         minus20,plus20,trunc.julian,last.julian)
}

runs

#expansion was based on truncation of data to this day:
as_date(273, origin = ymd("2020-01-01"))

#collapse to df:
runs.df <- do.call(rbind, runs)

ggplot(runs.df)+
  geom_col(aes(x=Year, y=plus20), fill="grey10")+
  geom_col(aes(x=Year, y=minus20), fill="white")+
  geom_point(aes(x=Year, y=trunc.est), col="red")+
  theme_babine4()

plot.difffromcount <- ggplot(runs.df)+
  geom_hline(aes(yintercept=1), linetype = "dashed")+
  geom_hline(aes(yintercept=1.2), linetype = "dotted")+
  geom_hline(aes(yintercept=0.8), linetype = "dotted")+
  geom_point(aes(x=Year, y=perc.diff, size=total.counted))+
  scale_y_continuous(limits = c(0.4,1.4),
                     breaks = seq(0.4,1.4,0.2),
                     labels = c("-60%","-40%","-20%","equal",
                                "+20%","+40%"))+
  scale_x_continuous(breaks=seq(1946,2021,4))+
  labs(x="",y="Difference From Count",
       size="Total Counted")+
  theme_babine4()

plot.difffromcount

# ggsave(plot=plot.difffromcount, filename="plot.difffromcount.png",
#   device="png",width=6, height = 4)


#### Hydrology #### 

# Babine R at outlet of Nilkitkwa L: 08EC013 (just d/s of fence)
# Babine R at Fort Babine: 08EC001 (at Babine lake outlet)
# X Skeena R at Glen Vowell: 08EB003 * big time gap between 1985 and 2022
# Skeena R above Babine: 08EB005
# Skeena R at Usk: 08EF001

library(tidyhydat)

# Babine River hydro station just d/s of the fence:

#download_hydat()


hy_stations(station_number = "08EC013")

bab.flows.real.raw <- realtime_dd(station_number = "08EC013")
bab.flows.hist.raw <- hy_daily_flows(station_number = "08EC013")

range(bab.flows.real.raw$Date)
range(bab.flows.hist.raw$Date)

#### temperature data from 08EC013: ####

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


#Meziadin fence

# mez <- read_excel("Meziadin Daily Update 2021.xlsx", 
#                   sheet="CohoAdults")
# str(mez)
# 
# mez.daily <- mez %>% 
#   gather("Year","cohoAL",-Date) %>% 
#   mutate(Date= ymd(paste0(Year,"-",substr(Date,6,10)))) %>% 
#   mutate(julian = yday(Date)) %>% 
#   filter(Year %in% c(2015:2020)) %>% 
#   mutate(fake.date = as_date(julian,origin="2021-01-01")) 
# 
# ggplot(data=mez.daily)+
#   geom_point(aes(x=as_date(fake.date),y=cohoAL, col=as.factor(Year)), 
#              size=2)+
#   geom_smooth(aes(x=as_date(fake.date),y=cohoAL, col=as.factor(Year)),
#               method = "loess", se=F)+
#   #facet_wrap(~Year)+
#   scale_x_date(limits = c(as_date("2021-aug-01"),as_date("2021-oct-31")),
#                date_breaks= "1 week", date_labels = "%d-%b")+
#   labs(title="Mez Fence Coho",x="appr.date", y="adult Coho")+
#   theme_dark()+
#   theme(legend.position = "bottom",
#         axis.text.x = element_text(size=8,angle =45, hjust=1),
#         legend.text = element_text(size=8),title = element_text(size=10),
#         legend.box = "vertical")




#### Harvest vs. counting timing - SOCKEYE ####

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

# # read in 2023 data:
# babineSK.2023 <- read_excel("BabineFence_2023-preliminaryALL.xlsx",
#                             sheet="Daily Summary", range = "A4:R145",
#                             col_names = c("date","lgSK","jkSK","CO","PK","lgCH","jkCH",
#                                           "ST","Char","SK.morts","CO.morts","PK.morts",
#                                           "CH.morts","Other.morts","FSC.SK","dead1","dead2",
#                                           "Tot.return.SK")) %>% 
#   select(date,lgSK,jkSK,Tot.return.SK) %>% 
#   mutate(date = as_date(date), year = year(date)) %>% 
#   mutate(julian = yday(date)) %>% 
#   left_join(ave.daily.prop) %>% 
#   group_by(year) %>% 
#   mutate(total.lgSK = sum(Tot.return.SK, na.rm=T)) %>% #, total.jkSK = sum(jkSK, na.rm=T)) %>% 
#   mutate(pred.lgSK = ifelse(date < ymd("2023-07-20"),round(ave.daily.prop.lgSK*total.lgSK,0),Tot.return.SK)) %>%
#   mutate(newtotal.lgSK = sum(pred.lgSK, na.rm=T)) %>% 
#   mutate(prop.daily23 = pred.lgSK/newtotal.lgSK)
#   #mutate(pred.jkSK = ifelse(date < ymd("2023-07-20"),round(ave.daily.prop.jkSK*total.jkSK,0),jkSK))
#                               
# sum(babineSK.2023$pred.lgSK, na.rm=T)
# 
# write_csv(babineSK.2023, "predicted.early.run2023SK.csv")

# # read in 2024 data:
babineSK.2024 <- read_excel("BabineFence_2024_DBQA_Nov21_CampanQA_Aug16.xlsx",
                            sheet="Daily Summary", range = "A4:T146",
                            col_names = c("date","lgSK","jkSK","CO","PK","lgCH","jkCH",
                                          "ST","Char","SK.morts","CO.morts","PK.morts",
                                          "CH.morts","Other.morts","FSC.SK","FSC.PK",
                                          "FSC.CO","DemojkSK","DemoLgSK",
                                          "Tot.return.SK")) %>%
  select(date,lgSK,jkSK,Tot.return.SK) %>%
  mutate(date = as_date(date), year = year(date)) %>%
  mutate(julian = yday(date), fake.date= as_date(julian-1, origin = ymd("2024-01-01")),
         total.SK = sum(Tot.return.SK, na.rm=T), prop.daily = Tot.return.SK/total.SK)
  
babineSK.2024


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



#### Compile dataset with QA'd fence data (2014 to 2024 (Missing 2020)) ####

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
      fence2021, fence2022, fence2023, fence2024) %>% 
  mutate(year = year(date), fyear = as.factor(year), yday = yday(date)) 



# fence20142024 %>% 
#   filter(!is.na(DemoPK)) %>% 
#   group_by(year) %>% 
#   summarize(tot.PKdemo = sum(DemoPK)) #ask Karlena if this is actually a demo PK fishery or mislabeled FSC?
#sounds like these may have been permitted bycatch on the sockeye fishery

#all pink data: 

babine.pink.historic <- read_excel("./fencedata/Babine Adult Weir Count-sourceStockdrive_copy5-Mar-2025.xls",
           sheet = "PINK DATA", range = "A395:BO570")
yr.names <- as.character(c(1946, 1947,1949:1963,1965:2013))
colnames(babine.pink.historic) <- c("date.int",yr.names)

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





 # interpolate missing data from the 2018 and 2023 wildfire closures

ave.daily.prop <- fence20142024 %>% 
  group_by(julian) %>% 
  summarize(ave.daily.prop.lgSK = mean(daily.prop.lgSK, na.rm=T)) %>% 
  mutate(year = "1993-2024",  
         fake.date= as_date(julian-1, origin = ymd("2024-01-01")))
#ave.daily.prop.jkSK = mean(daily.prop.jkSK, na.rm=T))

unique(babine.dailySK$year)

ggplot(ave.daily.prop)+
  geom_line(aes(x=julian, y=ave.daily.prop.lgSK), col="blue")+
  #geom_line(aes(x=julian, y=ave.daily.prop.jkSK), col="red")+
  labs(x="date",y="ave. daily proportion \n(blue=large SK, red=jack SK)")




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





