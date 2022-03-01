
# This script is used to clean, QA and analyse the Babine fish fence data

# Author: Kristen P., DFO 
# Created 2 Sept 2021

#Trevor starting hacking it Feb 22, 2022

#library(plyr)
#library(dplyr)
library(tidyverse)
#library(ggplot2)
library(readxl)
library(lubridate)

#### ggplot theme ####
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


##### LOAD DATA: Babine fence ####

#newer coho data #Babine 1946-2021:
babine194620 <- read_excel("../Data/DFO/Babine Coho Daily 1946-2021.xlsx", sheet="Coho",
                          col_names = T ) %>% 
  gather("Year","Count",-Date) %>% 
  mutate(Year= as.numeric(Year),day=day(Date),month=month(Date),julian=yday(Date),
         Date=ymd(paste(Year,month,day)),fake.date=as_date(julian,origin="2021-01-01")) %>% 
  filter(!is.na(Count))

#get total counts by year
(total.counts.CO <- babine194620 %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(total.CO = sum(Count)))

#calc cumulative proportion of the daily run by year, add columns
babine.all <- ddply(babine194620, "Year", summarize, Date=Date, Count=Count, 
      cumulsum = cumsum(Count)) %>% 
  left_join(total.counts.CO) %>% 
  mutate(daily.prop = Count/total.CO, 
         cumulprop = round(cumulsum/total.CO,2),
         day=day(Date),month=month(Date),julian=yday(Date),
        fake.date=as_date(julian,origin="2021-01-01"),
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
yday(ymd("2021-10-16")) #this excludes 1950, which operated until 288

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
base.yrs = c(1950,1952,1957,1976,1977,1979,1985,1989,1995,1996,1998,
             1999,2000,2001) #these additional three years added later

#filter df for base years
babine.extensions2 <- babine.all %>% 
  filter(Year %in% base.yrs)

# #current year:
 babine2021 <- babine.all %>% 
   filter(Year %in% 2021)
          
 
 
#plot years together:              
plot.decadal.coho <- ggplot()+
  geom_ribbon(data=extension.by.decade,
              aes(x=fake.date, ymin=CI.lower, ymax=CI.upper, fill=decade),
              alpha = 0.5)+
  geom_line(data=babine2021,aes(x=Date,y=Count),col="black",size=1)+
  scale_x_date(limits = c(as_date("2021-Aug-10"),as_date("2021-dec-05")),
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
  geom_line(data=babine2021,aes(x=Date,y=Count),col="black",size=1.5)+
  scale_x_date(limits = c(as_date("2021-Aug-10"),as_date("2021-nov-22")),
               date_breaks= "1 week", date_labels = "%d%b")+
  labs(title="2021 compared to Base Years (Holtby)")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),title = element_text(size=6),
        legend.box = "vertical",
        plot.title=element_text(size=10))
  

#### Timing ####

#box plots of coho timing by year ####

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
  filter(Year %in% c(2000:2021))

plot.dist.timing1 <- ggplot(data=tmp1)+
  geom_ribbon(aes(x=fake.date, ymin=0, ymax=Count, fill=periodf))+
  geom_line(aes(x=fake.date, y=Count), col="black")+
  geom_vline(aes(xintercept=(ymd("2021-10-01"))), linetype="dashed")+
  facet_wrap(~Year,scales = "free_y")+
  labs(x="Date",y="Daily Coho",fill="")+
  theme_babine4()
plot.dist.timing1
 # ggsave(plot=plot.dist.timing1, filename = "plot.dist.timing1.png",
 #        device="png",width=6, height=6)

plot.dist.timing2 <- ggplot(data=tmp2)+
  geom_ribbon(aes(x=fake.date, ymin=0, ymax=Count, fill=periodf))+
  geom_line(aes(x=fake.date, y=Count), col="black")+
  geom_vline(aes(xintercept=(ymd("2021-10-01"))), linetype="dashed")+
  facet_wrap(~Year,scales = "free_y")+
  labs(x="Date",y="Daily Coho",fill="")+
  theme_babine4()
plot.dist.timing2
 # ggsave(plot=plot.dist.timing2, filename = "plot.dist.timing2.png",
 #        device="png",width=6, height=6)

plot.dist.timing3 <- ggplot(data=tmp3)+
  geom_ribbon(aes(x=fake.date, ymin=0, ymax=Count, fill=periodf))+
  geom_line(aes(x=fake.date, y=Count), col="black")+
  geom_vline(aes(xintercept=(ymd("2021-10-01"))), linetype="dashed")+
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
  scale_x_continuous(breaks=seq(1950,2021,2), labels = seq(1950,2021,2))+
  labs(x="Year", y="Julian Day")+
  theme_babine4()
  # theme(axis.text.x = element_text(angle=45, hjust=1),
  #          legend.position = "none")
plot.timing.ext
# ggsave(plot = plot.timing.ext, filename = "plot.timing.ext.png",
#        device="png", width=6, height=4)



# regressions testing changes over time: ####

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
  scale_x_continuous(breaks=seq(1946,2021,2), labels = seq(1946,2021,2))+
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
  scale_x_continuous(breaks=seq(1950,2021,2), labels = seq(1950,2021,2))+
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
  scale_x_continuous(breaks=seq(1950,2021,2), labels = seq(1950,2021,2))+
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
  scale_x_continuous(breaks=seq(1946,2021,2), labels = seq(1946,2021,2))+
  #theme(axis.text.x = element_text(angle=45, hjust=1))+
  theme_babine4()
plot.stop.timing






#### Coho expansion - Holtby's method ####

# calculate ave daily proportion of run for base years. Holtby
# apparently split pre-1992 and post-1992, and post 1998



#can assign a cut-off day for days to make 
# averages for (currently all days)
cut.off.holtby <- yday(ymd("2021-12-31"))

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
         prepost = ifelse(Year <= 1992, "pre",
                          ifelse(Year > 1992&Year <= 1998,"post","new"))) %>% 
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
  mutate(prepost = ifelse(Year <= 1992, "pre",
                          ifelse(Year > 1992&Year <= 1998,"post","new")))

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

new.expanded.yrs <- total.nonbase.byyr %>%
  filter(last.julian <= cut.off.holtby, prepost %in% "new") %>%
  left_join(ave.daily.base.yrs, by= c("fake.date", "prepost")) %>%
  mutate(est.total = total/ave.daily.prop) %>%
  select(-c(julian, ave.daily.prop, sd.daily.prop, prepost))

non.expanded.yrs <- total.nonbase.byyr %>% 
  filter(last.julian > cut.off.holtby) %>% 
  mutate(est.total = total )


base.yrs.noexpansion <- babine.extensions2 %>% 
  group_by(Year) %>% 
  #filter(fake.date < ymd("2021-10-15")) %>% #in Holtby, appear to cut the timing to Oct 14th for the total run
  summarize(total = sum(Count), last.julian = tail(julian,1),
            fake.date = tail(fake.date,1)) %>% 
  mutate(est.total = total)



#put back together:
total.byyr <- rbind(pre.expanded.yrs,post.expanded.yrs,new.expanded.yrs,
                    non.expanded.yrs,base.yrs.noexpansion) %>% 
  mutate(est.total = round(est.total,0),
         additional = est.total-total, Year=as.numeric(Year)) %>% 
  arrange(Year) %>% 
  select(Year, total.counted = total, last.day.counted = last.julian,
         fake.date, estimated.total = est.total, 
         added.by.expansion = additional)
total.byyr

#write_csv(total.byyr, "coho.totalsbyyr.csv")


# second2021 <- total.byyr %>% 
#   filter(Year > 2021.1) %>% 
#   mutate(Year = 2022, Year.lab = "2021 Actual")
  

plot.holtby.exp <- ggplot(total.byyr)+
  geom_col(aes(x=Year, y=estimated.total), fill="grey10") + 
  geom_col(aes(x=Year, y=total.counted), fill="grey50") +
   geom_hline(aes(yintercept=1200))+
   geom_hline(aes(yintercept=11500),linetype="dashed")+
  scale_x_continuous(limits = c(1946,2023),breaks = seq(1946,2021,4))+
  scale_y_continuous(limits = c(0,60000))+
  labs(y="Coho Count (grey) and Expansion (black)",x="", title="Holtby")+
  theme_babine4()
plot.holtby.exp  
        



# The non-Holtby expansion ####

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
  scale_x_continuous(limits = c(1946,2023),breaks = seq(1946,2021,4))+
  scale_y_continuous(limits = c(0,60000))+
  labs(y="Coho Count (grey) and Expansion (black)",x="", title="expansion")+
  theme_babine4()
plot.extension.exp

#compare plots
plot.holtby.exp #note this splits pre/post 1992 and pre/post 1998
plot.extension.exp
#

### Leave-on-out expansion ####

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

#collapse to df and run to real:
runs.df <- do.call(rbind, runs)

ggplot(runs.df)+
  geom_col(aes(x=Year, y=plus20), fill="grey10")+
  geom_col(aes(x=Year, y=minus20), fill="white")+
  geom_point(aes(x=Year, y=trunc.est), col="red")+
  theme_babine4()

ggplot(runs.df)+
  geom_hline(aes(yintercept=1), linetype = "dashed")+
  geom_hline(aes(yintercept=1.2), linetype = "dotted")+
  geom_hline(aes(yintercept=0.8), linetype = "dotted")+
  geom_point(aes(x=Year, y=perc.diff, size=total.counted))+
  scale_x_continuous(breaks=seq(1946,2021,4))+
  labs(x="",y="Difference From Count\n (proportion)",
       size="Total Counted")+
  theme_babine4()






# Hydrology #### 

# Babine R at outlet of Nilkitkwa L: 08EC013
# Babine R at Fort Babine: 08EC001

hydro.babine <- hy_daily_levels(station_number = c("08EC013")) %>% 
  mutate(Year = year(Date), julian = yday(Date)) %>% 
  filter(Year %in% c(2010:2017)) %>% 
  #filter(julian<=278 & julian >= 213) %>% 
  mutate(fake.date = as_date(julian,origin="2021-01-01")) %>% 
  mutate(fyear = factor(Year, order = T))  
  #filter(julian %in% c(yday(ymd("2021-aug-01")),yday(ymd("2021-dec-01"))) ) %>% 
  #filter(!is.na(year))


hydro.skeena <- hy_daily_levels(station_number = c("08EB005")) %>% 
  mutate(Year = year(Date), julian = yday(Date)) %>% 
  filter(Year %in% c(2010:2017)) %>% 
  #filter(julian<=278 & julian >= 213) %>% 
  mutate(fake.date = as_date(julian,origin="2021-01-01")) %>% 
  mutate(fyear = factor(Year, order = T))  
#filter(julian %in% c(yday(ymd("2021-aug-01")),yday(ymd("2021-dec-01"))) ) %>% 
#filter(!is.na(year))

yday(as_date("2021=oct-31"))

ggplot()+
  geom_line(data=hydro.babine,aes(x=fake.date, y=Value), col="green", size=1) + 
  geom_line(data=hydro.skeena,aes(x=fake.date, y=Value), col="blue", size=1) + 
  facet_wrap(~Year)


#plot together - don't really correlate at all, so take out hydro

ggplot()+
  geom_point(data = babine,aes(x=as_date(fake.date),y=tot.CO, col=as.factor(Year)), 
             size=2)+
  geom_smooth(data = babine, aes(x=as_date(fake.date),y=tot.CO, col=as.factor(Year)),
              method = "loess", se=F)+
  #geom_line(data=hydro.skeena, aes(x=fake.date, y=Value*50), size=1) +
  facet_wrap(~Year)+
  scale_x_date(limits = c(as_date("2021-aug-01"),as_date("2021-oct-31")),
               date_breaks= "1 week", date_labels = "%d%b")+
  labs(title="Babine Fence Coho",x="appr.date")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=6),title = element_text(size=6),
        legend.box = "vertical")


#Meziadin fence

mez <- read_excel("Meziadin Daily Update 2021.xlsx", 
                  sheet="CohoAdults")
str(mez)

mez.daily <- mez %>% 
  gather("Year","cohoAL",-Date) %>% 
  mutate(Date= ymd(paste0(Year,"-",substr(Date,6,10)))) %>% 
  mutate(julian = yday(Date)) %>% 
  filter(Year %in% c(2015:2020)) %>% 
  mutate(fake.date = as_date(julian,origin="2021-01-01")) 

ggplot(data=mez.daily)+
  geom_point(aes(x=as_date(fake.date),y=cohoAL, col=as.factor(Year)), 
             size=2)+
  geom_smooth(aes(x=as_date(fake.date),y=cohoAL, col=as.factor(Year)),
              method = "loess", se=F)+
  #facet_wrap(~Year)+
  scale_x_date(limits = c(as_date("2021-aug-01"),as_date("2021-oct-31")),
               date_breaks= "1 week", date_labels = "%d-%b")+
  labs(title="Mez Fence Coho",x="appr.date", y="adult Coho")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),title = element_text(size=10),
        legend.box = "vertical")

