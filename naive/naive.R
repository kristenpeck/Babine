### This model here is to run all of the naive models -- i.e models that only use day-of-year as a predictor.  This will be used as the basis for all other models to be built upon.
### Last updated March 11, 2022


#### LOAD PACKAGES
library(tidyverse)
library(readxl)
library(lubridate)
library(bookdown)
library(splines)
library(rstan)
library(posterior)

#### LOAD DATA 
data_all <- read_excel("../../Data/DFO/Babine Coho Daily 1946-2021.xlsx", sheet="Coho",  col_names = T )  %>% 
  gather("Year","Count",-Date) %>% mutate(Year= as.numeric(Year),day=day(Date),month=month(Date),julian=yday(Date), Date=ymd(paste(Year,month,day)),fake.date=as_date(julian,origin="2021-01-01")) %>% 
  filter(!is.na(Count))

data_base <- read_excel("../../Data/DFO/Babine Coho Daily 1946-2021.xlsx", sheet="Baseyrs",  col_names = T )  %>% 
  gather("Year","Count",-Date) %>% mutate(Year= as.numeric(Year),day=day(Date),month=month(Date),julian=yday(Date), Date=ymd(paste(Year,month,day)),fake.date=as_date(julian,origin="2021-01-01")) %>% 
  filter(!is.na(Count))

####################################
### Simulated data
##################################

r <- 10000  # total run
p <- 250  # peak escapement DAY
sigma <- 10 # error
d <- 200:300 #days

set.seed(111)
psi <- sum(exp(-((d-p)^2)/(2*sigma^2)))
run <- r*(exp(-((d-p)^2)/(2*sigma^2)))/psi
run_error <- rlnorm(length(run),log(run),0.2)

single_sim <- data.frame(julian=d,Count=round(run_error,0))
ggplot(data=single_sim,aes(x=julian,y=Count))+ geom_segment(aes(x = julian,xend=julian,y=0,yend=Count),alpha=0.75)+theme_bw()

##### Set STAN conditions
warmups <- 1000
total_iterations <- 2000
max_treedepth <-  10
n_chains <-  1
n_cores <- 4
adapt_delta <- 0.95

### Format data for stan model
sim_dat <- with(single_sim,list(y=Count,x=julian,N=nrow(single_sim)))


### Run stan model
fit <- stan(file = 'naive_single.stan', data = sim_dat,
            chains = n_chains,
            warmup = warmups,
            iter = total_iterations,
            cores = n_cores,
            refresh = 250,
            init = list(list(log_r=9,p=230,sigma=2,reciprocal_phi=.1,phi=.1)))


a <- summarise_draws(fit)



mu <- as.vector((a[209:309,])$mean)
preds2 <- data.frame(x=200:300,y=(mu))
                     
ggplot(data=single_sim,aes(x=julian,y=Count))+ geom_segment(aes(x = julian,xend=julian,y=0,yend=Count),alpha=0.75)+theme_bw()+geom_line(data=preds2,aes(x=x,y=y))

inits <- get_inits(fit)
inits_chain1 <- inits[[1]]
print(inits_chain1)










##########################################
### SINGLE-YEAR BAYESIAN MODEL IN STAN 
###########################################

single_base <- data_base %>% filter(Year %in% 1952,julian >=225,julian <=325) #Put reasonabe bounds around the data

### Always plot your data.  Add a GAM line for approximate trend 
ggplot(data=single_base,aes(x=julian,y=Count)) + geom_segment(aes(x = julian,xend=julian,y=0,yend=Count),alpha=0.75) + geom_ribbon(stat = "smooth", aes(ymin = 0, ymax = ..y..), colour="black",alpha = .2,  method = "gam", se=FALSE, formula = y ~ s(x, k = 40)) +theme_bw()

### Format data for stan model
single_dat <- with(single_base,list(count=Count,d=seq(1:nrow(single_base)),N=nrow(single_base)))

##### Set STAN conditions
warmups <- 1000
total_iterations <- 2000
max_treedepth <-  10
n_chains <-  4
n_cores <- 4
adapt_delta <- 0.95


### Run stan model
fit <- stan(file = 'naive_single2.stan', data = single_dat,
            chains = n_chains,
            warmup = warmups,
            iter = total_iterations,
            cores = n_cores,
            refresh = 250,
            init = list(list(log_r = 5,p=275,sigma=1,sigma_arrival=1)))
#/Users/mykiss/Dropbox/mydocs/employment/2022/Babine/stan/babine


