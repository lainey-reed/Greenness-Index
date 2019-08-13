###
#The following scirpt contains useful functions for analyzing the visual indices that the extractVIs function came up with
#It uses nls.multstart and tidyverse to fit a non linear model which can be grouped by year, plant groups, and vantage
# The figures and data table on the github is a subset of the 2018 data


#Necessary packages
library(nls.multstart)
library(nlstools)
library(tidyverse)
library(broom)
library(lubridate)

non_linear_model <- function(DOY,minGI,maxGI,mS,S,mA,A) {
  GI_est <- minGI + (maxGI-minGI)*(1/(1+exp(-mS*(DOY-S)))+1/(1+exp(mA*(DOY-A)))-1)
  return(GI_est)
}


#Import data
all_data<-read.csv(file=("./VIs/VI_data.csv")) 


#Next step: separate major plant groups, make location a column
all_data2<- all_data %>%
  separate_rows(Major_Plants,sep=" ") %>%
  #Filename format should mean that first underscore is before the date
  mutate(location1=substr(Filename,1,str_locate(Filename,"_")-1)) %>%
  #Some dates have multiple photos of the same site, so there is a number at the end that needs to be removed
  #there are two because would not let me do both at once
  mutate(location=stringr::str_replace_all(location1,"\\d$","")) %>%
  select(-location1) %>%
  mutate(Minor_Plants=na_if(Minor_Plants,"N/A"))


#If you want to filter the date by year:
data_2018 <- all_data2 %>%
  #alternatively, if you have multiple years, could change filter to group_by
  filter(between(Date, as.POSIXct("2018-01-01 12:00:00"), as.POSIXct("2018-12-31 12:00:00")))


#use nls mulstart to come up with a model
#works best with at least 15 data points, depends on how well spaced out they are
#Note: if not converging well try changing the number of iterations or the starting parameters
non_linear_fit <- all_data2 %>%
  group_by(Major_Plants) %>%
  nest() %>%
  mutate(fit=purrr::map(data,~nls_multstart(Green_Index ~ non_linear_model(DOY=DOY,minGI,maxGI,mS,S,mA,A),
                                            data=.x,
                                            iter=1000,
                                            #These values worked well with the GCREW pics, may not work with others
                                            start_lower=c(minGI=.32,maxGI=.375,mS=.1,S=115,mA=.001,A=200),
                                            start_upper =c(minGI=.34,maxGI=.52,mS=.3,S=150,mA=.1,A=300),
                                            #Note: will say not converging, but it means for the initial estimates
                                            supp_errors = "N")))

#confidence intervals are a nice way to visualize model certainty
CI <- non_linear_fit %>% 
  unnest(fit %>% map(~ confint2(.x) %>%
                       data.frame() %>%
                       rename(., conf.low = X2.5.., conf.high = X97.5..))) %>%
  group_by(., Major_Plants) %>%
  mutate(., term = c("minGI","maxGI","mS","S","mA","A")) %>%
  ungroup()

#this will give you just parameters estimate
params <- non_linear_fit %>%
  unnest(fit %>% map(tidy))

params2 <- merge(params, CI, by = intersect(names(params), names(CI)))

#this will return original values, estiamted values for the same day, and residuals
preds <- non_linear_fit %>%
  unnest(fit %>% map(augment))
#note: if you want to look at location potentially, do a cbind with preds, by=DOY,Green_Index

#for the sake of getting values for each day
params3<- params2 %>%
  select(term,Major_Plants,estimate) %>%
  spread(term,estimate) 

daily_estimates<-params3 %>%
  mutate(DOY=list(seq(1,365,by=1))) %>%
  unnest() %>%
  mutate(Green_Est=non_linear_model(DOY,minGI,maxGI,mS,S,mA,A))

#Plot for visualizing confidience intervals of the estiamted parameters:
ggplot(params2) +
  geom_point(aes(Major_Plants,estimate,color=Major_Plants)) +
  geom_linerange(aes(Major_Plants, ymin = conf.low, ymax = conf.high,color=Major_Plants)) +
  facet_wrap(~term,ncol=3,scales="free") +
  coord_flip() +
  ggtitle("Confidence Intervals") +
  xlab('Plant') +
  ylab('')

ggsave("./Figures/Confidence_Intervals.jpg", plot = last_plot())

#graph of actual values and calculated values for those days
ggplot(preds) +
  geom_point(mapping=aes(x=DOY,y=Green_Index,colour=Major_Plants)) +
  geom_line(mapping=aes(x=DOY,y=.fitted,colour=Major_Plants)) +
# scale_color_manual(values=c("#99CC00","#FF9900"))+
  ggtitle("DOY vs. Green Index of all data \nSorted by Major Plant Communities")

ggsave("./Figures/Fitted_Curves.jpg", plot = last_plot())
