## Creating a model

#preliminary stuff

library(tidyverse)
library(gtools)
library(data.table)
#some of the winter pictures I initially put as unknown, but based on later photos they are c3, and some misspellings
all_data4<-all_data3 %>% 
  mutate(plant=fct_recode(plant, "phragmites"="phragmties","c3"="unknown"))

##### Creating a model for just phragmites


#.3 and .5 were chosen as arbitrary min and max values, may look into other values
phragmites<-all_data4 %>%
  filter(plant=="phragmites")

y_phrag<-logit(pull(phragmites,Green_Index),.3,.5)
x_phrag<-phragmites %>% pull(DOY)

model_phrag <-lm(y_phrag ~ x_phrag)

function_phrag <- function(x) .3 + .2/(1+exp(-model_phrag[["coefficients"]][2]*x-model_phrag[["coefficients"]][1]))
plot_phrag<- ggplot(data=data.frame(x=0), mapping = aes(x=x)) + stat_function(fun=function_phrag)+xlim(0,250) + geom_point(data = phragmites,mapping=aes(x=DOY,y=Green_Index))
plot_phrag


###### Modelling the whole data set
y_all<-logit(pull(all_data4,Green_Index),.3,.5)
x_all<-all_data4 %>% pull(DOY)

model_all <-lm(y_all ~ x_all)

function_all <- function(x) .3 + .2/(1+exp(-model_all[["coefficients"]][2]*x-model_all[["coefficients"]][1]))
plot_all<- ggplot(data=data.frame(x=0), mapping = aes(x=x)) + stat_function(fun=function_all)+xlim(0,250) + geom_point(data = all_data4,mapping=aes(x=DOY,y=Green_Index))
plot_all


##### c3 modelling

c3<-all_data4 %>%
  filter(plant=="c3")

y_c3<-logit(pull(c3,Green_Index),.3,.5)
x_c3<-c3 %>% pull(DOY)

model_c3 <-lm(y_c3 ~ x_c3)

function_c3 <- function(x) .3 + .2/(1+exp(-model_c3[["coefficients"]][2]*x-model_c3[["coefficients"]][1]))
plot_c3<- ggplot(data=data.frame(x=0), mapping = aes(x=x)) + stat_function(fun=function_c3)+xlim(0,250) + geom_point(data = c3,mapping=aes(x=DOY,y=Green_Index))
plot_c3


#plot_curves <- ggplot(data=data.frame(x=0), mapping = aes(x=x, colour=plant)) + xlim(0,250) + stat_function(fun=function_c3) + stat_function(fun=function_all) + stat_function(fun=function_phrag)
#plot_curves

phrag_table<-data.table(DOY=1:250) %>% 
  mutate(Greenness=.3 + .2/(1+exp(-model_phrag[["coefficients"]][2]*DOY-model_phrag[["coefficients"]][1]))) %>%
  add_column(plant="phragmites")

all_plants_table <- data.table(DOY=1:250) %>% 
  mutate(Greenness=.3 + .2/(1+exp(-model_all[["coefficients"]][2]*DOY-model_all[["coefficients"]][1]))) %>%
  add_column(plant="all")
  
c3_table <- data.table(DOY=1:250) %>% 
    mutate(Greenness=.3 + .2/(1+exp(-model_c3[["coefficients"]][2]*DOY-model_c3[["coefficients"]][1]))) %>%
    add_column(plant="c3")


all_curves_table<-all_plants_table%>%
  rbind(phrag_table) %>%
  rbind(c3_table)

ggplot(data=all_curves_table) + geom_point(mapping=aes(x=DOY,y=Greenness, colour=plant))



#Next goal to see if they will all work at once
############# Using factors

mylist<-list(phrag_table,c3_table,all_plants_table)
models<-mylist %>% map(~ lm(logit(Greenness,.3,.5) ~ DOY, data=.x))
#This worked, but probably there is an easier wayto set it up


