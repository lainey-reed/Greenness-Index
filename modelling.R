## Creating a model

########### preliminary stuff

library(tidyverse)
library(gtools)
library(data.table)
#some of the winter pictures I initially put as unknown, but based on later photos they are c3, and some misspellings
all_data4<-all_data3 %>% 
  mutate(plant=fct_recode(plant, "phragmites"="phragmties","c3"="unknown"))

######## The first part of this program analyzes the pictures by species
# This part is not applicable to other projects that may use other species
# Except for the curve created based on all the plant species

##### Creating a model for just phragmites

#.3 and .5 were chosen as arbitrary min and max values, may look into other values
# phragmites<-all_data4 %>%
#   filter(plant=="phragmites")
# 
# y_phrag<-logit(pull(phragmites,Green_Index),.3,.5)
# x_phrag<-phragmites %>% pull(DOY)
# 
# model_phrag <-lm(y_phrag ~ x_phrag)
# 
# function_phrag <- function(x) .3 + .2/(1+exp(-model_phrag[["coefficients"]][2]*x-model_phrag[["coefficients"]][1]))
# plot_phrag<- ggplot(data=data.frame(x=0), mapping = aes(x=x)) + 
#   stat_function(fun=function_phrag)+xlim(0,250) + 
#   geom_point(data = phragmites,mapping=aes(x=DOY,y=Green_Index))
# plot_phrag


######## Modelling the whole data set
y_all<-logit(pull(all_data4,Green_Index),.3,.5)
x_all<-all_data4 %>% pull(DOY)

model_all <-lm(y_all ~ x_all)

function_all <- function(x) .3 + .2/(1+exp(-model_all[["coefficients"]][2]*x-model_all[["coefficients"]][1]))
plot_all<- ggplot(data=data.frame(x=0), mapping = aes(x=x)) + 
  stat_function(fun=function_all)+xlim(0,250) + geom_point(data = all_data4,mapping=aes(x=DOY,y=Green_Index))
plot_all


##### c3 modelling

# c3<-all_data4 %>%
#   filter(plant=="c3")
# 
# y_c3<-logit(pull(c3,Green_Index),.3,.5)
# x_c3<-c3 %>% pull(DOY)
# 
# model_c3 <-lm(y_c3 ~ x_c3)
# 
# function_c3 <- function(x) .3 + .2/(1+exp(-model_c3[["coefficients"]][2]*x-model_c3[["coefficients"]][1]))
# plot_c3<- ggplot(data=data.frame(x=0), mapping = aes(x=x)) + 
#   stat_function(fun=function_c3)+xlim(0,250) + 
#   geom_point(data = c3,mapping=aes(x=DOY,y=Green_Index))
# plot_c3


#plot_curves <- ggplot(data=data.frame(x=0), mapping = aes(x=x, colour=plant)) + xlim(0,250) + stat_function(fun=function_c3) + stat_function(fun=function_all) + stat_function(fun=function_phrag)
#plot_curves

# phrag_table<-data.table(DOY=1:250) %>% 
#   mutate(Greenness=.3 + .2/(1+exp(-model_phrag[["coefficients"]][2]*DOY-model_phrag[["coefficients"]][1]))) %>%
#   add_column(plant="phragmites")
# 
# all_plants_table <- data.table(DOY=1:250) %>% 
#   mutate(Greenness=.3 + .2/(1+exp(-model_all[["coefficients"]][2]*DOY-model_all[["coefficients"]][1]))) %>%
#   add_column(plant="all")
#   
# c3_table <- data.table(DOY=1:250) %>% 
#     mutate(Greenness=.3 + .2/(1+exp(-model_c3[["coefficients"]][2]*DOY-model_c3[["coefficients"]][1]))) %>%
#     add_column(plant="c3")
# 
# 
# all_curves_table<-all_plants_table%>%
#   rbind(phrag_table) %>%
#   rbind(c3_table)
# 
# ggplot(data=all_curves_table) + 
#   geom_line(mapping=aes(x=DOY,y=Greenness, colour=plant))



#Next goal to see if they will all work at once
############# Using factors

##### Manually Creating lists
# mylist<-list(phrag_table,c3_table,all_plants_table)
# models<-mylist %>% map(~ lm(logit(Greenness,0,1) ~ DOY, data=.x))
# #This worked, but probably there is an easier wayto set it up
# ggplot(data=models[]) + geom_line(mapping=aes(x=DOY,y=Greenness, colour=plant))

#####Creating n models to correspond with n plant groups, will work with any number of plant groups

# Combine everything into groups based on plant types
plant_list3<- all_data4 %>%
  group_by(plant) %>%
  nest() #now the number of rows is the number of plant groups

# Linearize Greenness for each plant group at the same time using map
plant_model<-plant_list3$data %>% map(~ lm(logit(Green_Index,.3,.5) ~ DOY, data=.x))

# These two functions are necessary to pull the coefficients out of the model
mod_func<-function(mod) coefficients(mod)[[1]]
mod_func2<-function(mod) coefficients(mod)[[2]]

# Again using map to create a data table containing each curve which is then graphed using ggplot
plant_list4<- plant_list3 %>%
  select(plant) %>% #Only keep plant
  mutate(m1=map_dbl(plant_model,mod_func)) %>% #puts corresponding coefficients in for each plant
  mutate(m2=map_dbl(plant_model,mod_func2)) %>% 
  #Some values did not have enough to come up with curve, they are removed here
  drop_na() %>%
  mutate(DOY=list(1:250)) %>%
  unnest() %>%
  # formula for logistic curve is in the form: y=ymin+(ymax-ymin)/(1+e^(-m2*DOY+m1))
  mutate(Greenness_Est=.3+.2/(1+exp(-m2*DOY-m1)))

#plot the line based on color
ggplot(data=plant_list4) + 
  geom_line(mapping=aes(x=DOY,y=Greenness_Est, colour=plant)) + 
  geom_point(data=all_data4,mapping=aes(x=DOY,y=Green_Index,colour=plant))

##### Next goal: model based on location that the photo was taken

#First need make column for each string, should be able to cut the string before the first underscore
#Then need to do the same methods as the plant except sorting by the location
library(stringr) #used for string editing

string_list<-all_data4 %>%
  #Filename format should mean that first underscore is before the date
  mutate(location1=substr(Filename,1,str_locate(Filename,"_")-1)) %>%
  #Some dates have multiple photos of the same site, so there is a number at the end that needs to be removed
  mutate(location2=stringr::str_replace_all(string_list$location1,"\\d$",""))


string_list2 <- string_list %>%
  group_by(location2) %>%
  nest() #number of rows is same as number of vantage points
## There are some repeat photos of sites on the same day

location_model<-string_list2$data %>% 
  map(~ lm(logit(Green_Index,.3,.5) ~ DOY, data=.x)) 

#The same functions are going to be used again
# mod_func<-function(mod) coefficients(mod)[[1]]
# mod_func2<-function(mod) coefficients(mod)[[2]]

string_list3<- string_list2 %>%
  #for the sake of faceting by the plant community, select is not used here
  mutate(m1=map_dbl(location_model,mod_func)) %>%
  mutate(m2=map_dbl(location_model,mod_func2)) %>%
  drop_na() %>%
  unnest() %>%
  #keeps all combinations of plant and vantage
  distinct(location2,plant,.keep_all=TRUE) %>%
  #counts the DOY by 5 to keep the list smaller
  mutate(DOY2=list(seq(1,250,by=5))) %>%
  unnest() %>%
  #New column is the logistic function
  mutate(Greenness_Est=.3+.2/(1+exp(-m2*DOY2-m1)))

#
# fun1 <- function(x, y1, y2) .3+.2/(1+exp(-y2*x-y1))
# function_list<- string_list3 %>%
#   map2(y1=m1,y2=m2,fun1)


ggplot(data=string_list3) + 
  geom_line(mapping=aes(x=DOY2,y=Greenness_Est,colour=location2)) +
  geom_point(data=string_list, mapping=aes(x=DOY,y=Green_Index,colour=location2)) +
  facet_wrap(.~plant)



##### Looking at error

error_func<- function(mod) fitted.values(mod)

error_location<- string_list2 %>%
  mutate(fitted_values=map(location_model,error_func)) %>%
  unnest() %>%
  drop_na()
  
ggplot(data=error_location) + 
  geom_line(mapping=aes(x=DOY,y=fitted_values,colour=location2)) + 
  geom_errorbar(mapping=aes(x=DOY,ymax=logit(Green_Index,.3,.5),ymin=fitted_values,colour=location2))

error_plant<- plant_list3 %>%
  mutate(fitted_values=map(plant_model,error_func)) %>%
  unnest() %>%
  drop_na()

ggplot(data=error_plant) + 
  geom_line(mapping=aes(x=DOY,y=fitted_values,colour=plant)) + 
  geom_errorbar(mapping=aes(x=DOY,ymax=logit(Green_Index,.3,.5),ymin=fitted_values,colour=plant))

#largest error seems to occur in the middle of the growing season, at ends the values converge more



######### Using mixed effect modelling

#The next few lines remove some of the data points that have too few values to model
#E.g. some of the locations with only a few photos, some of the plants that appear only once
string_list_me <- string_list2[-c(1,5,6,7), ] %>%
  unnest()

string_list_me<-string_list_me[-c(16,21), ] 



library(lme4)

data_me <- lmer(logit(Green_Index,.3,.5) ~ DOY + plant + (1|location2), data=string_list_me, REML = FALSE)

summary(data_me)
