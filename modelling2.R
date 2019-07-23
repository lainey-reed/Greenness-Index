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
  mutate(location=stringr::str_replace_all(string_list$location1,"\\d$",""))


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



data_me4 <- lmer(logit(Green_Index,.3,.5) ~ DOY + plant + (1+ plant|location2), data=string_list_me, REML = FALSE)

summary(data_me4)
coef_me3<-coef(data_me3)
string_list_me$plant


plant_list5<- string_list %>%
  group_by(plant) %>%
  nest() %>%
  #for the sake of faceting by the plant community, select is not used here
  mutate(m1=map_dbl(plant_model,mod_func)) %>%
  mutate(m2=map_dbl(plant_model,mod_func2)) %>%
  drop_na() %>%
  unnest() %>%
  filter(location2=="rightmarsh" | location2=="estuary" | location2=="centermarsh") %>%
  
# distinct(location2,plant,.keep_all=TRUE) %>%
  # #counts the DOY by 5 to keep the list smaller
  # mutate(DOY2=list(seq(1,250,by=5))) %>%
  #unnest() %>%
  #New column is the logistic function
  mutate(Greenness_Est=.3+.2/(1+exp(-m2*DOY-m1))) %>%
  mutate(difference=Green_Index-Greenness_Est)

mixed_effects_model6 <- lmer(difference ~ + (1|location2), data=plant_list5)
coef(mixed_effects_model6)
summary(mixed_effects_model6)
qqnorm(resid(mixed_effects_model6))
qqline(resid(mixed_effects_model6))

# 1 - has plant and DOY, difference is the Y
# 2 - has just plant, difference is the y
# 3 - plant and DOY, logit of GreenIndex is the Y - makes the error bigger, use difference as the y
# 4 - same as 3, just DOY
# 5 - no x
# 6 

#ggplot(data=coef_model2) +
#  geom_line(mapping=aes(x=1:250,y=x*coef_model2$location2["plantmixed"]))



ggplot(data=all_data_2018) +
  geom_point(mapping=aes(x=DOY,y=Green_Index,colour=plant))

all_data_2018<-bind_rows(all_data_2018,all_data4)

all_data_2018_2 <- all_data_2018 %>%
  filter(DOY<175) %>%
  mutate(location1=substr(Filename,1,str_locate(Filename,"_")-1))# %>%
  #Some dates have multiple photos of the same site, so there is a number at the end that needs to be removed
  
all_data_2018_2<- all_data_2018_2 %>%
  mutate(location2=stringr::str_replace_all(all_data_2018_2$location1,"\\d$","")) %>%
  group_by(plant) %>%
  nest()

y_min<-min(all_data_2018$Green_Index)
y_max<-max(all_data_2018$Green_Index)


model_2018_greenup<- all_data_2018_2$data %>% 
  map(~ lm(logit(Green_Index,.3,.52) ~ DOY, data=.x)) 
      
data_2018_3<-all_data_2018_2 %>%
  #select(plant) %>% #Only keep plant
  mutate(m1=map_dbl(model_2018_greenup,mod_func)) %>% #puts corresponding coefficients in for each plant
  mutate(m2=map_dbl(model_2018_greenup,mod_func2)) %>% 
  #Some values did not have enough to come up with curve, they are removed here
  drop_na() %>%
  unnest() %>%
  distinct(location2,plant,.keep_all=TRUE) %>%
  #counts the DOY by 5 to keep the list smaller
  mutate(DOY2=list(seq(1,250,by=5))) %>%
  unnest() %>%
  # formula for logistic curve is in the form: y=ymin+(ymax-ymin)/(1+e^(-m2*DOY+m1))
  mutate(Greenness_Est=.3+(.18)/(1+exp(-m2*DOY2-m1)))

ggplot(data=data_2018_3) +
  geom_line(mapping=aes(x=DOY2,y=Greenness_Est,colour=plant)) +
  facet_wrap(.~location2)


all_data_green_up<-rbind(all_data_2018,all_data4)
  
all_data_green_up_2 <- all_data_green_up %>%
  filter(DOY<175) %>%
  mutate(location1=substr(Filename,1,str_locate(Filename,"_")-1))# %>%
#Some dates have multiple photos of the same site, so there is a number at the end that needs to be removed

all_data_green_up_2<- all_data_green_up_2 %>%
  mutate(location2=stringr::str_replace_all(all_data_green_up_2$location1,"\\d$","")) %>%
  group_by(plant) %>%
  nest()

y_min<-min(all_data_green_up$Green_Index)
y_max<-max(all_data_green_up$Green_Index)


model_greenup<- all_data_green_up_2$data %>% 
  map(~ lm(logit(Green_Index,.3,.55) ~ DOY, data=.x)) 

  

data_green_up_3<-all_data_green_up_2 %>%
  #select(plant) %>% #Only keep plant
  mutate(m1=map_dbl(model_greenup,mod_func)) %>% #puts corresponding coefficients in for each plant
  mutate(m2=map_dbl(model_greenup,mod_func2)) %>% 
  #Some values did not have enough to come up with curve, they are removed here
  drop_na() %>%
  unnest() %>%
  distinct(location2,plant,.keep_all=TRUE) %>%
  #counts the DOY by 5 to keep the list smaller
  mutate(DOY2=list(seq(25,250,by=5))) %>%
  unnest() %>%
  # formula for logistic curve is in the form: y=ymin+(ymax-ymin)/(1+e^(-m2*DOY+m1))
  mutate(Greenness_Est=.3+(.25)/(1+exp(-m2*DOY2-m1)))

ggplot(data=data_green_up_3) +
  geom_line(mapping=aes(x=DOY2,y=Greenness_Est,colour=plant)) +
  geom_point(data=all_data_green_up, mapping=aes(x=DOY,y=Green_Index,colour=plant))
  #facet_wrap(.~location2)
  
#Some went into excel for editing, make sure date formats are the same

all_data_tested<-rbind(all_data_2017,all_data_july,all_data_april,all_data_fall_2018,all_data3_edited,all_data5)

all_data_tested2 <- all_data_tested%>% 
  mutate(plant=fct_recode(plant, "mixed"="spartina","mixed"="grass"))

all_data_tested2<-all_data_tested2 %>% 
  mutate(plant=fct_recode(plant, "phragmites"="phragmties","c3"="C3"))

ggplot(data=all_data_tested2) +
  geom_point(mapping=aes(x=DOY,y=Green_Index,colour=plant))

library(phenopix)
get_date<-function(x) extractDateFilename(x,"mm_dd_yy_HHMM")

Filenames<-all_data_tested2$Filename

date<-map_dbl(Filenames,get_date)
date2<-as.POSIXct(date,tz="",origin="1970-01-01")

all_data_tested2<- all_data_tested2 %>%
  mutate(Date2=date2) 

all_data_tested3 <- all_data_tested2 %>%
  select(-Date) %>%
  #filter(DOY<175) %>%
  mutate(location1=substr(Filename,1,str_locate(Filename,"_")-1))# %>%
  #Some dates have multiple photos of the same site, so there is a number at the end that needs to be removed

all_data_tested3<- all_data_tested3 %>%
  mutate(location2=stringr::str_replace_all(all_data_tested3$location1,"\\d$","")) %>%
  group_by(plant) %>%
  nest()


model_all_data<- all_data_tested3$data %>% 
  
  map(~ lm(logit(Green_Index,.3,.5) ~ DOY, data=.x)) 

all_data_tested4<-all_data_tested3 %>%
  #select(plant) %>% #Only keep plant
  mutate(m1=map_dbl(model_all_data,mod_func)) %>% #puts corresponding coefficients in for each plant
  mutate(m2=map_dbl(model_all_data,mod_func2)) %>% 
  #Some values did not have enough to come up with curve, they are removed here
  drop_na() %>%
  unnest() %>%
  distinct(location2,plant,.keep_all=TRUE) %>%
  #counts the DOY by 5 to keep the list smaller
  mutate(DOY2=list(seq(1,250,by=5))) %>%
  unnest() %>%
  # formula for logistic curve is in the form: y=ymin+(ymax-ymin)/(1+e^(-m2*DOY+m1))
  mutate(Greenness_Est=.3+(.2)/(1+exp(-m2*DOY2-m1)))

ggplot(data=all_data_tested4) +
  geom_line(mapping=aes(x=DOY2,y=Greenness_Est,colour=plant)) +
  facet_wrap(.~location2)


all_data_tested5 <- all_data_tested2 %>%
  filter(DOY>250) %>%
  mutate(location1=substr(Filename,1,str_locate(Filename,"_")-1))# %>%
#Some dates have multiple photos of the same site, so there is a number at the end that needs to be removed

all_data_tested5<- all_data_tested5 %>%
  mutate(location2=stringr::str_replace_all(all_data_tested5$location1,"\\d$","")) %>%
  group_by(plant) %>%
  nest()


model_all_data2<- all_data_tested5$data %>% 
  map(~ lm(logit(Green_Index,.3,.5) ~ DOY, data=.x)) 

all_data_tested6<-all_data_tested5 %>%
  #select(plant) %>% #Only keep plant
  mutate(m1=map_dbl(model_all_data2,mod_func)) %>% #puts corresponding coefficients in for each plant
  mutate(m2=map_dbl(model_all_data2,mod_func2)) %>% 
  #Some values did not have enough to come up with curve, they are removed here
  drop_na() %>%
  unnest() %>%
  distinct(location2,plant,.keep_all=TRUE) %>%
  #counts the DOY by 5 to keep the list smaller
  mutate(DOY2=list(seq(250,365,by=5))) %>%
  unnest() %>%
  # formula for logistic curve is in the form: y=ymin+(ymax-ymin)/(1+e^(-m2*DOY+m1))
  mutate(Greenness_Est=.3+.5/(1+exp(-m2*(DOY2-250)+m1)))

ggplot(data=all_data_tested6) +
  geom_line(mapping=aes(x=DOY2,y=Greenness_Est,colour=plant)) +
  geom_line(data=all_data_tested4,mapping=aes(x=DOY2,y=Greenness_Est,colour=plant)) +
  facet_wrap(.~location2)
  

all_data_tested7 <-all_data_tested2 %>%
  select(-Date) %>%
  mutate(location1=substr(Filename,1,str_locate(Filename,"_")-1)) #

all_data_tested8 <- all_data_tested7 %>%
  mutate(location2=stringr::str_replace_all(all_data_tested7$location1,"\\d$",""))

write.csv(all_data_tested8,"all_data_tested.csv")

all_data_phragmites<- all_data_tested8 %>%
  filter(plant=="phragmites") 

ggplot(all_data_phragmites) +
  geom_point(mapping=aes(x=DOY,y=Green_Index))

#all_data_phragmites_2018 <-all_data_phragmites %>%
 # filter(Date > "2018-01-01 12:00:00" && Date < "2019-01-01 12:00:00")

model_phrag2 <-lm(logit(all_data_phragmites$Green_Index,.3325,.42) ~ all_data_phragmites$DOY)
c<-model_phrag2[["coefficients"]][["all_data_phragmites$DOY"]]
d<-(-model_phrag2[["coefficients"]][["(Intercept)"]]-1)/c

phrag<-data.table(DOY=60:200) %>%
  mutate(green=.3325+(.42-.3325)/(1+exp(-c*((DOY)-d))))

ggplot(phrag) +
  geom_line(mapping=aes(x=DOY,y=green))+
  geom_point(data=all_data_phragmites,mapping=aes(x=DOY,y=Green_Index))


phrag_model3<-nls(Green_Index ~ I(minp + (maxp-minp)/(1+exp(-mS*(DOY-S)))),
                  data=all_data_phragmites,
                  start=list(minp=.3325,maxp=.42,mS=.05,S=125))

phrag<-data.table(DOY=60:200) %>%
  mutate(green=coefficients(phrag_model3)[['minp']]+(coefficients(phrag_model3)[['maxp']]-coefficients(phrag_model3)[['minp']])/(1+exp(-coefficients(phrag_model3)[['mS']]*((DOY)-coefficients(phrag_model3)[['S']]))))

ggplot(phrag) +
  geom_line(mapping=aes(x=DOY,y=green))+
  geom_point(data=all_data_phragmites,mapping=aes(x=DOY,y=Green_Index))


phrag_model4<-nls(Green_Index ~ I(minp + (maxp-minp)*(1/(1+exp(-mS*(DOY-S)))+1/(1+exp(mA*(DOY-A)))-1)),
                  data=all_data_phragmites,
                  start=list(minp=.3345,maxp=.41,mS=.15,S=125,mA=.1,A=250))

minp<- coefficients(phrag_model4)[['minp']]
maxp<-coefficients(phrag_model4)[['maxp']]
mS<-coefficients(phrag_model4)[['mS']]
S<-coefficients(phrag_model4)[['S']]
mA<-coefficients(phrag_model4)[['mA']]
A<-coefficients(phrag_model4)[['A']]

phrag2<-data.table(DOY=1:365) %>%
  mutate(green=minp+(maxp-minp)*(1/(1+exp(-mS*(DOY-S)))+1/(1+exp(mA*(DOY-A)))-1)) %>%
  mutate(plant="phragmites")

ggplot(phrag2) +
  geom_line(mapping=(aes(x=DOY,y=green)))+
  geom_point(data=all_data_phragmites,mapping=aes(x=DOY,y=Green_Index))

all_data_tested9<-all_data_tested8 %>%
  group_by(plant) %>%
  nest()

#This is not working, need to try to get more accurate start estimates, so will do that by the plant
model_by_plant <- function(df) nls(Green_Index ~ I(minp + (maxp-minp)*(1/(1+exp(-mS*(DOY-S)))+1/(1+exp(mA*(DOY-A)))-1)),
                                   data=df,
                                   start=list(minp=.3345,maxp=.43,mS=.15,S=125,mA=.1,A=250))

all_data_tested10<- all_data_tested9 %>%
  mutate(model=map(data,model_by_plant))

#Steps: sort by plant
# for each plant, estimate a min and a max
# solve for green-up parameters
# sovle for green-down parameters
# use nls with new parameters

#estimate min and max
#going to find average between DOY=150 and 200 for each plant, use as the min
#average between 70 and 100 will be the min
max_est<-all_data_tested9 %>%
  unnest() %>%
  filter(DOY>=150 & DOY<=200) %>%
  group_by(plant) %>%
  summarise(max_avg=mean(Green_Index)) 

min_est<-all_data_tested9 %>%
  unnest() %>%
  filter((DOY>=70 & DOY<=100) | (DOY>=300 & DOY<=365)) %>%
  group_by(plant) %>%
  summarise(min_avg=mean(Green_Index))

#### now use those to come up with two linear models, one for green-up, one for green-down for each plant
# solving for mS, S, mA, A
# going to do DOY until 200 for green-up
# will do 175 to 365 for green-down

green_up<- all_data_tested9 %>%
  unnest() %>%
  filter(DOY<=200) %>%
  group_by(plant) %>%
  nest() %>%
  left_join(max_est,by="plant") %>%
  left_join(min_est,by="plant") %>%
  unnest() %>%
  #mutate(y2=if_else(Green_Index<min_avg,min_avg,Green_Index)) %>%
  #mutate(y3=if_else(y2>max_avg,max_avg,y2)) %>%
  mutate(y_log=log((Green_Index-min_avg)/(max_avg-min_avg))) %>%
  drop_na(y_log) %>%
  group_by(plant) %>%
  nest()

green_up_model<- green_up$data %>%
  map(~ lm(y_log ~ DOY, data=.x)) 

mod_func3<-function(mod) (coefficients(mod)[[1]]+1)/(-1*coefficients(mod)[[2]])

mS2 <- map_dbl(green_up_model,mod_func2)
S2<- map_dbl(green_up_model,mod_func3)

green_down<- all_data_tested9 %>%
  unnest() %>%
  filter(DOY>180) %>%
  group_by(plant) %>%
  nest() %>%
  left_join(max_est,by="plant") %>%
  left_join(min_est,by="plant") %>%
  unnest() %>%
  #mutate(y2=if_else(Green_Index<min_avg,min_avg,Green_Index)) %>%
  #mutate(y3=if_else(y2>max_avg,max_avg,y2)) %>%
  mutate(y_log=log((Green_Index-min_avg)/(max_avg-min_avg))) %>%
  mutate(y_log2=if_else(is.finite(y_log),y_log,)) %>%
  drop_na() %>%
  group_by(plant)# %>%
  nest()

green_down_model<- green_down$data %>%
  map(~ lm(y_log ~ DOY, data=.x)) 

mod_func3<-function(mod) (coefficients(mod)[[1]]+1)/(-1*coefficients(mod)[[2]])

mA2 <- map_dbl(green_up_model,mod_func2)
A2<- map_dbl(green_up_model,mod_func3)



###############################################

all_data_c3<-all_data_tested8 %>%
  filter(plant=="c3") 

c3_model4<-nls(Green_Index ~ I(minp + (maxp-minp)*(1/(1+exp(-mS*(DOY-S)))+1/(1+exp(mA*(DOY-A)))-1)),
                  data=all_data_c3,
                  start=list(minp=.3345,maxp=.41,mS=.15,S=125,mA=.1,A=250))

minp_c3<- coefficients(c3_model4)[['minp']]
maxp_c3<-coefficients(c3_model4)[['maxp']]
mS_c3<-coefficients(c3_model4)[['mS']]
S_c3<-coefficients(c3_model4)[['S']]
mA_c3<-coefficients(c3_model4)[['mA']]
A_c3<-coefficients(c3_model4)[['A']]

c32<-data.table(DOY=1:365) %>%
  mutate(green=minp_c3+(maxp_c3-minp_c3)*(1/(1+exp(-mS_c3*(DOY-S_c3)))+1/(1+exp(mA_c3*(DOY-A_c3)))-1)) %>%
  mutate(plant="c3")

ggplot(c32) +
  geom_line(mapping=(aes(x=DOY,y=green)))+
  geom_point(data=all_data_c3,mapping=aes(x=DOY,y=Green_Index))

########################################33

all_data_iva<-all_data_tested8 %>%
  filter(plant=="iva" | plant=="mixed") 

iva_model4<-nls(Green_Index ~ I(minp + (maxp-minp)*(1/(1+exp(-mS*(DOY-S)))+1/(1+exp(mA*(DOY-A)))-1)),
               data=all_data_iva,
               start=list(minp=.3345,maxp=.42,mS=.16,S=125,mA=.1,A=250))

minp_iva<- coefficients(iva_model4)[['minp']]
maxp_iva<-coefficients(iva_model4)[['maxp']]
mS_iva<-coefficients(iva_model4)[['mS']]
S_iva<-coefficients(iva_model4)[['S']]
mA_iva<-coefficients(iva_model4)[['mA']]
A_iva<-coefficients(iva_model4)[['A']]

iva2<-data.table(DOY=1:365) %>%
  mutate(green=minp_iva+(maxp_iva-minp_iva)*(1/(1+exp(-mS_iva*(DOY-S_iva)))+1/(1+exp(mA_iva*(DOY-A_iva)))-1)) %>%
  mutate(plant="iva")

ggplot(iva2) +
  geom_line(mapping=(aes(x=DOY,y=green)))+
  geom_point(data=all_data_c3,mapping=aes(x=DOY,y=Green_Index))


ggplot(data=all_data_tested8)+
  geom_point(mapping=aes(x=DOY,y=Green_Index,colour=plant)) +
  geom_line(data=iva2,mapping=aes(x=DOY,y=green,colour=plant)) +
  geom_line(data=c32,mapping=aes(x=DOY,y=green,colour=plant)) +
  geom_line(data=phrag2,mapping=aes(x=DOY,y=green,colour=plant))



########################################
#So here I am trying to use multstart to do multiple nls runs at once, with each plant species
# I also went through in excel and edited the plant groups
#plants_edited has plant groups mixed changed to have specific species
#They are divided into major and minor, not sure if I will use the minor
#Will need to change the exctract VIs to include this

library(nls.multstart)

plants_edited<-read.csv("plants_edited.csv")

plants_edited<- plants_edited %>%
  select(-Green_Index) %>%
  rename(Filename=ï..Filename,plant2=plant)

all_data_edited_plants <- all_data_tested2 %>%
  full_join(plants_edited,by=c("Filename","ROI"))

all_data_edited_plants3<-all_data_edited_plants %>%
  unite("Major_Plants",c("Major_Plant_1","Major_Plant_2"),remove=FALSE)


ggplot(all_data_edited_plants3) +
  geom_point(mapping=aes(x=DOY,y=Green_Index,colour=Major_Plants)) +
  ggtitle("DOY vs. Green Index of all data \nSorted by Major Plant Communities")

all_data_edited_plants2 <- all_data_edited_plants %>%
  group_by(Major_Plant_1,Major_Plant_2) %>%
  nest()

non_linear_model <- function(DOY,minGI,maxGI,mS,S,mA,A) {
  GI_est <- minGI + (maxGI-minGI)*(1/(1+exp(-mS*(DOY-S)))+1/(1+exp(mA*(DOY-A)))-1)
  return(GI_est)
}

#heres the multstart function, it needs an upper and lower bound which is useful since they vary by species somewhat
non_linear_fit <- all_data_edited_plants3 %>%
  group_by(Major_Plants) %>%
  nest() %>%
  mutate(fit=purrr::map(data,~nls_multstart(Green_Index ~ non_linear_model(DOY=DOY,minGI,maxGI,mS,S,mA,A),
                                            data=.x,
                                            iter=500,
                                            start_lower=c(minGI=.32,maxGI=.375,mS=.1,S=115,mA=.001,A=200),
                                            start_upper =c(minGI=.34,maxGI=.52,mS=.3,S=150,mA=.1,A=275),
                                            supp_errors = "N")))

#according to the tutorial I should just be able to use broom, but it's not working
select(non_linear_fit,Major_Plants,data,fit)
fit<-non_linear_fit$fit

minGI_func<-function(list1) coefficients(list1)[["minGI"]]

fit2<-map(fit,mod_func3) 
fit3<-fit2[-10]

fit3<-transpose(as.data.frame(fit3))
names(fit3)<-c("minGI","maxGI","mS","S","mA","A")

fit3 <- fit3 %>%
  mutate(Y=1:11)

#here's all the data with all the coefficients
#typha got removed, not enough data points
#also need to remove 2, 7,8,9
all_data_edited_plants4<-all_data_edited_plants3 %>%
  group_by(Major_Plants) %>%
  nest() %>%
  filter(Major_Plants!="typha_") %>%
  mutate(Y=1:11) %>%
  left_join(fit3,by="Y") %>%
  select(-Y) %>%
  filter(Major_Plants!="c4_" & Major_Plants!="iva_phragmites" & Major_Plants!="spartina_" & Major_Plants!="c4_iva") %>%
  select(-data)

all_data_edited_plants5<- all_data_edited_plants4 %>%
  mutate(DOY=list(seq(1,365,by=5))) %>%
  unnest() %>%
  mutate(Green_Est=minGI + (maxGI-minGI)*(1/(1+exp(-mS*(DOY-S)))+1/(1+exp(mA*(DOY-A)))-1))

ggplot(all_data_edited_plants5) +
  geom_line(mapping=aes(x=DOY,y=Green_Est,color=Major_Plants)) +
  geom_point(data=all_data_edited_plants3,mapping=aes(x=DOY,y=Green_Index,color=Major_Plants))


all_data_edited_plants6<- all_data_edited_plants3 %>%
  separate_rows(Major_Plants) %>%
  filter(Major_Plants!="") 

non_linear_fit2 <- all_data_edited_plants6 %>%
  group_by(Major_Plants) %>%
  nest() %>%
  mutate(fit=purrr::map(data,~nls_multstart(Green_Index ~ non_linear_model(DOY=DOY,minGI,maxGI,mS,S,mA,A),
                                            data=.x,
                                            iter=500,
                                            start_lower=c(minGI=.32,maxGI=.375,mS=.1,S=115,mA=.001,A=200),
                                            start_upper =c(minGI=.34,maxGI=.52,mS=.3,S=150,mA=.1,A=275),
                                            supp_errors = "N")))

## using this everything converges!!!!

fit_model2<-non_linear_fit2$fit
fit_model2_2<-transpose(as.data.frame(map(fit_model2,mod_func3)))
names(fit_model2_2)<-c("minGI","maxGI","mS","S","mA","A")
fit_model2_2<-fit_model2_2 %>%
  mutate(Y=1:6)

all_data_edited_plants7<-all_data_edited_plants6 %>%
  group_by(Major_Plants) %>%
  nest() %>%
  mutate(Y=1:6) %>%
  left_join(fit_model2_2,by="Y") %>%
  select(-Y) %>%
  select(-data) %>%
  mutate(DOY=list(seq(1,365,by=5))) %>%
  unnest() %>%
  mutate(Green_Est=minGI + (maxGI-minGI)*(1/(1+exp(-mS*(DOY-S)))+1/(1+exp(mA*(DOY-A)))-1))

ggplot(all_data_edited_plants7) +
  geom_line(mapping=aes(x=DOY,y=Green_Est,color=Major_Plants)) +
  geom_point(data=all_data_edited_plants6,mapping=aes(x=DOY,y=Green_Index,color=Major_Plants))
