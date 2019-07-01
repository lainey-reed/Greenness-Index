## Curating phenopix outputs #####

library(tidyverse)

current_data<-read_csv("all_data3.csv")

current_data2<-select(current_data, Filename, plant) #only two columns selected

current_data3<-select(current_data,-X1) #removed column X1

glimpse(current_data3) # same as head, displays columns downward w/o cutting them off

head(current_data3)

filtered_data<-filter(current_data3,Green_Index>.45)
glimpse(filtered_data)

mutated_data<-mutate(filtered_data,Violet_Index=Red_index*Blue_Index/Green_Index)


 # use the pipe shape to do mulitple things at once
curated_data<- current_data %>% #data frame acted on is the first line
  select(-X1) %>% 
  filter(Green_Index>.45) %>%
  mutate(Violet_Index=Red_index*Blue_Index/Green_Index)

grouped_data<-all_data3 %>% 
  group_by(Filename,plant)%>%
  summarise(n=n(),Greenness=mean(Green_Index,na.rm=TRUE))


gathered_data <- gather(current_data, key=Color,value=Digital_Numbers,contains("Avg"),-Brightness_Avg)
glimpse(gathered_data)
unique(gathered_data$Color)

spread_data <- spread(gathered_data,key=)