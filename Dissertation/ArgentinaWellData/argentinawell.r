
library(ggrepel)

df = read_csv("ArgentinaWellData.csv", col_names = TRUE)

glimpse(df)

str(df)
colnames(df)

df<-df[,-c(16)]


count(df[which(df$Drilling_Start=='NULL'),c('Drilling_Start')])


as.date(df$Drilling_Start)



df%>%
  ggplot()+
  geom_bar(aes(x=reorder(df$Operator,df$well_cost),y=well_cost,fill=df$`Well type code`),stat='summary')+
  coord_flip()

df%>%
  ggplot()+
  geom_bar(aes(x=reorder(df$Field_Name,df$well_cost),y=well_cost,fill=df$Field_Name),stat='summary')+
  coord_flip()

df%>%
  ggplot()+
  geom_bar(aes(x=reorder(df$Operator,df$well_cost),y=well_cost,fill=df$Operator),stat='summary')+
  coord_flip()

df%>%
  ggplot()+
  geom_bar(aes(x=reorder(df$`Well type code`,df$well_cost),y=well_cost,fill=df$`Well type code`),stat='summary')+
  coord_flip()

df%>%
  ggplot()+
  geom_bar(aes(x=df$well_cost,reorder(Operator,well_cost),fill= df$`Well type code`),stat='summary')+
  geom_text(stat='count', aes(y=Operator,label=..count..), hjust=-0.3)


df%>%
  ggplot()+
  geom_bar(aes(x=df$well_cost,reorder(Operator,well_cost),fill= df$Operator),stat='summary')+
  geom_text(stat='count', aes(y=Operator,label=..count..), hjust=-0.3)

df%>%
  ggplot()+
  geom_bar(aes(x=df$Measured_Depth,y=reorder(Operator,df$Measured_Depth),fill=Operator),stat='summary')+
  geom_text(stat='count', aes(y=Operator,label=..count..), hjust=-0.3)

df%>%
ggplot()+      
  geom_bar(aes(x= df$Operator,fill=df$Operator))+
  coord_flip()

ggplot(df)+
  geom_line(aes(x=df$Drilling_Start, y=well_cost))+
  geom_label_repel(aes(x=Drilling_Start, y=well_cost, label=df$Completion_Date))


df%>%
  mutate(completion_time= df$Completion_Date-df$Drilling_Start)%>%
  ggplot(aes(x=Completion_Date,y=well_cost))+
  geom_line()

df%>%
  ggplot()+
  geom_bar(aes(x=reorder(df$state_name,df$well_cost), y=df$well_cost, 
               fill=df$Operator), 
           stat = "summary", 
           position="dodge")+
  coord_flip()

df%>%
  ggplot()+
  geom_bar(aes(x=reorder(df$state_name,df$well_cost), y=df$well_cost, 
               fill=df$Reservoir), 
           stat = "summary", 
           position="dodge")+
  coord_flip()


df%>%
  ggplot()+
  geom_bar(aes(x=reorder(df$state_name,df$well_cost), y=df$well_cost, 
               fill=df$state_name), 
           stat = "summary", 
           position="dodge")+
  coord_flip()

df %>%
  group_by(df$state_name,df$Operator)%>%
  summarise(df$well_cost)


plot(df$Drilling_end,df$Completion_Date)

unique(as.factor(df$Field_Name))

#install.packages('lubridate')

ggplot(df)+
  geom_line(aes(x=format(Completion_Date,"%Y"), y=well_cost, group=format(Completion_Date,"%m")))+
  facet_wrap(~format(Completion_Date,"%m"))


df%>%
  ggplot()+
  coord_flip()+
  geom_boxplot(aes(x=df$well_cost, y=df$`Well type code`))


plot(temp$timetocomplete,temp$well_cost)

           
dtreedf<-df%>%
  select(state_name,Operator,Reservoir,`Well type code`,well_cost,Field_Name)%>%
  mutate(state_name=as.factor(state_name))%>%
  mutate(Operator=as.factor(Operator))%>%
  mutate(Reservoir=as.factor(Reservoir))%>%
  mutate(`Well type code`=as.factor(`Well type code`))%>%
  mutate(Field_Name=as.factor(Field_Name))

hist(df$well_cost)

df%>%
  filter(state_name!='Chubut')%>%
  ggplot()+
  geom_point(aes(x=log(well_cost), y=log(Measured_Depth),
                 color=state_name)) 

