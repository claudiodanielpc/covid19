###Total confirmed COVID-19 deaths by country 
##Loading required packages


if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, gganimate,utils,
               dplyr,gifski,lubridate)

 ##Johns Hopkins Github's URL
urljh<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

##Download the data
jh<-read.csv(urljh,
             header=T)

##Tidy the dataset

jh<-jh%>%
  ##Rename all variables to lower
  rename_all(tolower)%>%
  #long format
  gather(date, value, -country.region,-province.state,-lat,-long)%>%
  ##Remove "x" from date variable
  mutate(date = mdy(str_remove(date, "x")))%>%
  ##Select only the variables that we need to plot the chart
  select(country.region,date,value)%>%
  ##Group by country and date and sum the number of deaths
  group_by(country.region,date)%>%
  summarise(value=sum(value))%>%
  ungroup()%>%
  ##Keep only data of dates greater than March 31, 2020
  filter(date>=as.Date("2020-04-01"))%>%
# Group by date, arrange, rank the data according to daily value 
  ##and filter the first ten places of each day 
  group_by(date)%>%
arrange(date,-value)%>%
  mutate(rank=1:n())%>%
  ungroup()%>%
  filter(rank<=10)

##Color palette for the bars in the chart
my_pal<-c('#a6cee3','#1f78b4','#b2df8a',
          '#33a02c','#fb9a99','#e31a1c','#fdbf6f',
          '#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928',"#990000")


##Plot
p<-jh%>%
  ggplot(aes(rank, group = country.region)) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9, fill=country.region), alpha = 0.9) +
  scale_fill_manual(values = my_pal) +
  scale_x_reverse()+
  ##Define the limits of the chart... You can rescale if you want.
  scale_y_continuous(limits = c(-5000, 100000))+
  theme_minimal()+
  # Labels on bars
  geom_text(aes(y = value, label = format(value,big.mark = ",")),
             hjust =0, size = 8,position = position_dodge(width = 1))+
  geom_text(aes(y = 0, label = country.region),
             hjust = "right", size = 7) +
  geom_text(aes(x=8,y=80000, label=paste0(date)),size=15, color = 'gray45')+
  # Titles and all that stuff
  labs(title = "Total confirmed COVID-19 deaths by country",
       subtitle = paste0("Last updated: ", format(max(jh$date),"%Y-%b-%d")),
       caption = "Source: @claudiodanielpc with data from Johns Hopkins University (JHU)",
       x = '', y = '') +
  # Define horizontal bars
  coord_flip(clip = 'off') +
  theme(plot.title = element_text(hjust = 0, size=20,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=12, face="italic"),
        plot.caption = element_text(hjust = 0,size=8),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.text.x = element_blank())
##Show the plot
p

##Begin the animation with gganimate :)
anim<-p+
transition_states(date,
                  transition_length = 1,
                  state_length = 0,
                  wrap = FALSE)

anim %>% animate(fps = 30,
                 nframes = 300,
                 detail = 10,
                 start_pause = 30,
                 end_pause = 40,
                 rewind = FALSE,
                 width = 1500,
                 height = 800)

# Save the gif
anim_save('barracecovid.gif')