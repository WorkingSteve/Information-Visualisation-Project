install.packages('ggplot2')
install.packages('plotly')
install.packages('tidyverse')
install.packages('dplyr')

start.time <- Sys.time()
library('ggplot2')
library('plotly')
library('tidyverse')
library('dplyr')

#read data
data = read.csv("C:/cities_air_quality_water_pollution.18-10-2021.csv")


#remove region columns
data1 = data %>% select(-Region)

#rename Country to region to left join map 
colnames(data1)[2] <- 'region'
#view(data1)


#get a mean value from every region
wpdata = aggregate(x=data1$WaterPollution, by=list(region=data1$region), FUN=mean)
aqdata = aggregate(x=data1$AirQuality, by=list(region=data1$region), FUN=mean)

#check attributes / strings to see if it will match the mapdata to prevent left join errors
str(aqdata)
str(wpdata)
str(map_data)

#remove outermost spaces -> first or last spaces before a formal character checked & set numbers to int
wpdata = wpdata %>%
  mutate_all(trimws) %>%
  mutate(x=round(wpdata$x, digits=0))

#remove na values
wpdata = wpdata %>% drop_na(x)
  
#get the map package and left join the water pollution data by region
mapdata <- map_data('world')
wpdata1 <- left_join(mapdata, wpdata)

#remove na values
wpdata1 = wpdata1 %>% drop_na(x)

#pollution water map
wpmap <- ggplot(wpdata1, aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(data = region, fill = x, text = paste(region, ': ', x),), color = "black") + 
  scale_fill_steps(name='Water Polluted Range', low='white',high='red') + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    rect = element_blank()
  )
ggplotly(wpmap + ggtitle("Water Pollution in Every Country"), tooltip = 'text')

#remove outermost spaces -> first or last spaces before a formal character checked & set numbers to int
aqdata = aqdata %>%
  mutate_all(trimws) %>%
  mutate(x=round(100-aqdata$x, digits=0))

#remove na values
aqdata = aqdata %>% drop_na(x)
view(aqdata)

#get the map package and left join the water pollution data by region
mapdata <- map_data('world')
aqdata1 <- left_join(mapdata, aqdata)

#remove na values
aqdata1 = aqdata1 %>% drop_na(x)

#pollution air map
aqmap <- ggplot(aqdata1, aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(data = region, fill = x, text = paste(region, ': ', x),), color = "black") + 
  scale_fill_steps(name='Air Polluted Range', low='white',high='red') + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    rect = element_blank()
  )
ggplotly(aqmap + ggtitle("Air Pollution in Every Country"), tooltip = 'text')

#find top 10 most polluted water country
top10_wp = wpdata[order(-wpdata$x),] %>% top_n(10)
wpbar<- ggplot(top10_wp, aes(x=region, y=x)) + 
  geom_col(aes(fill = x, text = paste(region, ': ', x)), width = 0.8) + 
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
  ) + 
  scale_fill_steps(name='Water Polluted Range')
ggplotly(wpbar + coord_flip() + ggtitle("Top 10 Countries with the Most Polluted Water"), tooltip='text')

#find top 10 most polluted air country
top10_ap = aqdata[order(-aqdata$x),] %>% top_n(10)
apbar<- ggplot(top10_ap, aes(x=region, y=x)) + 
  geom_col(aes(fill = x, text = paste(region, ': ', x)), width = 0.8) + 
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
  ) + 
  scale_fill_steps(name='Air Polluted Range')
ggplotly(apbar + coord_flip() + ggtitle("Top 10 Countries with the Most Polluted Air"), tooltip='text')

#find top 10 cleanest water country
top10_wc = wpdata[order(wpdata$x),] %>% slice(1:10)
top10_wc = top10_wc %>% mutate(x=(100-top10_wc$x))
wcbar<- ggplot(top10_wc, aes(x=region, y=x)) + 
  geom_col(aes(fill = x, text = paste(region, ': ', x)), width = 0.8) + 
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
  ) + 
  scale_fill_steps(name='Clean Water Range', low='lightgrey',high='royalblue')
ggplotly(wcbar + coord_flip() + ggtitle("Top 10 Countries with the Cleanest Water"), tooltip='text')

#find top 10 cleanest air country
top10_ac = aqdata[order(aqdata$x),] %>% slice(1:10)
top10_ac = top10_ac %>% mutate(x=(100-top10_ac$x))
acbar<- ggplot(top10_ac, aes(x=region, y=x)) + 
  geom_col(aes(fill = x, text = paste(region, ': ', x)), width = 0.8) + 
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
  ) + 
  scale_fill_steps(name='Clean Air Range', low='lightgrey',high='royalblue')
ggplotly(acbar + coord_flip() + ggtitle("Top 10 Countries with the Cleanest Air"), tooltip='text')

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken











