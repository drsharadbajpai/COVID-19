#Installing Libraries
library(tidyverse)
library(ggmap)
library(dplyr)
library(lubridate)
library(ggplot2)
library(formattable)
library(plotly)
library(sf)
library(mapview)
options(scipen = 1000000000)
#Uploading Data Set
library(readr)
master_df <- read_csv("C:/Users/User/Downloads/Individual Project/Individual Project/COVID19/BRAZIL/owid-covid-data3.csv")
#owid_covid_data <- read_csv("../input/covid19/owid-covid-data3.csv")

summary(master_df)
master_df$new_cases<-abs(master_df$new_cases)
master_df$new_deaths<-abs(master_df$new_deaths)
master_df$new_cases_per_million<-abs(master_df$new_cases_per_million)
master_df$new_deaths_per_million<-abs(master_df$new_deaths_per_million)
summary(master_df)

TD_all<-master_df %>% 
  group_by(location) %>% 
  top_n(1, date) %>% 
  arrange(desc(total_deaths))
which(is.na(TD_all$location))
which(is.na(TD_all$total_cases))
which(is.na(TD_all$total_deaths))
which(is.na(TD_all$population))
TD_all<-TD_all[-c(1,212,159),]

TD_worldcount<-TD_all %>% 
  mutate(Tdeaths=sum(TD_all$total_deaths), Tcases=sum(TD_all$total_cases), Tpopulation=sum(TD_all$population), DR=(Tdeaths/Tpopulation)*100)


plot_1<- TD_worldcount %>% 
  select("location", "date", "total_cases", "total_deaths", "population", "gdp_per_capita", "diabetes_prevalence") %>% 
  arrange(desc(TD_worldcount$total_cases))
plot_1<-plot_1[-c(1),]

formattable(plot_1, align =c("l","c","c","c","c","c","r"), list(
  `plot_1$location` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), area(col = 3) ~ color_tile("#71CA97","#FA6148"),
  area(col = 4) ~ color_tile("#71CA97","#FA6148"),
  area(col = 5) ~ color_tile("#71CA97","#FA6148"),
  area(col = 6) ~ color_tile("#71CA97","#FA6148"),
  area(col = 7) ~ color_tile("#71CA97","#FA6148")))

#Creating subset of the dataset
world_data<-master_df %>% 
  filter(location%in%c("United States","Brazil", "Spain","France","Russia","Germany" ,"Turkey", "India", "Iran","Peru", "Canada", "China", "United Kingdom", "Italy", "Saudi Arabia", "Chile", "Mexico","Belgium", "Pakistan", "Qatar", .preserve=TRUE))

#Observing the dataset
str(world_data)
summary(world_data)

world_data<-world_data %>% 
  dplyr::mutate(year= lubridate::year(date),
                month= lubridate::month(date),
                day= lubridate::day(date))

T_country<-world_data %>% 
  group_by(location) %>% 
  top_n(1, date) 

plot_2<-ggplot(T_country, aes(reorder(location,total_cases), total_cases), fill="grey50")+
  geom_bar(stat = "identity") +
  labs(x="Country", y="Total Cases", subtitle = NULL)+
  ggtitle("Total cases by country")+
  scale_x_discrete(labels=c("United States"="USA", "Brazil"="BRZ", "Spain"="SPA","France"="FRA","Russia"="RUS","Germany"="GER" ,"Turkey"="TUR", "India"="IND", "Iran"="IRN", "Peru"="PRU", "Canada"="CAN", "China"="CHI", "United Kingdom"="UK", "Italy"="ITA", "Saudi Arabia"="SAU", "Chile"="CHL", "Mexico"="MEX","Belgium"="BEL", "Pakistan"="PAK", "Qatar"="QAT"))
ggplotly(plot_2)

plot_3<-ggplot(T_country, aes(reorder(location, total_cases_per_million), total_cases_per_million), fill="grey50")+
  geom_bar(stat = "identity") +
  labs(x="Country", y="Total Cases per million", subtitle = NULL)+
  ggtitle("Total cases per million by country")+
  scale_x_discrete(labels=c("United States"="USA", "Brazil"="BRZ", "Spain"="SPA","France"="FRA","Russia"="RUS","Germany"="GER" ,"Turkey"="TUR", "India"="IND", "Iran"="IRN", "Peru"="PRU", "Canada"="CAN", "China"="CHI", "United Kingdom"="UK", "Italy"="ITA", "Saudi Arabia"="SAU", "Chile"="CHL", "Mexico"="MEX","Belgium"="BEL", "Pakistan"="PAK", "Qatar"="QAT"))
ggplotly(plot_3)

plot_4<-ggplot(T_country, aes(reorder(location,total_deaths), total_deaths), fill="grey50")+
  geom_bar(stat = "identity") +
  labs(x="Country", y="Total Deaths", subtitle = NULL)+
  ggtitle("Total deaths by country")+
  scale_x_discrete(labels=c("United States"="USA", "Brazil"="BRZ", "Spain"="SPA","France"="FRA","Russia"="RUS","Germany"="GER" ,"Turkey"="TUR", "India"="IND", "Iran"="IRN", "Peru"="PRU", "Canada"="CAN", "China"="CHI", "United Kingdom"="UK", "Italy"="ITA", "Saudi Arabia"="SAU", "Chile"="CHL", "Mexico"="MEX","Belgium"="BEL", "Pakistan"="PAK", "Qatar"="QAT"))
ggplotly(plot_4)

plot_5<-ggplot(T_country, aes(reorder(location, total_deaths_per_million), total_deaths_per_million), fill="grey50")+
  geom_bar(position = "dodge", stat = "identity") +
  labs(x="Country", y="Total Deaths per million", subtitle = NULL)+
  ggtitle("Total deaths per million by country")+
  scale_x_discrete(labels=c("United States"="USA", "Brazil"="BRZ", "Spain"="SPA","France"="FRA","Russia"="RUS","Germany"="GER" ,"Turkey"="TUR", "India"="IND", "Iran"="IRN", "Peru"="PRU", "Canada"="CAN", "China"="CHI", "United Kingdom"="UK", "Italy"="ITA", "Saudi Arabia"="SAU", "Chile"="CHL", "Mexico"="MEX","Belgium"="BEL", "Pakistan"="PAK", "Qatar"="QAT"))
ggplotly(plot_5)


TT_country<-world_data %>% 
  group_by(location) %>% 
  top_n(1, total_tests) %>% 
  arrange(desc(total_tests))
head(TT_country)

plot_6<-ggplot(TT_country, aes(reorder(location, total_tests), total_tests), fill="grey50")+
  geom_bar(position = "dodge", stat = "identity") +
  labs(x="Country", y="Total Tests", subtitle = NULL)+
  ggtitle("Total Tests performed by country")+
  scale_x_discrete(labels=c("United States"="USA", "Brazil"="BRZ", "Spain"="SPA","France"="FRA","Russia"="RUS","Germany"="GER" ,"Turkey"="TUR", "India"="IND", "Iran"="IRN", "Peru"="PRU", "Canada"="CAN", "China"="CHI", "United Kingdom"="UK", "Italy"="ITA", "Saudi Arabia"="SAU", "Chile"="CHL", "Mexico"="MEX","Belgium"="BEL", "Pakistan"="PAK", "Qatar"="QAT"))
ggplotly(plot_6)

plot_7<-ggplot(TT_country, aes(reorder(location, total_tests_per_thousand), total_tests_per_thousand), fill="grey50")+
  geom_bar(position = "dodge", stat = "identity") +
  labs(x="Country", y="Total Tests per thousand", subtitle = NULL)+
  ggtitle("Total Tests per thousand performed by country")+
  scale_x_discrete(labels=c("United States"="USA", "Brazil"="BRZ", "Spain"="SPA","France"="FRA","Russia"="RUS","Germany"="GER" ,"Turkey"="TUR", "India"="IND", "Iran"="IRN", "Peru"="PRU", "Canada"="CAN", "China"="CHI", "United Kingdom"="UK", "Italy"="ITA", "Saudi Arabia"="SAU", "Chile"="CHL", "Mexico"="MEX","Belgium"="BEL", "Pakistan"="PAK", "Qatar"="QAT"))
ggplotly(plot_7)


plot_8<-ggplot(T_country, aes(reorder(location, new_deaths), new_deaths), fill="grey50")+
  geom_bar(position = "dodge", stat = "identity") +
  labs(x="Country", y="Deaths", subtitle = NULL)+
  ggtitle("New Deaths on 23rd of June 2020")+
  scale_x_discrete(labels=c("United States"="USA", "Brazil"="BRZ", "Spain"="SPA","France"="FRA","Russia"="RUS","Germany"="GER" ,"Turkey"="TUR", "India"="IND", "Iran"="IRN", "Peru"="PRU", "Canada"="CAN", "China"="CHI", "United Kingdom"="UK", "Italy"="ITA", "Saudi Arabia"="SAU", "Chile"="CHL", "Mexico"="MEX","Belgium"="BEL", "Pakistan"="PAK", "Qatar"="QAT"))
ggplotly(plot_8)

plot_9<-ggplot(T_country, aes(reorder(location, new_cases), new_cases), fill="grey50")+
  geom_bar(position = "dodge", stat = "identity") +
  labs(x="Country", y="New Cases", subtitle = NULL)+
  ggtitle("New Cases on 23rd of June 2020")+
  scale_x_discrete(labels=c("United States"="USA", "Brazil"="BRZ", "Spain"="SPA","France"="FRA","Russia"="RUS","Germany"="GER" ,"Turkey"="TUR", "India"="IND", "Iran"="IRN", "Peru"="PRU", "Canada"="CAN", "China"="CHI", "United Kingdom"="UK", "Italy"="ITA", "Saudi Arabia"="SAU", "Chile"="CHL", "Mexico"="MEX","Belgium"="BEL", "Pakistan"="PAK", "Qatar"="QAT"))
ggplotly(plot_9)

plot_10<-ggplot(T_country, aes(reorder(location, hospital_beds_per_thousand), hospital_beds_per_thousand), fill="grey50")+
  geom_bar(position = "dodge", stat = "identity") +
  labs(x="Country", y="Hospital beds per thousand", subtitle = NULL)+
  ggtitle("Hospital beds per thousand by country")+
  scale_x_discrete(labels=c("United States"="USA", "Brazil"="BRZ", "Spain"="SPA","France"="FRA","Russia"="RUS","Germany"="GER" ,"Turkey"="TUR", "India"="IND", "Iran"="IRN", "Peru"="PRU", "Canada"="CAN", "China"="CHI", "United Kingdom"="UK", "Italy"="ITA", "Saudi Arabia"="SAU", "Chile"="CHL", "Mexico"="MEX","Belgium"="BEL", "Pakistan"="PAK", "Qatar"="QAT"))
ggplotly(plot_10)


#Data by continents
#data_continent<- TD_all[-c(1),]
#data_continent<-as.data.frame(data_continent)
#library(countrycode)
#data_continent$continent <- countrycode(sourcevar = data_continent[, "location"],
 #                                       origin = "country.name",
  #                                      destination = "continent")
plot_13<-TD_all %>%
  group_by(continent) %>% 
  mutate(total_cases1=sum(total_cases), total_deaths1=sum(total_deaths), population1=sum(population))

plot_13<-plot_13 %>% 
  select(continent, total_cases1, total_deaths1, population1)
plot_13<- unique(plot_13)
plot_13a<-ggplot(plot_13, aes(reorder(continent, total_cases1), total_cases1, fill=total_cases1))+
  geom_bar(stat = "identity") +
  scale_fill_gradient(low="yellow", high= "orange")+
  labs(x="Continent", y="Total Cases", subtitle = NULL)+
  ggtitle("Total Cases by Continents as of 23rd of June 2020")
ggplotly(plot_13a)

plot_13b<-ggplot(plot_13, aes(reorder(continent, total_deaths1), total_deaths1, fill=total_deaths1))+
  geom_bar(stat = "identity") +
  scale_fill_gradient(low="green", high= "red")+
  labs(x="Continent", y="Total Deaths", subtitle = NULL)+
  ggtitle("Total Deaths by Continent as of 23rd of June 2020")
ggplotly(plot_13b)

preplot_13c<-plot_13 %>% 
  mutate(deathconv_rate=(total_deaths1/total_cases1)*100)
preplot_13c$deathconv_rate<-round(preplot_13c$deathconv_rate,2)

plot_13c<-ggplot(preplot_13c, aes(reorder(continent, deathconv_rate), deathconv_rate, fill=deathconv_rate))+
  geom_bar(stat = "identity") +
  scale_fill_gradient(low="lightblue", high= "darkblue")+
  labs(x="Continent", y="Death Conversion rate", subtitle = NULL)+
  ggtitle("Death conversion rate by Continent as of 23rd of June 2020")
ggplotly(plot_13c)

preplot_13d<-plot_13 %>% 
  mutate(deathrate=(total_deaths1/population1)*100, caserate=(total_cases1/population1)*100)

plot_13d<-ggplot(preplot_13d, aes(reorder(continent, deathrate), deathrate, fill=deathrate))+
  geom_bar(stat = "identity") +
  scale_fill_gradient(low="skyblue", high= "purple")+
  labs(x="Continent", y="Rate of deaths", subtitle = NULL)+
  ggtitle("Deathrate by Continent as of 23rd of June 2020")
ggplotly(plot_13d)

plot_13e<-ggplot(preplot_13d, aes(reorder(continent, caserate), caserate, fill=caserate))+
  geom_bar(stat = "identity") +
  scale_fill_gradient(low="pink", high= "violet")+
  labs(x="Continent", y="Rate of cases", subtitle = NULL)+
  ggtitle("Caserate by Continent as of 23rd of June 2020")
ggplotly(plot_13e)

#Spread of the disease by date
spread_df<- master_df %>% 
  filter(location!=c("World","International"))
which(is.na(spread_df$new_cases))
spread_df$new_cases[is.na(spread_df$new_cases)]<-0
spread_df$new_deaths[is.na(spread_df$new_deaths)]<-0

caseperday<-spread_df %>% 
  group_by(date) %>% 
  mutate(casesbyday=sum(new_cases))

caseperday_df <- caseperday %>% 
  select(date, casesbyday)
caseperday_df<-unique(caseperday_df)

plot_14<-ggplot(caseperday_df, aes(caseperday_df$date, caseperday_df$casesbyday))+
  geom_point(size=0.5, color="darkblue")+ geom_line(size=0.2, color="blue")+
  labs(x="Time period", y="Number of cases", subtitle = "As of 28th May 20") +
  ggtitle("Progression of the spread of COVID-19")
ggplotly(plot_14)

deathsperday<-spread_df %>% 
  group_by(date) %>% 
  mutate(deathsbyday=sum(new_deaths))

deathsperday_df <- deathsperday %>% 
  select(date, deathsbyday)
deathsperday_df<-unique(deathsperday_df)

plot_15<-ggplot(deathsperday_df, aes(deathsperday_df$date, deathsperday_df$deathsbyday))+
  geom_point(size=0.5, color="darkred")+ geom_line(size=0.2, color="red")+
  labs(x="Time period", y="Number of deathss", subtitle = "As of 28th May 20") +
  ggtitle("Progression of the deaths due to COVID-19")
ggplotly(plot_15)

CND_df<- left_join(caseperday_df, deathsperday_df, by= "date")
plot_16<- ggplot(CND_df)+ 
  geom_line(aes(date, casesbyday), color="darkblue")+
  geom_line(aes(date, deathsbyday), color="darkred")+
  geom_area(aes(date, casesbyday), fill="navyblue", alpha=1/5)+
  geom_area(aes(date, deathsbyday), fill="red", alpha=1/5)+
  labs(x="Time period", y="Number of cases")+
  ggtitle("Progression of cases and deaths over-time")
ggplotly(plot_16)
#---------------------------------------------------------------------------------------------------------------------------------------
#Progression of spread by continents:

if(FALSE){
  caseperday<-as.data.frame(caseperday)
caseperday$continent <- countrycode(sourcevar = caseperday[, "location"],
                                    origin = "country.name",
                                    destination = "continent")
}

cont1<- caseperday %>% 
  group_by(continent, date) %>% 
  mutate(ncase=sum(new_cases), ndeaths=sum(new_deaths)) %>% 
  select(continent, date, ncase, ndeaths)
cont1<-unique(cont1)
cont1$continent<-cont1$continent %>% replace_na("Others")

plot_17<-ggplot(cont1)+
  geom_line(aes(date, ncase), color="darkblue")+
  geom_line(aes(date, ndeaths), color="darkred")+
  geom_area(aes(date, ncase), fill="navyblue", alpha=1/5)+
  geom_area(aes(date, ndeaths), fill="red", alpha=1/5)+
  facet_wrap(~continent, scales = "free")+
  labs(x="Time period", y="Number of cases")+
  ggtitle("Progression of Covid-19 by continents")
ggplotly(plot_17)

#Progression of spread by top 20 current countries
country_caseperday<-world_data %>%
  select(location, date, new_cases, new_deaths)

plot_18<-ggplot(country_caseperday)+
  geom_line(aes(date, new_cases), color="darkblue")+
  geom_line(aes(date, new_deaths), color="darkred")+
  geom_area(aes(date, new_cases), fill="navyblue", alpha=1/5)+
  geom_area(aes(date, new_deaths), fill="red", alpha=1/5)+
  facet_wrap(~location, scales = "free")+
  labs(x="Time period", y="Number of cases")+
  ggtitle("Progression of Covid-19 by countries")
ggplotly(plot_18)

#Spatial Analysis
covid_mapping<- plot_1 %>% 
  select(location)
which(!is.character(covid_mapping))
covid_mapping<-as.data.frame(covid_mapping)
covid_mapping$location<-as.character(covid_mapping$location)
locs<-mutate_geocode(covid_mapping, location)


locs[128,2]<-42.01
locs[128,3]<-43.38
locs[124,2]<-31.15
locs[124,3]<-36.15
location_act<-as_tibble(locs)
locations_sf<-st_as_sf(location_act, coords = c("lon", "lat"), crs=4326)
mapview(locations_sf)

#locs<-locs[-c(190,125,137), ]

write_csv(locs, "C:/Users/bajpa/OneDrive/Desktop/Individual Project/locations.csv")
TC_geo <- left_join(TD_worldcount, location_act, by="location")
TC_geo<-as.data.frame(TC_geo)

--------------------------------------------------------------------------------
world<-map_data("world")
ggplot(legend = FALSE) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               color = 'black', fill = 'antiquewhite') +
  xlab('') + ylab('') + 
  geom_point(data = TC_geo, color = 'black', fill = 'red', 
             shape = 21, alpha = 0.4,
             aes(x = lon, y = lat, fill = total_cases, size = total_cases)) +
  theme_minimal() + ggtitle('Occurrences Map - COVID19') +
  theme(text = element_text(size = 25), legend.position = 'top',
        panel.background = element_rect(fill='lightblue', colour='blue'))
abc<-master_df
abc[is.na(abc)]<-0
which(is.na(abc))
a<-spread(abc, key = date, value = total_cases, convert = TRUE)
locations<-as_tibble(locations)
a<-left_join(abc, locations, by=location)
a<-as.data.frame(a)
a<-na.omit(a)

world<-map_data("world")
ggplot(legend = FALSE) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               color = 'black', fill = 'antiquewhite') +
  xlab('') + ylab('') + 
  geom_point(data = a, color = 'black', fill = 'red', 
             shape = 21, alpha = 0.4,
             aes(x = lon, y = lat, fill = total_cases, size = total_cases)) +
  theme_minimal() + ggtitle('Occurrences Map - COVID19') +
  theme(text = element_text(size = 25), legend.position = 'top',
        panel.background = element_rect(fill='lightblue', colour='blue'))+
  facet_wrap(.~date)
