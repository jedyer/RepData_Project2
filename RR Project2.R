library(lubridate)
library(tidyverse)
library(gridExtra)
library(scales)

#Download  file
if(!file.exists("StormData.csv.bz2")) {
  URL<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(URL,destfile="./StormData.csv.bz2")
}

#Load Data
rawdata<-read.csv("StormData.csv.bz2",stringsAsFactors = FALSE)

#Format data
rawdata$BGN_DATE<-mdy_hms(rawdata$BGN_DATE)
data<-rawdata%>%select(STATE,BGN_DATE,EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP,REFNUM)
data<-data%>%filter(BGN_DATE>"1995/12/31" & (FATALITIES>0 | INJURIES >0 | PROPDMG >0 | CROPDMG >0))
data<-within(data,PROPDMGEXP[REFNUM==605943]<-"M")

# check invalid codes in property damages
valid.mag <- c("", "H", "K", "M", "B")
magnitude <- 10^c(0, 2, 3, 6, 9)
data$nPROPDMG <- data$PROPDMG * magnitude[match(data$PROPDMGEXP, valid.mag)]
data$nCROPDMG <- data$CROPDMG * magnitude[match(data$CROPDMGEXP, valid.mag)]

#Condense Event Types
if(exists("data")){
    data$damagesource <- NA
    data[grepl("freeze|cold|frost|freezing|hyperthermia",
               data$EVTYPE,ignore.case=TRUE),"damagesource"] <- "Cold"
    data[grepl("dust",
               data$EVTYPE,ignore.case=TRUE),"damagesource"] <- "Dust"
    data[grepl("fire|smoke|volcanic",
               data$EVTYPE,ignore.case=TRUE),"damagesource"] <- "Fire & Volcano"
    data[grepl("flood|dam|drowning|surf|water|seiche|fld|seas|marine|current|wave|surge|tsunami|tide",
               data$EVTYPE,ignore.case=TRUE),"damagesource"] <- "Flooding & High Seas"
    data[grepl("heat|drought|warm",
               data$EVTYPE,ignore.case=TRUE),"damagesource"] <- "Heat & Drought"
    data[grepl("erosion|slump|slide",
               data$EVTYPE,ignore.case=TRUE),"damagesource"] <- "Landslide & Erosion"
    data[grepl("fog|strom|drizzle|rain|precip|hail|depression",
               data$EVTYPE,ignore.case=TRUE),"damagesource"] <- "Precipiation & Fog"
    data[grepl("ice|glaze|avalanche|blizzard|snow|winter",
               data$EVTYPE,ignore.case=TRUE),"damagesource"] <- "Snow & Ice"
    data[grepl("lightning|TSTM|thunderstorm|storm",
               data$EVTYPE,ignore.case=TRUE),"damagesource"] <- "Thunderstorms"
    data[grepl("funnel|landspout|tornado|waterspout",
               data$EVTYPE,ignore.case=TRUE),"damagesource"] <- "Tornados"
    data[grepl("hurricane|typhoon",
               data$EVTYPE,ignore.case=TRUE),"damagesource"] <- "Hurricanes"
    data[grepl("wind|burst",
               data$EVTYPE,ignore.case=TRUE),"damagesource"] <- "Wind"
}
   
# Cast damagesource as a factor
data$damagesource<- as.factor(data$damagesource)    
    
# Health and Economic impact
data<-data%>%mutate(health= FATALITIES + INJURIES)
data<-data%>%mutate(econ= nPROPDMG + nCROPDMG)

#Top 4 Harms to Population Health
d1<-data%>%
    group_by(damagesource)%>%
    summarize(health=sum(health, na.rm=TRUE))%>%
    arrange(desc(health))%>%
    top_n(4)

d1

#top 4 Economic Damage
d2<-data%>%
    group_by(damagesource)%>%
    summarize(econ=sum(econ, na.rm=TRUE))%>%
    arrange(desc(econ))%>%
    top_n(4)

d2

#Plots
p1<-ggplot(d1)+
    geom_bar(aes(x=damagesource,y=health),stat="identity", fill="steelblue") +
    ggtitle("Top 4 Dealths and Injuries by Event Type") +
    theme(plot.title=element_text(hjust = 0.5))+
    xlab("Event Type")+
    ylab("Deaths and Injuries")+
    scale_x_discrete(labels = wrap_format(10))
    

p2<-ggplot(d2)+
    geom_bar(aes(x=damagesource,y=econ/10^6),stat="identity", fill="steelblue") +
    ggtitle("Top 4 Economic Damage by Event Type") +
    theme(plot.title=element_text(hjust = 0.5))+
    xlab("Event Type")+
    ylab("Economic Damage (in millions)")+
    scale_x_discrete(labels = wrap_format(10))

#Display top 4 dealths/injureis and economic damage by event type
grid.arrange(p1,p2,nrow = 1)

# Trends
# Injuries/Deaths by year
human<-data%>%
    mutate(Year=year(BGN_DATE))%>%
    group_by(Year)%>%
    summarize(health=sum(health, na.rm=TRUE))
    
ggplot(data=human,aes(x=Year, y=health)) +
    geom_line(color="steelblue") +
    ggtitle("Weather Realted Deaths and Injuries Between 1996 and 2011")+
    theme(plot.title=element_text(hjust = 0.5))+
    ylab("Deaths and Injuries")

# Economic Damage by Year
damage<-data%>%
    mutate(Year=year(BGN_DATE))%>%
    group_by(Year)%>%
    summarize(econ=sum(econ, na.rm=TRUE))

ggplot(data=damage,aes(x=Year, y=econ/10^6)) +
    geom_line(color="steelblue") +
    ggtitle("Weather Related Economic Damage Between 1996 and 2011")+
    theme(plot.title=element_text(hjust = 0.5))+
    ylab("Economic Damage (in millions)")
