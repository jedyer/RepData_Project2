---
output:
  html_document:
    keep_md: yes
---

# Analysis of the human and economic costs of weather events in the US

By Jason Dyer
Date 03-23-2019

##Synopsis

The National Oceanic and Atmospheric Administration (NOAA) maintains a publicly available database the tracks major storms and weather events in the United States.  The analysis shows that events with a high human cost (injuries and deaths) are not correlated to events with high economic cost (property and crop damage).  Tornados and Flooding/High Seas result in the greatest number of injuries and deaths while they are only the third and forth in terms of economic costs.  In comparision, Hurricanes and thunderstorms result in the most economic damage but are not in the top 4 weather events the are responsible for injuries or death.

## Data Processing

### Load Libraries
Load the libraries need for the analysis

```r
library(lubridate)
library(tidyverse)
library(gridExtra)
library(scales)
```

###Download and extract source data

```r
if(!file.exists("StormData.csv.bz2")) {
  URL<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(URL,destfile="./StormData.csv.bz2")
}
```

###Load the Data

```r
rawdata<-read.csv("StormData.csv.bz2",stringsAsFactors = FALSE)
```

###Process, transform, and clean the data
The data from NOAA went under several processing steps before being analyzed.  THe field BGN_DATE is transformed from a factor to a POSIXct.  The NOAA data contains 37 variables which are reduced to 10.  The NOAA data began in January 1950. At that time they recorded one event type, tornado. They added more events gradually and only from January 1996 did they start recording all events type.  For this analysis, only data from January 1996 forward was used and only where the fatalities, injury, property damage, and crop damage where not all 0.  Two data cleaning steps were performed.  First, the PROPDMGEXP field for a flooding event in California (REFNUM=605943) was miscoded as a 'B'.  It was change to a 'M'.  The invalid codes in PROPDMGEXP were ignored.  Officially, there are 48 event types.  The event types were consolated into 12.  The final transformation steps combine fatalies and injuries into one variable and combine property and crop damage into another variable.

###Format data

```r
rawdata$BGN_DATE<-mdy_hms(rawdata$BGN_DATE)
data<-rawdata%>%select(STATE,BGN_DATE,EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP,REFNUM)
data<-data%>%filter(BGN_DATE>"1995/12/31" & (FATALITIES>0 | INJURIES >0 | PROPDMG >0 | CROPDMG >0))
```

###Correct mis-coded PROPDMGEXP

```r
data<-within(data,PROPDMGEXP[REFNUM==605943]<-"M")
```

### Check invalid codes in property damages

```r
valid.mag <- c("", "H", "K", "M", "B")
magnitude <- 10^c(0, 2, 3, 6, 9)
data$nPROPDMG <- data$PROPDMG * magnitude[match(data$PROPDMGEXP, valid.mag)]
data$nCROPDMG <- data$CROPDMG * magnitude[match(data$CROPDMGEXP, valid.mag)]
```

###Condense Event Types

```r
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
   
### Cast damagesource as a factor
data$damagesource<- as.factor(data$damagesource)
```

### Combine Variables

```r
data<-data%>%mutate(health= FATALITIES + INJURIES)
data<-data%>%mutate(econ= nPROPDMG + nCROPDMG)
```

##Results
The top 4 events for injuries or death are Tornados, Flooding/High Seas, Heat & Drought, and Wind. The top 4 events for economic damage are Hurricanse, Thunderstorms, Flooding/High Seas, and Tornados.  Hurricanes and thunderstorms are far more responisble for economic damage than a tornando, but a tornando is the leading cause of injury or death.  Heat & Drought and Wind have a high human cost but are not in the top 4 of econonmic damage.  

### Top 4 Harms to Population Health

```r
d1<-data%>%
    group_by(damagesource)%>%
    summarize(health=sum(health, na.rm=TRUE))%>%
    arrange(desc(health))%>%
    top_n(4)

d1
```

```
## # A tibble: 4 x 2
##   damagesource         health
##   <fct>                 <dbl>
## 1 Tornados              22183
## 2 Flooding & High Seas  11474
## 3 Heat & Drought         9742
## 4 Wind                   7761
```

### Top 4 Events for Economic Damage

```r
d2<-data%>%
    group_by(damagesource)%>%
    summarize(econ=sum(econ, na.rm=TRUE))%>%
    arrange(desc(econ))%>%
    top_n(4)

d2
```

```
## # A tibble: 4 x 2
##   damagesource                econ
##   <fct>                      <dbl>
## 1 Hurricanes           87068996810
## 2 Thunderstorms        62116961130
## 3 Flooding & High Seas 51501358870
## 4 Tornados             24906242020
```

### Side-by-Side Comparision of Human and Econonimc Costs by Event Type

```r
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
```

![](RR_Project2_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

###Human and Economic Cost Trend
The 1996-2011 trends of human and economic costs also show little correlation.  1998 had the highes injuries and deaths while 2005 had the greatest economic damage.  Neither the human cost or econonmic cost trends show any obvious up or downwards trend.

####Injuries and Deaths Per Year

```r
human<-data%>%
    mutate(Year=year(BGN_DATE))%>%
    group_by(Year)%>%
    summarize(health=sum(health, na.rm=TRUE))
    
ggplot(data=human,aes(x=Year, y=health)) +
    geom_line(color="steelblue") +
    ggtitle("Weather Realted Deaths and Injuries Between 1996 and 2011")+
    theme(plot.title=element_text(hjust = 0.5))+
    ylab("Deaths and Injuries")
```

![](RR_Project2_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

#### Economic Damage by Year

```r
damage<-data%>%
    mutate(Year=year(BGN_DATE))%>%
    group_by(Year)%>%
    summarize(econ=sum(econ, na.rm=TRUE))

ggplot(data=damage,aes(x=Year, y=econ/10^6)) +
    geom_line(color="steelblue") +
    ggtitle("Weather Related Economic Damage Between 1996 and 2011")+
    theme(plot.title=element_text(hjust = 0.5))+
    ylab("Economic Damage (in millions)")
```

![](RR_Project2_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
