setwd("/Users/steffenhartleib/Google Drive/CourseraReproducibleResearch/RepData_PeerAssessment2")
storm <- read.csv("repdata-data-StormData.csv", header = TRUE)
library(dplyr)
library(ggplot2)
options(scipen=999)
names(storm)
str(storm)
class(storm)
summary(storm)




storm93 <- filter(storm, Year > 1992)
FatTot <- sum(storm93$FATALITIES)
HealthImpact <- select(storm93, EVTYPE,FATALITIES) %>%
        group_by(EVTYPE) %>%
        summarise(TotFat = sum(FATALITIES),FatPercOfTotFat = round(sum(FATALITIES)/FatTot,2)) %>% 
        arrange(- TotFat) %>%
        mutate(CumeFatPercTotFat = cumsum(FatPercOfTotFat)) %>%
        filter(CumeFatPercTotFat <= .8)
colnames(HealthImpact) <- c("StormType", "Fatalities", "FatalitiesPercofTotal", "CumeFatalities %")
HealthImpact



DamTot <- sum(storm93$CROPDMG + storm93$PROPDMG)
EconImpact <- select(storm93, EVTYPE,CROPDMG,PROPDMG) %>%
group_by(EVTYPE) %>%
summarise( TotDam = round(sum(CROPDMG +PROPDMG),0),
           DamPercOfTotDam = round(sum(CROPDMG +PROPDMG)/DamTot,2)) %>%
arrange(- TotDam) %>%
mutate(CumeDamPercTotDam = cumsum(DamPercOfTotDam))%>%
filter(CumeDamPercTotDam <= .81)
colnames(EconImpact) <- c("Storm Type"," Damage ($US 000s)", "Damage % of Total", "Cume Damage %")
EconImpact


# Health and Econ Impact By Event Type
FatTot <- sum(storm$FATALITIES)
EconTot <- sum(storm$CROPDMG) + sum(storm$PROPDMG)
storm <- select(storm, BGN_DATE, Year EVTYPE,CROPDMG,PROPDMG, FATALITIES, INJURIES)
stormEvImp <- group_by(storm, EVTYPE)
stormEvImp <- summarise(stormEvImp, 
                         TotEvents = length(unique(BGN_DATE)),
                         TotFat = sum(FATALITIES), 
                         TotInj = sum(INJURIES),
                         TotEcon = round(sum(PROPDMG) + sum(CROPDMG),0),
                         SurvRate = round(1-(sum(FATALITIES)/(sum(FATALITIES) +sum(INJURIES))),2),
                         FatPercOfTotFat = round(sum(FATALITIES)/FatTot,2),
                         EconPercOfTotEcon  = round((sum(PROPDMG) + sum(CROPDMG))/EconTot,2))
                                            
# Show top Health Impact Events 
stormEvImpHealth <- select(stormEvImp, EVTYPE, TotEvents, TotFat, TotInj, SurvRate, FatPercOfTotFat)
stormEvImpHealth <- arrange(stormEvImpHealth, - TotFat)
stormEvImpHealth <- mutate(stormEvImpHealth, 
                           CumeFatPercTotFat = cumsum(FatPercOfTotFat))
stormEvImpHealth <- filter(stormEvImpHealth, CumeFatPercTotFat <= .8)
stormEvImpHealth

# Show top Eon Impact Events 
stormEvImpEcon <- select(stormEvImp, EVTYPE, TotEvents, TotEcon, EconPercOfTotEcon)
stormEvImpEcon <- arrange(stormEvImpEcon, - TotEcon)
stormEvImpEcon <- mutate(stormEvImpEcon, CumeFatPercTotEcon = cumsum(EconPercOfTotEcon))
stormEvImpEcon <- filter(stormEvImpEcon, CumeFatPercTotEcon <= .8)
stormEvImpEcon

library(xtable)
table <- xtable(stormEvImpEcon)
htmlTable(stormEvImpEcon)


# Tornado Over Time
Tornado <- filter(storm, storm$EVTYPE == "TORNADO")
names(Tornado)
#Tornado$BGN_DATE <- as.Date(Tornado$BGN_DATE, format = "%m/%d/%Y %H:%M:%S")
#Tornado <- mutate(Tornado, year = as.POSIXlt(Tornado$BGN_DATE)$year  + 1900)
Tornado <- group_by(Tornado, year)
Tornado <- summarise(Tornado, TornFatTot = sum(FATALITIES), 
                     TornInjTot = sum(INJURIES), 
                     #NoOfTornDays = length(unique(BGN_DATE)),
                     #SurvRate = 1-(sum(FATALITIES)/(sum(FATALITIES) + sum(INJURIES))),
                     #FatPerTorDay = sum(FATALITIES)/length(unique(BGN_DATE)),
                     #TornDam = sum(CROPDMG + PROPDMG)
                        )
Tornado

# Total Fatalites and Damages Per Year
        FatDamYear <- select(storm,BGN_DATE,FATALITIES,CROPDMG,PROPDMG, year)
        FatDamYear <- group_by(FatDamYear, year)
        FatDamYear <- summarize(FatDamYear, 
                                TotFat = sum(FATALITIES),
                                TotDam = sum(CROPDMG + PROPDMG))
TornVsTot <- cbind(Tornado$year, Tornado$TornFatTot, Tornado$TornDam, FatDamYear$TotFat, FatDamYear$TotDam)
TornVsTot <- as.data.frame(TornVsTot)
colnames(TornVsTot) <- c("Year","TornadoFat","TornadoDam","TotalFat", "TotalDam")
TornVsTot <- mutate(TornVsTot, TornFatVsTot = round(TornadoFat/TotalFat,2), TornDamVsTot = round(TornadoDam/TotalDam,2))
TornVsTot
plot(y = TornVsTot$TornFatVsTot, x = TornVsTot$Year, type = "l", col = "red", xlab = "Year",
     ylab = "Perc. of Total")
title(main = "Fatalities and Damages From Tornados as Perc. Of Yearly Totals")
lines(y = TornVsTot$TornDamVsTot, x = TornVsTot$Year, col = "blue")

TF <- ggplot(TornVsTot, aes(Year, y = TornFatVsTot + TornDamVsTot))
TF + geom_line()

TD <- ggplot(TornVsTot, aes(Year, y =  TornDamVsTot))
TD + geom_line()





T50 <- subset(storm, year == 1950 & EVTYPE == 'TORNADO')
T50 <- arrange(T50, BGN_DATE1)
T50

dim(T50)
Tornado <- filter(storm, EVTYPE == "TORNADO")
dim(Tornado)
Tornado <- group_by(storm, year)
Tornado <- summarise(Tornado, TornFatTot = sum(FATALITIES), TornInjTot = sum(INJURIES), 
                     NoOfTornados = length(unique(BGN_DATE1)))
Tornado
head(Tornado)

plot( y = Tornado$TornFatTot, x = Tornado$year, type = "l")
plot( y = Tornado$TornInjTot, x = Tornado$year, type = "l")

?round()


        