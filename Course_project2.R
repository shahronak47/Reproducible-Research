#Loading the file
setwd("E:/My Stuff/R/repdata-data-StormData.csv")
StormData <- read.csv("E:/My Stuff/R/repdata-data-StormData.csv/repdata-data-StormData.csv", stringsAsFactors=FALSE)

#Q1 - Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

summary(StormData)
str(StormData)
which.max(table(StormData$EVTYPE))
# HAIL 
# 244 
#So HAIL is the most occuring event type.
#we'll consider Fatalaties and injuries columns 

storm_list <- lapply(split(StormData$FATALITIES + StormData$INJURIES, StormData$EVTYPE), sum)
which.max(unlist(storm_list))
#TORNADO 
#834 
#So wrt population health Tornado is the most harmful. 834 is the index number.

#Q2 - Across the United States, which types of events have the greatest economic consequences?

#So here we would consider PROPDMG field and the corresponding PROPDMGEXP 
#Creating a new column PropertyDamage 
StormData[,38] <- NA

colnames(StormData)[38] <- "PropertyDamage"

if(StormData$PROPDMGEXP == 'K')
  {
    StormData$PropertyDamage = StormData$PROPDMG*1000 
  }
  else
    if(StormData$PROPDMGEXP == 'M')
    {
      StormData$PropertyDamage = StormData$PROPDMG*1000000 
    }
  else
    if(StormData$PROPDMGEXP == 'B')
    {
      StormData$PropertyDamage = StormData$PROPDMG*1000000000 
    }
  else
  {
    StormData$PropertyDamage == StormData$PROPDMG
  }




prop_loss <- lapply(split(StormData$PropertyDamage, StormData$EVTYPE), sum)
which.max(unlist(prop_loss))
#TORNADO 
#818 
barplot(unlist(prop_loss), xlab="TYPE", ylab="property damage", main="Property Damage per type")
