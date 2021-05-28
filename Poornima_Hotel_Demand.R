setwd("C:\\Users\\metro\\R\\MyExamples\\Hotel")
getwd()

hotel<-read.csv("hotel_bookings.csv")
hotel
dim(hotel)
str(hotel)
hotel_copy<-hotel

colSums(is.na(hotel))

colSums(is.null(hotel))

is.null(hotel)


#EDA /Data Exploration

dim(hotel_copy)

colnames(hotel_copy)


str(hotel_copy)

head(hotel_copy,c(4L,32L))

tail(hotel_copy,c(4L,32L))

levels(as.factor(hotel_copy$hotel))

levels(as.factor(hotel_copy$is_canceled))

hist(df$Credit.Score, breaks = 5, main = "Credit.Score",col="blue",xlab="Credit.Score",ylab="Frequency")







#(Data Preparation)

#Empty values assighned to NA
hotel_copy[hotel_copy=='']<-NA

#NULL values assigned to NA
hotel_copy[hotel_copy=="NULL"]<-NA

#Duplicated values
sum(duplicated(hotel_copy))#--we have 31994 duplicated values

#To get the duplicated values
#r1<-which(duplicated(hotel_copy)) #we get the rowid of the duplicated id

#hotel_copy<-hotel_copy[-r1,]
dim(hotel_copy)


#sapply(hotel_copy, function(x) length(unique(x))) #gives the length of each Attribute

str(hotel_copy$reservation_status_date) #noticed that reservation_status_date is of char type,
#so changing to date type
hotel_copy["reservation_status_date"]<-as.Date(hotel_copy$reservation_status_date)
str(hotel_copy$reservation_status_date)


#Below Attributes with Integer 
str(hotel_copy$arrival_date_year)
str(hotel_copy$arrival_date_month)
str(hotel_copy$arrival_date_day_of_month)
#future engineering
hotel_copy["arrival_date"]<-as.Date(with(hotel_copy,paste(arrival_date_year,
match(hotel_copy$arrival_date_month, month.name), arrival_date_day_of_month, sep="-")),"%Y-%m-%d")
str(hotel_copy$arrival_date)



#EDA

summary(hotel_copy)
colnames(hotel_copy)


sum(is.na(hotel_copy$hotel))

#how many types hotels we have in the dataset
levels(as.factor(hotel_copy$hotel)) #only 2 hotels

levels(as.factor(hotel_copy$is_canceled)) #0 -Not Canceled 1-Canceled

tbl<-table(hotel_copy$hotel) #no of observations for each hotel 
tbl

#----------------------------------------------------------------------------------
#Q1#Graphical representation of bookings in Hotel
#----------------------------------------------------------------------------------

ggplot(data = hotel_copy, aes(x = hotel)) + 
  geom_bar(stat = "count") +
  labs(title = "Booking Request by Hotel type",
       x = "Hotel type",
       y = "No. of bookings") +
  theme_classic() + scale_color_brewer(palette = "Set2")


#----------------------------------------------------------------------------------
  #UNIVARIENT ANALYSIS OF CONTINOUS VARIABLE
 #Q2: #HERE I am grouping Visualization of ADR VARIABLE (Target Variable ADR)
#----------------------------------------------------------------------------------
sum(is.na(hotel_copy$lead_time)) #0 No missing values

sum(is.na(hotel_copy$adr)) #0 No Missing values

#Getting summary for both Variables

summary(hotel_copy$adr)





split.screen( figs = c( 2, 1 ) )

summary(hotel_copy$adr)

#Remove the negative value for ADR
r2<-hotel_copy[which(hotel_copy["adr"]<0),]
r2

#Remove the outlier(may be a mistake input,it is only one observation)
r3<-hotel_copy[which(hotel_copy["adr"]==5400),]
r3

hotel_copy$adr<-ifelse(hotel_copy$adr<0, 0, hotel_copy$adr)

hotel_copy$adr<-ifelse(hotel_copy$adr==5400, mean(hotel_copy$adr), hotel_copy$adr)

summary(hotel_copy$adr)

split.screen( figs = c( 2, 1 ) )

# Freedman-Diaconis rule
screen( 1 )
hist(hotel_copy$adr,breaks = "FD",col.lab="blue",col.axis="red",
     main=" Distribution of Average daily rate",
     xlab="ADR(dollors)",
     xlim=c(0,500),col="green")


screen( 2)
boxplot(hotel_copy$adr, main="ADR boxplot",
        xlab="ADR")
IQR(hotel$adr)
dim(hotel)





close.screen( all = TRUE )




#------------------------------------------------------------------
#Q3#How many Bookings were Canceled at the Hotel? pie chart
#-----------------------------------------------------------------
str(hotel_copy)

#only 2 levels available 
levels(as.factor(hotel_copy$is_canceled))
levels(as.factor(hotel_copy$hotel))

split.screen( figs = c( 2, 1 ) )
screen( 1 )
HotelR<-subset(hotel_copy,hotel_copy$hotel=="Resort Hotel")
Bcount<-table(HotelR$is_canceled)
Bcount
Bcount[1]
Bcount[2]
class(Bcount)
Bslice<- c(Bcount[1],Bcount[2])
class(Bslice)
Bslice
Blbls<-c("Cancelled","Not Cancelled")
Bpct<-round(Bslice/sum(Bslice)*100)
Bpct
Blbls<-paste(Blbls,Bpct)
Blbls
Blbls<-paste(Blbls,"%",sep="")
pie(Bslice,labels=Blbls,col=rainbow(length(Blbls)),
    main="Resort Hotel",radius=1.15,cex=2)
screen( 2 )
HotelC<-subset(hotel_copy,hotel_copy$hotel=="City Hotel")
Bcount<-table(HotelC$is_canceled)
Bcount
Bcount[1]
Bcount[2]
class(Bcount)
Bslice<- c(Bcount[1],Bcount[2])
class(Bslice)
Bslice
Blbls<-c("Cancelled","Not Cancelled")
Bpct<-round(Bslice/sum(Bslice)*100)
Bpct
Blbls<-paste(Blbls,Bpct)
Blbls
Blbls<-paste(Blbls,"%",sep="")
pie(Bslice,labels=Blbls,col=rainbow(length(Blbls)),
    main="City Hotel",radius=1.13,cex=2)

#Resort Hotel: 72% Canceled (40060 rows) City Hotel : 58% Canceled  (79330 rows)
#Higher Cancellation rate in Resort Hotel (in spite of having the less observations for this hotel)


#---------------------------------------------------------------------------------------------
#Q4#Relationship between Average Daily Rate(ADR) and Arrival Month by Booking Cancellation Status
#----------------------------------------------------------------------------------------------

str(hotel_copy)

sum(is.na(hotel_copy$is_canceled))  #no missing value for this column

sum(is.na(hotel_copy$adr)) #no missing value for this column

sum(is.na(hotel_copy$arrival_date_month)) #no missing value for this column

#--------------------------------------------------------------------------------

#copying to different dataset the 3 columns I am comparing
ADRbyMonth1<-hotel_copy[c("adr","arrival_date_month","is_canceled")]

ADRbyMonth1

ADRbyMonth3<-aggregate(adr~arrival_date_month+is_canceled, data=ADRbyMonth1, FUN=mean)
#aggregate removes by defalt NA values

class(ADRbyMonth3)
#order by 3 columns and apply factor as the value of Months are full names
ADRbyMonth5<-ADRbyMonth3[order(factor(ADRbyMonth3$arrival_date_month,levels=month.name),ADRbyMonth3$is_canceled,ADRbyMonth3$adr),]
ADRbyMonth5

install.packages('ggplot2') 	# Installation 
library(ggplot2) 

ggplot(ADRbyMonth3, aes(x=factor(arrival_date_month,levels=month.name), y=adr, colour = factor(is_canceled))) +       
  geom_line(aes(group = factor(is_canceled))) + geom_point() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    ) + ggtitle("Relationship between Average Daily Rate(ADR) and Arrival Month by Booking Cancellation Status") + 
  xlab("Arrival Month") + 
  ylab("Average daily rate (adr)") + scale_color_discrete(name = "Booking_Status", 
                                                          labels=c("Not Cancelled -0 ", "Cancelled -1"))

#August is the busy month of both bookings being one of the reason may be due to high ADR



#-----------------------------------------------------------------------------
#Q5. Is there any relationship between ADR and is_canceled?
#-----------------------------------------------------------------------------

Convertis_cancelled<-hotel_copy

#add new column as Booking_canceled
##Bivarate Analysis for cointinouse(ADR ) Vs. categorical (is_canceled)
Convertis_cancelled$booking_status<-NULL
Convertis_cancelled$booking_status<-ifelse(Convertis_cancelled$is_canceled==0,"Booking Not cancelled","Booking Cancalled")
str(Convertis_cancelled)

agg1 <- aggregate(adr~ booking_status, Convertis_cancelled , mean)
agg1
names(agg1) <- c("Booking_Cancelled","Average daily Rate")
agg1

agg2<- cbind(aggregate(adr ~ booking_status, Convertis_cancelled , min),
             aggregate(adr~ booking_status, Convertis_cancelled , max)[,2],
             aggregate(adr~ booking_status, Convertis_cancelled , mean)[,2])

names(agg2) <- c("Booking_Cancelled","min_Average daily Rate","max_Average daily Rate","mean_Average daily Rate")
agg2


library(ggplot2)
qplot(booking_status, adr, data = Convertis_cancelled, 
      geom="boxplot", fill = booking_status)
# Changing histogram plot fill colors by is_canceled and usinging semi-transparent fill
p<-ggplot(Convertis_cancelled, aes(x=adr, fill=booking_status, color=booking_status)) +
  geom_histogram(position="identity", alpha=0.5,bins=30)
p
# Add mean lines
library(plyr)


mu <- ddply(Convertis_cancelled, "booking_status", summarise, grp.mean=mean(adr,na.rm=T))
head(mu)

p<-p+geom_vline(data=mu, aes(xintercept=grp.mean, color=booking_status),
                linetype="dashed")
p

#Add density
p<-ggplot(Convertis_cancelled, aes(x=adr, fill=booking_status, color=booking_status)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.5,bins = 30)+
  geom_density(alpha=0.6)
p
# Add mean lines and Change the legend position
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=booking_status),
             linetype="dashed")+ theme(legend.position="top")+
  labs(title="Booking histogram plot",x="ADR", y = "Density") 



t.test(adr ~ is_canceled, data=hotel_copy )
# After checking Assumption you can say there is association between mean of ADR and is_canceled at 5% significant level


#-------------------------------------------------------------------
#Q6#top 10 countries of guests visiting the 2 hotels
#-------------------------------------------------------------------

table(hotel_copy$country)
Ccount<-table(hotel_copy$country)
class(Ccount)

table(hotel$country)

Gcountry<- as.data.frame(Ccount)
names(Gcountry)
Gcountry
names(Gcountry)=c("Country","nc")

Top10Country = arrange(Gcountry,desc(nc))
Top10Country = Top10Country[1:10,]
Top10Country 



ggplot(data = Top10Country, mapping = aes(x=reorder(Country, -nc), y = nc, fill = reorder(Country, -nc))) +
  geom_col()   +
  labs(title="Top 10 countries of guests visiting the 2 hotels",x="Country", y = "Count" ,fill="Country") +
  geom_text(aes(label=nc),position = position_stack(vjust= 0.5),
            colour = "white", size = 3) + 
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

#Around 40% of all bookings were booked from Portugal followed by Great Britain(12%) & France(10%)



#-------------------------------------------------------------------
#Q7#Total bookings across years 
#-------------------------------------------------------------------

ggplot(hotel_copy,aes(arrival_date_year,fill = hotel)) +
  geom_bar(position = 'dodge') +
  ylab("No of Bookings") +
  xlab("Years") +
  ggtitle("Hotel Type with respect to bookings across years") +
  labs(fill = 'Hotel Type')

#More Bookings in 2016 Year

#-------------------------------------------------------------------
#Q8#Total bookings across Months 
#-------------------------------------------------------------------

ggplot(data = hotel_copy,aes(x=factor(arrival_date_month,levels=month.name),fill = (hotel))) +
  geom_bar(position = 'dodge') +
  facet_grid(hotel_copy$arrival_date_year) +
  scale_y_continuous(name = "# of Bookings",labels = scales::comma) +
  xlab("Months") +
  ggtitle("Hotel Type vs # of bookings across months") +
  labs(fill = 'Hotel Type')




#------------------------------------------------------------------------
#Q9 BiVariate analysis of continuous variable and category variable
#Average Daily Rates  of Bookings through various Market Segments  
#-------------------------------------------------------------------------

str(hotel_copy)

qplot(market_segment, adr, data = hotel_copy, 
      geom=c("boxplot"),fill = market_segment) +
  labs(title="ADR rates of Bookings through various Market Segments",x="Booking Agency",
       y = "Average Daily Rates") + stat_summary(fun=mean,shape=1,col='red',geom='point')

#------------------------------------------------------------------------
#Q10 Hotel Demand by Market Segment 
#-------------------------------------------------------------------------

#1 Method
Demand<-hotel_copy[c("hotel","market_segment","is_canceled")]

d1<-table(Demand$hotel,Demand$market_segment,Demand$is_canceled)
d1<-as.data.frame(d1)
d1
d1$total<-NA
d1$ratio<- NA

d2<-do.call(rbind, by(d1, d1[1], function(x) with(x,
                                                  data.frame(HotelName = Var1,
                                                             market_segment= Var2,
                                                             total = sum(Freq),
                                                             ratio = Freq/sum(Freq),
                                                             booking_status = ifelse(Var3 == 0, "No", "Yes")
                                                  ))))

ggplot(d2,aes(ratio, reorder(market_segment,ratio), fill = booking_status)) +
  geom_col(position = "dodge") +
  labs(x = "Percentage", y = "Market Segment", 
       title = "Hotel Demand by Market Segment",
       fill = "Booking Canceled") +
  facet_wrap(~HotelName, scales = "free_y")+
  #facet_grid would show Y axis only once and have 2 graphs together
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "top")
#2 Method
library(tidyverse)
de1<-hotel_copy %>% 
  group_by(hotel)   %>% 
count( market_segment, is_canceled) %>% 
  mutate(total = sum(n),
         ratio = n/total,
         is_canceled = ifelse(is_canceled == 0, "No", "Yes")) %>%
ungroup() %>%
  ggplot(aes(ratio, reorder(market_segment,ratio), fill = is_canceled)) +
  geom_col(position = "dodge") +
  labs(x = "Percentage", y = "Market Segment", 
       title = "Hotel Demand by Market Segment",
       fill = "Booking Canceled") +
  facet_wrap(~hotel, scales = "free_y") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "top")

de1
#as.data.frame(de1)
 
#Most of the booking done via travel agent, either online or offline, 
#which combined contributes more than 40% of the not-canceled total transactions

#------------------------------------------------------------------------
#Q11 ADR Distributions by Market Segments for Booking Not Canceled
#-------------------------------------------------------------------------
#
#copying to different dataset the 4 columns I am comparing 
market<-hotel_copy[c("hotel","market_segment","is_canceled","adr")]

#Segmenting only Booking Not Canceled  
market1<-market[(market$is_canceled=="0"),]

market2<-aggregate(adr~market_segment+hotel, data=market1, FUN=sum)

market3<-transform(market2, market_pct = ave(adr, hotel, FUN = function(x) x/sum(x) ))
# AVE averaged, where each subset consist of those observations with the same factor levels.
ggplot(market3,aes(market_pct, reorder(market_segment,market_pct))) +
  geom_col(fill = "firebrick") +
  labs(x = NULL, y = NULL, 
       title = "ADR Distributions by Market Segments")+
  facet_wrap(~hotel, scales = "free_y")+
  #facet_grid would show Y axis only once and have 2 graphs together
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "top")

str(hotel_copy)




#------------------------------------------------------------------------
#Q12 Chi square test between meals and hotel 
#-------------------------------------------------------------------------


#For meals provided in hotel
sum(is.na(hotel_copy$meal))
range(hotel_copy[,c(13)])


colnames(hotel_copy)

#Meals provided in each hotels
table(hotel_copy$meal)
tbl2<-table(hotel_copy$hotel,hotel_copy$meal)

add<-addmargins(xtabs(~ hotel_copy$hotel+hotel_copy$meal))

prop.table(xtabs(~ hotel_copy$hotel+hotel_copy$meal))#will give proportion

library(MASS)
library(vcd)

#hotel and meal are independent is the NULL hypothesis 
chisq.test(tbl2)

#as the PVALUE IS less than the significance value i.e is of 5%,thus the 2 columns are dependent 
assoc(tbl2,shade=TRUE)



mosaic(tbl2,shade=TRUE)
mosaicplot(tbl2,shade=TRUE,legend=TRUE)


#---
table(hotel_copy$adr)
tbl3<-table(hotel_copy$adr,hotel_copy$lead_time)

add<-addmargins(xtabs(~ hotel_copy$adr+hotel_copy$lead_time))

prop.table(xtabs(~ hotel_copy$adr+hotel_copy$lead_time))#will give proportion

library(MASS)
library(vcd)

#hotel and meal are independent is the NULL hypothesis 
chisq.test(tbl3)

##---------------MODEL--------------------

set.seed(1)

index <- sample(nrow(hotel_copy), nrow(hotel_copy)*0.3)

test <- hotel_copy[index,]       # save 30% as a test dataset
training <-hotel_copy[-index,]   # save the rest as a training set

dim(test)
dim(training)
