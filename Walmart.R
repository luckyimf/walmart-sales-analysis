#Calling libraries
library("dplyr") #Calling dplyr function for data manipulation 
library("ggplot2") # for data visualisation
library("scales") #for change of scales in data visualisation
library("zoo")
library("tidyverse")
library("tidyr")
library("lubridate")
library("car") #Companion to Applied Regression for Regression Visualisations
require(stats)
library("corrplot")
library("caTools")
library("MLmetrics")
library("repr")




# Loading up the Dataset into
df <- read.csv('C:/Users/USER/Dropbox/walmart/walmart_store_sales.csv')
head(df)


 #DATA PREPROCESSING
 
 #tables
 table(df$Store)
 table(df$Holiday_Flag)

#Checking NA values 
 colSums(is.na(df)) #Observed no NA values
 
 #Checking Duplicate Values
all(duplicated(df) == TRUE)
 #observed no duplicate values

 DATA MINING

#Aggregating data by 'Store' and Finding sum of 'Weekly_Sales' 
Store_Sales<- aggregate(Weekly_Sales ~ Store, data = df, sum)
 
#Changing column name of sales 
colnames(Store_Sales)[2] <- "Total_Sales_by_Store"
 
##Finding out Store with highest Sales 
Store_Sales <-arrange(Store_Sales, desc(Total_Sales_by_Store)) #Arranged Stores based on Sales in descending order
Store_Sales[1,] #Choosing the first store that comes in this order


# Converting Store column into factor so that order won't change for graph 
Store_Sales$Store <- as.character(Store_Sales$Store)
Store_Sales$Store <- factor(Store_Sales$Store, levels=unique(Store_Sales$Store))


#Plotting Store vs TotalSales
options(repr.plot.width = 14, repr.plot.height = 8)

a<-ggplot(data=Store_Sales, aes(x=Store, y=Total_Sales_by_Store)) + geom_bar(stat="identity",fill="red") +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=0.5))+ scale_x_discrete(breaks = df$Store)+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+ ggtitle('Store vs Sales')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Stores") + ylab("Total Sales")
a

## Finding store with the highest standard deviation
#Aggregating data by 'Store' and Finding Standard Deviation of 'Weekly_Sales' 
Store_Sales_Variation<-summarise(group_by(df,Store),sd(Weekly_Sales), mean(Weekly_Sales))

#Changing column names
colnames(Store_Sales_Variation)[2] <- "StandardDeviation_Sales_by_Store"
colnames(Store_Sales_Variation)[3] <- "Mean_Sales_by_Store"

#Creating Coefficient of Variation for Sales by Store in Store_Sales_Variation dataframe 
Store_Sales_Variation<- mutate(Store_Sales_Variation,CV_Sales_by_Store = (StandardDeviation_Sales_by_Store/Mean_Sales_by_Store)*100)


#------Finding Store with highest Standard deviation-------#

#Finding out the row with highest standard deviation 
Store_Sales_Variation[which.max(Store_Sales_Variation$StandardDeviation_Sales_by_Store), ]

#Storing store number with max std deviation value
store_sales_max_std <- Store_Sales_Variation[which.max(Store_Sales_Variation$StandardDeviation_Sales_by_Store), ]$Store

#Storing max std deviation value
max_sd <- Store_Sales_Variation[which.max(Store_Sales_Variation$StandardDeviation_Sales_by_Store), ]$StandardDeviation_Sales_by_Store

#Storing CV value for max std deviation
CV_max_sd <- Store_Sales_Variation[which.max(Store_Sales_Variation$StandardDeviation_Sales_by_Store), ]$CV_Sales_by_Store

#Store with highest variation in Sales - Store 14 & Standard Deviation - 317570, C.V - 5.7137

#printing the output
print(paste('Store no. ', store_sales_max_std,
            'has the maximum standard deviation of ', max_sd, 'Coefficient of Variation = ',CV_max_sd ))



#Density Plot for Store 14

options(repr.plot.width = 14, repr.plot.height = 8)


Store_14 <- df[df$Store == 14, ]
p <- ggplot(Store_14, aes(x=Weekly_Sales)) + geom_density(color="lightblue", fill="red",alpha=0.2)+
  geom_vline(aes(xintercept= mean(Weekly_Sales)),color="steelblue", linetype="dashed", size=1)+
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5))+ 
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  ggtitle('Store 14 Sales distribution')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Weekly Sales") + ylab("Density")
p


BEST PERFORMING STORE OF Q3
#Creating new dataframe to do alterations 
data2<-df
#Creating a month- year column in data2 
data2$month_Year = substr(data2$Date, 4, 10)

#Subsetting Q3-2012 data (i.e, 07-2012,08-2012,09-2012), Q2-2012 data (i.e, 04-2012,05- 2012,06-2012)
Q3_2012 <- filter(data2,month_Year == "07-2012" | month_Year== "08-2012" | month_Year== "09-2012")
Q2_2012 <- filter(data2,month_Year == "04-2012" | month_Year== "05-2012" | month_Year== "06-2012")

#Aggregating sales by store for Q3-2012 
Q3_2012_Sales<-summarise(group_by(Q3_2012,Store),sum(Weekly_Sales))

#Changing column names
colnames(Q3_2012_Sales)[2] <- "Q3_2012_Sales_by_Store"

#Aggregating sales by store each Q2-2012 
Q2_2012_Sales<-summarise(group_by(Q2_2012,Store),sum(Weekly_Sales))

#Changing column names
colnames(Q2_2012_Sales)[2] <- "Q2_2012_Sales_by_Store"

#merging two quarters data by store
Q3_2012_Growthrate <- merge ( Q2_2012_Sales , Q3_2012_Sales , by = 'Store')

#Creating Growth rate column for Sales by Store in the above dataframe 
Q3_2012_Growthrate <- mutate(Q3_2012_Growthrate, Growth_Rate = ((Q3_2012_Sales_by_Store - Q2_2012_Sales_by_Store)*100) / Q2_2012_Sales_by_Store)

#Creating only positive growth rates
positive_growthrate <- filter(Q3_2012_Growthrate, Growth_Rate > 0 ) 
positive_growthrate<-arrange(positive_growthrate, desc(Growth_Rate)) 
View(positive_growthrate)
a<- positive_growthrate$Store

#printing the output
print(paste(c('The positive growth rate Stores are', a),collapse=" " )) 
print(paste('Store',positive_growthrate[1,1], 'has highest growth rate & it is',positive_growthrate[1,4]))

# Store 7 -13.33% , Store 16 - 8.49% , Store 35 - 4.47% and 7 more stores with positive growth rates.



# Visual representation of growth rates
options(repr.plot.width = 14, repr.plot.height = 8)


c<-ggplot(data=Q3_2012_Growthrate, aes(x=Store, y=Growth_Rate)) +geom_bar(stat ="identity",fill="red")+
  ggtitle('Growth rates of Q3- 2012')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Stores") + ylab("Growth rate(%)") +
  scale_x_continuous("Stores", labels = as.character(Q3_2012_Growthrate$Store), breaks =
                       Q3_2012_Growthrate$Store)+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=0.5))
c



 MULTIPLE LINEAR REGRESSION
 
 #creating same data for alterations
 data4 <- df
 
 #selecting only first store as prediction Required only for first Store
 data4<- dplyr::filter(data4, Store ==1)
 
 #changing date column in dataframe to date format & arranging in ascending order as per dates
 data4$Date <- lubridate::dmy(data4$Date)
 data4 <- dplyr::arrange(data4,Date)
 
 #Creating a week number,month,quarter column in dataframe
 data4$Week_Number <- seq(1:length(unique(data4$Date)))
 
 #adding quarter & month columns
 data4$month <- lubridate::month(data4$Date)
 data4$quarter <- lubridate::quarter(data4$Date)
 
 ##Creating a event type dataframe##
 
 # creating Holiday_date vector
 Holiday_date <- c("12-02-2010", "11-02-2011", "10-02-2012", "08-02-2013","10-09-2010", "09-09-2011", "07-09-2012", "06-09-2013","26-11-2010", "25-11-2011", "23-11-2012", "29-11-2013","31-12-2010", "30-12-2011", "28-12-2012", "27-12-2013")
 
 #assigning date format to Holiday_date vector
 Holiday_date <- lubridate::dmy(Holiday_date)
 
 #Creating Events vector
 Events <-c(rep("Super Bowl", 4), rep("Labour Day", 4),rep("Thanksgiving", 4), rep("Christmas", 4))
 
 #Creating dataframe with Events and date
 Holidays_Data <- data.frame(Events,Holiday_date)
 
 #merging both dataframes
 data4<-merge(data4,Holidays_Data, by.x= "Date", by.y="Holiday_date", all.x = TRUE)
 
 #Replacing null values in Event with No_Holiday
 data4$Events = as.character(data4$Events)
 data4$Events[is.na(data4$Events)]= "No_Holiday"
 
 
 #linear regression graph
 par(mfrow=c(3,3))
 for(i in 3:11){
   plot(data4[,i], 
        data4$Weekly_Sales,
        main=names(data4[i]), 
        ylab="Weekly Sales", xlab =" ",
        col='red',
        abline(lm(data4[,i] ~ data4$Weekly_Sales, data = data4), col = "blue"))
 }
 

 
#Removing unnecessary columns and changing structure of Events
data5 = data4 
 
data5$Date <-NULL
data5$Store <- NULL
data5$Events <- as.factor(data5$Events)
str(data5)
 
data5$Holiday_Flag <- as.numeric(data5$Holiday_Flag)
data5$Week_Number <- as.numeric(data5$Week_Number)
data5$quarter <- as.numeric(data5$quarter)
 
 
 
 
 
 #correlation matrix and corr plot
 corr = cor(data5[, c(1:9)])
 View(corr)
 
 corrplot(corr, method = "color", cl.pos = 'n', rect.col = "black",  
          tl.col = "indianred4", addCoef.col = "black", number.digits = 2, 
          number.cex = 0.60, tl.cex = 0.7, cl.cex = 1,
          col = colorRampPalette(c("green4","white","red"))(100))
