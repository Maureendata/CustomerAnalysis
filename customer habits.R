install.packages("tidyverse")
library("tidyverse")
#importing data
cust_data<-read_csv('C:/Users/user/Documents/data analysis/case 3/sales analysis.csv')
View(cust_data)
str(cust_data)
#process
#data cleaning
install.packages("janitor")
library("janitor")
cust_data <- remove_empty(cust_data,which =c("rows"))
cust_data <- remove_empty(cust_data,which =c("cols"))
n_distinct(cust_data$index)
#34867
install.packages("skimr")
library(skimr)
skim_without_charts(cust_data)
#omit na rows
head(cust_data)
View(cust_data)
#removing a column
cust_data$Column1<-NULL
cust_data<-na.omit(cust_data)
skim_without_charts(cust_data)
sapply(cust_data,function(x) sum(is.na(x)))
View(cust_data)
sum(is.na(cust_data))
sum(duplicated(cust_data))
cust_data <-cust_data %>% distinct() %>% drop_na()
clean_names(cust_data)
View(cust_data)
cust_data<-cust_data %>% rename("id"="index")
colnames(cust_data)
duplicated(colnames(cust_data))
str(cust_data$Date)
install.packages("lubridate")
library("lubridate")
custclean<-cust_data
custclean$Date <-mdy(cust_data$Date)
str(custclean$Date)         
View(custclean)
str(custclean)
#renaming column names
#custclean %>% rename(Product_category=`Product Category`)
#suumary of data
custclean %>%  
  select(Country,`Product Category`,Quantity) %>%
  summary()
custclean %>%  
  select(Country,`Sub Category`,Quantity) %>%
  summary()
custclean %>%  
  select(State,`Sub Category`,Quantity) %>%
  summary()
custclean %>%  
  select(Month,`Customer Gender`,Revenue) %>%
  summary()
custclean %>%  
  select(id,Revenue) %>%
  summary()
#calculating margin
custclean$margin<-custclean$Revenue-custclean$Cost
View(custclean)
custclean$unitmargin<-round(custclean$`Unit Price`-custclean$`Unit Cost`,2)
View(custclean)
#product consumption based on country
sum(custclean$Revenue,na.rm=TRUE)
#22344576
ggplot(data=custclean,aes(x=Country,y=Quantity,fill=`Product Category`))+geom_bar(stat="identity")+labs(title="Product consumption per country")

#revenue based on country
prodfrance<-custclean %>% 
  filter(Country=="France") 
francerev<-sum(prodfrance$Revenue,na.rm=TRUE)
prodgerman<-custclean %>% 
  filter(Country=="Germany")
germanrev<-sum(prodgerman$Revenue,na.rm=TRUE)
view(francerev)
view(germanrev)
produs<-custclean %>% 
  filter(Country=="United States")
usrev<-sum(produs$Revenue,na.rm=TRUE)
produk<-custclean %>% 
  filter(Country=="United Kingdom")
ukrev<-sum(produk$Revenue,na.rm=TRUE)
View(usrev)
str(custclean)
#22344576

dataprod<-custclean %>% summarise(Sum_france = sum(francerev/22344576*100),
                                  Sum_germany = sum(germanrev/22344576*100),
                                  Sum_us=sum(usrev/22344576*100),
                                  Sum_uk=sum(ukrev/22344576*100)
                                  ) %>% round(digits=2)
#calculating total revenue
View(dataprod)
slices<-c(dataprod$Sum_france,dataprod$Sum_germany,dataprod$Sum_us,dataprod$Sum_uk)
lbls<-c("France","Germany","US","UK")
pie(slices,labels=paste(lbls,slices,sep=" ","%"),col=rainbow(6), main="Pie Chart - % of Revenue distribution")
ggplot(data=custclean,aes(x=Country,y=Revenue,fill=`Product Category`))+geom_bar(stat="identity")+labs(title="Product category revenue per country")
#it is noted that most revenue comes from Us
#margin based on country and product
rev_region<-custclean %>% group_by(Country,`Product Category`) %>% summarise(Total_Revenue=sum(Revenue),TotalMargin=sum(margin),.groups='drop')
View(rev_region)
ggplot(rev_region)+geom_col(mapping=aes(x=Country,y=TotalMargin,fill=`Product Category`),position="dodge")+labs(x="Country",y="Margin",title="Profit margin by country")
ggplot(rev_region)+geom_col(mapping=aes(x=Country,y=Total_Revenue,fill=`Product Category`),position="dodge")+labs(x="Country",y="Revenue",title="Profit margin by country")
#bikes are the greatest form of profits in every country
#United states records the lowest profits when it comes to bikes and yet it's the most profitable

#Quantity distribution per country
ggplot(custclean)+geom_col(mapping=aes(x=Country,y=Quantity,fill=`Product Category`),position="dodge")+labs(x="Country",y="Quantity",title="Quantity distribution per country")

rev_region2<-custclean %>% group_by(Country,`Product Category`,`Sub Category`) %>% summarise(Total_Revenue=sum(Revenue),TotalMargin=sum(margin),.groups='drop')
View(rev_region2)
View(custclean)
rev_region3<-custclean %>% group_by(Country,State,`Product Category`) %>% summarise(Total_Revenue=sum(Revenue),TotalMargin=sum(margin),.groups='drop')
View(rev_region3)
#United states is the most profitable
ggplot(rev_region3)+geom_col(mapping=aes(x=State,y=TotalMargin))+labs(x="State",y="Profit Margin",title="Profit margin by state")
#American states have negative profit on bikes
#Average margin per month
margin_month<-custclean %>% group_by(Country,Month) %>% summarise(AvgRevenue=mean(Revenue),AvgMargin=mean(margin),.groups='drop')
margin_month1<-custclean %>% group_by(Month) %>% summarise(AvgRevenue=mean(Revenue),AvgMargin=mean(margin),.groups='drop') %>% arrange(Month)
View(margin_month)
View(margin_month1)
margin_month1$Month <- factor(margin_month1$Month,
                                    levels= c("January", "February", "March", "April", "May", "June", "July","August","September","October","November","December"))
#sort months
margin_month1$Month <- sort(margin_month1$Month)
ggplot(data=margin_month1)+geom_col(mapping=aes(x=Month,y=AvgRevenue))+labs(title='Average profit per month')
#Revenue is good in the middle of the year
ggplot(data=margin_month1)+geom_col(mapping=aes(x=Month,y=AvgMargin))
#Profit is good in the middle of the year                                  )
ggplot(data=custclean)+geom_point(mapping=aes(x=Date,Revenue))+labs(title="Revenue distribution")
#revenue increased since july 2015 upto july 2016
ggplot(data=custclean)+geom_col(mapping=aes(x=`Customer Age`,y=margin))+labs(title="Margin per age")
#most of the profits  come from people between late 20s and late 40s
#the highest profit comes from people in their 30s
Totalmargin<-sum(custclean$margin)
ggplot(data=custclean)+geom_point(mapping=aes(y=`Product Category`,x=`Customer Age`,color=Quantity))
#people between late 20s and late 40s love accessories and bikes.
#margin based on country
mfrance<-custclean %>% 
  filter(Country=="France") 
francem<-sum(mfrance$margin,na.rm=TRUE)
mgerman<-custclean %>% 
  filter(Country=="Germany")
germanm<-sum(mgerman$margin,na.rm=TRUE)
view(francem)
view(germanm)
mus<-custclean %>% 
  filter(Country=="United States")
usm<-sum(mus$margin,na.rm=TRUE)
muk<-custclean %>% 
  filter(Country=="United Kingdom")
ukm<-sum(muk$margin,na.rm=TRUE)
View(usm)
View(ukm)
sum(custclean$margin)
#10145640
marginprod<-custclean %>% summarise(Margin_france = sum(francem/10145640*100),
                                  Margin_germany = sum(germanm/10145640*100),
                                  Margin_us=sum(usm/10145640*100),
                                  Margin_uk=sum(ukm/10145640*100)
) %>% round(digits=2)

View (marginprod)
slices<-c(marginprod$Margin_france,marginprod$Margin_germany,marginprod$Margin_us,marginprod$Margin_uk)
lbls<-c("France","Germany","US","UK")
pie(slices,labels=paste(lbls,slices,sep=" ","%"),col=rainbow(6), main="Pie Chart - % of Margin distribution")
#United states is the most profitable
View(custclean)

