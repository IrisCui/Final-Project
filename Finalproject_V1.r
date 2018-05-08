library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
data=read.csv("data.csv")

data%>%
  separate(Co.Bio,into=c("Age","Size"), sep=" ")

data_clean=data%>%
  separate(No..Maker.Contracts ,into=c("M","Maker_No."), sep=1)%>%
  separate(No..Advanced.Contracts ,into=c("A","Adv_No."), sep=1)

final_data= data_clean%>%
  filter(Current.Customer..Y.N.=="Y")%>%
  mutate(Total.Revenue = Maker.Revenue+ Advanced.Revenue )%>%
  mutate(New.customer= Period.Acquired==8)

final_data%>%
  group_by(New.customer)%>%
  summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count)%>%
  ggplot(aes(x=New.customer, y= Revenue.per.customer))+
  geom_col()+
  ggtitle("Revenue per customer by old/new customer")
# Old customers contrubute 2 times average revenue than new customers, implying that we should allocate more budget to old customers
# Next step is to disect by sectors.

final_data%>%
  group_by(Sector, New.customer)%>%
  summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count)%>%
  ggplot(aes(x=Sector, y= Revenue.per.customer, fill= New.customer))+
  ggtitle("Revenue per customer by sector by new/old customer")+
  geom_col(position = "dodge") + 
  scale_fill_manual(values = c("lightblue", "pink"))+
  scale_y_continuous(labels = scales::comma)
# We can enhance automotive's new customers, since their average revenue is much lower than old customers'.
# next step we can analyze region performance.

final_data%>%
  group_by(Region, New.customer)%>%
  summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count)%>%
  ggplot(aes(x=Region, y= Revenue.per.customer, fill= New.customer))+
  ggtitle("Revenue per customer by region by new/old customer")+
  geom_col(position = "dodge") + 
  scale_fill_manual(values = c("lightblue", "pink"))
# we don't need to focus on specific region, they are evenly distributed.

final_data%>%
  group_by(Acquisition.Channel,New.customer)%>%
  summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count)%>%
  filter(New.customer=="TRUE")%>%
  ggplot(aes(x=Acquisition.Channel, y= Revenue.per.customer))+
  ggtitle("Revenue per customer by acquisition channel (new customer)")+
  geom_col() 
# internal acquisition is the most effective channel for acquiring new customers


#summary statistics


final_data%>%
  group_by(New.customer)%>%
  summarise(Avg.Revenue = mean(Total.Revenue), Stdev.Rev= sd(Total.Revenue), Min.Rev=min(Total.Revenue), Max.Rev= max(Total.Revenue) )



final_data%>%
  group_by(Sector)%>%
  summarise(Avg.Revenue = mean(Total.Revenue), Stdev.Rev= sd(Total.Revenue), Min.Rev=min(Total.Revenue), Max.Rev= max(Total.Revenue) )



final_data%>%
  group_by(Acquisition.Channel)%>%
  summarise(Avg.Revenue = mean(Total.Revenue), Stdev.Rev= sd(Total.Revenue), Min.Rev=min(Total.Revenue), Max.Rev= max(Total.Revenue) )


#Conclusion/ Future work

##auto sector is short-term reveune generator. While defense and areospace sectors can be more profitable with long-term customer relationships.
## the company should be more focused in mainting the relationships with the old customers and in order to get quick profit, the company can focus
## more in auto sector by implementing mainly internal aqucisition strategy.


## We would like to look at more time serious data if there are any to compare the revenue performance as time changes. We would also want to
## access the cost data in order to analyze the profit gains.





















data3 <- data2 %>% 
  filter(Current.Customer..Y.N.=="Y")%>%
  mutate(Total.Revenue = Maker.Revenue+ Advanced.Revenue )

ggplot(data3, aes(x= Region, y= Total.Revenue))+
  geom_col()+
  ggtitle("Total Revenue by region")
# Midwest and Southeast generated most revenue
#data3 is current customer dataset

data4=data3%>%
  group_by(Region, Sector)%>%
  summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count)
#data4 is group by region, sector and revenue

ggplot(data4, aes(x=Region, y= Revenue.per.customer))+
  geom_col()+
  ggtitle("Revenue per customer by region")
# Region west is a promising market area to invest in the future

ggplot(data4, aes(x = Region, y = Sector, fill = Revenue.per.customer )) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "darkred") + 
  ggtitle("Heatmap of Sector and Region") +
  xlab("Region") + ylab ("Sector")
# Defense sector generated the most revenue per customer, especially in Southeast

data5=data3%>%
  group_by(Region,Acquisition.Channel)%>%
  summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count)
#data5 is group by region, acquistiion channel and revenue

ggplot(data5, aes(x = Region, y = Acquisition.Channel, fill = Revenue.per.customer )) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "darkred") + 
  ggtitle("Heatmap of region and acquisition channel") +
  xlab("Region") + ylab ("Acquisition Channel")
# Referral is the most effective channel to invest

data6= data3%>% mutate(New.customer= Period.Acquired==8)%>%
  group_by(New.customer)%>%
  summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count)
# data6 is adding a new column indicating whether it's a newly acquired customer

ggplot(data6, aes(x= Region, y= Total.Revenue))+
  geom_col()+
  ggtitle("Total Revenue by region")






