library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(fiftystater)

#########################
#       Load Data       #
#########################

data=read.csv("data.csv")

data("fifty_states")


#########################
#    Data Cleanup       #
#########################

data%>%
  separate(Co.Bio,into=c("Age","Size"), sep=" ")
# seperated company age and size information into two new variables: "Age" and "Size"

data_clean=data%>%
  separate(No..Maker.Contracts ,into=c("M","Maker_No."), sep=1)%>%
  separate(No..Advanced.Contracts ,into=c("A","Adv_No."), sep=1)
# Extracted the numerical information from the two variables by 
# splitting each colume into two new ones: "M", "Maker_No." and "A", "Adv_No."

final_data= data_clean%>%
  filter(Current.Customer..Y.N.=="Y")%>%
  mutate(Total.Revenue = Maker.Revenue+ Advanced.Revenue )%>%
  mutate(New.customer= Period.Acquired==8)
# Filtered for Current.Customers..Y.N.=="Y" because 
# we only want to look at current customers.
# Added up the values from Maker.Revenue and Advanced.Revenue 
# and saved it to a new variable called "Total.Revenue".
# Customers acquired in period 8 are saved to "New.customers".

final_data$State = str_to_lower(final_data$State)
# Converted state names to lower case for later mapping 

#########################
#    Data Analysis      #
#########################

final_data%>%
  group_by(New.customer)%>%
  summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count)%>%
  ggplot(aes(x=New.customer, y= Revenue.per.customer))+
  geom_col()+
  ggtitle("Average Revenue for New Customers vs. Retained Customers ")+
  xlab(" ")+
  ylab("Average Revenue Per Customer ($)")+
  scale_x_discrete(limit = c("FALSE", "TRUE"),
                   labels = c("Retained Customers", "New Customers"))
# Graphed average revenue of retained customers vs new customers
# Retained customers contrubute 2 times average revenue than new customers, 
# implying that we should allocate more budget to retained customers. 

final_data%>%
  group_by(Sector, New.customer)%>%
  summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count)%>%
  ggplot(aes(x=Sector, y= Revenue.per.customer, fill= New.customer))+
  ggtitle("Average Customer Revenue By Sector")+
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = c("lightblue", "pink"),
                    labels=c("Retained Customer", "New Customer"))+
  theme(legend.title = element_blank())+
  ylab("Average Revenue Per Customer ($)")
# Graphed average customer revenue for different sectors
# Noticed average revenue of new Automotive customers is much lower than others.

final_data%>%
  group_by(Region, New.customer)%>%
  summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count)%>%
  ggplot(aes(x=Region, y= Revenue.per.customer, fill= New.customer))+
  ggtitle("Regional Performance: New Customers vs. Retained Customers")+
  geom_col(position = "dodge") + 
  scale_fill_manual(values = c("lightblue", "pink"),
                  labels=c("Retained Customer", "New Customer"))+
  theme(legend.title = element_blank())+
  ylab("Average Revenue Per Customer ($)")
# Next we analyzed region performance by graphing out the average revenue per customer
# for each geographic region.
# All regions are roughly the same, nothing notiable 

final_data%>%
  group_by(Acquisition.Channel,New.customer)%>%
  summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count)%>%
  filter(New.customer=="TRUE")%>%
  ggplot(aes(x=Acquisition.Channel, y= Revenue.per.customer))+
  ggtitle("Average Customer Revenue By Acquisition Channel")+
  geom_col()+
  ylab("Average Revenue Per Customer ($)")+
  xlab("Acquisition Channel")
# Graphed the average customer revenue by acquisition channel.
# Internal acquisition produces the most profitable customers.


# Create state-wise revenue data
State_data = final_data %>%
  group_by(State) %>%
  summarise(Revenue = sum(Total.Revenue))

# Revenue quantile 
state_qtile = quantile(State_data$Revenue, prob=c(0,0.25,0.5,0.75,1))

# Reveunue tiers map
final_data %>%
  group_by(State) %>%
  summarise(Revenue = sum(Total.Revenue), Tier=ifelse(((Revenue>=state_qtile[1])&(Revenue<state_qtile[2])),"Tier 4",
                                                      ifelse(((Revenue>=state_qtile[2])&(Revenue<state_qtile[3])),"Tier 3",
                                                             ifelse(((Revenue>=state_qtile[3])&(Revenue<state_qtile[4])),"Tier 2","Tier 1")))) %>%
  ggplot(aes(map_id = State)) +
  geom_map(aes(fill = Tier), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  ggtitle("Revenue Tier by State") +
  scale_fill_manual(values = c("#20e020","#ffdd00","#ff8000","#ff0000")) +
  theme(legend.position = "right", 
        panel.background = element_blank()) +
  fifty_states_inset_boxes()
# Tier 1: 0% ~ 25%:   87000   ~ 452000
# Tier 2: 25% ~ 50%:  452000  ~ 622500
# Tier 3: 50% ~ 75%:  622500  ~ 1103250
# Tier 4: 75% ~ 100%: 1103250 ~ 2164000

# Pie chart of revenue contribution %
final_data %>%
  mutate(Customer.Cat = ifelse(New.customer,str_c("New ",Sector),str_c("Retained ",Sector))) %>%
  group_by(Customer.Cat) %>%
  summarise(Revenu.Percent = sum(Total.Revenue)/sum(final_data$Total.Revenue)) %>%
  ggplot(aes(x="",y=Revenu.Percent, fill=Customer.Cat)) +
  geom_bar(width=1,stat = "identity") +
  coord_polar("y", 0) +
  scale_fill_manual(name = "Customer Category",
                    values=c("#0066dd","#0044bb","#002299","#00dd00","#00bb00","#009900")) +
  labs(x = "", y = "") +
  ggtitle("Revenue Contribution % by Customer Category")
# Created pie chart to show total revenue contribution from different customers.
# Pie chart shows that retained customers contributed the most revenues in the last period.
# Also, among the retained customers, the Defense and Aerospace were the most profitable sectors.


#########################
#  summary statistics   #
#########################


final_data%>%
  group_by(New.customer)%>%
  summarise(Avg.Revenue = mean(Total.Revenue), Stdev.Rev= sd(Total.Revenue), 
            Min.Rev=min(Total.Revenue), Max.Rev= max(Total.Revenue) )


final_data%>%
  group_by(Sector)%>%
  summarise(Avg.Revenue = mean(Total.Revenue), Stdev.Rev= sd(Total.Revenue), 
            Min.Rev=min(Total.Revenue), Max.Rev= max(Total.Revenue) )

final_data%>%
  group_by(Acquisition.Channel)%>%
  summarise(Avg.Revenue = mean(Total.Revenue), Stdev.Rev= sd(Total.Revenue), 
            Min.Rev=min(Total.Revenue), Max.Rev= max(Total.Revenue) )
  




#########################
#       Shiny R         #
#########################

library(shiny)

ui <- fluidPage(
  titlePanel(title="Customer Revenue Analysis",
             windowTitle = "Customer Revenue Analysis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Please select"),
      selectInput(inputId = "bar_var",
                  label = "Segment by",
                  choices=c("New Customer",
                            "Sector",
                            "Acquisition Channel",
                            "Region"),
                  selected = "New Customer"),
      selectInput(inputId = "pie_var",
                  label = "Pick a sector",
                  choices=c("Aerospace",
                            "Automotive",
                            "Defense"),
                  selected = "New Customer"),
      selectInput(inputId = "map_var",
                  label = "Pick a representaion",
                  choices=c("Revenue",
                            "Revenue tier"),
                  selected = "Revenue")
    ),
    mainPanel(
      plotOutput(outputId = "bar"),
      plotOutput(outputId = "pie"),
      plotOutput(outputId = "map")
    )
  )
) 

server <- function(input, output) {
  output$bar=renderPlot({
    bar_title=str_c("Total Revenue by ",input$bar_var)
    
    bar_plot = switch(input$bar_var,
                      "New Customer" = group_by(final_data,New.customer) %>%
                        summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count) %>%
                        ggplot(aes(x=New.customer,y=Revenue.per.customer)),
                      "Sector" = group_by(final_data,Sector) %>%
                        summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count) %>%
                        ggplot(aes(x=Sector,y=Revenue.per.customer)),
                      "Acquisition Channel" = group_by(final_data,Acquisition.Channel) %>%
                        summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count) %>%
                        ggplot(aes(x=Acquisition.Channel,y=Revenue.per.customer)),
                      "Region" = group_by(final_data,Region) %>%
                        summarise(Revenue = sum(Total.Revenue), Count = n(), Revenue.per.customer= Revenue/Count) %>%
                        ggplot(aes(x=Region,y=Revenue.per.customer)))
    
    bar_plot +
      geom_col() +
      ggtitle(bar_title)
  })
  
  
  output$pie=renderPlot({
    pie_title=str_c("Revenue Contribution % in Sector ",input$pie_var)
    
    final_data %>%
      filter(Sector==input$pie_var) %>%
      group_by(New.customer) %>%
      summarise(Revenu.Percent = sum(Total.Revenue)/sum(final_data$Total.Revenue)) %>%
      ggplot(aes(x="",y=Revenu.Percent, fill=New.customer)) +
      geom_bar(width=1,stat = "identity") +
      coord_polar("y", 0) +
      scale_fill_manual(values=c("#0066dd","#00dd00")) +
      labs(x = "", y = "") +
      ggtitle(pie_title)
    
  })
  
  output$map=renderPlot({
    map_title=str_c("Revenue Contribution Map (",input$map_var,")")
    
    map_data = final_data %>%
      group_by(State) %>%
      summarise(Revenue = sum(Total.Revenue), Tier=ifelse(((Revenue>=state_qtile[1])&(Revenue<state_qtile[2])),"Tier 4",
                                                          ifelse(((Revenue>=state_qtile[2])&(Revenue<state_qtile[3])),"Tier 3",
                                                                 ifelse(((Revenue>=state_qtile[3])&(Revenue<state_qtile[4])),"Tier 2","Tier 1"))))
    
    ggplot(map_data, aes(map_id = State)) +
      geom_map(aes(fill = Tier), map = fifty_states) + 
      scale_fill_manual(values = c("#20e020","#ffdd00","#ff8000","#ff0000"))
    
    map_plot = switch(input$map_var,
                      "Revenue" = ggplot(map_data, aes(map_id = State)) +
                        geom_map(aes(fill = Revenue), map = fifty_states),
                      "Revenue tier" = ggplot(map_data, aes(map_id = State)) +
                        geom_map(aes(fill = Tier), map = fifty_states) + 
                        scale_fill_manual(values = c("#20e020","#ffdd00","#ff8000","#ff0000")))
    
    map_plot +
      expand_limits(x = fifty_states$long, y = fifty_states$lat) +
      coord_map() +
      scale_x_continuous(breaks = NULL) + 
      scale_y_continuous(breaks = NULL) +
      labs(x = "", y = "") +
      ggtitle(map_title) +
      theme(legend.position = "right", 
            panel.background = element_blank()) +
      fifty_states_inset_boxes()
    
  })
  
}

shinyApp(ui, server)





