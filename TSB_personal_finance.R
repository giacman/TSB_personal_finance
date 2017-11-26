#----------------------------------------------------#
# APP -----------------------------
#----------------------------------------------------#

library(shiny)
library(rstudioapi)
library(zoo)
library(dplyr)
library(lubridate)
library(gdata)
library(ggplot2)
library(gridExtra)
library(plotly)


# Loading csv file statements ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- getwd()

# classic account
path_classic = paste0(path,"/statements/tsb_classic_enhance/")

temp_classic = list.files(path = path_classic, pattern="*.csv")

classic_enhance_list = list()

for (i in 1:length(temp_classic)){
  df <- read.csv(paste0(path_classic,temp_classic[i]))
  classic_enhance_list[[i]] <- df
  rm(df)
}

classic_enhance = do.call(rbind, classic_enhance_list)
rm(classic_enhance_list)

# classic plus
path_plus = paste0(path,"/statements/tsb_classic_plus/")

temp_plus = list.files(path = path_plus, pattern="*.csv")

classic_plus_list = list()

for (i in 1:length(temp_plus)){
  df <- read.csv(paste0(path_plus,temp_plus[i]))
  classic_plus_list[[i]] <- df
  rm(df)
}

classic_plus = do.call(rbind, classic_plus_list)
rm(classic_plus_list)

total_movements <- rbind(classic_enhance, classic_plus)

# One off fix for an issue with data (forgot to pay rent for 3 months !!!)
m1 <- total_movements %>% 
  filter(Transaction.Type == 'FPO' & Transaction.Date == '10/07/2017') %>% 
  mutate(Debit.Amount = Debit.Amount/3) %>% 
  mutate(Transaction.Date = '10/06/2017')

m2 <- total_movements %>% 
  filter(Transaction.Type == 'FPO' & Transaction.Date == '10/07/2017') %>% 
  mutate(Debit.Amount = Debit.Amount/3) %>% 
  mutate(Transaction.Date = '10/05/2017')

total_movements <- total_movements %>%
  mutate(Debit.Amount = ifelse(Transaction.Type == 'FPO' & Transaction.Date == '10/07/2017',
                               Debit.Amount/3,
                               Debit.Amount)
  )

total_movements_raw <- rbind(total_movements,m1,m2)

# adding month variable
total_movements_raw <- total_movements_raw %>%
  mutate(month = format(parse_date_time(total_movements_raw$Transaction.Date, "dmy"), "%Y-%m"))

# Save Balance at beginning of analysis (to be used later in tab 3)
initial_balance <- total_movements_raw %>%
  filter(month == min(month))%>%
  arrange(Transaction.Date)%>%
  slice(n())%>%
  select(Balance, month)

# Removing first and last month to avoid incomplete data on months
current_month <- format(parse_date_time(Sys.Date(), "ymd"), "%Y-%m")

total_movements <- total_movements_raw %>%
  filter(month != current_month & month != min(month))

total_movements %>%
  filter(month == min(month))%>%
  slice(1)%>%
  select(Balance)

# Excluding movements between accounts
total_movements <- total_movements[!grepl('G VANNUCCHI',total_movements$Transaction.Description),]

# Cleaning workspace
keep(total_movements,initial_balance, sure = TRUE)


# Server ---------------------------------------------

server = shinyServer(function(input, output){
  
  # tab 1) Monthly Average expenses, revenues and net income ----
  
  # Monthly Average Revenues
  monthly_revenues <- total_movements %>%
    select(month, Transaction.Date, Credit.Amount,Transaction.Type, Transaction.Description ) %>%
    na.omit() %>%
    group_by(month) %>%
    summarise(monthly_revenues = sum(Credit.Amount))
  
  # Monthly Average Expenses 
  monthly_expenses <- total_movements %>%
    select(month, Transaction.Date, Debit.Amount,Transaction.Type, Transaction.Description ) %>%
    na.omit() %>%
    group_by(month) %>%
    summarise(monthly_expense = sum(Debit.Amount))
 
  # #Table 
  # a <- reactive({
  #   
  # })
  # 
  # output$tab_a <- DT::renderDataTable({
  #   DT::datatable(a())
  # })
  # 
  
  # Plot monthly revenues vs monthly expenses
  plot_rev_exp <- reactive({
    ggplot()+
      geom_point(data = monthly_revenues, aes(x = month, y = monthly_revenues))+
      geom_line(data = monthly_revenues, aes(x = month, y = monthly_revenues, group = 1), colour = 'blue')+
      geom_point(data = monthly_expenses, aes(x = month, y = monthly_expense))+
      geom_line(data = monthly_expenses, aes(x = month, y = monthly_expense, group = 1), colour = 'red')
  })
  
  output$plot_rev_exp <- renderPlotly({plot_rev_exp()})

  
  
  # Net Income
  monthly_net_income <- total_movements %>%
    select(month, Transaction.Date, Debit.Amount,Credit.Amount, Transaction.Type, Transaction.Description ) %>%
    mutate(Credit.Amount = replace(Credit.Amount,is.na(Credit.Amount),0))%>%
    mutate(Debit.Amount = replace(Debit.Amount,is.na(Debit.Amount),0))%>%
    group_by(month) %>%
    summarise(monthly_net_income = sum(Credit.Amount - Debit.Amount))
  
  # Plot Net Income
  plot_net_income <- reactive({
    ggplot()+
      geom_point(data = monthly_net_income, aes(x = month, y = monthly_net_income))+
      geom_line(data = monthly_net_income, aes(x = month, y = monthly_net_income, group = 1))+
      geom_hline(yintercept=0, colour = 'red')
  })


    
  output$plot_net_income <- renderPlotly(plot_net_income())
})

#ggplotly(plot1)
#ggplotly(plot2)


# Ui -------------------------------------------------

ui = {
  fluidPage(
    headerPanel('TSB Personal Finances Dashobard'),
    sidebarPanel(
      width = 2 ,
      selectInput('time_interval', 'select time period:',
                                   choices = c('daily',
                                               'weekly',
                                               'monthly',
                                               'yearly')
                                   , selected = 'weekly'
                  ),
    dateRangeInput('date', 'select time interval',
                   start = Sys.Date() %m+% months(-6),
                   end = Sys.Date(),
                   min = '2012-01-01',
                   max = Sys.Date(),
                   width = 300
                   ),
    selectInput('time_interval', 'select time period:',
                choices = c('daily',
                            'weekly',
                            'monthly',
                            'yearly')
                , selected = 'weekly'
                )
  ),
    mainPanel(
      tabsetPanel(id = 'panel',
                  tabPanel('Monthly View',
                           # br(),
                           # DT::dataTableOutput('tab_a', width = 1500),
                           # br(),
                           plotlyOutput('plot_net_income'),
                           plotlyOutput('plot_rev_exp')
                  )
                  # ,
                  # tabPanel('b',
                  #          br(),
                  #          DT::dataTableOutput('tab_b', width = 1500),
                  #          br(),
                  #          plotlyOutput('plotly_plot_b')
                  # )
      )
      
      
    )
  )}


# App ------------------------------------------------
shinyApp(ui = ui, server = server)

# 
# # Tab 2) Expenses and Revenues with breakdown voices over months ------
# 
# # attach a tag to every transaction based on logic above:
# shelter <- ('CENTRAL|HOUSEKEEP|L B WALTHAM FOREST 48507504N|TIDYCHOICE|M TAYLOR|DEPOSIT PROTECTION|TIDYCHOI')
# forniture <- ('EMMALOVES CD 0111|LNK SELIN FOOD & W CD 0111 18FEB17|UNTO THIS LAST|ARGOS|MUJI|ANAMICLIMITED|IKEA LIMITED|ROSELAND')
# nursery <- ('ANUTA DUNCA|MICHELLE PEARSON|PAYPAL *JOHNLEWISP|ALESSANDRA COVINO|HM PASSPORT OFFICE')
# giving <- ('HELPINGRHI|WIKIMEDIAF|UNHCR|GUIDE DOGS')
# utilities <- ('BRGAS|VIRGIN|THAMES WATER|SKY DIGITAL|BRITISH GAS')
# learning <- ('GITHUB|COURSERAIN|LINKEDIN|LCODETHW|CODESCHOOL.COM|DATAQUEST.IO|CODECADEMY|UDEMY')
# media <- ('NETFLIX|Spotify|Prime|SONY|ITUNES.COM|NEW YORKER|PLAYSTATIO|NYTDIGITALSUBSCRIP|NEWYORKTIM')
# transport <- ('WALTHAMSTOW CYCLE|TFL.GOV.UK|UBER|Uber|LONDON OVERGROUND|ADDISONLEE|TFL CYCLE HIRE|ABOUT THE BIKE')
# grocery <- ('Co-op|SUPERMARKET|OCADORETAI|SAINSBURYS|EKOL')
# work_lunch <- ('HUSSEYS|CINNAMON|BOTTEGA|CAPTAIN|GASTRONOMICA|RIVER VIEW RESTAUR|PROSPECT OF WHITBY|RIVERVIEW SEAFOOD')
# travel_expenses <- ('METROPOLITAN HOTEL|GIOVANNIRE|CHRISTIAN GUILD|GIANNELLIF|EUROS|RIALTO|GATWICK EXPRSS|FLEXICOVER.CO.UK|GATWICK SOUTH|Europcar.com|NIVI CREDIT')
# travel_tickets <- ('RYANAIR|EASYJET|CARHIRE|TRENITALIA|CITYJET|VUELING')
# eating_out <- ('AKARI UDON NOODLE|MARKSMAN|DINER|MAI SUSHI|PILGRIMS|EAT17|FRANCO MANCA|SODO|PIZZA EXPRESS|FRANZ|THE ALBION IN GOLD|DELIVEROO|DELIVEROOCOUK|HARE AND HOUNDS|PADRON|DISHOOM|THE BREAKFAST CLUB|SANTO REMEDIO|BELL BOI|POPPIESFISH|TONKOTSU|IL GUSCIO RESTAURA|MEAT MISSION|KAHAILA|LA BOUCHE|AMICI MIEI|BIRLEYS SANDWICHES|HOMESLICE|RED DOG SALOON|BURRO E SALVIA|OMBRA|BUSABA SHOREDITCH|TAYYABS|PING PONG')
# going_out <- ('THE CAMBERWELL ARM|THE NAGS HEAD|PICTUREHOU|BRITISH FILM INSTI|WWW.GENESISCINEMA|WWW.BARBICAN|OWL AND PUSSYCAT|TATE|THE STAR BY HACKN|ALBION|BAR KICK|THE KINGS ARMS|RICH MIX|THE FLORIST ARMS|WATER POET|CARGO|BEST OF THEATRE|MILAGROS|THE TALBOT|CASPAR SAMBROOK')
# fashion <- ('BEAU LONDON|TIMBERLAND|COS|LEVI STRAUSS|DR MARTENS|ARMYPANDAG|CALZEDONIA|SNEAKERSNDSTUFF|AESOP|BARBER & PARLOUR|TAYLOR TAYLOR')
# sport <- ('HARLANDS 2006133A-BODYSTUDI|PLAYFOOTBALL|DECATHLON|KOBUDORAKUTENSHOP|WWW.OPRO.COM')
# salary <- ('FLUBIT LIMITED|FLUBIT LTD')
# interests_income <- ('INTEREST')
# extra_income <- ('VANNUCCHI P|CARANDINI S|BATTAGLINI|CIONNINI')
# 
# total_movements_tagged <- total_movements %>%
#   mutate(transaction_amount = ifelse(!is.na(Debit.Amount), -1 * Debit.Amount,Credit.Amount )) %>%
#   mutate(transaction_type = ifelse(!is.na(Debit.Amount), 'expense','revenue')) %>%
#   mutate(Transaction.Description = as.character(Transaction.Description))%>%
#   mutate(tag = ifelse(grepl(shelter,Transaction.Description) & transaction_type == 'expense', 'shelter',
#                       ifelse(grepl(nursery,Transaction.Description) & transaction_type == 'expense', 'nursery',
#                                   ifelse(grepl(giving,Transaction.Description) & transaction_type == 'expense', 'giving',
#                                          ifelse(grepl(utilities,Transaction.Description) & transaction_type == 'expense', 'utilities',
#                                                 ifelse(grepl(learning,Transaction.Description) & transaction_type == 'expense','learning',
#                                                        ifelse(grepl(media,Transaction.Description) & transaction_type == 'expense', 'media',
#                                                               ifelse(grepl(transport,Transaction.Description) & transaction_type == 'expense','transport',
#                                                                      ifelse(grepl(grocery,Transaction.Description) & transaction_type == 'expense','grocery',
#                                                                             ifelse(grepl(work_lunch,Transaction.Description) & transaction_type == 'expense', 'work_lunch',
#                                                                                     ifelse(grepl(travel_expenses,Transaction.Description) & transaction_type == 'expense','travel_expenses',
#                                                                                            ifelse(grepl(travel_tickets,Transaction.Description) & transaction_type == 'expense', 'travel_tickets',
#                                                                                                   ifelse(grepl(eating_out,Transaction.Description) & transaction_type == 'expense', 'eating_out',
#                                                                                                          ifelse(grepl(going_out,Transaction.Description) & transaction_type == 'expense', 'going_out',
#                                                                                                                ifelse(grepl(fashion,Transaction.Description) & transaction_type == 'expense', 'fashion',
#                                                                                                                       ifelse(grepl(sport,Transaction.Description) & transaction_type == 'expense', 'sport',
#                                                                                                                              ifelse(grepl(forniture,Transaction.Description) & transaction_type == 'expense', 'forniture',
#                                                                                                                                     ifelse(grepl(salary,Transaction.Description) & transaction_type == 'revenue', 'salary',
#                                                                                                                                            ifelse(grepl(interests_income,Transaction.Description) & transaction_type == 'revenue', 'interests_income',
#                                                                                                                                                   ifelse(grepl(extra_income,Transaction.Description) & transaction_type == 'revenue', 'extra_income', 'uncategorised')
#                                                                                                                                                   )
#                                                                                                                        )
#                                                                                                                 )
#                                                                                                          )
#                                                                                                   )
#                                                                                            )
#                                                                                     )
#                                                                             )
#                                                                      )
#                                                               )
#                                                        )
#                                                 )
#                                          )
#                                   )
#                              ) 
#                       )
#                 )
#             )
#          ) %>%
#   select(Transaction.Date, month, Transaction.Description, tag, transaction_type, transaction_amount)
# 
# plot3 <- ggplot(data = total_movements_tagged[total_movements_tagged$transaction_type == 'expense',], 
#        aes(x = month, y = -1 * transaction_amount, fill = tag))+
#   geom_bar(stat = 'identity')
# 
# ggplotly(plot3)
# 
# ## Uncomment below to review uncategorised items
# # View(total_movements_tagged %>% 
# #   filter(tag == 'uncategorised')%>%
# #   arrange(transaction_amount))
# 
# # Notes:
# # revenues and expenses go up  lot in March 2017 because of deposit moving in andd out
# # 26/08/2016 IZ *BELL BOI LTD CD 0111 e'  il Babbo che e' stato a Londra
# 
# 
# # Tab 3) Savings and Targets -----
# 
# # calculate Income trend and forecasting
# 
# income_timeseries <- total_movements %>%
#   select(month, Transaction.Date, Debit.Amount,Credit.Amount, Transaction.Type, Transaction.Description ) %>%
#   mutate(Credit.Amount = replace(Credit.Amount,is.na(Credit.Amount),0))%>%
#   mutate(Debit.Amount = replace(Debit.Amount,is.na(Debit.Amount),0))%>%
#   group_by(month) %>%
#   summarise(Balance = sum(Credit.Amount - Debit.Amount))%>%
#   bind_rows(initial_balance) %>%
#   arrange(month)%>%
#   mutate(Balance = cumsum(Balance))%>%
#   select(month,Balance)
# 
# 
# plot4 <- ggplot(data = income_timeseries, aes(x = month, y = Balance, group = 1)) + 
#   geom_line(color = 'blue')+
#   geom_area(fill = 'blue', alpha = .1)
# 
# ggplotly(plot4)
# 
# # Forecast
# 
# # Target = # whats your target
# 
# # Time Estimate to reach target
# 
