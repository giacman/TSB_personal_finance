library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(tidyr)
library(DT)

## Loading statements ----

# load files in classic account
path_classic = "./statements/tsb_classic_enhance/"
temp_classic = list.files(path = path_classic, pattern="*.csv")

classic_enhance_list = list()
for (i in 1:length(temp_classic)){
  df <- read.csv(paste0(path_classic,temp_classic[i]))
  drops <- c("X")
  df <- df[ , !(names(df) %in% drops)]
  names(df) <- tolower(names(df))
  classic_enhance_list[[i]] <- df
  rm(df)
}

classic_enhance = do.call(rbind, classic_enhance_list)
rm(classic_enhance_list)

# load files in classic plus
path_plus = "./statements/tsb_classic_plus/"
temp_plus = list.files(path = path_plus, pattern="*.csv")

classic_plus_list = list()

for (i in 1:length(temp_plus)){
  df <- read.csv(paste0(path_plus,temp_plus[i]))
  drops <- c("X")
  df <- df[ , !(names(df) %in% drops)]
  names(df) <- tolower(names(df))
  classic_plus_list[[i]] <- df
  rm(df)
}

classic_plus = do.call(rbind, classic_plus_list)
rm(classic_plus_list)

total_movements_raw <- rbind(classic_enhance, classic_plus)

# Processing data ----

# adding month variable
total_movements_raw <- total_movements_raw %>%
  mutate(month = format(parse_date_time(total_movements_raw$transaction.date, "dmy"), "%Y-%m"))

# Save balance at beginning of analysis (to be used later in tab 3)
initial_balance <- total_movements_raw %>%
  filter(month == min(month))%>%
  arrange(transaction.date)%>%
  slice(n())%>%
  select(balance, month)

# Removing first and last month to avoid incomplete data on months
current_month <- format(parse_date_time(Sys.Date(), "ymd"), "%Y-%m")

total_movements <- total_movements_raw %>%
  filter(month != current_month & month != min(month))

total_movements %>%
  filter(month == min(month))%>%
  slice(1)%>%
  select(balance)

# Excluding movements between accounts
total_movements <- total_movements[!grepl('G VANNUCCHI',total_movements$transaction.description),]

# Adding categories to transaction names ----
shelter <- ('CENTRAL|HOUSEKEEP|L B WALTHAM FOREST 48507504N|TIDYCHOICE|M TAYLOR|DEPOSIT PROTECTION|TIDYCHOI')
forniture <- ('EMMALOVES CD 0111|LNK SELIN FOOD & W CD 0111 18FEB17|UNTO THIS LAST|ARGOS|MUJI|ANAMICLIMITED|IKEA LIMITED|ROSELAND')
nursery <- ('ANUTA DUNCA|MICHELLE PEARSON|PAYPAL *JOHNLEWISP|ALESSANDRA COVINO|HM PASSPORT OFFICE')
giving <- ('HELPINGRHI|WIKIMEDIAF|UNHCR|GUIDE DOGS')
utilities <- ('BRGAS|VIRGIN|THAMES WATER|SKY DIGITAL|BRITISH GAS')
learning <- ('GITHUB|COURSERAIN|LINKEDIN|LCODETHW|CODESCHOOL.COM|DATAQUEST.IO|CODECADEMY|UDEMY')
media <- ('NETFLIX|Spotify|Prime|SONY|ITUNES.COM|NEW YORKER|PLAYSTATIO|NYTDIGITALSUBSCRIP|NEWYORKTIM')
transport <- ('WALTHAMSTOW CYCLE|TFL.GOV.UK|UBER|Uber|LONDON OVERGROUND|ADDISONLEE|TFL CYCLE HIRE|ABOUT THE BIKE')
grocery <- ('Co-op|SUPERMARKET|OCADORETAI|SAINSBURYS|EKOL')
work_lunch <- ('HUSSEYS|CINNAMON|BOTTEGA|CAPTAIN|GASTRONOMICA|RIVER VIEW RESTAUR|PROSPECT OF WHITBY|RIVERVIEW SEAFOOD')
travel_expenses <- ('METROPOLITAN HOTEL|GIOVANNIRE|CHRISTIAN GUILD|GIANNELLIF|EUROS|RIALTO|GATWICK EXPRSS|FLEXICOVER.CO.UK|GATWICK SOUTH|Europcar.com|NIVI CREDIT')
travel_tickets <- ('RYANAIR|EASYJET|CARHIRE|TRENITALIA|CITYJET|VUELING')
eating_out <- ('AKARI UDON NOODLE|MARKSMAN|DINER|MAI SUSHI|PILGRIMS|EAT17|FRANCO MANCA|SODO|PIZZA EXPRESS|FRANZ|THE ALBION IN GOLD|DELIVEROO|DELIVEROOCOUK|HARE AND HOUNDS|PADRON|DISHOOM|THE BREAKFAST CLUB|SANTO REMEDIO|BELL BOI|POPPIESFISH|TONKOTSU|IL GUSCIO RESTAURA|MEAT MISSION|KAHAILA|LA BOUCHE|AMICI MIEI|BIRLEYS SANDWICHES|HOMESLICE|RED DOG SALOON|BURRO E SALVIA|OMBRA|BUSABA SHOREDITCH|TAYYABS|PING PONG')
going_out <- ('THE CAMBERWELL ARM|THE NAGS HEAD|PICTUREHOU|BRITISH FILM INSTI|WWW.GENESISCINEMA|WWW.BARBICAN|OWL AND PUSSYCAT|TATE|THE STAR BY HACKN|ALBION|BAR KICK|THE KINGS ARMS|RICH MIX|THE FLORIST ARMS|WATER POET|CARGO|BEST OF THEATRE|MILAGROS|THE TALBOT|CASPAR SAMBROOK')
fashion <- ('BEAU LONDON|TIMBERLAND|COS|LEVI STRAUSS|DR MARTENS|ARMYPANDAG|CALZEDONIA|SNEAKERSNDSTUFF|AESOP|BARBER & PARLOUR|TAYLOR TAYLOR')
sport <- ('HARLANDS 2006133A-BODYSTUDI|PLAYFOOTBALL|DECATHLON|KOBUDORAKUTENSHOP|WWW.OPRO.COM')
salary <- ('FLUBIT LIMITED|FLUBIT LTD')
interests_income <- ('INTEREST')
extra_income <- ('VANNUCCHI P|CARANDINI S|BATTAGLINI|CIONNINI')


# Server ---------------------------------------------

server = shinyServer(function(input, output){
  
  # tab 1 Monthly Average expenses, revenues and net income ----
  
  # Monthly Average Revenues
  monthly_revenues <- total_movements %>%
    select(month, transaction.date, credit.amount,transaction.type, transaction.description ) %>%
    na.omit() %>%
    group_by(month) %>%
    summarise(monthly_revenues = sum(credit.amount))
  
  # Monthly Average Expenses 
  monthly_expenses <- total_movements %>%
    select(month, transaction.date, debit.amount,transaction.type, transaction.description ) %>%
    na.omit() %>%
    group_by(month) %>%
    summarise(monthly_expense = sum(debit.amount))

  # Net Income
  monthly_net_income <- total_movements %>%
    select(month, transaction.date, debit.amount,credit.amount, transaction.type, transaction.description ) %>%
    mutate(credit.amount = replace(credit.amount,is.na(credit.amount),0))%>%
    mutate(debit.amount = replace(debit.amount,is.na(debit.amount),0))%>%
    group_by(month) %>%
    summarise(monthly_net_income = sum(credit.amount - debit.amount))
  
  monthly_movements <- inner_join(monthly_revenues,monthly_expenses, by = "month")
  monthly_movements <- inner_join(monthly_movements,monthly_net_income, by = "month")
  
  
  # Plots
  first_tab_plot <- reactive(
    
    if(input$plot_income == TRUE){
      
      # Plot Net Income
      ggplot()+
          geom_point(data = monthly_net_income, aes(x = month, y = monthly_net_income))+
          geom_line(data = monthly_net_income, aes(x = month, y = monthly_net_income, group = 1))+
          geom_hline(yintercept=0, colour = 'red')+
        theme(axis.text.x = element_text(angle = 45, hjust = 3),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())

    }else {
      
      # Plot monthly revenues vs monthly expenses
      ggplot()+
          geom_point(data = monthly_revenues, aes(x = month, y = monthly_revenues))+
          geom_line(data = monthly_revenues, aes(x = month, y = monthly_revenues, group = 1), colour = 'blue')+
          geom_point(data = monthly_expenses, aes(x = month, y = monthly_expense))+
          geom_line(data = monthly_expenses, aes(x = month, y = monthly_expense, group = 1), colour = 'red')+
        theme(axis.text.x = element_text(angle = 45, hjust = 3),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())
        
    }
  )
  
output$first_tab_plotly <- renderPlotly({
  ggplotly(first_tab_plot()) %>% 
    layout(autosize = F, width = 1200, height = 400,
           margin = list(b = 160))
  })

  # Tables

  first_tab_table <- reactive({
    monthly_movements %>%
      gather(type, amount, -month) %>% 
      mutate(type = factor(type, levels = c('monthly_revenues', 'monthly_expense', 'monthly_net_income')))%>%
      spread(month,amount)
  })

  
  output$first_tab_table <- DT::renderDataTable({
    t <- DT::datatable(first_tab_table(), rownames = FALSE, 
    extensions = c("Scroller","FixedColumns"), 
    selection = "none",
    options = list(
      fixedColumns = list(leftColumns = 1, rightColumns = 0),
      pageLength = 15,
      bInfo = FALSE,
      pageWidth = 10,
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#004564', 'color': '#fff'});",
        "}"
      )
      ,
      scrollX = TRUE,
      sScrollY = '30vh',
      bPaginate = FALSE,
      bFilter = FALSE
    ))
    t <- t %>% DT::formatCurrency(2:ncol(first_tab_table()), digits = 2, currency = '£') 
  
  })
  
  
  # tab 2 Expenses and Revenues category breakdown ----
  
  total_movements_tagged <- total_movements %>%
    mutate(transaction_amount = ifelse(!is.na(debit.amount), -1 * debit.amount,credit.amount )) %>%
    mutate(transaction_type = ifelse(!is.na(debit.amount), 'expense','revenue')) %>%
    mutate(transaction.description = as.character(transaction.description))%>%
    mutate(tag = ifelse(grepl(shelter,transaction.description) & transaction_type == 'expense', 'shelter',
                        ifelse(grepl(nursery,transaction.description) & transaction_type == 'expense', 'nursery',
                               ifelse(grepl(giving,transaction.description) & transaction_type == 'expense', 'giving',
                                      ifelse(grepl(utilities,transaction.description) & transaction_type == 'expense', 'utilities',
                                             ifelse(grepl(learning,transaction.description) & transaction_type == 'expense','learning',
                                                    ifelse(grepl(media,transaction.description) & transaction_type == 'expense', 'media',
                                                           ifelse(grepl(transport,transaction.description) & transaction_type == 'expense','transport',
                                                                  ifelse(grepl(grocery,transaction.description) & transaction_type == 'expense','grocery',
                                                                         ifelse(grepl(work_lunch,transaction.description) & transaction_type == 'expense', 'work_lunch',
                                                                                ifelse(grepl(travel_expenses,transaction.description) & transaction_type == 'expense','travel_expenses',
                                                                                       ifelse(grepl(travel_tickets,transaction.description) & transaction_type == 'expense', 'travel_tickets',
                                                                                              ifelse(grepl(eating_out,transaction.description) & transaction_type == 'expense', 'eating_out',
                                                                                                     ifelse(grepl(going_out,transaction.description) & transaction_type == 'expense', 'going_out',
                                                                                                            ifelse(grepl(fashion,transaction.description) & transaction_type == 'expense', 'fashion',
                                                                                                                   ifelse(grepl(sport,transaction.description) & transaction_type == 'expense', 'sport',
                                                                                                                          ifelse(grepl(forniture,transaction.description) & transaction_type == 'expense', 'forniture',
                                                                                                                                 ifelse(grepl(salary,transaction.description) & transaction_type == 'revenue', 'salary',
                                                                                                                                        ifelse(grepl(interests_income,transaction.description) & transaction_type == 'revenue', 'interests_income',
                                                                                                                                               ifelse(grepl(extra_income,transaction.description) & transaction_type == 'revenue', 'extra_income', 'uncategorised')
                                                                                                                                        )
                                                                                                                                 )
                                                                                                                          )
                                                                                                                   )
                                                                                                            )
                                                                                                     )
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
    )
    ) %>%
    select(transaction.date, month, transaction.description, tag, transaction_type, transaction_amount)
  
  
  
  second_tab_plot <- reactive({
      # Plot Expenses and Revenues category breakdown
      ggplot(data = total_movements_tagged[total_movements_tagged$transaction_type == 'expense',],
                    aes(x = month, y = -1 * transaction_amount, fill = tag))+
      geom_bar(stat = 'identity')+
      theme(axis.text.x = element_text(angle = 45, hjust = 3),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
    })
  
  output$second_tab_plotly <- renderPlotly({
    ggplotly(second_tab_plot()) %>% 
      layout(autosize = F, width = 1200, height = 400,
             margin = list(b = 160))
  })
  
  # Tab 3 Savings over time -----
  
  # calculate Savings over time
  
  income_timeseries <- total_movements %>%
    select(month, transaction.date, debit.amount,credit.amount, transaction.type, transaction.description ) %>%
    mutate(credit.amount = replace(credit.amount,is.na(credit.amount),0))%>%
    mutate(debit.amount = replace(debit.amount,is.na(debit.amount),0))%>%
    group_by(month) %>%
    summarise(balance = sum(credit.amount - debit.amount))%>%
    bind_rows(initial_balance) %>%
    arrange(month)%>%
    mutate(balance = cumsum(balance))%>%
    select(month,balance)
  
  third_tab_plot <- reactive({
    # Plot Savings over time
    ggplot(data = income_timeseries, aes(x = month, y = balance, group = 1)) +
      geom_line(color = 'blue')+
      geom_area(fill = 'blue', alpha = .1)+
      theme(axis.text.x = element_text(angle = 45, hjust = 3),
            axis.title.x=element_blank())
  })
  
  output$third_tab_plotly <- renderPlotly({
    ggplotly(third_tab_plot()) %>% 
      layout(autosize = F, width = 1200, height = 400,
             margin = list(b = 160))
  })
  
})
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
                                   , selected = 'monthly'
                  ),
    dateRangeInput('date', 'select time interval',
                   start = Sys.Date() %m+% months(-6),
                   end = Sys.Date(),
                   min = '2012-01-01',
                   max = Sys.Date(),
                   width = 300
                   ),
    conditionalPanel("input.panel == 'Monthly View'",
                     checkboxInput('plot_income',
                                   'Plot Net Income',
                                   value = FALSE)
    )
  ),
    mainPanel(
      tabsetPanel(id = 'panel',
                  tabPanel('Monthly View',
                           br(),
                           plotlyOutput('first_tab_plotly'),
                           br(),
                           DT::dataTableOutput('first_tab_table', width = 1200)
                  ),
                  tabPanel('Expenses breakdown by category',
                           br(),
                           plotlyOutput('second_tab_plotly')
                  ),
                  tabPanel('Savings over time',
                           br(),
                           plotlyOutput('third_tab_plotly')
                  )
      )
      
      
    )
  )}


# App ------------------------------------------------
shinyApp(ui = ui, server = server)

