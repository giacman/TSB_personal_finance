library(ggplot2)
library(zoo)
library(dplyr)
library(lubridate)
library(rstudioapi)

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

total_movements <- rbind(total_movements,m1,m2)

# adding month variable
total_movements <- total_movements %>%
  mutate(month = format(parse_date_time(total_movements$Transaction.Date, "dmy"), "%Y-%m"))

# Removing first and last month to avoid incomplete data on months
current_month <- format(parse_date_time(Sys.Date(), "ymd"), "%Y-%m")

total_movements <- total_movements %>%
  filter(month != current_month & month != min(month))

# Excluding movements between accounts
total_movements <- total_movements[!grepl('G VANNUCCHI',total_movements$Transaction.Description),]

# Cleaning workspace
library(gdata)
keep(total_movements, sure = TRUE)

# 1) Monthly Average expenses and revenues ----

# Monthly Average Revenues
monthly_revenues <- total_movements %>%
  select(month, Transaction.Date, Credit.Amount,Transaction.Type, Transaction.Description ) %>%
  na.omit() %>%
  #filter(Transaction.Type != 'TFR')%>%
  group_by(month) %>%
  summarise(monthly_revenues = sum(Credit.Amount))

mean_monthly_revenues = mean(monthly_revenues$monthly_revenues)
monthly_revenues
mean_monthly_revenues

# Monthly Average Expenses 
monthly_expenses <- total_movements %>%
  select(month, Transaction.Date, Debit.Amount,Transaction.Type, Transaction.Description ) %>%
  na.omit() %>%
  #filter(Transaction.Type != 'TFR')%>%
  group_by(month) %>%
  summarise(monthly_expense = sum(Debit.Amount))

mean_monthly_expenses = mean(monthly_expenses$monthly_expense)
monthly_expenses
mean_monthly_expenses

# Plot monthly revenues vs monthly expenses
ggplot()+
  geom_point(data = monthly_revenues, aes(x = month, y = monthly_revenues))+
  geom_line(data = monthly_revenues, aes(x = month, y = monthly_revenues, group = 1), colour = 'blue')+
  geom_point(data = monthly_expenses, aes(x = month, y = monthly_expense))+
  geom_line(data = monthly_expenses, aes(x = month, y = monthly_expense, group = 1), colour = 'red')


# Net Income
monthly_net_income <- total_movements %>%
  select(month, Transaction.Date, Debit.Amount,Credit.Amount, Transaction.Type, Transaction.Description ) %>%
  mutate(Credit.Amount = replace(Credit.Amount,is.na(Credit.Amount),0))%>%
  mutate(Debit.Amount = replace(Debit.Amount,is.na(Debit.Amount),0))%>%
  #filter(Transaction.Type != 'TFR')%>%
  group_by(month) %>%
  summarise(monthly_net_income = sum(Credit.Amount - Debit.Amount))

# Finally, how much are we saving on average per month?
monthly_net_income
mean_monthly_net_income = mean(monthly_net_income$monthly_net_income)

# Plot Net Income
ggplot()+
  geom_point(data = monthly_net_income, aes(x = month, y = monthly_net_income))+
  geom_line(data = monthly_net_income, aes(x = month, y = monthly_net_income, group = 1))+
  geom_hline(yintercept=0, colour = 'red')


### TO DO: the two plots should become one split in two parts.


# 2) Select Month Detailed Analysis ----

# taking month August 2017 as representative month

selected_month = '2017-09'

# EXPENSES:
month_expenses <- total_movements %>%
  select(month, Transaction.Date, Debit.Amount,Transaction.Type, Transaction.Description ) %>%
  na.omit() %>%
  filter(month == selected_month)

# Shelter (Rent, Mortgage, Council Tax, agency fees)
shelter_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('CENTRAL|HOUSEKEEP|ANUTA DUNCA|L B WALTHAM FOREST 48507504N',Transaction.Description))

# Giving (Charities and Donations)
giving_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('HELPINGRHI|WIKIMEDIAF|UNHCR',Transaction.Description))

# Utilites
utilities_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('BRGAS|VIRGIN',Transaction.Description))

# Learning, personal growth
learning_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('GITHUB|COURSERAIN|LINKEDIN|LCODETHW',Transaction.Description))

# Media Subscriptions
media_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('NETFLIX|Spotify|Prime|SONY|ITUNES.COM/BILL',Transaction.Description))

# Transaportation (Commute + Taxis and extra journeys)
transport_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('TFL.GOV.UK|UBER',Transaction.Description))

# Grocery
grocery_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('Co-op|SUPERMARKET|OCADORETAI|SAINSBURYS',Transaction.Description))

# Work Lunch
work_lunch_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('HUSSEYS|CINNAMON|BOTTEGA|CAPTAIN|GASTRONOMICA|RIVER VIEW RESTAUR|PROSPECT OF WHITBY',
               Transaction.Description))

# Travel expenses
travel_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('GIANNELLIF|VISAXR|RIALTO',Transaction.Description))

# Travel tickets
travel_tickets <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('RYANAIR|EASYJET|CARHIRE|TRENITALIA',Transaction.Description))

# Eating out
eating_out <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('MARKSMAN|DINER|MAI SUSHI|PILGRIMS|EAT17',Transaction.Description))

# Fashion
fashion <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('COS|LEVI STRAUSS',Transaction.Description))

# Uncathegorised / extra expenses 
extra_expenses <- month_expenses %>%
  anti_join(shelter_expenses) %>%
  anti_join(giving_expenses) %>%
  anti_join(utilities_expenses) %>%
  anti_join(learning_expenses) %>%
  anti_join(media_expenses) %>%
  anti_join(transport_expenses) %>%
  anti_join(grocery_expenses) %>%
  anti_join(work_lunch_expenses) %>%
  anti_join(travel_tickets) %>%
  anti_join(eating_out) %>%
  anti_join(fashion) %>%
  anti_join(travel_expenses)

# Expenses Details and Totals:
expense_voices <- list(shelter_expenses, transport_expenses, media_expenses,
                       learning_expenses, utilities_expenses, giving_expenses,
                       shelter_expenses, grocery_expenses, work_lunch_expenses,
                       travel_tickets,travel_expenses, eating_out, fashion, extra_expenses)

expense_voices_names <- list('shelter_expenses', 'transport_expenses', 'media_expenses',
                            'learning_expenses', 'utilities_expenses', 'giving_expenses',
                            'shelter_expenses', 'grocery_expenses', 'work_lunch_expenses',
                            'travel_tickets','travel_expenses', 'eating_out', 'fashion','extra_expenses')

names(expense_voices) <- expense_voices_names

# Detail Function
calc_detail <- function(dt, type){
  if (type == 'expences'){
    detail <- dt %>%
      group_by(Transaction.Description) %>% 
      summarise(n = sum(Debit.Amount))
    return(detail)
  } else {
    detail <- dt %>%
      group_by(Transaction.Description) %>% 
      summarise(n = sum(Credit.Amount))
    return(detail)
  }
}
# Detail Total
calc_total <- function(dt, type){
  if (type == 'expences'){
    total <- dt %>%
      summarise(total = sum(Debit.Amount))
  return(total)
  } else {
    total <- dt %>%
      summarise(total = sum(Credit.Amount))
    return(total)
  }
}

expences_detail <- lapply(expense_voices, type = 'expences', calc_detail)
expences_total <- lapply(expense_voices, type = 'expences', calc_total)

for (voice in expense_voices_names) {
  print('############################')
  print(voice)
  print(expences_detail[[voice]])
  print(expences_total[[voice]])
}

# INCOME:
month_income <- total_movements %>%
  select(month, Transaction.Date, Credit.Amount,Transaction.Type, Transaction.Description ) %>%
  na.omit()%>%
  filter(month == selected_month)

# Work Income
work_income <- month_income %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('FLUBIT LIMITED',Transaction.Description))

# Work Income
interest_income <- month_income %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('INTEREST',Transaction.Description))

# Family Extra Income
family_extra_income <- month_income %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('VANNUCCHI P| CARANDINI S| BATTAGLINI',Transaction.Description))

# Extra Income
extra_income <- month_income %>%
  anti_join(work_income) %>%
  anti_join(interest_income) %>%
  anti_join(family_extra_income)

# Expenses Details and Totals:
income_voices <- list(work_income, interest_income, family_extra_income, extra_income)

income_voices_names <- list('work_income',  'interest_income', 'family_extra_income', 'extra_income')
names(income_voices) <- income_voices_names


income_detail <- lapply(income_voices, type = 'income', calc_detail)
income_total <- lapply(income_voices, type = 'income', calc_total)

for (voice in income_voices_names) {
  print('############################')
  print(voice)
  print(as.data.frame(income_detail[[voice]]))
  print(income_total[[voice]])
}

# 3) Expenses and Revenues with breakdown voices over months 

# attach a tag to every transaction based on logc above:

total_movements_tagged <- total_movements %>%
  mutate(transaction_amount = ifelse(!is.na(Debit.Amount), -1 * Debit.Amount,Credit.Amount )) %>%
  mutate(transaction_type = ifelse(!is.na(Debit.Amount), 'expense','revenue')) %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  mutate(tag = ifelse(grepl('CENTRAL|HOUSEKEEP|ANUTA DUNCA|L B WALTHAM FOREST 48507504N',Transaction.Description) & transaction_type == 'expense', 'shelter',
                                  ifelse(grepl('HELPINGRHI|WIKIMEDIAF|UNHCR',Transaction.Description) & transaction_type == 'expense', 'giving',
                                         ifelse(grepl('BRGAS|VIRGIN',Transaction.Description) & transaction_type == 'expense', 'utilities',
                                                ifelse(grepl('GITHUB|COURSERAIN|LINKEDIN|LCODETHW',Transaction.Description) & transaction_type == 'expense','learning',
                                                       ifelse(grepl('NETFLIX|Spotify|Prime|SONY|ITUNES.COM/BILL',Transaction.Description) & transaction_type == 'expense', 'media',
                                                              ifelse(grepl('TFL.GOV.UK|UBER',Transaction.Description) & transaction_type == 'expense','transport',
                                                                     ifelse(grepl('Co-op|SUPERMARKET|OCADORETAI|SAINSBURYS',Transaction.Description) & transaction_type == 'expense','grocery',
                                                                            ifelse(grepl('HUSSEYS|CINNAMON|BOTTEGA|CAPTAIN|GASTRONOMICA|RIVER VIEW RESTAUR|PROSPECT OF WHITBY',Transaction.Description) & transaction_type == 'expense', 'work_lumch',
                                                                                    ifelse(grepl('GIANNELLIF|VISAXR|RIALTO',Transaction.Description) & transaction_type == 'expense','travel_expenses',
                                                                                           ifelse(grepl('RYANAIR|EASYJET|CARHIRE|TRENITALIA',Transaction.Description) & transaction_type == 'expense', 'travel_tickets',
                                                                                                  ifelse(grepl('MARKSMAN|DINER|MAI SUSHI|PILGRIMS|EAT17',Transaction.Description) & transaction_type == 'expense', 'eating_out',
                                                                                                         ifelse(grepl('COS|LEVI STRAUSS',Transaction.Description) & transaction_type == 'expense', 'fashion',
                                                                                                                ifelse(grepl('FLUBIT LIMITED',Transaction.Description) & transaction_type == 'revenue', 'work_income',
                                                                                                                       ifelse(grepl('INTEREST',Transaction.Description) & transaction_type == 'revenue', 'interests_income',
                                                                                                                              ifelse(grepl('VANNUCCHI P| CARANDINI S| BATTAGLINI',Transaction.Description) & transaction_type == 'revenue', 'extra_income', 'uncategorised')
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
  select(Transaction.Date, month, Transaction.Description, tag, transaction_type, transaction_amount)

ggplot(data = total_movements_tagged[total_movements_tagged$transaction_type == 'expense',], 
       aes(x = month, y = -1 * transaction_amount, fill = tag))+
  geom_bar(stat = 'identity')

# 4) Savings and Targets

# Savings = # get your savings here
# Target = # whats your target
# Time Estimste to achieve target

