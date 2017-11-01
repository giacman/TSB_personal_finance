library(ggplot2)
library(zoo)
library(dplyr)
library(lubridate)

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

# ### this is the initial amount transfered at beginning of period of analysis
# initial_amount <-sum(total_movements[grepl('G VANNUCCHI',total_movements$Transaction.Description),]$Credit.Amount, na.rm = TRUE) -
#   sum(total_movements[grepl('G VANNUCCHI',total_movements$Transaction.Description),]$Debit.Amount, na.rm = TRUE)
# initial_row <- total_movements[grepl('G VANNUCCHI',total_movements$Transaction.Description),][1,]

# Cleaning workspace
library(gdata)
keep(total_movements, sure = TRUE)

# 1) Monthly Average expenses and revenues ----

# Monthly Average Revenues
monthly_revenues <- total_movements %>%
  select(month, Transaction.Date, Credit.Amount,Transaction.Type, Transaction.Description ) %>%
  na.omit() %>%
  filter(Transaction.Type != 'TFR')%>%
  group_by(month) %>%
  summarise(monthly_revenues = sum(Credit.Amount))

mean_monthly_revenues = mean(monthly_revenues$monthly_revenues)
monthly_revenues
mean_monthly_revenues

# Monthly Average Expenses 
monthly_expenses <- total_movements %>%
  select(month, Transaction.Date, Debit.Amount,Transaction.Type, Transaction.Description ) %>%
  na.omit() %>%
  filter(Transaction.Type != 'TFR')%>%
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
  filter(Transaction.Type != 'TFR')%>%
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


# 2) Select Month Detailed Analysis ----

# taking month August 2017 as representative month

test_month = '2017-10'

month_expenses <- total_movements %>%
  select(Transaction.Date, Debit.Amount,Transaction.Type, Transaction.Description ) %>%
  mutate(month = format(parse_date_time(total_movements$Transaction.Date, "dmy"), "%Y-%m")) %>%
  na.omit() %>%
  filter(Transaction.Type != 'TFR')%>%
  filter(month == test_month)

# Shelter (Rent, Mortgage, Council Tax, agency fees)
shelter_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('CENTRAL|HOUSEKEEP|ANUTA DUNCA|L B WALTHAM FOREST 48507504N',Transaction.Description))

# Giving (Charities and Donations)
giving_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('HELPINGRHI|WIKIMEDIAF|UNHCR',Transaction.Description))

giving <- giving_expenses %>%
  summarise(total = sum(Debit.Amount))

# Utilites
utilities_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('BRGAS|VIRGIN',Transaction.Description))

utilities <- utilities_expenses %>%
  summarise(total = sum(Debit.Amount))

# Learning, personal growth
learning_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('GITHUB|COURSERAIN|LINKEDIN',Transaction.Description))

learning <- learning_expenses %>%
  summarise(total = sum(Debit.Amount))

# Media Subscriptions
media_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('NETFLIX|Spotify|Prime|SONY|ITUNES.COM/BILL',Transaction.Description))

media <- media_expenses %>%
  summarise(total = sum(Debit.Amount))

# Transaportation (Commute + Taxis and extra journeys)
transport_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('TFL|UBER',Transaction.Description))

# Grocery
grocery_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('Co-op|SUPERMARKET|OCADORETAI|SAINSBURYS',Transaction.Description))

grocery <- grocery_expenses %>%
  summarise(total = sum(Debit.Amount))

# Work Lunch
work_lunch_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('HUSSEYS|CINNAMON|BOTTEGA|CAPTAIN|GASTRONOMICA|RIVER VIEW RESTAUR',Transaction.Description))

work_lunch <- work_lunch_expenses %>%
  summarise(total = sum(Debit.Amount)) 

# Travel
travel_expenses <- month_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('GIANNELLIF|RYANAIR|EASYJET|CARHIRE|TRENITALIA',Transaction.Description))

travel <- travel_expenses %>%
  summarise(total = sum(Debit.Amount)) 



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
  anti_join(travel_expenses)

extra <- extra_expenses %>%
  summarise(total = sum(Debit.Amount))

# Budget expenditure items:

transport_detail <- transport_expenses %>% 
  group_by(Transaction.Description) %>% 
  summarise(n = sum(Debit.Amount))

transport_total <- transport_expenses %>%
  summarise(total = sum(Debit.Amount))

transport_detail
transport_total

media
learning
utilities
giving
shelter

shelter <- shelter_expenses %>%
  summarise(total = sum(Debit.Amount)) 

grocery
work_lunch
extra
travel

## break down income work, income other / rent, other expenses
month_income <- total_movements %>%
  select(Transaction.Date, Credit.Amount,Transaction.Type, Transaction.Description ) %>%
  mutate(month = format(parse_date_time(total_movements$Transaction.Date, "dmy"), "%Y-%m")) %>%
  na.omit() %>%
  filter(Transaction.Type != 'TFR')%>%
  filter(month == test_month)


# Work Income
work_income <- month_income %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('FLUBIT LIMITED',Transaction.Description))

work <- work_income %>%
  summarise(total = sum(Credit.Amount))

work

extra_income <- month_income %>%
  anti_join(work_income) %>%
  summarise(total = sum(Credit.Amount))

extra_income

## Net income, i.e. Profit (Loss)
net_income = sum(month_income$Credit.Amount) - sum(month_expenses$Debit.Amount)
net_income


## We should see these expnses for every month, and plot themover time 

## Capital growth and Targets
#capital = # get your savings here

#target = # whats your target


