library(ggplot2)
library(zoo)
library(dplyr)
library(lubridate)


# Data Preparation ----
path <- "/Users/giacomovannucchi/Projects/TSB_personal_finance/"

# classic account
path_classic = paste0(path,"statements/tsb_classic_enhance/")

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
path_plus = paste0(path,"statements/tsb_classic_plus/")

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

total_movements <- rbind(total_movements,m1,m2)

# Cleaning workspace
library(gdata)
keep(total_movements, sure = TRUE)

# 1) Analysis monthly average expenses and revenues ----

# Monthly Average Revenues (Removing first and last month to clean up data)
monthly_revenues <- total_movements %>%
  select(Transaction.Date, Credit.Amount,Transaction.Type, Transaction.Description ) %>%
  mutate(month = format(parse_date_time(total_movements$Transaction.Date, "dmy"), "%Y-%m")) %>%
  na.omit() %>%
  filter(Transaction.Type != 'TFR')%>%
  group_by(month) %>%
  summarise(monthly_revenues = sum(Credit.Amount))%>%
  filter(month != max(month) & month != min(month))

mean_monthly_revenues = mean(monthly_revenues$monthly_revenues)
mean_monthly_revenues

# Monthly Average Expenses (Removing first and last month to clean up data)
monthly_expenses <- total_movements %>%
  select(Transaction.Date, Debit.Amount,Transaction.Type, Transaction.Description ) %>%
  mutate(month = format(parse_date_time(total_movements$Transaction.Date, "dmy"), "%Y-%m")) %>%
  na.omit() %>%
  filter(Transaction.Type != 'TFR')%>%
  group_by(month) %>%
  summarise(monthly_expense = sum(Debit.Amount))%>%
  filter(month != max(month) & month != min(month))

mean_monthly_expenses = mean(monthly_expenses$monthly_expense)
mean_monthly_expenses

# Finally, how much are we saving on average per month?
savings = mean_monthly_revenues - mean_monthly_expenses
savings

# Plot monthly revenues vs monthly expenses
ggplot()+
  geom_point(data = monthly_revenues, aes(x = month, y = monthly_revenues))+
  geom_line(data = monthly_revenues, aes(x = month, y = monthly_revenues, group = 1), colour = 'blue')+
  geom_point(data = monthly_expenses, aes(x = month, y = monthly_expense))+
  geom_line(data = monthly_expenses, aes(x = month, y = monthly_expense, group = 1), colour = 'red')

# Net Income


# 2) Budget Analysis. Compare typical month expenses and google spreadsheet budget ----

# taking July 2017 as representative month
july_expenses <- total_movements %>%
  select(Transaction.Date, Debit.Amount,Transaction.Type, Transaction.Description ) %>%
  mutate(month = format(parse_date_time(total_movements$Transaction.Date, "dmy"), "%Y-%m")) %>%
  na.omit() %>%
  filter(Transaction.Type != 'TFR')%>%
  filter(month == '2017-07')

# Shelter (Rent, Mortgage, Council Tax, agency fees)
shelter_expenses <- july_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('CENTRAL|HOUSEKEEP',Transaction.Description))

shelter <- shelter_expenses %>%
  summarise(total = sum(Debit.Amount)) 

# Giving (Charities and Donations)
giving_expenses <- july_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('HELPINGRHI|WIKIMEDIAF|UNHCR',Transaction.Description))

giving <- giving_expenses %>%
  summarise(total = sum(Debit.Amount))

# Utilites
utilities_expenses <- july_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('BRGAS|VIRGIN',Transaction.Description))

utilities <- utilities_expenses %>%
  summarise(total = sum(Debit.Amount))

# Learning, personal growth
learning_expenses <- july_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('GITHUB|COURSERAIN|LINKEDIN',Transaction.Description))

learning <- learning_expenses %>%
  summarise(total = sum(Debit.Amount))

# Media Subscriptions
media_expenses <- july_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('NETFLIX|Spotify|Prime|SONY',Transaction.Description))

media <- media_expenses %>%
  summarise(total = sum(Debit.Amount))

# Transaportation (Commute + Taxis and extra journeys)
transport_expenses <- july_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('TFL|UBER',Transaction.Description))

transport <- transport_expenses %>%
  summarise(total = sum(Debit.Amount))

# Grocery
grocery_expenses <- july_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('Co-op|SUPERMARKET|OCADORETAI|SAINSBURYS',Transaction.Description))

grocery <- grocery_expenses %>%
  summarise(total = sum(Debit.Amount))

# Work Lunch
work_lunch_expenses <- july_expenses %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('HUSSEYS|CINNAMON|BOTTEGA|CAPTAIN|GASTRONOMICA',Transaction.Description))

work_lunch <- work_lunch_expenses %>%
  summarise(total = sum(Debit.Amount)) 

# Medical

# Insurance

# Uncathegorised / extra expenses 
extra_expenses <- july_expenses %>%
  anti_join(shelter_expenses) %>%
  anti_join(giving_expenses) %>%
  anti_join(utilities_expenses) %>%
  anti_join(learning_expenses) %>%
  anti_join(media_expenses) %>%
  anti_join(transport_expenses) %>%
  anti_join(grocery_expenses) %>%
  anti_join(work_lunch_expenses)

extra <- extra_expenses %>%
  summarise(total = sum(Debit.Amount))

# Budget expenditure items:

transport
media
learning
utilities
giving
shelter
grocery
work_lunch
extra

## break down income work, income other / rent, other expenses
july_income <- total_movements %>%
  select(Transaction.Date, Credit.Amount,Transaction.Type, Transaction.Description ) %>%
  mutate(month = format(parse_date_time(total_movements$Transaction.Date, "dmy"), "%Y-%m")) %>%
  na.omit() %>%
  filter(Transaction.Type != 'TFR')%>%
  filter(month == '2017-07')


# Work Lunch
work_income <- july_income %>%
  mutate(Transaction.Description = as.character(Transaction.Description))%>%
  filter(grepl('FLUBIT LIMITED',Transaction.Description))

work <- work_income %>%
  summarise(total = sum(Credit.Amount))

work

extra_income <- july_income %>%
  anti_join(work_income) %>%
  summarise(total = sum(Credit.Amount))

extra_income

## Net income, i.e. Profit (Loss)
net_income = sum(july_income$Credit.Amount) - sum(july_expenses$Debit.Amount)

net_income

## Capital growth and Targets
#capital = # get your savings here

#target = # whats your target

