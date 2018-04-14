# TSB Finance Shiny Dashboard

A Shiny app to visualise and monitor your personal finances using TSB Bank statements. The dashboard opens in your default Browser and allows you to see Revenue and Expenses over time, Net Income, Cost category breakdown and Savings over time.

### Screenshots:

[![revenues_and_expenses.png](https://s18.postimg.cc/m658isvnt/revenues_and_expenses.png)](https://postimg.cc/image/9er2calvp/)

[![net_income.png](https://s18.postimg.cc/6xfb51hex/net_income.png)](https://postimg.cc/image/9rigihjl1/)


[![savings_over_time.png](https://s18.postimg.cc/imjat00nt/savings_over_time.png)](https://postimg.cc/image/bw2tjkdhx/)

[![expenses_breakdown.png](https://s18.postimg.cc/ongzq0i49/expenses_breakdown.png)](https://postimg.cc/image/91zo6265x/)

### Requirements: 

- Install `R`(https://cran.r-project.org/)
- Install `R-Studio`

### Setup:
- run `Rscript setup.R` to install libraries.
- Download your TSB bank statements as csv files and put them in `/statements/account_name` folder under the root directory.

### Run:
Open `TSB_personal_finance.R` in `R-Studio` and click `Run App`. The app will open in your default Browser.

#### Directory layout

    ├── .gitignore
    ├── README.md
    ├── TSB_personal_finance.R # Shiny app file
    ├── statements # folder used to save account statements
        ├──tsb_classic_enhance  # subfolder with name of the account
        |   |-- 2017_1.csv 
        |   |-- 2017_2.csv
        |   |-- 2017_3.csv
        |-- tsb_classic_plus
            |-- 2017_1.csv
            |-- 2017_2.csv
            |-- 2017_3.csv
            
            
### Limitation: 
- The App has to be run from R-Studio.
- account names are currently hardcoded.