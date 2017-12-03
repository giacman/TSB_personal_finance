# TSB Finance Shiny Dashboard

R Shiny App to visualise and monitor personal finances using TSB account statements.

For each TSB account owned, statements have to be manually downloaded as csv file and put into a /statements/account_name folder under the root directory. This folder is added to the `.gitignore`.

Requirements: 

* `R` and required libraries (including `shiny`)
* `R-Studio`

The App has to be run from R-Studio. The Dashboard will open in a tab in your default Browser.

#### Directory layout

    ├── .gitignore
    ├── README.md
    ├── TSB_personal_finance.R # Shiny app file
    ├── statements # folder used to save account statements
        ├──tsb_classic_enhance  # subfolder with name of the account
        |   |-- 2017_1.csv # downloaded statement
        |   |-- 2017_2.csv
        |   |-- 2017_3.csv
        |-- tsb_classic_plus
            |-- 2017_1.csv
            |-- 2017_2.csv
            |-- 2017_3.csv
            
            
