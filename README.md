# TSB Finance Shiny Dashboard

Shiny App to visualise and monitor your personal finances using TSB Bank account statements. The Dashboard will open in your default Browser.


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