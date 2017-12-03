# TSB Finance Shiny Dashboard

R Shiny App to visualise and monitor personal finances using TSB account statements.

For each TSB account owned, statements have to be manually downloaded as csv file and put into a /statements/account_name folder under the root directory. This folder is added to the .gitignore.

Requirements: 
* R and libraries (including shiny)
* R-Studio

The App has to be run from R-Studio. The Dashboard will open in a tab in your default Browser.


[ '|-- TSB_personal_finance',
  '    |-- .gitignore',
  '    |-- README.md',
  '    |-- TSB_personal_finance.R',
  '    |-- statements',
  '        |-- tsb_classic_enhance',
  '        |   |-- 2017_1.csv',
  '        |   |-- 2017_2.csv',
  '        |   |-- 2017_3.csv',
  '        |-- tsb_classic_plus',
  '            |-- 2017_1.csv',
  '            |-- 2017_2.csv',
  '            |-- 2017_3.csv',
  '' ]