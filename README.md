# Competition-Shiny
A test of Shiny for DoD Competition data - project for DIIG and Coursera Data Products class May 2016

Both UI and Server functions are contained in app.R

app.R will read competition data from CSV file and:
  * Calculate DoD spending as percentage of state GDP
  * Subset data into several dataframes for quick use by the plot function
  * Create a plot with user choice of:
    - three different variables to display (Spending, Spending as % of GDP, Competition rate)
    - three subsets of contract data (Products, Services, R&D)
    - year of data displayed (2000-2014)

Additional details about function can be found in comments of app.R
