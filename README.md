################################################################################
# Exploring-Relationship-Between-Greenhouse-Gas-Emissions-Economic-Indicators-in-Europe-Asia
################################################################################
This project explores the relationship between greenhouse gas emissions and economic indicators in Europe and Asia. The project includes an R script for analysing the data and a PowerBI file for visualising the results.

################################################################################
# Requirements
################################################################################
    R version 3.6.0 or higher
    PowerBI Desktop
################################################################################
# Installation
################################################################################
    Install R from https://cran.r-project.org/
    Install PowerBI Desktop from https://powerbi.microsoft.com/

################################################################################
# Data
################################################################################
The data for this analysis is obtained from [The World Bank DataBank World Development Indicators](https://databank.worldbank.org/source/world-development-indicators). To download the data, follow these steps:
    Go to the [The World Bank DataBank World Development Indicators](https://databank.worldbank.org/source/world-development-indicators) website.
    Select the required Countries ("Bangladesh", "China", "France", "Germany", "India", "Ireland", "Korea, Rep.", "Netherlands", "Norway", "Philippines", "Thailand", "United Kingdom"), Series ("GDP (Constant 2015 US$)", "Total greenhouse gas emissions (kt of CO2 equivalent)", "Population, Total"), and Time Period (1997 - 2018).
    Go to the layout tab and select custom orientation with {Series: Column, Country: Row, Time: Row}
    Next select Format Number sub-tab with {NA Prefrence: #N/A, Scale: Units, Precision: 0.00}
    Click on apply changes
    Click on the "Download Options" button to download the data in Excel format.
    Save the downloaded file to your desired location on your computer.
    Alternatively, the data is included "P_Data_Extract_From_World_Development_Indicators.xlsx".

################################################################################
# Usage
################################################################################
    Open the R script file main.R in RStudio.
    Install the required R packages by running the following code in the R console:
install.packages(c("dplyr", "ggplot2", "tidyverse", "readr", "readxl", "corrplot", "Kendall", "TTR", "car"))
    Set the working directory containing the downloaded dataset.
    Run the code in the R script to generate the analysis and save the results to a CSV file.
    Open the PowerBI file Final.pbix in PowerBI Desktop.
    The PowerBI file should now be populated with the analysis results and you can explore the visualizations.

################################################################################
# Contact
################################################################################
For any questions or issues, please contact y.panchal@edu.salford.ac.uk
