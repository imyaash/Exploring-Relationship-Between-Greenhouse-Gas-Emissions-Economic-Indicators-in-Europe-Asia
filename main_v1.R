# Set the working directory

# Load libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(readxl)
library(corrplot)
library(Kendall)
library(TTR)
library(car)

# Set options for the scientific notation
options(scipen = 999)

# Read in the data from the Excel file
df <- read_excel("P_Data_Extract_From_World_Development_Indicators.xlsx")
df <- head(df, -5)
df <- data.frame(Year = df$Time, Country = df$`Country Name`, GDP = df$`GDP (constant 2015 US$) [NY.GDP.MKTP.KD]`, GHGE = df$`Total greenhouse gas emissions (kt of CO2 equivalent) [EN.ATM.GHGT.KT.CE]`, Population = df$`Population, total [SP.POP.TOTL]`)

# Check the structure of the data
str(df)
# Convert the "Year" and "Country" columns to factors
df$Year <- as.factor(df$Year)
df$Country <- as.factor(df$Country)
str(df)

# Check for missing values
sum(is.na(df))

# Compute the correlations among the columns
corr <- cor(df[, 3:5])
# Plot the correlations
corrplot(corr, type = "upper", order = "hclust")

# Create a new data frame with the mean and median values for each column
mctDF <- data.frame(Indicators = colnames(df[, 3:5]), Mean = apply(df[, 3:5], 2, mean), Median = apply(df[, 3:5], 2, median))
# Print the new data frame
print(mctDF)


# Define a function to calculate the skewness of a vector
skew <- function(x){
  # Calculate the third moment of the vector
  m3 <- mean((x - mean(x)) ^ 3)
  # Divide the third moment by the cube of the standard deviation to compute the skewness
  skew <- m3 / (sd(x) ^ 3)
}

# Define a function to calculate the kurtosis of a vector
kurtosis <- function(x){
  # Calculate the fourth moment of the vector
  m4 <- mean((x - mean(x)) ^ 4)
  # Divide the fourth moment by the fourth power of the standard deviation and subtract 3 to compute the kurtosis
  kurtosis <- m4 / (sd(x) ^ 4) - 3
}

# Use dplyr to group the data by country and calculate the statistics for each group
statistics <- df %>%
  group_by(Country) %>%
  summarize_at(vars(2:4), funs(mean, median, sd, var, skew, kurtosis))
statistics

# Create a scatter plot to visualize the relationship between GHGE and Year
ggplot(data = df, aes(x = Year, y = GHGE)) + 
  geom_point() + 
  labs(title = "Year & GHGE")
# Create a scatter plot to visualize the relationship between GHGE and GDP
ggplot(data = df, aes(x = GDP, y = GHGE)) + 
  geom_point() + 
  labs(title = "GDP & GHGE")
# Create a scatter plot to visualize the relationship between GHGE and Population
ggplot(data = df, aes(x = Population, y = GHGE)) + 
  geom_point() + 
  labs(title = "Population & GHGE")

# Fit a linear regression model to investigate the relationship between GHGE and GDP
lmGDP = lm(df$GHGE ~ df$GDP)
# Print the model
lmGDP
# Print a summary of the model
summary(lmGDP)
# Create a scatter plot to visualize the relationship between GHGE and GDP
plot(df$GDP, df$GHGE)
# Add the fitted regression line to the scatter plot
abline(lmGDP)

# Fit a linear regression model to investigate the relationship between GHGE and Population
lmPop = lm(df$GHGE ~ df$Population)
# Print the model
lmPop
# Print a summary of the model
summary(lmPop)
# Create a scatter plot to visualize the relationship between GHGE and Population
plot(df$Population, df$GHGE)
# Add the fitted regression line to the scatter plot
abline(lmPop)

# Fit a multiple linear regression model to investigate the relationship between GHGE and GDP and Population
multiLM = lm(df$GHGE ~ df$Population + df$GDP)
# Print the model
multiLM
# Print a summary of the model
summary(multiLM)
# Create diagnostic plots for the model
avPlots(multiLM)

# Filter the data to include only observations for India
india <- df %>% dplyr::filter(Country == "India") %>% dplyr::select("Year", "GHGE", "GDP", "Population")
# Fit a multiple linear regression model for India
lmIndia = lm(india$GHGE ~ india$Population + india$GDP)
# Print the model
lmIndia
# Print a summary of the model
summary(lmIndia)
# Create diagnostic plots for the model
avPlots(lmIndia)

# Filter the data to include only observations for China
china <- df %>% dplyr::filter(Country == "China") %>% dplyr::select("Year", "GHGE", "GDP", "Population")
# Fit a multiple linear regression model for China
lmChina = lm(china$GHGE ~ china$Population + china$GDP)
# Print the model
lmChina
# Print a summary of the model
summary(lmChina)
# Create diagnostic plots for the model
avPlots(lmChina)

# Filter the data to include only observations for Germany
germany <- df %>% dplyr::filter(Country == "Germany") %>% dplyr::select("Year", "GHGE", "GDP", "Population")
# Fit a multiple linear regression model for Germany
lmGermany = lm(germany$GHGE ~ germany$Population + germany$GDP)
# Print the model
lmGermany
# Print a summary of the model
summary(lmGermany)
# Create diagnostic plots for the model
avPlots(lmGermany)

# Filter the data to include only observations for the United Kingdom
uk <- df %>% dplyr::filter(Country == "United Kingdom") %>% dplyr::select("Year", "GHGE", "GDP", "Population")
# Fit a multiple linear regression model for the United Kingdom
lmUK = lm(uk$GHGE ~ uk$Population + uk$GDP)
# Print the model
lmUK
# Print a summary of the model
summary(lmUK)
# Create diagnostic plots for the model
avPlots(lmUK)

# Create a time series object using the GHGE column from the data frame
dfTS <- df %>%
  group_by(Year) %>%
  summarise_at(vars(3), funs(sum))

timeSeries <- ts(dfTS$GHGE)
timeSeries
# Plot the time series data
plot.ts(timeSeries)
# Calculate the simple moving average of the time series data
TSComponents <- SMA(timeSeries)
# Plot the moving average data
plot.ts(TSComponents)
# Fit a Holt-Winters model to the time series data, with beta and gamma set to false
TSForecast <- HoltWinters(timeSeries, beta = F, gamma = F)
TSForecast
# Extract the fitted values from the forecast object
TSForecast$fitted
# Plot the fitted values
plot(TSForecast)

indiaDF <- india %>%
  group_by(Year) %>%
  summarise_at(vars(1), funs(sum))
chinaDF <- china %>%
  group_by(Year) %>%
  summarise_at(vars(1), funs(sum))
germanyDF <- germany %>%
  group_by(Year) %>%
  summarise_at(vars(1), funs(sum))
ukDF <- uk %>%
  group_by(Year) %>%
  summarise_at(vars(1), funs(sum))

# Create a time series object using the GHGE column from the india data frame
indiaTimeSeries <- ts(indiaDF$GHGE)
indiaTimeSeries
# Plot the time series data
plot.ts(indiaTimeSeries)
# Calculate the simple moving average of the time series data
indiaTSComponents <- SMA(indiaTimeSeries)
# Plot the moving average data
plot.ts(indiaTSComponents)
# Fit a Holt-Winters model to the time series data, with beta and gamma set to false
indiaTSForecast <- HoltWinters(indiaTimeSeries, beta = F, gamma = F)
indiaTSForecast
# Extract the fitted values from the forecast object
indiaTSForecast$fitted
# Plot the fitted values
plot(indiaTSForecast)

# Create a time series object using the GHGE column from the china data frame
chinaTimeSeries <- ts(chinaDF$GHGE)
chinaTimeSeries
# Plot the time series data
plot.ts(chinaTimeSeries)
# Calculate the simple moving average of the time series data
chinaTSComponents <- SMA(chinaTimeSeries)
# Plot the moving average data
plot.ts(chinaTSComponents)
# Fit a Holt-Winters model to the time series data, with beta and gamma set to false
chinaTSForecast <- HoltWinters(chinaTimeSeries, beta = F, gamma = F)
chinaTSForecast
# Extract the fitted values from the forecast object
chinaTSForecast$fitted
# Plot the fitted values
plot(chinaTSForecast)

# Create a time series object using the GHGE column from the germany data frame
germanyTimeSeries <- ts(germanyDF$GHGE)
germanyTimeSeries
# Plot the time series data
plot.ts(germanyTimeSeries)
# Calculate the simple moving average of the time series data
germanyTSComponents <- SMA(germanyTimeSeries)
# Plot the moving average data
plot.ts(germanyTSComponents)
# Fit a Holt-Winters model to the time series data, with beta and gamma set to false
germanyTSForecast <- HoltWinters(germanyTimeSeries, beta = F, gamma = F)
germanyTSForecast
# Extract the fitted values from the forecast object
germanyTSForecast$fitted
# Plot the fitted values
plot(germanyTSForecast)

# Create a time series object using the GHGE column from the uk data frame
ukTimeSeries <- ts(ukDF$GHGE)
ukTimeSeries
# Plot the time series data
plot.ts(ukTimeSeries)
# Calculate the simple moving average of the time series data
ukTSComponents <- SMA(ukTimeSeries)
# Plot the moving average data
plot.ts(ukTSComponents)
# Fit a Holt-Winters model to the time series data, with beta and gamma set to false
ukTSForecast <- HoltWinters(ukTimeSeries, beta = F, gamma = F)
ukTSForecast
# Extract the fitted values from the forecast object
ukTSForecast$fitted
# Plot the fitted values
plot(ukTSForecast)

# Create vectors containing the names of countries in Europe and Asia
eu <- c("Germany", "France", "United Kingdom", "Ireland", "Norway", "Netherlands")
as <- c("India", "China", "Bangladesh", "Korea, Rep.", "Philippines", "Thailand")
# Subset the data frame to only include data for the countries in the Europe and Asia vectors
Eu <- df[df$Country %in% eu, ]
Asia <- df[df$Country %in% as, ]

euDF <- Eu %>%
  group_by(Year) %>%
  summarise_at(vars(3), funs(sum))
asiaDF <- Asia %>%
  group_by(Year) %>%
  summarise_at(vars(3), funs(sum))

# Perform a Mann-Kendall trend test on the GHGE column of the Asia and Eu data frames
mkTestAsia <- MannKendall(asiaDF$GHGE)
mkTestEu <- MannKendall(euDF$GHGE)
# Print the results of the trend tests to the console
mkTestAsia
mkTestEu

# Perform the trend test on individual countries
mkTestIndia <- MannKendall(indiaDF$GHGE)
mkTestChina <- MannKendall(chinaDF$GHGE)
mkTestGermany <- MannKendall(germanyDF$GHGE)
mkTestUK <- MannKendall(ukDF$GHGE)
# Print the results of the individual country trend tests to the console
mkTestIndia
mkTestChina
mkTestGermany
mkTestUK

# Create a new column called DevStatus in the df data frame
# This column will contain the values "Developed" or "Developing" depending on the GDP per capita of the country
df$DevStatus <- ifelse((df$GDP / df$Population) > 25000, "Developed", "Developing")
# Create a new column called Continent in the df data frame
# This column will contain the values "Asia" or "Europe" on the country
df$Continent <- ifelse(df$Country == "France", "Europe", ifelse(df$Country == "Germany", "Europe", ifelse(df$Country == "Ireland", "Europe", ifelse(df$Country == "Netherlands", "Europe", ifelse(df$Country == "Norway", "Europe", ifelse(df$Country == "United Kingdom", "Europe", "Asia"))))))

# Perform a Wilcoxon rank-sum test on the GHGE column of the df data frame
# Use the DevStatus column as the grouping variable
wilcox.test(df$GHGE ~ df$DevStatus)

# Perform a Wilcoxon rank-sum test on the GHGE column of the df data frame
# Use the Continent column as the grouping variable
wilcox.test(df$GHGE ~ df$Continent)
