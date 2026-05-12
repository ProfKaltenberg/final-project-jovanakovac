What Determines Life Expectancy Across Countries?
Jovana Kovac

#packages
library(dplyr)
library(tidyverse)
library(estimatr)
library(stargazer)

#Setting working directory to where the csv file is saved
setwd("/Users/jovanakovac/Documents/Eco590/Data_Analysis_Python_R/final-project-jovanakovac")

world_data <- read.csv("world_data.csv")
options(scipen = 100)

```{r}
#Regression 1: Does log GDP per capita predict life expectancy
#Controlling for unemployment rate and health expenditure
#Log GDP is used because the relationship between income and life expectancy is not linear
ols <- lm(life_expectancy ~ log_gdp + unemployment_rate + health_expenditure, data = world_data)
summary(ols)

#Regression 2: Filtering for countries with above average GDP per capita
#Isolating wealthier countries to see if the relationship still holds within this group
world_high <- world_data[world_data$log_gdp > mean(world_data$log_gdp, na.rm = TRUE),]
ols_high <- lm(life_expectancy ~ log_gdp + unemployment_rate + health_expenditure, data = world_high)
summary(ols_high)

#Regression 3: Adding health expenditure squared to capture diminishing returns
#At very high spending levels, additional health expenditure may have less impact on life expectancy
world_data$health_exp_sq <- world_data$health_expenditure^2

ols_p <- lm(life_expectancy ~ log_gdp + unemployment_rate + health_expenditure + health_exp_sq, data = world_data)
summary(ols_p)

#Sisplaying all three regressions together
stargazer(ols, ols_high, ols_p, type="text",
          title = "Life Expectancy Regression Results",
          dep.var.labels = c("Life Expectancy", "Life Expectancy (High GDP)", "Life Expectancy"),
          covariate.labels = c("Log GDP per Capita", "Unemployment Rate", "Health Expenditure", "Health Expenditure Squared"))

#Summary

Regression 1 shows that holding unemployment rate and health expenditure constant, 
a one percent increase in GDP per capita is associated with 0.049 additional years 
of life expectancy. This result is statistically significant at the 1% level. 
Unemployment rate and health expenditure are not statistically significant.

Regression 2 shows that among high income countries, the effect of log GDP remains 
positive and significant but smaller. A one percent increase is associated with 0.032 
additional years. Unemployment rate becomes significant in this group.

Regression 3 is the preferred regression. A one percent increase in GDP per capita 
is associated with 0.046 additional years of life expectancy, holding all else constant. 
Neither health expenditure nor its squared term are statistically significant, suggesting 
spending alone does not predict life expectancy once GDP is accounted for.
```