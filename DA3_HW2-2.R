setwd("/Users/appleuser/Downloads")
data<-as.data.table(read.csv("~/Downloads/hotels_all_nov21.csv"))

## Download hotels_all_nov21.csv. Pick a city. 
## Consider hotels and hostels. Consider all with at least 2 stars. 

library(haven)
library(ggplot2)
library(data.table)
install.packages("segmented")
library(segmented)
install.packages('lspline')
library(lspline)
data<-as.data.table(read.csv("hotels_all_nov21.csv"))
head(data)
data <- data.table(data)
data[,.N, by=accommodation_type][order(N)]
install.packages("data.table")
str(data)
install.packages('wbstats')
library(wbstats)
install.packages('reshape2')
library(reshape2)
install.packages('stargazer')
library(stargazer)

# Pick Amsterdam 2*, hotels and hostels

dt_amsterdam_2star_hotelhostel <- data[city == "Amsterdam" & stars ==2 & accommodation_type %in% c("Hotel","Hostel")]
dt_amsterdam_2star_hotelhostel
data.table(dt_amsterdam_2star_hotelhostel)
str(dt_amsterdam_2star_hotelhostel)


## 1. Pick a set of variables. Describe all variables used in the analysis. 

# select multiple variables by name

dt_amsterdam_2star_hotelhostel[, c("distance", "price", "rating")]

# describe all variables used in the analysis

# distance: Continuous/quantitative variable -> Ratio -> no 0 variable
# price: Continuous/quantitative variable -> Ratio -> no 0 variable
# rating:Continuous/quantitative variable -> Interval -> Can be measured along a continuum and has a numerical value

## Simple descriptive stats and histograms of variables.

summary(dt_amsterdam_2star_hotelhostel$rating)
ggplot(dt_amsterdam_2star_hotelhostel, aes(rating)) + geom_histogram() + ggtitle("Rating - Hist") + theme(plot.title = element_text(hjust = 0.8))

summary(dt_amsterdam_2star_hotelhostel$distance)
ggplot(dt_amsterdam_2star_hotelhostel, aes(distance)) + geom_histogram() + ggtitle("Distance - Hist") + theme(plot.title = element_text(hjust = 0.8))

summary(dt_amsterdam_2star_hotelhostel$rating_reviewcount)
ggplot(dt_amsterdam_2star_hotelhostel, aes(price)) + geom_histogram() + ggtitle("Price - Hist") + theme(plot.title = element_text(hjust = 0.8))


### Plots for price(dependent) vs. given explanatory variable

ggplot(dt_amsterdam_2star_hotelhostel, aes(x = stars, y = price)) +
  geom_boxplot() +
  ggtitle("stars vs. price") +
  labs(x = "Number of stars", y = "Price in EUR") +
  theme_bw()

ggplot(dt_amsterdam_2star_hotelhostel, aes(x = rating, y = price)) + 
  ggtitle("rating vs. price") +
  geom_point(size = 2, shape = 5,colour = "green") +
  labs(x = "Rating", y = "Price in EUR") +
  theme_bw()

ggplot(dt_amsterdam_2star_hotelhostel, aes(x = distance, y = price)) + 
  ggtitle("distance vs. price") +
  geom_point(size = 2, shape = 5,colour = "green") +
  labs(x = "Distance to city center in km", y = "Price in EUR") +
  theme_bw()


## 2. Investigate potential nonlinearity of each explanatory variable in simple regressions of the dependent variable. 
## Decide on a parametric functional form for each. 


# In the following price will be taken as dependent variable

# Distance:

ggplot(dt_amsterdam_2star_hotelhostel, aes(x = distance, y = price)) + geom_point() + stat_smooth(method = 'loess', color = "green")

reg_poly_distance <- lm(formula = price ~ poly(distance, 3), data = dt_amsterdam_2star_hotelhostel)
summary(reg_poly_distance)
ggplot(dt_amsterdam_2star_hotelhostel, aes(x = distance, y = price)) + geom_point() + geom_smooth(method = 'lm', formula = y ~ poly(x, 3), color = "red")

# Stars:

ggplot(dt_amsterdam_2star_hotelhostel, aes(x = stars, y = price)) + geom_boxplot()
ggplot(dt_amsterdam_2star_hotelhostel, aes(x= as.factor(stars), y = price)) + geom_point() + geom_smooth(method = "lm", color = "purple")

# Rating:

ggplot(dt_amsterdam_2star_hotelhostel, aes(x = rating, y = price)) + geom_point() + stat_smooth(method = 'loess', color = "purple")

reg_poly_rating <- lm(formula = price ~ poly(rating, 3), data = dt_amsterdam_2star_hotelhostel)
summary(reg_poly_rating)
ggplot(dt_amsterdam_2star_hotelhostel, aes(x = rating, y = price)) + geom_point() + geom_smooth(method = 'lm', formula = y ~ poly(x, 3), color = "green")

Spmodel_rating <- lm(price ~ bs(rating,knots = c(4,4.3), degree = 1),data = dt_amsterdam_2star_hotelhostel)
summary(hotels_sp_model_rating)
ggplot(dt_amsterdam_2star_hotelhostel, aes(x = rating, y = price )) + geom_point() + geom_smooth(method = 'lm', formula = y ~ bs(x,knots = c(4,4.3), degree = 1), color = "yellow")

## 3. Estimate a multiple regression with all explanatory variables in the functional form you specified previously. 

## Multivariate Regression

multi_reg <- lm(price ~ lspline(distance, knots = 2) + stars + rating, data = dt_amsterdam_2star_hotelhostel)
multi_reg
multi_reg1 <- lm(price ~ lspline(distance, knots = 2) + stars + rating, data = dt_amsterdam_2star_hotelhostel)
multi_reg1
multi_reg2 <- lm(price ~ lspline(distance, knots = 2) + stars + rating2_ta, data = dt_amsterdam_2star_hotelhostel)
stargazer(multi_reg, multi_reg1, multi_reg2, type = "text")
multi_reg2

## 4. Pick two slope coefficients, interpret them, and compute and interpret their 95% CI. 

stargazer(multi_reg2, ci=T, type = "text")

## 5. Describe your strategy to find the best deal. 

## See PDF

## 6. List the hotels with the smallest (most negative) residuals. 
## List their prices and other characteristics as well. Comment on the results. 

dt_amsterdam_2star_hotelhostel[, predicted_price := (predict(multi_reg2))]
dt_amsterdam_2star_hotelhostel[, dif_price := price - predicted_price]
dt_amsterdam_2star_hotelhostel[, dif_price_percent := (price - predicted_price)/price]

# get top 10 best deals:
best_deals <- dt_amsterdam_2star_hotelhostel[order(dif_price)][1:10]
best_deals


