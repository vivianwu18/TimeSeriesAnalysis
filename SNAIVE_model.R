library(fpp3)
library(stats)
library(tsibbledata)
library(dplyr)

### Austalia Retail Turnover
# create time series plot
set.seed(1)
aus_retail <- aus_retail
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries_train <- myseries %>%
  filter(year(Month) < 2011)

autoplot(myseries, Turnover) +
  autolayer(myseries_train, Turnover, colour = "red")

# fit SNAIVE model and observe residuals
fit <- myseries_train %>%
  model(SNAIVE(Turnover))

fit %>% gg_tsresiduals() 

# produce forecasts and calculate accuracy
forecast <- fit %>%
  forecast(new_data = anti_join(myseries, myseries_train))
forecast %>% autoplot(myseries)

fit %>% accuracy()
forecast %>% accuracy(myseries)