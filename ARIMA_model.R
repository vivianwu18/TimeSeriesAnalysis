library(fpp3)
library(stats)
library(tsibbledata)
library(dplyr)


### The number of furs traded by the Hudson Bay Company between 1845 and 1935
# plot the data
pelt <- pelt
pelt %>%
  autoplot()

# fit ARIMA model and observe residuals
# Assume you decide to fit the ARIMA(4, 0, 0) model:
fit_pelt <- pelt %>%
  model(ARIMA(Hare ~ pdq(4, 0, 0)))

gg_tsdisplay(pelt, plot_type = "partial")

# produce forecasts
fit_pelt %>%
  forecast(h = 3)