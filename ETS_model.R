library(fpp3)
library(stats)
library(tsibbledata)
library(dplyr)

### Australia Gas Production
# plot the data
aus_production <- aus_production 

aus_production %>%
  autoplot(Gas)

# fit the multiple ETS models
fit_gas <- aus_production %>%
  model(
    add = ETS(Gas ~ error("A") + trend("A") + season("N")),
    mult = ETS(Gas ~ error("M") + trend("A") + season("N")),
    add_with_sea = ETS(Gas ~ error("M") + trend("A") + season("A")), 
    mult_with_sea = ETS(Gas ~ error("M") + trend("A") + season("M")),
    mult_with_sea_damp = ETS(Gas ~ error("M") + trend("Ad") + season("M"))
  )
I
# make an forecast for five years
forecast_gas <- fit_gas %>% 
  forecast(h = 20) 

# plot the forecast
forecast_gas %>%
  autoplot(aus_production, level = NULL) %>%
  labs(title = "Gas Production in Australia")