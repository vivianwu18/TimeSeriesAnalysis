library('USgas')

### US Gas Consumption
# read the data
df <- us_total %>% as_tsibble(index = year,key = state)
head(df)

# create time series plot
NewEngland_Gas <- df %>%
  select(year, state, y) %>%
  filter(state %in% c('Maine','Vermont','New Hampshire','Massachusetts','Connecticut','Rhode Island'))

# options(scipen = 999)
NewEngland_Gas %>%
  autoplot(y) +
  labs(title = 'Annual Natural Gas Consumption by State' ,
       subtitle = 'New England area',
       y = 'Gas Consumption',
       x = 'Year')
 
### Australia Gas Production
head(aus_production)

# create the basic time series plot
gas <- tail(aus_production, 5*4) %>% 
  select(Gas)

gas %>%
  autoplot(Gas)

gas %>%
  gg_season(Gas)

# classical multiplicative decomposition
gas_cmd <- gas %>%
  model(classical_decomposition(Gas, type = "multiplicative")) %>%
  components()

# create the plot
gas %>%
  model(classical_decomposition(Gas, type = "multiplicative")) %>%
  components() %>%
  autoplot() +
  labs(y = "Gas Production",
       x = "Quarter",
       title = "Classical multiplicative decomposition of gas production")

# calculate seasonal circle and trend circle
Seasonal_circle = mean(gas_cmd$seasonal)
Trend_circle = mean(gas_cmd$trend, na.rm = TRUE)
cat(" The calculated seasonality is ", Seasonal_circle, " and the trend is", Trend_circle)

# seasonally adjusted data
gas_cmd$season_adjust

# seasonally adjusted plot
gas_cmd %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Gas, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "",title = "Classical multiplicative decomposition of gas production") +
  scale_colour_manual(values = c("gray", "blue", "red"),
                      breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

# observe the effect of outliers
gas_copy <- gas
gas_copy$Gas[10] <- gas_copy$Gas[10]+300

gas_copy %>%
  model(classical_decomposition(Gas,type = "multiplicative")) %>%
  components() %>%
  autoplot() + 
  ggtitle("Classical multiplicative decomposition of gas production with outlier (middle) ")

gas_copy %>%
  model(classical_decomposition(Gas,type = "multiplicative")) %>%
  components() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Gas, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "", title = "Classical multiplicative decomposition of gas production with outlier (middle) ") +
  scale_colour_manual(values = c("gray", "blue", "red"),
                      breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

# observe the difference in the middle
gas_copy01 <- gas
gas_copy01$Gas[20] <- gas_copy01$Gas[20]+300

gas_copy01 %>%
  model(classical_decomposition(Gas,type = "multiplicative")) %>%
  components() %>%
  autoplot() + 
  ggtitle("Classical multiplicative decomposition of gas production with outlier (end)")

gas_copy01 %>%
  model(classical_decomposition(Gas,type = "multiplicative")) %>%
  components() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Gas, colour = "Data")) +
  geom_line(aes(y = season_adjust,colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "",title = "Classical multiplicative decomposition of gas production with outlier (end)") +
  scale_colour_manual(values = c("gray", "blue", "red"),
                      breaks = c("Data", "Seasonally Adjusted", "Trend")
  )