library(fpp3)

# Workshop Activity 1 -----------------------------------------
## Use the tsibble created from `tourism` 
## for holiday travel in Victoria and Queensland. 

## a. Plot the series to remind yourself what these look like. 

holidays <- tourism |>
  filter(State %in% c("Victoria", "Queensland")) |> 
  filter(Purpose == "Holiday") |>
  as_tibble() |>
  summarise(Trips = sum(Trips), .by = c("State", "Quarter")) |>
  as_tsibble(index = Quarter, key = State)

holidays |> autoplot()

## b. Use the `ETS()` function to fit models to both series.
## Explore the resulting `mable` using `report()`, `glance()` 
## and `tidy()`

fit <- holidays |>
  model(ets = ETS(Trips))

fit
  # Two very different models fitted
  # Notice how quickly that happened
  # We will learn after the break how

fit |>
  report()
  # Why the warning message?
  
  # Hence the above has reverted to 
fit |> glance() 
  # glance() reports something very different to report()

  # let's try report for each model
fit |>
  filter(State == "Queensland") |>
  report() 

fit |>
  filter(State == "Victoria") |>
  report() 

  # Another useful function
fit |> tidy()
  # Notice the parameters estimated


## c. Plot the estimated components of each model.

fit |>
  filter(State == "Queensland") |>
  components() |>
  autoplot() + 
  labs(subtitle="")

fit |>
  filter(State == "Victoria") |>
  components() |>
  autoplot()

## d. Generate forecasts using `forecast()`.

fit |>
  forecast() |>
  autoplot(holidays) + #, show_gap = FALSE
  labs(y="Overnight trips (thousands)")


# Workshop Activity 2 -----------------------------------------
## Use the exports data exports data for Algeria from the `global_economy` tsibble.

## a. Plot the data. Is this time series white noise?
## What ETS model would be appropriate?

algeria_economy <- global_economy  |> 
  filter(Country == "Algeria")

algeria_economy |> autoplot() + geom_point() #Be careful at what you are plotting

algeria_economy |> autoplot(Exports) + geom_point()

algeria_economy |> ACF(Exports) |> autoplot()

## b. Use the `ETS()` function to fit appropriate models
## with both additive and multiplicative errors. What model
## is chosen automatically? Explore the estimated models.

fit <- algeria_economy |>
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    MNN = ETS(Exports ~ error("M") + trend("N") + season("N")),
    autoNN = ETS(Exports ~ trend("N") + season("N")), #it will choose error("")
    auto = ETS(Exports), #it will choose everything
  )
  
fit
  # an MNN is automatically chosen

fit |> select(ANN) |> report()
fit |> select(MNN) |> report()
  # Notice the different parameters and sigma

fit |> tidy()
fit |> glance() 


# You can do various overrides 
algeria_economy |>
  model(
    ANN = ETS(Exports ~ error("A") + trend("N", alpha=0.2) 
              + season("N"))
  ) |> 
  report()

algeria_economy |>
  model(
    ANN = ETS(Exports ~ error("A") + trend("N", alpha_range=c(0.2, 0.7)) 
              + season("N"))
  ) |> 
  report()
  # Normally we leave these alone

# Drop the automated
fit <- fit |>
  select(Country, ANN, MNN)

fit |> tidy()

## c. Plot the components of the two models. What do you see?

fit |>
  components() 

fit |>
  components() |> 
  autoplot()

fit |>
  select(MNN) |> 
  components() |> 
  autoplot()

  # Join in the fitted values
fit |> 
  select(Country, ANN) |> 
  components() |>
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

fit |> 
  select(Country, MNN) |> 
  components() |>
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

## d. Explore the residuals of the two models. What do you see? 

fit |> 
  select(ANN) |> 
  augment()

fit |> 
  select(MNN) |> 
  augment()
  # Notice the difference in .innov and .resid
  # Why the difference?

fit |> 
  select(ANN) |> 
  gg_tsresiduals()

fit |> 
  select(MNN) |> 
  gg_tsresiduals()

# e. Generate and plot forecasts.

  # Recall point forecasts are the mean of all possible futures
  # i.e., expected values. 
  # Notice the point forecasts are flat. 
fit |>
  forecast(h = 5) |>
  autoplot(algeria_economy) +
  labs(y= "Exports (% of GDP)")

fit |>
  select(Country, ANN) |> 
  forecast(h = 5) |>
  autoplot(algeria_economy) +
  labs(y="Exports (% of GDP)")

  # equivalently
fit |>
  forecast(h = 5) |>
  filter(.model=="MNN") |> 
  autoplot(algeria_economy) +
  ylab("Exports (% of GDP)") + xlab("Year")

# Workshop Activity 3 -----------------------------------------
## Use the population data for Australia from the `global_economy` tsibble.

## a. Scale the data to be in millions and plot it. 


aus_economy <- global_economy |> 
  filter(Code == "AUS")  |> 
  mutate(Pop = Population/1e6)

aus_economy |> 
  autoplot(Pop)
  # Looks like linear trend

## b. Fit both a model with a linear and a damped trend 
## and study the estimated parameters. 

fit <- aus_economy |> 
  model(
    holt = ETS(Pop ~ error("A") + trend("A") + season("N")),
    damped = ETS(Pop ~ error("A") + trend("Ad") + season("N")),
    autoETS = ETS(Pop)
    )

fit

fit |> 
  select(-autoETS) |> 
  tidy()
  
  # Notice ETS selects linear trend
  # Comment on phi=0.98

## c. Generate forecasts for 30 years ahead from both 
## models and plot them. What do you see? 

fit |> 
   select(-autoETS) |> 
   forecast(h = 30) |> 
   autoplot(aus_economy, level=NULL)

## d. Fit both models using data up to 2010. Generate forecast 
## for the remainder of the sample and evaluate their accuracy.


fit <-   aus_economy |> 
  filter(Year<=2010) |> 
  model(
    holt = ETS(Pop ~ error("A") + trend("A") + season("N")),
    damped = ETS(Pop ~ error("A") + trend("Ad") + season("N"))
    )

fit |> 
  forecast(h=7) |> 
  autoplot(aus_economy,level=NULL)

fit |> 
  forecast(h=7) |> 
  autoplot(aus_economy |> filter(Year>=2005),level=NULL)


fit |> 
  accuracy() |> 
  arrange(RMSSE) |> 
  select(-ME,-MAE,-MPE, -ACF1)

fit |> 
  forecast(h=7) |> 
  accuracy(aus_economy)|> 
  arrange(RMSSE)|> 
  select(-ME,-MAE,-MPE, -ACF1)


# fpp3 8.8, Ex6

# Forecast the Chinese GDP from the `global_economy` data set using an ETS model. 
# Experiment with the various options in the `ETS()` function to see how much the 
# forecasts change with damped trend, or with a Box-Cox transformation. Try to 
# develop an intuition of what each is doing to the forecasts.

# Hint: use `h=20` when forecasting, so you can clearly see the differences 
# between the various options when plotting the forecasts.

china <- global_economy |>
  filter(Country == "China")

china |> autoplot(GDP)

  # It clearly needs a relatively strong transformation due to the increasing variance.

china |> autoplot(box_cox(GDP, 0.2))
china |> features(GDP, guerrero)

  # Making lambda=0.2 looks ok.
  # The Guerrero method suggests an even stronger transformation. 
  # Let's also try a log.
china |> autoplot(box_cox(GDP, 0))

fit <- china |>
  model(
    ets = ETS(GDP),
    ets_damped = ETS(GDP ~ trend("Ad")),
    ets_bc = ETS(box_cox(GDP, 0.2)),
    ets_log = ETS(log(GDP))
  )

fit

augment(fit)

fit |>
  forecast(h = "4 years") |>
  autoplot(china, level = NULL)

  # The transformations have a big effect, with small lambda values 
  # creating big increases in the forecasts. 
  
