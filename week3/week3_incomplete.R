library(fpp3)


# White noise and random walks ---------------

# White noise

set.seed(100)
T=100 # Change to 1000
my_data <- tsibble(t = seq(T), y = rnorm(T), index = t)
my_data |> autoplot(y)
my_data |> ACF(y) |> autoplot()
my_data |> gg_tsdisplay(y, plot_type = "histogram")


# Random walks

T=100 # Change to 1000
tsibble(t = seq(T), y = cumsum(rnorm(T)), index = t) |>
  gg_tsdisplay(y, plot_type = "histogram")


# Transformations

## us_economy ----------------------------------------------
us_economy <- global_economy |>
  filter(Country == "United States")

us_economy |>
  autoplot(GDP)

us_economy |>
  autoplot(log(GDP))
# log may seem too strong
# Hard to see the damping effect on variance due
# to the lack of noise

# Note: can also take the log-transformation
# using the Box-Cox function
us_economy |>
  autoplot(box_cox(GDP, 0))

# Let's try something weaker
us_economy |>
  autoplot(box_cox(GDP, 0.3))
# Seems to work fairly well

# Let's see what the Guerrero measure give us
us_economy |>
  features(GDP, features = guerrero) -> lambda
# Attempts to balance the seasonal fluctuations
# and random variation across the series.
# Always check the results.

lambda$lambda_guerrero

us_economy |>
  autoplot(box_cox(GDP, lambda$lambda_guerrero))
# More or less the same. Box-Cox transformations
# are usually insensitive to the choice of lambda
# at the second decimal place.

## aus_livestock ----------------------------------------------
aus_livestock |>
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") |>
  autoplot(Count)
# A bit tougher to see what is needed

# Over to you
# Try a log transformation
# What does Guerrero stat suggest?

aus_livestock |>
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") |>
  autoplot(log(Count))
# Yes may be it has helped

aus_livestock |>
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") |>
  features(Count, guerrero)
# suggesting a log so we might go with that



# 1.-----------------------------------------
# Find an appropriate Box-Cox transformation in order to stabilise the variance for
# Gas production from `aus_production`.


## aus_production ------------------------------------------------

aus_production |>
  autoplot(Gas)

# Hint: use log


# Hint: find and use Guerrero


# 2.-----------------------------------------
# Why is a Box-Cox transformation unhelpful for the `canadian_gas` data?

# Monthly Canadian gas production,
# billions of cubic metres, January 1960 - February 2005

# Hint: plot the data and try a transformation


# 3. ---------------------------------------------------------
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)
# A single series - thousands of people employed
# in Retail Trade in the US

# Aim: Can we pull out the trend/cycle and seasonal component
us_retail_employment |>
  autoplot(Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

## STL decomposition
## US retail employment

dcmp <- us_retail_employment |>
  model(stl = STL(Employed))
# STL
# Seasonal Trend decomposision
# using LOcal regrESSions

## a. Plot the decomposition.---------------------------------


## b. Fit the trend/cycle over the data ---------------------------
us_retail_employment |>
  autoplot(Employed, color='gray') +
  autolayer(components(dcmp), trend, color='#D55E00') +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

## c. Fit the trend and the seasonally adjusted --------------
# Hint: use another autolayer


## d. How does the seasonal shape change over time? --------------

# Hint: use gg_subseries()

# Let's explore more What happens with periodic
us_retail_employment |>
  model(stl = STL(Employed~season(window="periodic"))) |>
  components() |>
  autoplot()

## e. What happens as you change the values of the two `window` arguments? -------------


## f. Can you produce a plausible seasonally adjusted series? -------------
