library(fpp3)

# Explore some rich tsibbles ----------------------------------------


## Olympics ---------------------------------------------------------
olympic_running
olympic_running |> distinct(Length)
olympic_running$Year |> range()

olympic_running |>
  filter(Length=="100") |>
  autoplot(Time)

# Just to show you what you can do
olympic_running |>
  ggplot(aes(x=Year, y = Time, colour = Sex)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~ Length, scales = "free_y", nrow = 2) +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "Olympic running times",
       y="Seconds")

## PBS --------------------------------------------------------------

  # Let's sum Cost of A10 across Concession and Type, i.e., total A10 cost
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6) -> a10

a10

# Time Series patterns ----------------------------------------

a10 |>
   autoplot() #+ #Cost
   # labs(
   #   title = "Australian antidiabetic drug sales",
   #   y = "$ (millions)"
   #   )
   # Note - it will pick the first variable it sees.
   # Hence select appropriate variable


  # I could do it with ggplot()
a10 |>
   ggplot(aes(x=Month, y=Cost)) +
   geom_line() +
   labs(
      title = "Australian antidiabetic drug sales",
      y = "Cost in $ (millions)"
      )

a10 |>
  autoplot(Cost) +
  geom_point() +
  labs(
    title = "Australian antidiabetic drug sales - Cost",
    y = "Cost in $ (millions)"
    )
  # What patterns do we see?

aus_production |>
  filter(year(Quarter) >= 1980) |>
  autoplot(Electricity) +
  geom_point() +
  labs(
    title = "Australian electricity production",
    y = "GWh"
    )

us_employment |>
  filter(Title == "Retail Trade", year(Month) >= 1980) |>
  autoplot(Employed / 1e3) +
  labs(
    y = "Million people",
    title = "Retail employment, USA"
    )
  # A mistake in the video - peak in December - Xmas

## Google, Apple, Facebook, Amazon
gafa_stock |>
  filter(Symbol == "AMZN", year(Date) >= 2018) |>
  autoplot(Close) +
  labs(
    title = "Amazon closing stock price",
    y="$"
    )
  # No seasonality in stock prices - especially if market is efficient
  # Seems no trend or cycle because only looking at 1-year
  # Change year(Date) >= 2014 to see what happens


## Explore the pelt tsibble
pelt #fur
help(pelt)
?pelt #https://en.wikipedia.org/wiki/Hudson%27s_Bay_Company
pelt |> autoplot(Lynx)

  # A bit of a plotting trick/lesson that may be useful later on
pelt |> autoplot()

  # Add some axis labels and title
pelt |>
  pivot_longer(2:3, names_to = "Animal")  |>
  autoplot() +
  labs(title="Lynx eating Hare",
       y="No. of animals")
  # Note this is annual data - talk about population cycle
  # Lynx eating Hare


### BACK TO SLIDES

# ACF  -------------

## Aus bricks - you have seen this in the video
aus_production |>
  filter(year(Quarter) >= 1979) |>
  autoplot(Beer) +
  geom_point() +
  labs(
    title = "Australian beer production",
    y="megalitres"
  )

aus_production |>
  filter(year(Quarter) >= 1979) |>
  gg_season(Beer)

aus_production |>
  filter(year(Quarter) >= 1979) |>
  gg_lag(Beer)

aus_production |>
  filter(year(Quarter) >= 1979) |>
  gg_lag(Beer, geom = "point", lags = 1:12)

aus_production |>
  filter(year(Quarter) >= 1979) |>
  ACF(Beer, lag_max = 20)

aus_production |>
  filter(year(Quarter) >= 1979) |>
  ACF(Beer) |>
  autoplot()


## Contrast this with Brick production
aus_production |>
  autoplot(Bricks) +
  labs(
    title = "Australian clay brick production",
    y="million units"
  )

aus_production |>
  ACF(Bricks, lag_max = 48) |>
  autoplot()

## An even more pronounced trend

a10 |>
  autoplot() +
  geom_point() +
  labs(
    title = "Australian antidiabetic drug sales",
    y = "$ (millions)"
  )

a10 |>
  ACF(Cost, lag_max = 48) |>
  autoplot()

## Lynx ACF
pelt |> autoplot(Lynx)

pelt |>
  ACF(Lynx) |>
  autoplot()
# you'll see plenty of examples in the tutes.


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



# Workshop activity 1  ----------------------------------------------------------

## Snowy mountains tourism --------------------------------------------

snowy <- tourism |>
  filter(Region == "Snowy Mountains") |>
   summarise(Trips = sum(Trips))

# Notice there are 4 purposes of travel
# hence sum over all four of them before you continue

# Use autoplot(), gg_season(), gg_subseries()
# gg_lag(), ACF() to explore feature of the time series



# Workshop Activity 3 -----------------------------------------------------

## Differencing Amazon closing price ------------------------------

dAMZN <- gafa_stock |>
  filter(Symbol == "AMZN", year(Date) >= 2018) |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index=trading_day, regular=TRUE) |>
  mutate(diff = difference(Close))


# Pigs -------------------------------------------------------------

pigs <- aus_livestock |>
  filter(State == "Victoria",
         Animal == "Pigs",
         year(Month) >= 2014)

pigs |> autoplot(Count/1e3) +
  labs(title = "Number of pigs slaughtered in Victoria",
       y="Thousands")

pigs |> gg_season()
pigs |> ACF() |> autoplot()


