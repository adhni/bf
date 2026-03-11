library(fpp3)

# Explore some rich tsibbles ----------------------------------------

## Olympics ---------------------------------------------------------
olympic_running
olympic_running |> distinct(Length)
olympic_running$Year |> range()

olympic_running |>
  filter(Length=="100") |>
  autoplot(Time) +  
 geom_point()

# Why the missing observations?  

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

# Back to slides

## PBS --------------------------------------------------------------

# Let's sum Cost of A10 across Concession and Type, i.e., total A10 cost

a10 <- PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6)

a10

# Time Series patterns ----------------------------------------

a10 |>
  autoplot(Cost) + #Cost
  labs(
    title = "Australian antidiabetic drug sales",
    y = "Cost $ (millions)"
    )
  # Note - it will pick the first variable it sees - hence select appropriate variable

  # I could do it with ggplot()
a10 |>
   ggplot(aes(x=Month, y=Cost)) +
   geom_line() +
   #geom_point() +
   labs(
      title = "Australian antidiabetic drug sales",
    y = "$ (millions)"
    )

a10 |>
  autoplot() +
  geom_point() +
  labs(
    title = "Australian antidiabetic drug sales",
    y = "$ (millions)"
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
   # Switch in production due to cooling

us_employment |>
  filter(Title == "Retail Trade", year(Month) >= 1980) |>
  autoplot(Employed / 1e3) +
  labs(
    y = "Million people",
    title = "Retail employment, USA"
    )
   # A mistake in the video - peak in December - Xmas
   # 1991-1992 - Gulf War
   # 2009-2010 - GFC

## Google, Apple, Facebook, Amazon
gafa_stock |>
  filter(Symbol == "AMZN", year(Date) >= 2014) |>
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

## Aus beer - you have seen this in the video

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
  gg_season(Beer, labels = "right")

aus_production |>
   filter(year(Quarter) >= 1979) |>
   gg_subseries(Beer)

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
  autoplot(Cost) +
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


# Lectorial activity 1  ----------------------------------------------------------

## Snowy mountains tourism --------------------------------------------

snowy <- tourism |>
   filter(Region == "Snowy Mountains") |>
   summarise(Trips = sum(Trips))

# Notice there are 4 purposes of travel
# hence sum over all four of them before you continue

# Use autoplot(), gg_season(), gg_subseries() and ACF() to explore feature of the time series

## Completed  ---------------------------------------------------------

snowy |> autoplot(Trips) + geom_point()

snowy |> gg_season(Trips, labels="both")

snowy |> gg_subseries(Trips)

snowy |> ACF(Trips) |> autoplot()


## Snowy (more on tourism) -----------------------------------------------------
  # Or in the same way

dat <- filter(tourism,
              Region %in% c("Snowy Mountains","Gold Coast")
              ) |>
  group_by(Region) |>
  summarise(Trips=sum(Trips))

dat |> autoplot(Trips) + geom_point()
  # Very different patterns between the two
  # Let's see it better

dat |> gg_season(Trips,labels="right")
  # Very different patterns between the two

dat |> gg_subseries(Trips)

## Australian state tourism
  # We explored this in the lecture video
  # If we have time we will go through it
  # Just quickly show plotting on the log-scale
  # skip the rest

holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

  # original scale
holidays |> autoplot(Trips) +
  labs(
    title= "Australian domestic holiday nights",
    y = "thousands of trips", x= "Year"
    )

  # log scale
holidays |> autoplot(Trips) +
  scale_y_log10()+
  labs(
    title= "Australian domestic holiday nights",
    y = "thousands of trips", x= "Year"
  )

  # let's filter the top three
top3_tourism <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  filter(State %in% c("New South Wales", "Victoria", "Queensland")) |>
  summarise(Trips = sum(Trips))

  # Show these and comment here - slides are a little too squashed
top3_tourism |> gg_season(Trips) +
  labs(
    title= "Australian domestic holiday nights",
    y = "thousands of trips", x= "Year"
    )

top3_tourism |>
  gg_subseries(Trips) +
  labs(
    title= "Australian domestic holiday nights",
    y = "thousands of trips", x= "Year"
  )


# Lectorial Activity 3 -----------------------------------------------------

## Differencing Amazon closing price ------------------------------

# You can compute and plot the daily changes in the Amazon stock price 
# in 2018 using the code below. Do the daily changes look like white noise?

dAMZN <- gafa_stock |>
  filter(Symbol == "AMZN", year(Date) >= 2018) |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index=trading_day, regular=TRUE) |>
  mutate(diff = difference(Close))


## Completed ------------------------------------------------------

gafa_stock |>
  filter(Symbol == "AMZN", year(Date) >= 2018) |>
  autoplot()

dAMZN <- gafa_stock |>
  filter(Symbol == "AMZN", year(Date) >= 2018) |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index=trading_day, regular=TRUE) |>
  mutate(diff = difference(Close))

dAMZN |> autoplot(Close)
dAMZN |> ACF(Close) |> autoplot()

dAMZN |> autoplot(diff)
dAMZN |> ACF(diff) |> autoplot()

# A random walk accumulates random shocks over time. 
# If we remove the accumulation by taking the difference 
# between consecutive values, what remains are just 
# the random shocks themselves, i.e., the white noise.


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


