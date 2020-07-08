library(tidyverse)
library(nycflights13)
library(ggplot2)
### Grouped summaries
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
#> # A tibble: 1 x 1
#>   delay
#>   <dbl>
#> 1  12.6

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
#> # A tibble: 365 x 4
#> # Groups:   year, month [12]
#>    year month   day delay
#>   <int> <int> <int> <dbl>
#> 1  2013     1     1 11.5 
#> 2  2013     1     2 13.9 
#> 3  2013     1     3 11.0 
#> 4  2013     1     4  8.95
#> 5  2013     1     5  5.73
#> 6  2013     1     6  7.15
#> # … with 359 more rows

names(flights)
# the by_destship between the distance and average delay for each location
by_dest <- flights %>%
        group_by(dest) %>%
        summarise(
                  count = n(),
                  dist = mean(distance, na.rm = TRUE),
                  delay = mean(arr_delay, na.rm = TRUE)
                  ) %>%
        filter(by_dest, count > 20, dest != "HNL")  #to ignore data with small observations

ggplot(data = by_dest, mapping = aes(x = dist, y = delay)) +
        geom_point(aes(size = count), alpha = 1/3) +
        geom_smooth(se = FALSE)

nycflights13::flights
data(flights)
summary(flights)

library(Lahman)

batting <- Lahman::Batting
batters <- batting %>%
        group_by(playerID) %>%
        summarise(
                ba = sum(H, na.rm = T)/sum(AB, na.rm = T),
                ab = sum(AB, na.rm = T)
        )

batters %>%
        ggplot(mapping = aes(x = ab, y = ba)) +
        geom_point(aes(color = ab), alpha = 1/10)