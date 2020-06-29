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
# the relationship between the distance and average delay for each location
relation <- flights %>%
        group_by(dest) %>%
        summarise(
                  count = n(), #to ignore data with small observations
                  dist = mean(distance, na.rm = TRUE),
                  delay = mean(arr_delay, na.rm = TRUE)
                  )
relation <- filter(relation, count > 20, dest != "HNL")
ggplot(data = relation, mapping = aes(x = dist, y = delay)) +
        geom_point(aes(size = count), alpha = 1/3) +
        geom_smooth(se = FALSE)
