# Missing Data

rm(list=ls())

library(tidyverse)
library(nycflights13)

a <- NA
a
9 > NA
NA == NA

bof_selin <- NA
bof_pelin <- NA
bof_pelin == bof_selin

flights |> 
  filter(dep_time == NA)

vec <- c(2, FALSE, NA)
vec == NA

is.na(vec)

flights |> 
  filter(is.na(dep_time))

flights |> 
  filter(month == 1 & day == 1) |> 
  arrange(dep_time)

flights |> 
  filter(month == 1 & day == 1) |> 
  arrange(desc(is.na(dep_time)), dep_time)

# %in% combine == and |

NA %in% NA

flights |> 
  filter(month %in% c(11, 12))

flights |> 
  filter(dep_time %in% c(NA, 0800))

# Explicit Missing Values

treatment <- tribble(
  ~person, ~treatment, ~response,
  "Derrick Whitmore", 1, 7,
  NA, 2, 10,
  NA, 3, NA,
  "Katherine Burke", 1, 4
)
treatment

# Last observation carried forward 

# tidyr::fill()
treatment |> 
  fill(everything())

# Fixed 
# dplyr::coalesce()
x <- c(1, 4, 5, 7, NA)
x
coalesce(x, 0)

x <- c(1, 4, 5, 7, -99)
na_if(x, -99)

x[is.na(x)] = 0
x

# Nan
x <- c(NA, NaN)
x * 10
x == 1
is.na(x)
is.nan(x)
0/0
0*Inf
Inf - Inf
sqrt(-1)

# Implicit missing values 

stocks <- tibble(
  year = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr = c(1, 2, 3, 4, 2, 3, 4),
  price = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)
stocks
# Pivoting
stocks |> 
  pivot_wider(
    names_from = qtr,
    values_from = price
  )

# Complete
## bU fcomplete fonkisyonu bizim eksik yagis verilerinde kullanılır
## saatlik yagislarda eksik oldugunda bu saatler yok zira
stocks |> 
  complete(year, qtr)

stocks |> 
  complete(year = 2019:2021, qtr)

# Joins 
flights |> 
  distinct(faa = dest) |> 
  anti_join(airports)

airports
View(airports)

flights |> 
  distinct(tailnum) |> 
  anti_join(planes)
planes


# Exercise: Can you find any relationship between the carrier
# and the rows that appear to be missing from planes?

flights |> 
  anti_join(planes, by = "tailnum") |> 
  count(carrier, sort = TRUE) |> 
  mutate(p = n/sum(n))

# Factors and empty groups
health <- tibble(name   = c("Ikaia", "Oletta", "Leriah", "Dashay", "Tresaun"), 
                 smoker = factor(c("no", "no", "no", "no", "no"), levels = c("yes", "no")),
                 age    = c(34, 88, 75, 47, 56),
)
health |> 
  count(smoker, .drop = FALSE)

ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

health |> 
  group_by(smoker) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  ) |> 
  complete(smoker)
