#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("nycflights13")
library(tidyverse)
library(lubridate)
library(nycflights13)

head(flights)


# Tip 1 of 18: Create new columns in a count or group_by

flights %>%
  mutate(long_flight = (air_time >= 6 * 60)) %>%
  View()

flights %>%
  mutate(long_flight = (air_time >= 6 * 60)) %>%
  count(long_flight)

flights %>%
  count(long_flight = air_time >= 6 * 60)

flights %>%
  count(flight_path = str_c(origin, " -> ", dest), sort = TRUE)

flights %>%
  group_by(date = make_date(year, month, day)) %>%
  summarise(flights_n = n(), air_time_mean = mean(air_time, na.rm = TRUE)) %>%
  ungroup()


# Tip 2 of 18: Sample and randomly shuffle data with slice_sample()

flights %>%
  slice_sample(n = 15)

flights %>%
  slice_sample(prop = 0.15)

flights %>%
  slice_sample(prop = 1)

flights %>%
  group_by(origin) %>%
  slice_sample(n = 3) %>%
  ungroup()


# Tip 3 of 18: Create a date column specifying year, month, and day

flights %>%
  select(year, month, day) %>%
  mutate(date = make_date(year, month, day))


# Tip 4 of 18: Parse numbers with parse_number()

numbers_1 <- tibble(number_col = c("#1", "#2", "#3"))
numbers_2 <- tibble(number_col = c("Number 5", "#6", "7"))
numbers_3 <- tibble(number_col = c("1.2%", "2.5%", "50.9%"))

numbers_1
numbers_1 %>% mutate(number_col = parse_number(number_col))

numbers_2
numbers_2 %>% mutate(number_col = parse_number(number_col))

numbers_3
numbers_3 %>% mutate(number_col = parse_number(number_col))


# Tip 5 of 18: Select columns with starts_with, ends_with, etc.

flights %>%
  select(starts_with("dep_"))

flights %>%
  select(starts_with("dep_"), everything())

flights %>%
  select(ends_with("hour"))

flights %>%
  select(contains("hour"))


# Tip 6 of 18: case_when to create or change a column when conditions are met

flights %>%
  select(origin)

flights %>%
  mutate(origin = case_when(
    (origin == "EWR") & dep_delay > 20 ~ "Newark International Airport - DELAYED",
    (origin == "EWR") & dep_delay <= 20 ~ "Newark International Airport - ON TIME DEPARTURE",
  )) %>%
  count(origin)


# Tip 7 of 18: str_replace_all to find and replace multiple options at once

flights %>%
  mutate(origin = str_replace_all(origin, c(
    "^EWR$" = "Newark International",
    "^JFK$" = "John F. Kennedy International"
  ))) %>%
  count(origin)


# Tip 8 of 18: Transmute to create or change columns and keep only those columns

flights %>%
  transmute(date = make_date(year, month, day), tailnum)


# Tip 9 of 18: Use pipes %>% everywhere including inside mutates

airlines %>%
  mutate(name = name %>%
           str_to_upper() %>%
           str_replace_all(" (INC|CO)\\.?$", "") %>%
           str_replace_all(" AIR ?(LINES|WAYS)?( CORPORATION)?$", "") %>%
           str_to_title() %>%
           str_replace_all("\\bUs\\b", "US")) %>%
  count(name)

airlines %>%
  mutate(name = str_replace_all(str_to_title(str_replace_all(str_replace_all(str_to_upper(name), " (INC|CO)\\.?$", ""), " AIR ?(LINES|WAYS)?( CORPORATION)?$", "")), "\\bUs\\b", "US"))


# Tip 10 of 18: Filter groups without making a new column

flights %>%
  count(carrier, sort = TRUE)

flights_top_carriers <- flights %>%
  group_by(carrier) %>%
  filter(n() >= 10000) %>%
  ungroup()

flights_top_carriers %>%
  count(carrier, sort = TRUE)


# Tip 11 of 18: Split a string into columns based on a regular expression

name<-airlines %>%
  count(name)

airlines %>%
  extract(
    name,
    into = c("short_name", "remainder"),
    regex = "^([^\\s]+) (.*)$"
  )

airlines %>%
  extract(
    name,
    into = c("short_name", "remainder"),
    regex = "^([^\\s]+) (.*)$",
    remove = FALSE
  )


# Tip 12 of 18: semi_join to pick only rows from the first table which are matched in the second table

airways_beginning_with_a <- airlines %>%
  filter(name %>% str_detect("^Am"))

flights %>%
  semi_join(airways_beginning_with_a, by = "carrier") %>%
  count(carrier)


# Tip 13 of 18: anti_join to pick only rows from the first table which are NOT matched in the second table

flights %>%
  anti_join(airways_beginning_with_a, by = "carrier")


# Tip 14 of 18: fct_reorder to sort bar charts

flights_with_airline_names <- flights %>%
  left_join(airlines, by = "carrier")

flights_with_airline_names %>%
  count(name) %>%
  ggplot(aes(name, n)) +
  geom_col()

flights_with_airline_names %>%
  count(name) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(name, n)) +
  geom_col()


# Tip 15 of 18: coord_flip to display counts more beautifully

flights_with_airline_names %>%
  count(name) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(name, n)) +
  geom_col() +
  coord_flip()


# Tip 16 of 18: fct_lump to lump some factor levels into "Other"

flights_with_airline_names %>%
  mutate(name = fct_lump(name, n = 5)) %>%
  count(name) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(name, n)) +
  geom_col() +
  coord_flip()


# Tip 17 of 18: Generate all combinations using crossing

crossing(
  customer_channel = c("Bus", "Car"),
  customer_status = c("New", "Repeat"),
  spend_range = c("$0-$10", "$10-$20", "$20-$50", "$50+")
)


# Tip 18 of 18: Create functions that take column names with double curly braces

col_summary <- function(data, col_names, na.rm = TRUE) {
  data %>%
    summarise(across({{ col_names }},
                     list(
                       min = min,
                       max = max,
                       median = median,
                       mean = mean
                     ),
                     na.rm = na.rm,
                     .names = "{col}_{fn}"
    ))
}

flights_with_airline_names %>%
  col_summary(c(air_time, arr_delay))

flights_with_airline_names %>%
  group_by(carrier) %>%
  col_summary(c(air_time, arr_delay))