install.packages("tidyverse")
library("tidyverse")
install.packages("ggplot2")
library("ggplot2")
install.packages("lubridate")
library("lubridate")
install.packages("dplyr")
library("dplyr")
install.packages("nycflights13")
library(nycflights13)
install.packages("ggbeeswarm")
install.packages("lvplot")
install.packages("ggstance")

install.packages("hexbin")
library("hexbin")
library("ggbeeswarm")
library("lvplot")
library("ggstance")
install.packages("modelr")
library(modelr)

View(flights)

flights_times <- mutate(flights, 
                        dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                        sched_dep_time_mins = (sched_dep_time %/% 100 *60 +
                                                 sched_dep_time %% 100) %% 1440)

select(flights_times, dep_time, dep_time_mins, sched_dep_time,
       sched_dep_time_mins)

flights_airtime <- mutate(flights, 
                          arr_time2 = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
                          dep_time2 = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                          air_time_diff = air_time - arr_time2 + dep_time2)

nrow(filter(flights_airtime, air_time_diff != 0))

flights_deptime <- mutate(flights,
                          dep_time_min =(dep_time %/% 100 * 60 + dep_time %% 100)%% 1440,
                          sched_dep_min = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440,
                          dep_delay_diff = dep_delay - dep_time_min + sched_dep_min)

filter(flights_deptime, dep_delay_diff !=0)    

ggplot(flights_airtime,aes(x =  air_time_diff))+
  geom_histogram(binwidth = 1)

ggplot(filter(flights_airtime, dest == "LAX"),
       aes(x = air_time_diff))+
  geom_histogram(binwidth = 1)

ggplot(filter(flights_deptime, dep_delay_diff > 0),aes(x = dep_delay_diff, 
    y = sched_dep_min ))+
  geom_point()

rankme <- tibble(x = c(10, 5, 1, 5, 5))
rankme <- mutate(rankme, 
x_row_number = row_number(x),
x_min_rank = min_rank(x),
X_dense_rank = dense_rank(x))

arrange(rankme, x)

flights_delay <- mutate(flights,
dep_delay_min_rank = min_rank(desc(dep_delay)),
dep_delay_row_number = row_number(desc(dep_delay)),
dep_delay_dense_rank = dense_rank(desc(dep_delay)))

flights_delay <- filter(flights_delay, 
!(dep_delay_min_rank >10 | dep_delay_row_number > 10 |
    dep_delay_dense_rank > 10))

flights_delay <- arrange(flights_delay, dep_delay_min_rank)
print(select(flights_delay, month, day, carrier, flight, dep_delay, 
             dep_delay_min_rank, dep_delay_row_number, dep_delay_dense_rank),
      n = Inf)

flights_delayed <- arrange(flights, desc(dep_delay))
flights_delayed <- slice (flights_delayed, 1:10)
select(flights_delayed, month, day, carrier, flight, dep_delay)

flights_delayed2 <- top_n(flights, 10, dep_delay)
flights_delayed2 <- arrange(flights, desc(dep_delay))
select(flights_delayed2, month, day, carrier, flight, dep_delay)

1:3 + 1:10

summarize(flights, delay =  mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm =  TRUE))

by_dest <- group_by(flights, dest)
delay <- summarize(by_dest, count = n(),
  dist =  mean(distance, na.rm = TRUE),
  delay = mean(arr_delay, na.rm =TRUE))

delay <- filter(delay, count > 20, dist != "HNL")

ggplot(data =  delay, mapping = aes(x = dist, y = delay))+
  geom_point(aes(size = count), alpha = 1/3)+
  geom_smooth(se = FALSE)

delays <- flights %>% 
  group_by(dest) %>% 
  summarize(count = n(),
     dist= mean(distance, na.rm = TRUE),
          delay = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(count > 20, dist != "HNL")

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))


not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize(delay = mean(arr_delay))
  
ggplot(data = delays, mapping = aes(x = delay))+
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize(delay = mean(arr_delay, na.rm = TRUE), n = n())

ggplot(data = delays, mapping = aes( y = delay, x = n ))+
  geom_point(alpha = 1/10)

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x =n, y = delay))+
  geom_point(alpha = 1/10)

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(n = length(dest))

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(n = n())

not_cancelled %>% 
  group_by(tailnum) %>% 
  tally()

not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(n = sum(distance))

not_cancelled %>% 
  group_by(tailnum) %>% 
  tally(distance)

cancelled_per_day <- flights %>% 
  mutate(cancelled = (is.na(arr_delay))| (is.na(dep_delay))) %>% 
  group_by(year, month, day) %>% 
  summarise(
    cancelled_number = sum(cancelled),
    flights_num = n()
  )

ggplot(cancelled_per_day)+
  geom_point(aes(y = cancelled_number, x = flights_num))

cancelled_and_delay <-
  flights %>% 
  mutate(cancelled = (is.na(arr_delay))| (is.na(dep_delay))) %>% 
  group_by(year, month, day) %>% 
  summarise(
    cancelled_group = mean(cancelled),
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    avg_arr_delay = mean(arr_delay, na.rm =  TRUE)
      ) %>% 
  ungroup()

ggplot(data = cancelled_and_delay)+
  geom_point(aes(x = avg_arr_delay, y = cancelled_group))

ggplot(data = cancelled_and_delay)+
  geom_point(aes(x = avg_dep_delay, y = cancelled_group))

flights %>% 
  group_by(carrier) %>% 
  summarise(arr_delay = mean(arr_delay,na.rm = TRUE)) %>% 
  arrange(desc(arr_delay))


colnames(airlines)
select(airlines, carrier, name)



flights %>% 
  filter(!is.na(arr_delay)) %>% 
  #Total delay by carrier within each origin, dest
  group_by(origin, dest, carrier) %>% 
  summarise(
    arr_delay =  sum(arr_delay),
    flights = n()
  ) %>% 
  # Total delay within each origin, dest
  group_by(origin, dest) %>% 
  mutate(
    arr_delay_total = sum(arr_delay),
    flights_total = sum(flights)
  ) %>% 
  # average delay of each carrier - average delay of other carriers
  ungroup() %>% 
  mutate(
    arr_delay_others = (arr_delay_total - arr_delay)/
      (flights_total -  flights),
    arr_delay_mean = arr_delay / flights,
    arr_delay_diff = arr_delay_mean - arr_delay_others
  ) %>% 
  # remove NaN values (when there is only one carrier)
  filter(is.finite(arr_delay_diff)) %>% 
  # average over all airports it flies to
  group_by(carrier) %>% 
  summarise(arr_delay_diff = mean(arr_delay_diff)) %>% 
  arrange(desc(arr_delay_diff))

?is.finite
    
flights %>% 
  count(dest, sort = TRUE)

flights %>% 
  filter(!is.na(tailnum)) %>% 
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>% 
  group_by(tailnum) %>% 
  summarise(on_time = mean(on_time),n =n()) %>% 
  filter(min_rank(on_time) == 1)


quantile(count(flights,tailnum)$n)

flights %>% 
  filter(!is.na(tailnum), is.na(arr_time) | !is.na(arr_delay)) %>% 
  mutate(on_time =  is.na(arr_time) & is.na(arr_delay <= 0)) %>% 
  group_by(tailnum) %>% 
  summarise(on_time = mean(on_time), n = n()) %>% 
  filter(n >= 20) %>% 
  filter(min_rank(on_time) == 1)
   
flights %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(tailnum) %>% 
  summarise(arr_delay = mean(arr_delay), n = n()) %>% 
  filter(n >20) %>% 
  filter(min_rank(desc(arr_delay)) == 1)
  
  flights %>% 
    group_by(hour) %>% 
  summarise(arr_delay = mean(arr_delay, na.rm =TRUE)) %>% 
    arrange(arr_delay)
    
flights %>% 
  filter(arr_delay > 0) %>% 
  group_by(dest) %>% 
  mutate(arr_delay_total = sum(arr_delay),
         arr_delay_prop = arr_delay / arr_delay_total) %>% 
  select(dest, month, day, dep_time, carrier,flight,
         arr_delay_total, arr_delay_prop) %>% 
  arrange(dest, desc(arr_delay_prop))
  
flights %>% 
  filter(arr_delay > 0) %>% 
  group_by(origin, dest, carrier, flight) %>% 
  summarise(arr_delay = sum(arr_delay)) %>% 
  group_by(dest) %>% 
  mutate(arr_delay_prop = arr_delay / sum(arr_delay)) %>% 
  arrange(dest, desc(arr_delay_prop)) %>% 
  select(carrier, flight, origin, dest, arr_delay_prop)

lagged_delays <- flights %>% 
  arrange(origin, month, day, dep_time) %>% 
  group_by(origin) %>% 
  mutate(dep_delay_lag = lag(dep_delay)) %>% 
  filter(!is.na(dep_delay)| (! is.na(dep_delay_lag)))

lagged_delays %>% 
  group_by(dep_delay_lag) %>% 
  summarise(dep_delay_mean =  mean(dep_delay)) %>% 
  ggplot(aes(x = dep_delay_lag, y = dep_delay_mean ))+
  geom_point()+
  scale_x_continuous(breaks = seq(0, 1500, by = 120))+
  labs(y = "depature_delay", x = "previous_depature_delay")

lagged_delays %>% 
  group_by(origin, dep_delay_lag) %>% 
  summarise(dep_delay_mean = mean(dep_delay)) %>% 
  ggplot(aes(x = dep_delay_lag, y = dep_delay_mean))+
  geom_point()+
  facet_wrap(~origin, ncol = 1)+
  labs(y = "depature_delay", x = "previous_depature_delay")
  
standardized_flights <- flights %>% 
  filter(!is.na(air_time)) %>% 
  group_by(origin, dest) %>% 
  mutate(
    air_time_mean = mean(air_time),
    air_time_sd = sd(air_time),
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(air_time_standard = (air_time - air_time_mean) / 
          ( air_time_sd + 1))
ggplot(standardized_flights, aes(x = air_time_standard))+
  geom_density()

standardized_flights %>% 
  arrange(air_time_standard) %>% 
  select(carrier, flight, origin, dest, month, day, air_time, 
         air_time_mean, air_time_standard ) %>% 
  head(10) %>% 
  print(width = Inf)

standardized_flights2 <- flights %>% 
  filter(!is.na(air_time)) %>% 
  group_by(origin, dest) %>% 
  mutate(
    air_time_median = median(air_time),
    air_time_iqr = IQR(air_time),
    n = n(),
    air_time_standard = (air_time - air_time_median) / air_time_iqr
  ) 

ggplot(standardized_flights2, aes(x = air_time_standard))+
  geom_density()

standardized_flights2 %>% 
  arrange(air_time_standard) %>% 
  select(carrier, flight, origin, dest, month, day, air_time, 
         air_time_median, air_time_standard ) %>% 
  head(10) %>% 
  print(width = Inf)

flights %>% 
  mutate(mph = distance / (air_time/60)) %>% 
  ggplot(aes(x = mph))+
  geom_histogram(bindwith =10)

flights %>% 
  mutate(mph = distance /(air_time /60 )) %>% 
  arrange(desc(mph)) %>% 
  select(mph, flight, carrier, flight, month, day, dep_time) %>% 
  head(5)

flights %>% 
  mutate(mph = distance /( air_time / 60)) %>% 
  arrange(desc(mph)) %>% 
  select(origin, dest, mph, year, month, day, dep_time, flight, 
         carrier, dep_delay, arr_delay)


air_time_delayed <-
  flights %>% 
  group_by(origin, dest) %>% 
  mutate(
    air_time_min = min(air_time, na.rm = TRUE),
    air_time_delay = air_time - air_time_min,
    air_time_delay_pct = air_time_delay / air_time_min * 100
  )

air_time_delayed %>% 
  arrange(desc(air_time_delay)) %>% 
  select(
    air_time_delay, carrier, flight, origin, dest, year,
    month, day, dep_time, air_time, air_time_min
  ) %>% 
  head() %>% 
  print(width = Inf)


history(max.show=25)

flights %>% 
  # find all airports with > 1 carrier
  group_by(dest) %>% 
  mutate(n_carrier = n_distinct(carrier)) %>% 
  filter(n_carrier > 1) %>% 
  # rank carriers by number of destinations
  group_by(carrier) %>% 
  summarize(n_dest = n_distinct(dest)) %>% 
  arrange(desc(n_dest))

select(airlines, carrier, name)

flights %>% 
  # sort in increasing order
  select(tailnum, year, month, day, dep_delay) %>% 
  filter(!is.na(dep_delay)) %>% 
  arrange(tailnum, year, month, day) %>% 
  group_by(tailnum) %>% 
  # cumulative number of flights delayed over one hour
mutate(cumulative_hr_delay = cumsum(dep_delay > 60)) %>% 
  # count the number of flights == 0
  summarise(total_flights = sum(cumulative_hr_delay < 1)) %>% 
  arrange(total_flights)

  
flights %>% 
  select(tailnum, year, month, day, arr_delay) %>% 
  filter(!is.na(arr_delay)) %>% 
  arrange(tailnum, year, month, day) %>% 
  group_by(tailnum) %>% 
  # cumulative number of flights delayed over one hour
mutate(cumulative_hrs_delay = cumsum(arr_delay > 60)) %>% 
  # count the number of flights < 1
  summarise(total_flights2 = sum(cumulative_hrs_delay < 0)) %>% 
  arrange(total_flights2)


not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avr_delay2 = mean(arr_delay[arr_delay > 0])
  )

# Why is distance to some destinations more variable
# than to others?
not_cancelled %>% 
  group_by(dest) %>% 
  summarize(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

not_cancelled %>% 
  group_by(carrier,year, month, day) %>% 
  summarize(
    fisrt = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

not_cancelled %>% 
  group_by(dest) %>% 
  summarize(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>% 
  group_by(carrier, year, month, day) %>% 
  summarize(n_early = sum(dep_time < 500))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(hour_perc = mean(arr_delay > 60))

daily <- group_by(flights, year, month, day)
(per_day <- summarize(daily, flights = n()))
(per_month <- summarize(per_day, flights = sum(flights)))

daily %>% 
  ungroup() %>% 
  summarize(flights = n())


flights_sml <- flights %>% 
    filter(!is.na(arr_delay)| !is.na(dep_delay)) %>% 
  select(year, month, day, dep_delay, arr_delay, distance)



flights_sml %>% 
  group_by(year, month, day) %>% 
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests

popular_dests %>% 
  filter(arr_delay > 0 ) %>% 
  mutate(prop_delay = arr_delay/ sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time =  sched_hour + sched_min / 60
      ) %>% 
  ggplot(mapping = aes(sched_dep_time))+
  geom_freqpoly(
    mapping = aes(color = cancelled),
    bindwith = 1/4
  )

diamonds %>% 
  mutate(cut = if_else(runif(n()) < 0.1, NA_character_, as.character(cut))) %>% 
  ggplot()+
  geom_bar(aes(x =cut))

mean(c(0, 1, 2, NA), na.rm = TRUE)
sum(c( 0, 1, 2, NA), na.rm = TRUE)

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
     ) %>% 
  ggplot()+
  geom_boxplot(aes(y =  sched_dep_time, x = canceled))

flights %>% 
  group_by(month, dest) %>% 
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x =factor(month), y = dest, fill = dep_delay))+
  geom_tile()+
  labs(x = "month", y ="destination", fill = "Depature Delay")


flights %>% 
  group_by(month, dest) %>% 
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  group_by(dest) %>% 
  filter(n() == 12) %>% 
  ungroup() %>% 
  mutate(dest = reorder(dest, dep_delay)) %>% 
  ggplot(aes(x =factor(month), y = dest, fill = dep_delay))+
  geom_tile()+
  labs(x = "month", y ="destination", fill = "Depature Delay")