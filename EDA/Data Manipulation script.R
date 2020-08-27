#install.packages("tidyverse")
#install.packages(c("nycflights13", "gapminder", "Lahman"))   #hese packages provide data on airline flights, world development, and baseball that we'll use to illustrate key data science ideas.
#install.packages("maps")
tidyverse_update()
library("tidyverse")
library("nycflights13")
library("gapminder")
library("Lahman")
library("maps")


?flights

summary(flights)

jan1<-filter(flights,month==1,day==1)

#outer Blackets for data viewing after defining
(dec25<-filter(flights,month==12,day==25))

sqrt(2)^2 ==2
near(sqrt(2)^2,2)

filter(flights,month==11 | month==12)

# or 

nov_dec<-filter(flights, month %in% c(11,12))

#find flights that weren't delayed (on arrival or departure) by more than two hours

filter(flights,arr_delay<=120 & dep_delay<=120)
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights,arr_delay <= 120 ,dep_delay <= 120)
x<-NA
is.na(x)


df <- tibble(x = c(1, NA, 5))
filter(df, x > 1)




filter(df,is.na(x)|x>1)

filter(flights,arr_delay>=120)
filter(flights,dest =="IAH" | dest =="HOU")
filter(flights,carrier %in% c("AA","DL","UA"))
filter(flights,arr_delay>120 & dep_delay<=0)
filter(flights,dep_time>="1200" & dep_time<="600")
arrange(flights,year,month,day)
arrange(flights,desc(dep_delay))
df<-tibble(x=c(5,2,NA))
arrange(df,x)
arrange(df,desc(is.na(x)))

select(flights,year,month,day)


head(flights)

# create a calculate column with mutate

flights_sml <-select(flights,
                     year:day,
                     ends_with("delay"),
                     distance,
                     air_time,
                     hour
)


mutate(flights_sml,
       gain=dep_delay -arr_delay,
       speed=distance/air_time *60
)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)


# to get only new calcualation field
transmute(flights_sml,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)



y<-c(1,2,2,NA,3,4)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

summarise(flights,delay=mean(dep_delay,na.rm = TRUE))

by_day<-group_by(flights,year,month,day)
summarise(by_day,delay=mean(dep_delay,na.rm = TRUE))




by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)


delays <-flights %>%
  group_by(dest) %>%
  summarise(
    count=n(),
    dist=mean(distance,na.rz=TRUE),
    delay=mean(arr_delay,na.rm=TRUE)
  ) %>%
  filter(count>20,dest!="HNL")



#  not eliminate missing value, cannot calculate mean
flights %>%
  group_by(year,month,day) %>%
  summarise(mean=mean(dep_delay))

flights %>%
  group_by(year,month,day) %>%
  summarise(mean=mean(dep_delay,na.rm = TRUE))

# or if known can eliminate NA value first  (not cancelled flights)
not_cancelled <-flights %>%
  filter(!is.na(dep_delay),!is.na(arr_delay))

not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(mean=mean(dep_delay))

delays <-not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay=mean(arr_delay)
  )
ggplot(data = delays, mapping = aes(x =delay)) +
  geom_freqpoly(binwidth=10)



delays<-not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay=mean(arr_delay,na.rm = TRUE),
    n=n()
  )
ggplot(data =delays, mapping = aes(x=n,y=delay)) +
  geom_point(alpha=1/10)


delays %>%
  filter(n>25) %>%
  ggplot(mapping = aes(x=n,y=delay))+
  geom_point(alpha=1/10)



batting <- as_tibble(Lahman::Batting)


batters <- batting %>%
  group_by(playerID) %>%
  summarise(
    ba=sum(H,na.rm = TRUE)/sum(AB, na.rm = TRUE),
    ab=sum(AB,na.rm = TRUE)
  )

batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x=ab,y=ba)) +
  geom_point() +
  geom_smooth(se=FALSE)
#> `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'


not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    avg_delay1=mean(arr_delay),
    avg_delay2=mean(arr_delay[arr_delay>0])  # the average positive delay
  )


not_cancelled %>%
  group_by(dest)%>%
  
  
  # Why is distance to some destinations more variable than to others?
  not_cancelled %>%
  group_by(dest)%>%
  summarise(distance_sd=sd(distance)) %>%
  arrange(desc(distance_sd))


# When do the first and last flights leave each day?

not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    fist=min(dep_time),
    last=max(dep_time)
  )

# or better one
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )


not_cancelled %>%
  group_by(year,month,day) %>%
  mutate(r=min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))


# Which destinations have the most carriers?   # need to count ditinct
not_cancelled%>%
  group_by(dest)%>%
  summarise(carriers=n_distinct(carrier)) %>%
  arrange(desc(carriers))


#Counts are so useful that dplyr provides a simple helper if all you want is a count
not_cancelled%>%
  count(dest)

#You can optionally provide a weight variable. For example, you could use this to "count" (sum) the total number of miles a plane flew
not_cancelled%>%
  count(tailnum,wt=distance)



# How many flights left before 5am? (these usually indicate delayed flights from the previous day)
not_cancelled%>%
  group_by(year,month,day) %>%
  summarise(n_early=sum(dep_time<500))


# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month,day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))


#progressive sum
daily <-group_by(flights,year,month,day)

(per_day <-summarise(daily,flights=n()))

(per_month   <- summarise(per_day, flights = sum(flights)))

(per_year <-summarise(per_month,flights=sum(flights)))


#If you need to remove grouping, and return to operations on ungrouped data, use ungroup

daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())  # all flights


#Find the worst members of each group
flights_sml %>%
  group_by(year,month,day) %>%
  filter(rank(desc(arr_delay))<10)

popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n()>365)


#Standardise to compute per group metrics
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)




