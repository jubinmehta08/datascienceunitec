#install.packages("hexbin")

library("tidyverse")
library(modelr)

summary(diamonds)

#categorical data can use bar chart
ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut))

# to see number
diamonds %>%
  count(cut)

#examine the distribution of a continuous variable, use a histogram
ggplot(data=diamonds) +
  geom_histogram(mapping=aes(x=carat),binwidth = 0.5)

# compute by hand 
diamonds %>%
  count(cut_width(carat,0.5))

#zoom in
smaller <- diamonds %>%
  filter(carat<3)

ggplot(data=smaller, mapping=aes(x=carat)) +
  geom_histogram(bedwidth=0.1)



#overlay multiple histograms in the same plot, use geom_freqpoly()
ggplot(data=smaller,mapping = aes(x=carat,color=cut))+
  geom_freqpoly(binwidth=0.1)


ggplot(data=smaller, mapping = aes(x=carat)) +
  geom_histogram(binwidth = 0.01)


#example eruption data
summary(faithful)

ggplot(data=faithful, mapping = aes(x=eruptions))+
  geom_histogram(binwidth =0.25)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)


# to zoom to small values of the y-axis with coord_cartesian()
ggplot(diamonds) +
  geom_histogram(mapping = aes(x=y),binwidth = 0.5) +
  coord_cartesian(ylim=c(0,50))

#above there are three unusual values: 0, ~30, and ~60, so pluck them out with dplyr
unusual <- diamonds %>%
  filter(y<3 | y>20) %>%
  select(price,x,y,z)%>%
  arrange(y)



#### Missing values

#Drop the entire row with the strange values
diamonds2 <- diamonds %>%
  filter(between(y,3,20))

#recommend replacing the unusual values with missing values
diamonds2 <- diamonds %>%
  mutate(y=ifelse(y<3 | y>20,NA,y))


ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

#removed NZ warning
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)


nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time=sched_hour + sched_min /60
  ) %>%
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(mapping = aes(color=cancelled), binwidth=1/4)



###### Covariation
# normal plot is hard to see when too much differnt for values
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

#on the y-axis, Instead of displaying count, display density
ggplot(data=diamonds, mapping = aes(x=price, y= ..density..)) +
  geom_freqpoly(mapping = aes(color =cut), binwidth =500)

#still hard to understand the data, use boxplot instead
ggplot(data=diamonds,mapping = aes(x=cut,y,price)) +
  geom_boxplot()

# sometimes may need to reorder categories for a better visualisation
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data=mpg) +
  geom_boxplot(mapping = aes(x= reorder(class,hwy,FUN = median),y=hwy))

# long variable names use coord_flip()
ggplot(data=mpg) +
  geom_boxplot(mapping = aes(x=reorder(class,hwy,FUN = median),y=hwy)) +
  coord_flip()

##Two categorical variables
ggplot(data=diamonds) +
  geom_count(mapping = aes(x=cut,y=color))

diamonds %>% 
  count(color, cut)

#visualise with geom_tile() and the fill aesthetic
diamonds %>%
  count(color,cut) %>%
  ggplot(mapping = aes(x=color,y=cut)) +
  geom_tile(mapping = aes(fill=n))

##Two continuous variables
# boxplot is useless if the data size is big

ggplot(data=diamonds) +
  geom_point(mapping = aes(x=carat,y=price))
#Scatterplots also become less useful as the size of your dataset grows, because points begin to overplot
#sing the alpha aesthetic to add transparency
ggplot(data=diamonds) +
  geom_point(mapping = aes(x=carat,y=price),alpha=1/100)


# geom_bin2d() creates rectangular bins. geom_hex() creates hexagonal bins
ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x=carat,y=price))

ggplot(data=smaller) +
  geom_hex(mapping = aes(x=carat,y=price))

#Another option is to bin one continuous variable so it acts like a categorical variable
ggplot(data=smaller,mapping = aes(x=carat,y=price)) +
  geom_boxplot(mapping = aes(group=cut_width(carat,0.1)))

#still hard to see. Another approach is to display approximately the same number of points in each bin
ggplot(data=smaller, mapping = aes(x=carat,y=price)) +
  geom_boxplot(mapping = aes(group=cut_number(carat,20)))


##Patterns and models
#Patterns in your data provide clues about relationships
ggplot(data=faithful) +
  geom_point(mapping = aes(x=eruptions,y=waiting))

#Models are a tool for extracting patterns out of data
# fits a model that predicts price from carat and then computes the residuals
mod <- lm(log(price) ~ log(carat),data=diamonds)

diamonds2 <-diamonds %>%
  add_residuals(mod) %>%
  mutate(resid=exp(resid))

ggplot(data=diamonds2) +
  geom_point(mapping = aes(x=carat,y=resid))

ggplot(data=diamonds2) +
  geom_boxplot(mapping = aes(x=cut,y=resid))

