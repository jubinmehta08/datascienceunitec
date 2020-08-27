#install.packages("tidyverse")
#install.packages(c("nycflights13", "gapminder", "Lahman"))   #hese packages provide data on airline flights, world development, and baseball that we'll use to illustrate key data science ideas.
#install.packages("maps")
tidyverse_update()
library("tidyverse")
library("nycflights13")
library("gapminder")
library("Lahman")
library("maps")

?mpg
summary(mpg)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# Left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Right
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# facet for subplot
ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_wrap(~class,nrow=2)

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_grid(drv~cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(.~cyl)


ggplot(data=mpg) +
  geom_smooth(mapping=aes(x=displ,y=hwy))


# use linetype to draw the 3rd variable (category would be better)
ggplot(data=mpg) +
  geom_smooth(mapping=aes(x=displ,y=hwy,linetype=drv))



# 2 geoms  with color of each
ggplot(data=mpg) +
  geom_smooth(mapping=aes(x=displ,y=hwy,linetype=drv, color=drv))+
  geom_point(mapping = aes(x=displ,y=hwy,color=drv))

# to reduce duplication

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(linetype=drv)


ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)  # SE is to show the extimation range


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color=drv)) + 
  geom_smooth(se = FALSE)



#########################
?diamonds
summary(diamonds)

ggplot(data=diamonds)+
  geom_bar(mapping = aes(x = cut))

#or 
ggplot(data=diamonds)+
  stat_count(mapping=aes(x=cut))


ggplot(data=diamonds)+
  stat_count(mapping = aes(x = cut,y=..prop..,group=1))

ggplot(data=diamonds)+
  stat_summary(
    mapping=aes(x=cut,y=depth),
    fun.ymin=min,
    fun.ymax=max,
    fun.y=median
  )

ggplot(data=diamonds)+
  stat_count(mapping=aes(x=cut,fill=cut))

ggplot(data=diamonds)+
  stat_count(mapping=aes(x=cut, fill=clarity))



ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/10, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")



ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")



ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")   #  when would like to see every dot (not round up)


#Coordinate systems flip
ggplot(data =mpg, mapping=aes(x=class,y=hwy))+
  geom_boxplot()

# flip
ggplot(data=mpg,mapping=aes(x=class,y=hwy)) +
  geom_boxplot() +
  coord_flip()

#coord_quickmap()
nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()


#coord_polar() uses polar coordinates. Polar coordinates reveal an interesting connection between a bar chart and a Coxcomb chart.

bar<- ggplot(data=diamonds)+
  geom_bar(
    mapping = aes(x=cut, fill=cut),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x=NULL,y=NULL)


bar
bar + coord_flip()
bar + coord_polar()


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))+
  coord_polar()


ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() + coord_fixed()












