?datasets

library(help = "datasets")

# dont know use ? or highlight or F1

data("iris")
summary(iris)
?iris
plot(iris)


data(mtcars)
summary(mtcars)
?mtcars
plot(mtcars)



str(mtcars)
ncol(mtcars)
nrow(mtcars)

head(mtcars)
tail(mtcars)



pollution <- read.csv("avgpm25.csv", colClasses = c("numeric", "numeric", "numeric", "numeric"))
str(pollution)
ncol(pollution)
nrow(pollution)

head(pollution)
tail(pollution)

fivenum(pollution$pm25) 
summary(pollution$pm25) 

#Boxplot
boxplot(pollution$pm25, col = "blue")
boxplot(pollution$pm25, col = "blue",ylim=c(0,15))

#hostogram
hist(pollution$pm25, col ="green")
rug(pollution$pm25)


hist(pollution$pm25, col ="green", breaks=100)
rug(pollution$pm25)


#Overlaying Feature
boxplot(pollution$pm25, col = "blue")
abline(h=12)
abline(h=2)

hist(pollution$pm25, col="green")
abline(v=12, lwd=2)
abline(v=median(pollution$pm25),col="magenta",lwd=4)


#barplot
summary(pollution$State.Code)
barplot(table(pollution$State.Code), col="wheat", main = "State code")


#multiple Boxplot

boxplot(pollution)
boxplot(pollution,col = "red")


boxplot(pollution[,c(2,3)], col = "red")
boxplot(pollution[,2:3], col = "red")

boxplot(pm25 ~ State.Code , data=pollution, col = "red")




#multiple histograms





par(mfrow = c(2,1))
par(mfrow = c(2,1),mar=c(4, 4, 2,1))
?par()

hist (subset(pollution, State.Code==1)$pm25, col = "green") 
hist (subset(pollution, State.Code==2)$pm25, col = "green")


## Scatter plot  good for continuous Variables
with(pollution, plot(County.Code, pm25))
abline(h=12, lwd=2, lty=2, col="red")

#reset plot
par(mfrow = c(1,1))

### Scatter plot with colour
with (pollution,plot(County.Code,pm25,col=State.Code))


#### Plotting systems
#
library(datasets)
data(cars)

?cars
summary(cars)

plot(cars)
str(cars)
ncol(cars)
nrow(cars)
head(cars)
tail(cars)

#base plot
with(data=cars,plot(x=speed,y=dist))


data(airquality)
?airquality
summary(airquality)
plot(airquality)
str(airquality)
ncol(airquality)
nrow(airquality)
head(airquality)
tail(airquality)

# Histogram
hist(airquality$Ozone)  ## Draw a new plot

#scatterplot
with(data=airquality,plot(x=Wind,y=Ozone))


?plot()
?par()   # global

par(mfcol = c(1,2), bg="white") 
hist(airquality$Ozone) 
hist(airquality$Temp)

par(mfcol = c(1,1))    # reset par()

#Default graphic parameters
par("lty")       #line type  (default=solid)
par("col")       #ploting color
par("pch")       # ploting symbol (default=open circile) 
par("bg")        # background color  
par("mar")       # margin size
par("mfrow")     # number of plot per row


# base plot with annotation
with(airquality,plot(Wind,Ozone))
title(main="Ozone and Wind in New York City")   ## Add title

# we can conbind the command
with (airquality, plot (Wind, Ozone, main = "Ozone and Wind in NY City"))
#we can also overlay  (whenever you do not draw a new plot)
with (subset (airquality, Month==5), points (Wind, Ozone, col = "blue"))


with (airquality, plot (Wind, Ozone, main = "Ozone and Wind in NY City", type="n"))     # n is do not produce any points or lines
with (subset (airquality, Month==5), points (Wind, Ozone, col = "blue"))
with (subset (airquality, Month!=5), points (Wind, Ozone, col = "red"))
legend("topright", pch=1, col=c("blue","red"), legend=c("May","Other Months"))

#Base Plot with regression line
with (airquality, plot (Wind, Ozone, main = "Ozone and Wind in New York City", pch=20))
model <- lm (Ozone ~ Wind, airquality) 
abline(model, lwd=2, col="red")


# Multiple Base Plots
par(mfrow=c(1,2))
par(mfrow=c(2,1))
par(mfrow=c(1,1))
with(airquality, {
  plot(Wind, Ozone, main="Ozone and Wind")
  plot(Solar.R, Ozone, main="Ozone and Solar Radiation")
})



## The Lattice System
#Lattice PLot
library('lattice')

state <-data.frame(state.x77, region = state.region)     # state.x77 is Matrix. so need to transfrom to datafram ot make things easier

install.packages("tidyverse")
library("tidyverse")
state %>%count(region)
table(state$region)

xyplot(Life.Exp ~ Income | region, data=state, layout=c(4,1))



### the ggplot2 System
library(ggplot2)
library("tidyverse")
library(help = "tidyverse")

data(mpg)
qplot(x=displ,y=hwy, data=mpg)


####Graphical Devices in R
?Devices

dev.off() 
data("faithful")


#Create a Plot on a PDF file
pdf(file='myplot.pdf') ## Open PDF device: create 'myplot.pdf' in my working directory
## Create plot and send to a file (no plot appears on screen)
with(faithful, plot(x=eruptions, y= waiting))
title(main="Old Faithful Geyser data")  ## Annotation plot; still nothing on screen
dev.off()
## Noe you can view the file 'myplot.pdf' on your computer





### Copying PLots
# dev.copy
# dev.copy2pdf


## alternatively use ggsave()
with(faithful, plot(x=eruptions, y= waiting))
title(main="Old Faithful Geyser data")  ## Annotation plot; still nothing on screen
dev.copy(png,file='geyserplot.png')  ## copy my plot to a PNG file
dev.off()   # dont forget

## alternatively use ggsave()
with(faithful, plot(x=eruptions, y= waiting))
title(main="Old Faithful Geyser data")  ## Annotation plot; still nothing on screen
ggsave(file='GGgeyserplot.png')



