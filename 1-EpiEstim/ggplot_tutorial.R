# Setup
## GGplot converts your data and code into "layers" by using aesthetic mapping aes()
options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
data("midwest", package = "ggplot2")  # load the data
# midwest <- read.csv("http://goo.gl/G1K41K") # alt source 

# 1. Init Ggplot
g1 <- ggplot(midwest, aes(x=area, y=poptotal))  # area and poptotal are columns in 'midwest'
plot(g1)

#  2. We can add points and smoothing features
g2 <- g1 + geom_point() + geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands
plot(g2)

# 3. We can add Title and Labels
g3a <- g2 + labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
plot(g3a)

# or

g3b <- g2 + ggtitle("Area Vs Population", subtitle="From midwest dataset") + xlab("Area") + ylab("Population")
plot(g3b)

# 4. We can make the graph prettier by zooming in and coloring
g4 <- g3a + geom_point(aes(col=state), size=3)  # Set color to vary based on state categories.
plot(g4)

g5 <- g4 + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))  # zooms in
plot(g5)

# Change the tick marks
g6 <- g5 + scale_x_continuous(breaks=seq(0, 0.1, 0.01))
plot(g6)

# Change the Axis
g7 <- g6 + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = sprintf("%1.2f%%", seq(0, 0.1, 0.01))) + 
  scale_y_continuous(breaks=seq(0, 1000000, 200000), labels = function(x){paste0(x/1000, 'K')})
plot(g7)

# More customization
# See https://r-statistics.co/ggplot2-Tutorial-With-R.html
