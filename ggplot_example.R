install.packages("ggplot2")
library(ggplot2)
?seq
index <- seq(1,length(computers$Minutes))
print(index)
?ggplot
?aes
ggplot() + geom_point(data=computers,aes(x = index, y = Minutes),size=2) + geom_hline(yintercept=mean(computers$Minutes)) + 
geom_segment(data=computers,aes(x=index,y=Minutes,xend=index,yend=mean(Minutes)),linetype="dotted",size=0.5) +
ggtitle("Plot of computers$Minutes") + theme(plot.title = element_text(hjust = 0.5))


# -----------------

#Code to create the following scatter plot
library(ggplot2)
ggplot() + geom_point(data=computers,aes(x = Units, y = Minutes),size=2) + 
ggtitle("Plot of time taken to repair a computer Vs. No. of units being replaced")
