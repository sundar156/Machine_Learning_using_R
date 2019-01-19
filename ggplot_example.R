install.packages("ggplot2")
library(ggplot2)
?seq
index <- seq(1,length(computers$Minutes))
print(index)
?ggplot
ggplot() + geom_point(data=computers,aes(x = index, y = Minutes),size=2) + geom_hline(yintercept=mean(computers$Minutes)) + 
geom_segment(data=computers,aes(x=index,y=Minutes,xend=index,yend=mean(Minutes)),linetype="dotted",size=0.5) +
ggtitle("Plot of computers$Minutes") + theme(plot.title = element_text(hjust = 0.5))
