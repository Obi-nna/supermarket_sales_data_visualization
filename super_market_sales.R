library(tidyverse)
library(lubridate)
library(ggplot2)

smkt<-read.csv("C:/Users/Obinna/Desktop/MA304Reassessment/supermarket_sales.csv",sep=",", header=T, stringsAsFactors=T)


.........#Number of product line sold per Branch from Jan 2019 to March 2019#..........

ggplot(smkt, aes(x = Product.line, fill= Branch)) +
  geom_bar(stat="count", position = position_dodge()) +
  geom_text(stat = "count",aes(label=..count..), vjust = 2.9, position=position_dodge(0.9)) +
  scale_fill_manual(values = c("#FF9933", "#CC6600","663300")) +
  xlab("Product Line") +
  ylab("Count") +
  theme(axis.title = element_text(size = 11, face = 'bold'),
        axis.text = element_text(size = 11, colour = 'black'),
        axis.text.x = element_text(size = 9),
        legend.title = element_text(size = 12, face = 'bold'),
        legend.text = element_text(size = 14, colour = 'black')) +
  ggtitle("Number of product line sold per Branch from Jan 2019 to March 2019")




...........#Customer rating on their overall shopping experience#.........

ggplot(smkt, aes(x=Rating)) +
  geom_histogram(binwidth = 1, color="red", fill="grey") +
  ggtitle("Customer rating on their overall shopping experience ") +
  xlab("Customer Ratings") +
  ylab("Customer count") +
  xlim(2.5,12) +
  theme(axis.title = element_text(size = 12, face = 'bold'),
        axis.text = element_text(size = 13, colour = 'black'))




.........#Type of payment used by Male and Female in the Branches#........

ggplot(smkt, aes(x = Payment, fill= Gender)) +
  geom_bar(stat="count", position = position_dodge()) +
  geom_text(stat = "count",aes(label=..count..), vjust = 2.9, position=position_dodge(0.9), colour = "black", size = 3.5) +
  scale_fill_manual(values = c("#FF9933", "#0066CC")) +
  xlab("Payment") +
  ylab("Count") +
  facet_grid(~Branch) +
  theme(axis.title = element_text(size = 11, face = 'bold'),
        axis.text = element_text(size = 11, colour = 'black'),
        legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12, colour = 'black')) +
  ggtitle("Type of payment used by Male and Female in the Branches")




.........#Gross income on sales across the Branches from Jan 2019 to March 2019#.........

smkt = group_by(smkt, Product.line, Date, Branch)
smkt = summarise(smkt, gross.income = sum(gross.income))

ggplot(smkt, aes(x = Date, y = gross.income, color = Product.line)) +
  geom_line(lwd=0.5) +
  scale_x_date(date_breaks = "4 weeks", date_labels = "%Y %b") +
  xlab("Sales period") +
  ylab("Gross Income on sales") +
  facet_wrap(~Branch) +
  ggtitle("Gross income on sales across the Branches from Jan 2019 to March 2019")



...........#Correlation between Unit Price and Gross Income#.........

ggplot(smkt, aes(x=Unit.price, y=gross.income)) +
  geom_point(size=1.5) +
  xlab("Unit Price($)") +
  ylab("Gross Income($)") +
  theme(axis.title = element_text(size = 11, face = 'bold'),
        axis.text = element_text(size = 11, colour = 'black')) +
  ggtitle("Correlation between Unit Price and Gross Income")