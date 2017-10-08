
library(dplyr)
library(plyr)
library(ggplot2)


df = read.csv("temparature-cave.txt", sep = "\t", skip=23, encoding="UTF-8", header = FALSE, colClasses = c("V3" = "POSIXct"))
df = df %>% select(V1, V3, V5, V7) %>% rename(c("V1" = "SampleNumber", "V3" = "Time", "V5" = "Temperature", "V7" = "Humidity"))
ggplot(df,aes(Time,Temperature)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth(method="loess") + 
  scale_x_datetime(date_breaks = "6 hours") + theme(axis.text.x=element_text(angle=90, hjust=1))
