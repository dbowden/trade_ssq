library(ggplot2)

yearly <- settle %>% 
  group_by(year) %>% 
  summarize(settlements=sum(new_settle, na.rm=T))

ggplot(subset(yearly, year>=1870), aes(x=year, y=settlements)) + geom_line()
