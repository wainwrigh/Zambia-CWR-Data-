x = sort(asf$FarmSize[asf$Objective=="Area"])
plot(x, type="l")
x = quantile(x, seq(0, 1, by=0.01))
plot(x, type="l", xlab="Percent")


x=seq(0, 1, by=0.01)
library(dplyr)
x = Zambia %>%
  select(ID, Objective, FarmSize, Model)

x$IDX = paste0(x$Objective, x$ID, x$Model)

x$Model = factor(x$Model, levels=c("Low", "Medium ", "High "))

# This is the graph plotting 
x %>% 
  filter(Model=="Medium ") %>% 
  mutate(IDX=reorder(IDX, FarmSize)) %>% 
  ggplot(aes(IDX, FarmSize, colour=Objective, group=1)) +
  geom_point() +
  #scale_y_log10() +
  facet_wrap(~Objective) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  
x %>% 
  group_by(Objective, ID) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
