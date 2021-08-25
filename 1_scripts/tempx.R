##thresh
source('setTheme.R')


##RT analysis
rts = all %>%
  group_by(subjID, choice_num) %>%
  summarise(rt=mean(rt, na.rm=T))

ggplot(rts, aes(choice_num, y=rt)) +
  geom_point() +
  facet_wrap(~subjID, scales = 'free_y')


