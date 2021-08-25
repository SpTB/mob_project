###import
source('1_scripts/preprocessing_funcs.R') # for data processing
source('1_scripts/plot_games.R') 

### Basic analysis:

##import all data
main = getDat(exp_num='p1', type='main') #main behavioral file
tech = getDat(exp_num='p1', type='tech') #technical details (browser/ip/etc..)
dem = getDat(exp_num='p1', type='demo', comments.txt = F) #demographic data

grouped = groupMain(main, winnings.txt=F) #game summaries

##descriptives
#TODO: wrap the plots in funcs
# successes
suc = grouped %>% 
  group_by(thresh) %>%
  summarise(success = mean(success),
            when_hit = mean(when_hit, na.rm=T),
            when_give = mean(when_give, na.rm=T)
  )

points = grouped %>% 
  filter(!is.na(when_give)) %>%
  group_by(thresh) %>%
  summarise(points = mean(max_points))


ggplot(grouped, aes(x=when_hit)) +
 # theme_classic() +
 # geom_text(aes(x=4, y=30, label=sd, color='black')) +
  geom_histogram(binwidth = 1, 
                 col="black", 
                 fill="gray", 
                 alpha = .2) +
  xlim(c(1,20)) +
  ggtitle('When Hit Goal') +
  facet_wrap(~safe_pay)

ggplot(grouped, aes(x=when_give)) +
  # theme_classic() +
  # geom_text(aes(x=4, y=30, label=sd, color='black')) +
  geom_histogram(binwidth = 1, 
                 col="black", 
                 fill="gray", 
                 alpha = .2) +
  xlim(c(1,20)) +
  ggtitle('When Gave Up') +
  facet_wrap(~safe_pay)

## plotting games
plotGamesMulti(subj=unique(main$subjID)[14:34])    

#TODO: rts

#TODO: basic inferencial stats