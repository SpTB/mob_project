##empirical threshs
getTheoreticalThresholds <-function(theo.csv=F, path='2_pipeline/') {
  opt = read_csv('2_pipeline/optimalThresholdExperiment.csv', col_types='ddddff') #optimal
  myo = read_csv('2_pipeline/myopicThresholdExperiment.csv', col_types='ddffdd') #myopic
  
  opt = opt %>%
    filter(time > 0) %>%
    select( -X1)
  
  myo = myo %>%
    filter(time > 0) %>%
    select( -X1)
  
  
  names(opt) = c('opt','choice_num','thresh','sd','safe_pay')
  names(myo) = c('thresh','safe_pay','sd','choice_num','myo')
  #theo = bind_rows(opt, myo)
  #theo$strat = as.factor(c(rep('opt',nrow(opt)), rep('myo', nrow(myo))))
  theo = opt %>%
    mutate(myo = myo$myo)
  
  if (write.theo) {
    writepath = paste(path,'theo.csv', sep='')
    write.csv(file = writepath, theo, row.names = F)
  }
  
  return(theo)
}

plotGames <-function(subj, theo='', silent=T) {
  #plots each game, showing give up points and theoretical curves for myopic and optimal strategies
  #subj: numberical vector specifying which subject data to plot
  #theo: theoretical file
  #returns a single subj plot
  
  if (theo=='') {
    theo = read_csv('2_pipeline/theo.csv', col_types = 'dddffd') 
  }
  
  a = all %>% filter(subjID == subj) %>%
    #group_by(game) %>%
    mutate(sd=as.factor(sd),
           safe_pay = as.factor(safe_pay),
    ) 
  
  #TODO: test full_join with proper row ordering
  comb = left_join(a, theo)
  
  #TODO: check if this is not better done during initial prepro
  comb = comb %>%
    mutate(when = ifelse(is.na(when), 21, when)) %>%
    #filter out choices after reaching threshold
    filter(choice_num<=when)%>%
    #display points only on hit threshold trials:
    mutate(when_hit_plot = ifelse( (when==choice_num)==1, points ,NA)) %>%
    group_by(game) %>%
    #display points only on give-up trials:
    mutate(when_give_plot = ifelse(((choice_num==max(choice_num)) & 
                                      (max(choice_num)<20) & 
                                      (max(points)<thresh)), points, NA))  %>%
    #display threshold value only on the first game trial:
    mutate(thresh_plot = ifelse(choice_num==1, paste('Goal:', thresh), NA)) %>%
    #display safe_pay value only on the first game trial:
    mutate(safe_plot = ifelse(choice_num==1, paste('Safe:', safe_pay), NA)) %>%
    ungroup()
  
  
  #TODO: pack into func
  #maybe write csv  option for comb/theo
  #each subj as a list element?
  
  p = ggplot(comb %>%filter(game<41), aes(x=choice_num, y=points, group=thresh)) +
    geom_point() +
    geom_line(aes(y=thresh), color='red',size=1.25) +
    geom_line(aes(y=myo), color='purple', size=1.25, alpha=.5) +
    geom_line(aes(y=opt), color='blue', size=1.25, alpha=.5 ) +
 #   geom_point(aes(y=when_give_plot), color='red', shape=4 ,size=6) +
  #  geom_point(aes(y=when_hit_plot), color='darkgreen', size=4) +
    geom_text(aes(x=16, y=-10, label=thresh_plot), size=6) +
    geom_text(aes(x=16, y=-30, label=safe_plot), size=6) +
    xlim(c(1,20)) +
    facet_wrap(~game)
  
  #if there are instances of hitting the goal, add to the plot
  if (mean(is.na(comb$when_hit_plot))<1) {
    p = p +
      geom_point(aes(y=when_hit_plot), color='darkgreen', size=4)
  }
  
  #if there are instances of giving up, add to the plot
  if (mean(is.na(comb$when_give_plot))<1) {
    p = p +
      geom_point(aes(y=when_give_plot), color='red', shape=4 ,size=6)
  }
  
  nam = paste('2_pipeline/p1/games_s', subj, '.png', sep='')
  ggsave(plot = p, filename=nam, units = 'cm', width=52,height = 40)
  
  if (!silent) {
    return (p)
  }
}

#TODO: plot everyone

plotGamesMulti<-function(subj) {
  for (i in seq_along(subj)) {
    plotGames(subj = subj[i], silent = T )
  }
}
