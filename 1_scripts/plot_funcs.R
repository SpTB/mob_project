plot_sims <- function(df) {
  
  #factorize for plotting
  df = df %>% mutate_at(vars('bonus', 'goal', 'lambda', 
                             'sigma', 'alpha', 'safe_pay'), as.factor) 
  
  p = ggplot(df) +
    aes(x = step, y = threshold, color = sigma, linetype = safe_pay) +
    geom_line(size = 1L) +
    facet_wrap(~goal)+
    geom_hline(yintercept = 0, linetype=3) +
    ggtitle('Optimal thresholds by goal difficulty') +
    scale_y_continuous(breaks = seq(-25, 100, 10)) +
    theme_classic() +
    theme(strip.background = element_blank())
    #
    #scale_color_manual(values = c("#0c4c8a", 'orange')) +

  return (p)
}