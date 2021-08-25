setTheme <-function(legend=F, size=26) {
  #set theme
  theme_set(theme_classic(base_size = size) +
              theme(strip.background = element_blank()) +
              theme(axis.ticks      = element_line(color='black'),
                    #   axis.text       = element_blank(),
                    axis.title = element_blank(),
                    axis.line = element_line(colour = 'black', size = 1.2),
                   # strip.text.x = element_blank(), <-responsible for subplot titles
                    )
  )
  
  if (!legend) {
    theme_set(theme_classic(base_size = size) +
                theme(strip.background = element_blank()) +
                theme(axis.ticks      = element_line(color='black'),
                      #   axis.text       = element_blank(),
                      axis.title = element_blank(),
                      axis.line = element_line(colour = 'black', size = 1.2),
                    #  strip.text.x = element_blank(),
                      legend.position = 'none')
                )
  }
}