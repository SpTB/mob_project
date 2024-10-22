library(targets)

source('1_scripts/sim_funcs.R') # contains functions for simulating thresholds
source('1_scripts/gen_funcs.R') # contains functions for generating data (generative modelling)
source('1_scripts/plot_funcs.R') # contains plotting functions

source('1_scripts/preprocessing_funcs.R') # for data processing
source('1_scripts/plot_games.R') 


# Set target-specific options such as packages.
tar_option_set(packages = "tidyverse")
setTheme() #

#start k
list(
  ###  simulate optimal w/alpha ###
  # specify  single par list
  tar_target(par_list,
             list(
               
               #vectors for multi-cond sim
               Bvec = 10, # the bonus received if the worker reaches his goal 
               thresholdVec = c(90, 100, 110), # the threshold above which the worker will get rewarded (goal). 
               l1Vec = 5, # the skill of the worker in the make-or-break task.
               vVec = c(.25, .5), # the reward of the worker in task a.
               sigmaVec = c(5, 10), #uncertainty performance
               alphaVec = 1,
               
               #how fine grain do we want?
               tt = 21, # the number of time units available
               n = 21,  # total number of steps
               perfBound = 150, # We truncate the distribution of performance between -M and M in order to simplify calculations (i.e., with the Wiener process, -inf to inf performance is possible)
               dy = 0.005 # performance step
             )
    ),

  #simulate multi
  tar_target(sims,
             do.call(sim_wrap, par_list),
             format = 'fst_tbl'),
  
  #plot
  tar_target(
    opt_plot,
    plot_sims(sims)
  ),


  ### DATA PILOT 1
  tar_target(all,
    getDat(exp_num='p1', type = all)
  )
)
