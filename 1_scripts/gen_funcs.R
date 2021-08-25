####simulate generative models
expectationMyo <- function(timeSteps, lambda, sigma, B, delta){
  phi <- pnorm((delta - (lambda * timeSteps))/(sigma * sqrt(timeSteps)))
  sumRewards<- ((1 - phi) * B) 
  return(sumRewards)
}

softmax <- function(vals, temperature) {
  #softmax choice function
  #vals: vector [2] with estimates for the two options
  p1 <- exp(vals[1]*temperature)
  p2 <- exp(vals[2]*temperature)
  prob <- p1/sum(p1,p2)
  return(prob)
}

gen_game <- function(numChoices, drift, sigma, threshold, safe, bonus, pars, type = 'myo', gen_rewards = T) {
  # generates a single game of the mob task
  # numChoices: number of choices in a game
  #pars: list of parameters
  # type: 'myo' <- myopic strat; TODO: add strats: all-or-nothing', ctrl points
  require(dplyr)
  
  #unpack parameters
  alpha = pars$alpha
  sigmaEst = pars$sigmaEst
  tau = pars$tau
  
  choice = mob_prob = mob_ev = safe_ev = rep(0, numChoices)
  if (gen_rewards) rewards = rnorm(n = numChoices, mean = drift, sd = sigma)
  rewards_accum = cumsum(rewards)
  threshold_adj = threshold # adjusted threshold 
  for (t in 1:numChoices) {
    choices_left = numChoices + 1 - t
    
    ctrl_trials = c(1,numChoices/2)
    
    if (type!='myo' && (!t %in% ctrl_trials)) { # if non-critical trial, just repeat choice
      choice[t] = choice[t-1]
    } else {
      safe_ev[t] = safe * choices_left
      #myopic estimation of the mean
      myo_est = expectationMyo(timeSteps = choices_left,
                               lambda = drift,
                               sigma = sigma,
                               B = bonus,
                               delta = threshold_adj)^alpha
      #mob (risky) expected value
      mob_ev[t] = max(rnorm(n = 1, myo_est, sigmaEst),0) # make sure the expectation is not below 0
      #accumulated_rewards = accumulated_rewards + rewards[t]
      threshold_adj = threshold_adj - rewards[t] # myopic tresh
      
      mob_prob[t] = round(softmax(c(mob_ev[t] ,safe_ev[t]), tau),6) # probability of choosing the mob task
      choice[t] = rbinom(n = 1, size = 1, prob = mob_prob[t]) # 1 -> mob; 0 -> safe
    }
    
    
    if (choice[t]==0) break
  }
  game = tibble(choice_no = 1:numChoices,
                drift     = rep(drift,     numChoices),
                sigma     = rep(sigma,     numChoices),
                safe      = rep(safe,      numChoices),
                threshold = rep(threshold, numChoices),
                tau       = rep(tau,       numChoices),
                sigmaEst  = rep(sigmaEst,  numChoices),
                rewards   = rewards,
                rewards_accum = c(0,rewards_accum[1:numChoices-1]), # accumulated reward before choice
                mob_ev = mob_ev,
                safe_ev = safe_ev,
                mob_prob  = mob_prob,
                choose_mob = choice
  )
  return (game)
}
