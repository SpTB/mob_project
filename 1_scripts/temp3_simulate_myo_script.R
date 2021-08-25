##MODEL 
#funcs
#Myopic Expectation




#specs
numChoices = 20

#parameters
tau = 1 # choice error
sigmaEst = 1 # estimation error
alpha = 1 # risk-propensity

#variables
drift = 5 # 
sigma = 5 # variability
threshold = 100 #
bonus = 20 # bonus for reaching thresh
safe  = 0.25 # safe reward

#generate data
gen_dataset = function(numSubj, pars, repeats, thresholdVec, safeVec, driftVec, sigmaVec, alphaVec, numChoices, bonus, type) {
  
  for (i in 1:numSubj) {
    subji
  }
}
  
#accumulated_rewards = 0
gen_subj = function(repeats, pars, thresholdVec, safeVec, driftVec, sigmaVec, alphaVec, numChoices, bonus, type) {
  #generates single-subj data
  #pars: list of parameters
  
  combs = crossing(driftVec, thresholdVec, safeVec, sigmaVec, alphaVec)
  combs = do.call('rbind', replicate(n=repeats, combs, simplify=F))
  
  numGames = nrow(combs) # set the number of games
  
  for (g in 1:numGames) {
    gamei = gen_game(numChoices = numChoices, 
                     drift = combs$driftVec[g],
                     sigma = combs$sigmaVec[g],
                     threshold = combs$thresholdVec[g],
                     safe = combs$safeVec[g],
                     alpha = combs$alphaVec[g],
                     bonus = bonus, 
                     type = type)
    gamei$game_num = rep(g, nrow(gamei))
     
    if (g == 1) out = gamei else out = bind_rows(out, gamei)
  }
  return = out
}
subj = gen_subj(repeats = 3, thresholdVec = c(90, 100, 110), safeVec = c(.25,.5),
                driftVec = 5, sigmaVec = c(5,10),alphaVec = 1, numChoices = 20,bonus = 20,type = 'myo')
game = gen_game(numChoices = 20,drift = 5, sigma = 5, threshold = 100, safe = .25, alpha = 1, bonus = 20, type = 'ctrl_points')
                
####
# Alternatively...
# 
# init_prob = xxx
# calculate ev based on stats. e.g. am I 50% there after 10/20 trials?
#   am I there at trial x
# keep track of progress 
# expected_reward = 1/numChoices * threshold (e.g. 5 on any trial if thresh=100 and numChoices = 20)
# if (reward > expected_reward) then continue else re-eval
# if (reward < 0) -> ???
#   
#   ////
#   sum pe vs negative preds
# if pe
# 
# ///
#   bayesiam updating -> N(m(ev) ,sd(ev)) 
#ev -> just expectations of goal points ()

a = rep(0,1000)
for (i in 1:1000) {
  ai = cumsum(rnorm(2,5,10))
  
  a[i] = ifelse(max(ai)>10, 1, 0)
}
mean(a)
sd(a)

belief <- mean(outcome) + offset

