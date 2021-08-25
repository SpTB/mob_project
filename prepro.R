
### single-file processing funcs

getMain <- function(file) {
  
  x = read_csv(file) %>%
    rename(subjID = run_id)
  
  ##check if there were any image problems
  if (sum(x$failed_images=='"')!= nrow(x)) {
    warning ('some images did not load correctly!')
  }
  
  thresholds = x %>%
    filter(trial_type %in% 'html-keyboard-response') %>%
    select(stimulus) %>%
    filter(str_detect(stimulus, "out of 40")) ##only game text stays
    
    
  ##extract thresholds per game
  goal = safe_pay = rep(NA, nrow(thresholds))
  for (i in 1:nrow(thresholds)) {
    goal[i] = substr(thresholds$stimulus[i], 
                     str_locate(pattern='Goal: ' , thresholds$stimulus[i])[2]+4,
                     str_locate(pattern='Goal: ' , thresholds$stimulus[i])[2] +6)
    safe_pay[i] = substr(thresholds$stimulus[i], 
                     str_locate(pattern='diate Payoff: ' , thresholds$stimulus[i])[2]+4,
                     str_locate(pattern='diate Payoff: ' , thresholds$stimulus[i])[2] +4)
  }
  goal = as.numeric(goal)
  safe_pay = ifelse(safe_pay=='2' ,0.25,0.50)
  
  dat = x %>%
    filter(trial_type == 'image-selector-bar' ) %>%
    select(subjID, points, sd, points2, rt) %>%
    filter(points!= '\"') %>% #remove empty rows
    mutate(points = as.numeric(points),
           sd = as.numeric(sd),
           points2 = as.numeric(points2),
           rt = as.numeric(rt),
           trial_num = row_number())
  
  ##add game and choice number vars
  dat$game = dat$thresh = dat$safe_pay = rep(1, nrow(dat))
  dat$choice_num = 1:nrow(dat)
  game = 1
  for (row in 1:(nrow(dat)-1)) {
    
    dat$thresh[row] = goal[dat$game[row]]
    dat$safe_pay[row] = safe_pay[dat$game[row]]
    if (dat$points2[row]>0 | dat$choice_num[row]==20) {
      game = game+1
      choice_num = 1
      trials_left = min(20, nrow(dat)-row)
      
      dat$game[(row+1):(row+trials_left)] = rep(game,trials_left)
      dat$choice_num[(row+1):(row+trials_left)] = 1:trials_left
    } 
  }
  
  dat$thresh[nrow(dat)] = dat$thresh[nrow(dat)-1] #fill in final row
  dat$safe_pay[nrow(dat)] = dat$safe_pay[nrow(dat)-1]
  
  #was the goal reached?
  dat = dat %>% 
    group_by(game) %>%
    mutate(there_yet = points>=thresh,
           when = which(there_yet)[1]) %>%
    ungroup()
  
  dat = dat %>% 
    group_by(game) %>%
    summarise(success = max(points>=thresh)) %>%
    left_join(dat) %>%
    select(-there_yet)
  
  return (dat)
  
}

getDemo <-function(file) {
  
  x = read_csv(file) %>%
    rename(subjID = run_id)
  demo = x %>%
    filter(trial_type %in% 'survey-html-form') %>%
    select(responses) 
  
  gender = substr(demo[1,], 
         str_locate(pattern='gender' , demo[1,])[2]+4,
         str_locate(pattern='gender' , demo[1,])[2] +4)
  
  #gender = substr(demo[1,], 12,12)
  age = as.numeric(substr(demo[1,], 
               str_locate(pattern='age', demo[1,])[2]+4,
               str_locate(pattern='age', demo[1,])[2]+5))
  #age = ifelse(gender=='f', substr(demo[1,], 27,28), substr(demo, 35,36))
  #handedness = ifelse(gender=='f', substr(demo, 53,53), substr(demo, 51,51))
  handedness = substr(demo[1,], 
                      str_locate(pattern='hand' , demo[1,])[2]+4,
                      str_locate(pattern='hand' , demo[1,])[2] +4)
  #prolificID = ifelse(gender=='f', substr(demo, 72,95), substr(demo, 70,93))
  prolificID = substr(demo[1,], 
                      str_locate(pattern='mail' , demo[1,])[2]+4,
                      str_locate(pattern='mail' , demo[1,])[2] +27)
  
  describe = substr(demo[2,], 12,nchar(as.character(demo[2,])))
  
  dem = tibble(gender, age, handedness, prolificID, describe)
  
  return(dem)
}

getTech <- function(file) {
  x = read_csv(file) %>%
    rename(subjID = run_id)
  
  tech_details = x %>%
    filter(row(x)==1) %>%
    select(subjID,  recorded_at, user_agent, device, ip,
           browser, browser_version, platform, #platform_version,
           source_code_version) #%>%
   # t() #transpose for ease of viewing?\
  
  return (tech_details)
}

### file combining functions
getDat <- function(exp_num = 'p1', type) {
  
  ##type: which type of data to preprocess: main, demo, tech or all
  
  path_pattern = paste("0_data/", exp_num, sep='')
  fi=list.files(path=path_pattern, pattern='.csv', full.names = T)
  if (type=='main') {
    all = map_df(fi, getMain)
  } else  if (type=='tech') {
    all = map_df(fi, getTech)
  } else if (type=='demo') {
    all = map_df(fi, getDemo)
    if (comments.txt) {
      write.table(all$describe, 'comments.txt')
    }
    
  } else if (type=='all') {
    all = list()
    all[[1]] = map_df(fi, getMain)
    all[[2]] = map_df(fi, getTech)
    all[[3]] = map_df(fi, getDemo)
  }
  
  return (all)
}


### summarising functions

groupMain = function(all, winnings.txt=F) {
  #groups data by game
  #all: result of getMain(type='main')
  #winnings.txt: whether to output a text file with all the winnings
  
  grouped = all %>% group_by(subjID, game) %>%
    summarise(
      sd = sd[1],
      thresh = thresh[1],
      success = max(success),
      when_hit = when[1],
      choices = max(choice_num),
      max_points = max(points),
      max_points2 = max(points2),            
    ) %>%
    mutate(when_give = ifelse(success==1, NA, choices))
  
    if (winnings.txt) {
      grouped %>% group_by(subjID) %>%
        summarise(success_total = sum(success),
                  safe_wins_total = sum(max_points2)) %>%
        mutate(pennies_total = success_total*10 + safe_wins_total) %>%
        mutate(prolificID = demo$prolificID) %>%
        write.table('winnings.txt')
    }
  return(grouped)
}




