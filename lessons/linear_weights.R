
#function to return linear weights

linear_weights <- function(year){
  site = "https://raw.githubusercontent.com/maxtoki/baseball_R/"
  fields <- read_csv(file = paste(site, "master/data/fields.csv", sep =""))
  data <- read_csv(file = paste(site, "master/data/all",year,".csv", sep = ""),
                      col_names = pull(fields, Header),
                      na = character())
  

  data %>% 
    mutate(RUNS = AWAY_SCORE_CT + HOME_SCORE_CT,
           HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
           RUNS.SCORED = 
             (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + 
             (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) ->
    data
  
  
  #compute maximum total score for each half inning
  data %>%
    group_by(HALF.INNING) %>%
    summarize(Outs.Inning = sum(EVENT_OUTS_CT),
              Runs.Inning = sum(RUNS.SCORED),
              Runs.Start = first(RUNS),
              MAX.RUNS = Runs.Inning + Runs.Start) -> 
    half_innings
  
  #compute runs scored in remainder of the inning (ROI)
  data %>%
    inner_join(half_innings, by = "HALF.INNING") %>%
    mutate(RUNS.ROI = MAX.RUNS - RUNS) ->
    data
  
  data %>%
    mutate(BASES = 
             paste(ifelse(BASE1_RUN_ID > '',1,0),
                   ifelse(BASE2_RUN_ID > '',1,0),
                   ifelse(BASE3_RUN_ID > '',1,0), sep = ""),
           STATE = paste(BASES, OUTS_CT)) ->
    data
  
  #NRUNNER1 - indicator if 1st base is occupied after the play
  data %>%
    mutate(NRUNNER1 =
             as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID == 1),
           NRUNNER2 = 
             as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 |
                          BAT_DEST_ID == 2),
           NRUNNER3 = 
             as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 |
                          RUN3_DEST_ID == 3 | BAT_DEST_ID == 3),
           NOUTS = OUTS_CT + EVENT_OUTS_CT,
           NEW.BASES = paste(NRUNNER1,NRUNNER2, NRUNNER3, sep = ""),
           NEW.STATE = paste(NEW.BASES, NOUTS)) ->
    data
  
  #only consider plays where the runners on base, outs, or runs scored changed
  data %>%
    filter((STATE != NEW.STATE) | (RUNS.SCORED > 0)) ->
    data
  
  #use only complete half-innings
  data %>%
    filter(Outs.Inning == 3) -> dataComplete
  
  #calculate expected number of runs scored for remainder of inning
  #for each bases/outs situation
  dataComplete %>%
    group_by(STATE) %>%
    summarize(Mean = mean(RUNS.ROI)) %>%
    mutate(Outs = substr(STATE,5,5)) %>%
    arrange(Outs) -> RUNS
  
  RUNS_out = matrix(round(RUNS$Mean,2), 8,3)
  colnames(RUNS_out) = c("0 outs", "1 out", "2 outs")
  rownames(RUNS_out) = c("000","001","010","011",
                         "100","101","110", "111")
  
  data %>% 
    left_join(select(RUNS, -Outs), by = "STATE") %>%
    rename(Runs.State = Mean) %>% 
    left_join(select(RUNS, -Outs),
              by = c("NEW.STATE" = "STATE")) %>% 
    rename(Runs.New.State = Mean) %>% 
    replace_na(list(Runs.New.State = 0)) %>% 
    mutate(run_value = Runs.New.State - Runs.State + 
             RUNS.SCORED) -> data
  
  #get only home runs
  data %>% 
    mutate(Event = case_when(
      EVENT_CD == 20 ~ "1B",
      EVENT_CD == 21 ~ "2B",
      EVENT_CD == 22 ~ "3B",
      EVENT_CD == 23 ~ "HR",
      EVENT_CD %in% c(2,3,19) ~ "Out",
      EVENT_CD %in% c(14,15,16) ~ "BB.HBP"
    )) %>% 
    group_by(Event) %>% 
    summarize(weight = mean(run_value)) %>% 
    filter(!is.na(Event)) -> weights
  
  return(list(weights = weights, expectancy_matrix = RUNS_out))  
}
