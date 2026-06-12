# This function does the following:
#     It takes in an event-based retro-sheet, and adds baserunner/out states
#     It then calculates the expected runs for each event
#     It saves the expected run matrix (erm_wide) to the global environment
#     It saves the retrosheet event data with run value features to global
#       environment as retro_event_run_value

get_run_expectancy_retro <- function(data = data){
    data <- data |> 
      mutate(runs_before = away_score_ct + home_score_ct,
             half_inning = paste(game_id, inn_ct, bat_home_id),
             runs_scored = 
               (bat_dest_id > 3) + (run1_dest_id > 3) + 
               (run2_dest_id > 3) + (run3_dest_id > 3))
    
    half_innings <- data |>
      group_by(half_inning) |>
      summarize(outs_inning = sum(event_outs_ct),
                runs_inning = sum(runs_scored),
                runs_start = first(runs_before),
                max_runs = runs_inning + runs_start)
    
    data <- data |>
      inner_join(half_innings, by = "half_inning") |>
      mutate(runs_roi = max_runs - runs_before)
    
    data <- data |>
      mutate(bases = 
               paste(ifelse(base1_run_id > '',1,0),
                     ifelse(base2_run_id > '',1,0),
                     ifelse(base3_run_id > '',1,0), sep = ""),
             state = paste(bases, outs_ct))
    
    data <- data |>
      mutate(is_runner1 =
               as.numeric(run1_dest_id==1 | bat_dest_id == 1),
             is_runner2 = 
               as.numeric(run1_dest_id == 2 | run2_dest_id == 2 |
                            bat_dest_id == 2),
             is_runner3 = 
               as.numeric(run1_dest_id == 3 | run2_dest_id == 3 |
                            run3_dest_id == 3 | bat_dest_id == 3),
             new_outs = outs_ct + event_outs_ct,
             new_bases = paste(is_runner1, is_runner2, is_runner3, sep = ""),
             new_state = paste(new_bases, new_outs))
    
    data <- data |>
      filter((state != new_state) | (runs_scored > 0))
    
    data_complete <- data |>
      filter(outs_inning == 3)
      
    erm <- data_complete |>
      group_by(bases, outs_ct) |> # same as grouping by state
      summarize(mean_run_value = mean(runs_roi))
    
    erm_wide <<- erm |> 
      pivot_wider(
        names_from = outs_ct, 
        values_from = mean_run_value, 
        names_prefix = "Outs="
      )
    
    data <- data |> 
      left_join(erm, join_by("bases", "outs_ct")) |> 
      rename(rv_start = mean_run_value) |> 
      left_join(
        erm, 
        join_by(new_bases == bases, new_outs == outs_ct)
      ) |> 
      rename(rv_end = mean_run_value) |> 
      replace_na(list(rv_end = 0)) |> 
      mutate(run_value = rv_end - rv_start + runs_scored)
    retro_event_run_value <<- data
}