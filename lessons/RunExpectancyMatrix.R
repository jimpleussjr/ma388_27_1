retro2011 <- retro2011 |> 
  mutate(runs_before = away_score_ct + home_score_ct,
         half_inning = paste(game_id, inn_ct, bat_home_id),
         runs_scored = 
           (bat_dest_id > 3) + (run1_dest_id > 3) + 
           (run2_dest_id > 3) + (run3_dest_id > 3))

half_innings <- retro2011 |>
  group_by(half_inning) |>
  summarize(outs_inning = sum(event_outs_ct),
            runs_inning = sum(runs_scored),
            runs_start = first(runs_before),
            max_runs = runs_inning + runs_start)

retro2011 <- retro2011 |>
  inner_join(half_innings, by = "half_inning") |>
  mutate(runs_roi = max_runs - runs_before)

retro2011 <- retro2011 |>
  mutate(bases = 
           paste(ifelse(base1_run_id > '',1,0),
                 ifelse(base2_run_id > '',1,0),
                 ifelse(base3_run_id > '',1,0), sep = ""),
         state = paste(bases, outs_ct))

retro2011 <- retro2011 |>
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

retro2011 <- retro2011 |>
  filter((state != new_state) | (runs_scored > 0))

retro2011_complete <- retro2011 |>
  filter(outs_inning == 3)

erm_2011 <- retro2011_complete |>
  group_by(bases, outs_ct) |> # same as grouping by state
  summarize(mean_run_value = mean(runs_roi))

erm_2011_wide <- erm_2011 |> 
  pivot_wider(
    names_from = outs_ct, 
    values_from = mean_run_value, 
    names_prefix = "Outs="
  )

retro2011 <- retro2011 |> 
  left_join(erm_2011, join_by("bases", "outs_ct")) |> 
  rename(rv_start = mean_run_value) |> 
  left_join(
    erm_2011, 
    join_by(new_bases == bases, new_outs == outs_ct)
  ) |> 
  rename(rv_end = mean_run_value) |> 
  replace_na(list(rv_end = 0)) |> 
  mutate(run_value = rv_end - rv_start + runs_scored)
