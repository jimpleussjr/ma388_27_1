
#function to return players Age, SLG, OBP, OPS
get_stats <- function(player.id){
  require(Lahman)
  Batting %>% 
    filter(playerID == player.id) %>% 
    inner_join(People, by = "playerID") %>% 
    mutate(birthyear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear),
           Age = yearID - birthyear,
           SLG = (H - X2B - X3B - HR +
             2 * X2B + 3*X3B + 4 * HR)/AB,
           OBP = (H + BB + HBP)/(AB + BB+ HBP + SF),
           OPS = SLG + OBP,
           AVG = H/AB,
           HR.rate = HR/AB,
           OB = (H + BB + HBP),
           PA = (AB + BB + HBP + SF)) %>% 
    select(playerID, Age, yearID, SLG, OBP, OPS, AVG, HR.rate, OB, PA, HR, AB)
}

#fits the quadratic model for career OPS trajectory from the text
fit_model <- function(d){
  fit <- lm(OPS ~ I(Age - 30) + I((Age - 30)^2),
            data = d)
  b <- coef(fit)
  Age.max <- 30 - (b[2]/(b[3]*2))
  Max <- b[1] - (b[2]^2/(4*b[3]))
  list(fit = fit, Age.max = Age.max, Max = Max)
}

#finds players most similar to player p
similar <- function(p, number=10){
  require(Lahman)
  vars = c("G", "AB", "R", "H", "X2B",
           "X3B", "HR", "RBI", "BB",
           "SO", "SB")
  Batting %>% 
    group_by(playerID) %>% 
    summarize_at(vars, sum, na.rm = TRUE) -> Career.totals
  
  Career.totals %>% 
    mutate(AVG = H/AB,
           SLG = (H - X2B - X3B - HR +
                    2 * X2B + 3*X3B + 4 * HR)/AB) -> Career.totals
  
  # determine which position each player played the most
  Fielding %>% 
    group_by(playerID) %>% 
    top_n(1, G) %>% 
    distinct(playerID, .keep_all = TRUE) %>% 
    select(playerID, POS) -> Fielding.POS
  
  Career.totals %>% 
    inner_join(Fielding.POS, by = "playerID") %>% 
    mutate(Value.POS = case_when(
      POS == "C" ~ 240,
      POS == "SS" ~ 168,
      POS == "2B" ~ 132,
      POS == "3B" ~ 84,
      POS == "OF" ~ 48,
      POS == "1B" ~ 12,
      TRUE ~ 0)) -> Career.totals
  
  Career.totals %>% filter(playerID == p) -> P
  Career.totals %>% 
    mutate(sim_score = 1000 -
             floor(abs(G - P$G) / 20) -
             floor(abs(AB - P$AB) / 75) -
             floor(abs(R - P$R) / 10) -
             floor(abs(H - P$H) / 15) -
             floor(abs(X2B - P$X2B) / 5) -
             floor(abs(X3B - P$X3B) / 4) -
             floor(abs(HR - P$HR) / 2) -
             floor(abs(RBI - P$RBI) / 10) -
             floor(abs(BB - P$BB) / 25) -
             floor(abs(SO - P$SO) / 150) -
             floor(abs(SB - P$SB) / 20) - 
             floor(abs(AVG - P$AVG) / 0.001) - 
             floor(abs(SLG - P$SLG) / 0.002) -
             abs(Value.POS - P$Value.POS)) %>% 
    arrange(desc(sim_score)) %>% 
    head(number)
}

plot_trajectories <- function(player, n.similar = 5, ncol = 2){
  require(Lahman)
  player = "Willie Mays"
  flnames <- unlist(strsplit(player, " "))
  
  People %>% 
    filter(nameFirst == flnames[1],
           nameLast == flnames[2]) %>% 
    select(playerID) -> player
  
  player.list <- player %>% 
    pull(playerID) %>% 
    similar(n.similar) %>% 
    pull(playerID)
  
  vars = c("G", "AB", "R", "H", "X2B",
           "X3B", "HR", "RBI", "SB", "CS",
           "BB", "SH","SF", "HBP")
  
  player.list %>% 
    map_df(get_stats) %>% 
    left_join(select(People, playerID, nameLast, nameFirst),
              by = "playerID") %>% 
    mutate(Name = paste(nameFirst, 
                        nameLast, 
                        sep = " ")) -> player.stats

  player.stats %>% 
    ggplot(aes(x = Age, y = OPS)) +
    geom_smooth(method = "lm",
                formula = y ~ x + I(x^2),
                size = 1.5) +
    facet_wrap( ~ Name, ncol = ncol) + theme_bw()
}
