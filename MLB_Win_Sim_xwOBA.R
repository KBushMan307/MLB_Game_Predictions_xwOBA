#install.packages(tidyverse)
#install.packages(rvest)
#install.packages(baseballr)
#install.packages(stringi)
#install.packages(tibble)
#install.packages(httr)
#install.packages(jsonlite)
#install.packages(RPostgres)
#install.packages(DBI)
#install.packages(knitr)
#install.packages(lubridate)
library(tidyverse)
library(rvest)
library(baseballr)
library(stringi)
library(tibble)
library(httr)
library(jsonlite)
library(RPostgres)
library(DBI)
library(knitr)
library(lubridate)



#conect to DB
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "railway",
  host     = "nozomi.proxy.rlwy.net",
  port     = 11470,
  user     = "postgres",
  password = "cudxFcbrmueliIyKTzLjPYNgijILiOPw",
  sslmode = 'require'
)



this_season <- as.numeric(format(Sys.Date(), "%Y"))
last_season <- this_season - 1
today <- format(Sys.Date(), "%Y-%m-%d")
#This number tells us how many games need to be played before this season and last season are weighted equally.
last_season_reference <- 40

teams <- mlb_teams() |>
  filter(sport_name == "Major League Baseball") |>
  select(team_full_name, team_abbreviation)

colnames(teams) <- c("Tm", "Team")

#necessary functions
logit <- function(p) log(p / (1 - p))
inv_logit <- function(x) exp(x) / (1 + exp(x))



key <- DBI::dbGetQuery(
  con,
  paste0("SELECT *
          FROM mlb_team_key")
)

key <- key |>
  select(Team, TeamName)



#get today's games and pitchers
url <- "https://statsapi.mlb.com/api/v1/schedule"

res <- GET(url, query = list(
  sportId = 1,
  date = today,
  hydrate = "probablePitcher"
))

data <- fromJSON(content(res, "text"), simplifyVector = FALSE)

# Extract pitchers
games <- data$dates[[1]]$games

matchups <- purrr::map_dfr(games, function(g) {
  data.frame(
    game_id = g$gamePk,
    home_team = g$teams$home$team$name,
    away_team = g$teams$away$team$name,
    home_pitcher = g$teams$home$probablePitcher$fullName %||% NA,
    away_pitcher = g$teams$away$probablePitcher$fullName %||% NA
  )
})

away_pitchers <- matchups |>
  select(game_id, away_team, away_pitcher)

colnames(away_pitchers) <- c("game_id", "team", "probablePitcher")

home_pitchers <- matchups |>
  select(game_id, home_team, home_pitcher)

colnames(home_pitchers) <- c("game_id", "team", "probablePitcher")

pitchers <- rbind(away_pitchers, home_pitchers)

pitchers$probablePitcher <- stri_trans_general(pitchers$probablePitcher, "Latin-ASCII")
pitchers$probablePitcher <- gsub("\\*", "",pitchers$probablePitcher)
pitchers$probablePitcher <- gsub("\\#", "",pitchers$probablePitcher)



#get batting data from last season
url <- paste("https://www.baseball-reference.com/leagues/majors/",
             last_season, "-standard-batting.shtml", sep = "")
table <- read_html(url)
Team_Stats <- data.frame(html_table(html_nodes(table, "table")[1], fill = TRUE)) |>
  filter(Tm != "" & Tm != "Tm" & Tm != "League Average") |>
  select(Tm, G, PA, H, X2B, X3B, HR, SB, CS, BB, OBP) |>
  mutate(across(-Tm, as.numeric))

Team_Stats$Walk <- Team_Stats$BB/Team_Stats$PA
Team_Stats$Homerun <- Team_Stats$HR/Team_Stats$PA
Team_Stats$Double <- Team_Stats$X2B/Team_Stats$PA
Team_Stats$Triple <- Team_Stats$X3B/Team_Stats$PA
Team_Stats$Out <- 1 - Team_Stats$OBP
Team_Stats$Single <- (Team_Stats$H - Team_Stats$X2B - Team_Stats$X3B - Team_Stats$HR)/
  Team_Stats$PA

Team_Stats$Tot <- Team_Stats$Single + Team_Stats$Double + Team_Stats$Triple +
  Team_Stats$Homerun + Team_Stats$Walk + Team_Stats$Out

Team_Stats$Reach_Error <- 1 - Team_Stats$Tot

Bat_ratios_prev <- Team_Stats |>
  select(Tm, G, Out, Single, Double, Triple, Homerun, Walk, Reach_Error)

Bat_ratios_prev$G <- last_season_reference

Bat_ratios_prev$Tm <- ifelse(Bat_ratios_prev$Tm == "Oakland Athletics",
                             "Athletics", Bat_ratios_prev$Tm)



#get baserunning data from last season
url <- paste("https://www.baseball-reference.com/leagues/majors/",
             last_season, "-baserunning-batting.shtml", sep = "")
table <- read_html(url)
Bsr_Stats <- data.frame(html_table(html_nodes(table, "table")[1], fill = TRUE)) |>
  filter(Tm != "" & Tm != "Tm" & Tm != "League Average") |>
  select(Tm, SBO, SB2, CS2, SB3, CS3, X1stS, X1stS2, X1stS3,
         X1stD, X1stD3, X1stDH, X2ndS, X2ndS3, X2ndSH) |>
  mutate(across(-Tm, as.numeric))

Bsr_Stats$Steal_2nd <- Bsr_Stats$SB2 + Bsr_Stats$CS2

Bsr_Stats$Steal_3rd <- Bsr_Stats$SB3 + Bsr_Stats$CS3

Bsr_Stats$Steal_Prop <- round(Bsr_Stats$Steal_2nd/Bsr_Stats$Steal_3rd, 0)

Bsr_Stats$Steal <- Bsr_Stats$Steal_2nd + Bsr_Stats$Steal_3rd

Bsr_Stats$Steal_Prob <- Bsr_Stats$Steal/Bsr_Stats$SBO

Bsr_Stats$Steals_3rd <- Bsr_Stats$Steal_Prob/(Bsr_Stats$Steal_Prop+1)

Bsr_Stats$Steals_2nd <- Bsr_Stats$Steals_3rd * Bsr_Stats$Steal_Prop

Bsr_Stats$Success_2nd <- Bsr_Stats$SB2/(Bsr_Stats$SB2 + Bsr_Stats$CS2)

Bsr_Stats$Success_3rd <- Bsr_Stats$SB3/(Bsr_Stats$SB3 + Bsr_Stats$CS3)

Bsr_Stats$First_to_2nd_single <- Bsr_Stats$X1stS2/Bsr_Stats$X1stS

Bsr_Stats$First_to_3rd_single <- Bsr_Stats$X1stS3/Bsr_Stats$X1stS

Bsr_Stats$First_to_Out_single <- 1 - Bsr_Stats$First_to_2nd_single -
  Bsr_Stats$First_to_3rd_single

Bsr_Stats$First_to_3rd_double <- Bsr_Stats$X1stD3/Bsr_Stats$X1stD

Bsr_Stats$First_to_Score_double <- Bsr_Stats$X1stDH/Bsr_Stats$X1stD

Bsr_Stats$First_to_Out_double <- 1 - Bsr_Stats$First_to_3rd_double -
  Bsr_Stats$First_to_Score_double

Bsr_Stats$Second_to_3rd_single <- Bsr_Stats$X2ndS3/Bsr_Stats$X2ndS

Bsr_Stats$Second_to_Score_single <- Bsr_Stats$X2ndSH/Bsr_Stats$X2ndS

Bsr_Stats$Second_to_Out_single <- 1 - Bsr_Stats$Second_to_3rd_single -
  Bsr_Stats$Second_to_Score_single

Bsr_ratios_prev <- Bsr_Stats |>
  select(Tm, Steals_2nd, Success_2nd, Steals_3rd, Success_3rd,
         First_to_2nd_single, First_to_3rd_single, First_to_Out_single,
         First_to_3rd_double, First_to_Score_double, First_to_Out_double,
         Second_to_3rd_single, Second_to_Score_single, Second_to_Out_single)

Bsr_ratios_prev$Tm <- ifelse(Bsr_ratios_prev$Tm == "Oakland Athletics",
                             "Athletics", Bsr_ratios_prev$Tm)

ratios_prev <- left_join(Bat_ratios_prev, Bsr_ratios_prev, by = "Tm")



plays <- DBI::dbGetQuery(
  con,
  paste0("SELECT game_date, player_name, pitcher, events, p_throws, home_team,
  away_team, inning, inning_topbot, game_pk, at_bat_number, pitch_number,
  home_score, away_score, bat_score, fld_score, post_home_score, post_away_score,
  post_bat_score, post_fld_score, estimated_woba_using_speedangle
          FROM mlb_pbp_", last_season)
)

events <- plays |>
  filter(is.na(events) == FALSE)

events <- events |>
  mutate(pitcher = str_trim(paste(
    str_split_fixed(player_name, ",", 2)[,2],
    str_split_fixed(player_name, ",", 2)[,1]
  )))

events$pitcher <- stri_trans_general(events$pitcher, "Latin-ASCII")

events$bat_team <- ifelse(events$inning_topbot == "Top",
                          events$away_team, events$home_team)

events$pitch_team <- ifelse(events$inning_topbot == "Top",
                            events$home_team, events$away_team)

events <- events |>
  arrange(game_pk, pitch_team, inning, pitch_number) |>
  group_by(game_pk, pitch_team) |>
  mutate(
    starter_pitcher = first(pitcher),
    pitcher_role = if_else(pitcher == starter_pitcher, "starter", "bullpen")
  ) |>
  ungroup()

Pit_xwOBA_prev <- events |>
  arrange(game_pk, pitch_team, inning, pitch_number) |>
  group_by(game_pk, pitcher) |>
  summarize(
    pitch_team = first(pitch_team),
    date = first(game_date),
    BF = n(),
    xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
    starter_pitcher = first(starter_pitcher),
    pitcher_role = first(pitcher_role),
    .groups = "drop"
  ) |>
  arrange(pitcher, date)

Pit_xwOBA_prev$pitcher <- ifelse(Pit_xwOBA_prev$pitcher_role == "starter",
                                 Pit_xwOBA_prev$pitcher,
                                 paste(Pit_xwOBA_prev$pitch_team,
                                       Pit_xwOBA_prev$pitcher_role,
                                       sep = "_"))

av_xwOBA_prev <- sum(Pit_xwOBA_prev$xwOBA * Pit_xwOBA_prev$BF, na.rm = TRUE) /
  sum(Pit_xwOBA_prev$BF, na.rm = TRUE)

BP_xwOBA_prev <- Pit_xwOBA_prev |>
  filter(grepl("_bullpen", pitcher))

BP_xwOBA_prev <- BP_xwOBA_prev |>
  group_by(pitch_team) |>
  summarize(
    xwOBA_last = sum(xwOBA * BF, na.rm = TRUE) / sum(BF, na.rm = TRUE)
  ) |>
  ungroup()

colnames(BP_xwOBA_prev)[1] <- "Team"

Pit_xwOBA_prev <- Pit_xwOBA_prev |>
  filter(!grepl("_bullpen", pitcher))

Pit_xwOBA_prev <- Pit_xwOBA_prev |>
  group_by(pitcher) |>
  summarize(
    Team = last(pitch_team),
    xwOBA_last = sum(xwOBA * BF, na.rm = TRUE) / sum(BF, na.rm = TRUE)
  ) |>
  ungroup()



#get pitcher data from last season
url <- paste("https://www.baseball-reference.com/leagues/majors/",
             last_season, "-standard-pitching.shtml", sep = "")
table <- read_html(url)
Pit_Stats <- data.frame(html_table(html_nodes(table, "table")[2], fill = TRUE)) |>
  select(Player, Team, G, IP) |>
  mutate(across(-c(Player, Team), as.numeric))

Pit_Stats <- Pit_Stats |>
  group_by(Player) |>
  mutate(
    # Identify "multi-team" labels
    multi_tm = Team %in% c("2TM", "3TM", "4TM", "5TM"),
    
    # Extract latest non-multi-team team within the player history
    last_real_team = if (any(!multi_tm)) {
      last(Team[!multi_tm])
    } else {
      NA_character_
    },
    
    # Replace multi-team rows with the real team
    Team = if_else(multi_tm, last_real_team, Team)
  ) |>
  select(-multi_tm, -last_real_team) |>
  ungroup()

Pit_Stats <- Pit_Stats |>
  distinct(Player, .keep_all = TRUE) |>
  mutate(Hand = if_else(grepl("\\*", Player), "L", "R"))

Pit_Stats$Player <- stri_trans_general(Pit_Stats$Player, "Latin-ASCII")
Pit_Stats$Player <- gsub("\\*", "",Pit_Stats$Player)
Pit_Stats$Player <- gsub("\\#", "",Pit_Stats$Player)

Pit_Stats$IP.G <- Pit_Stats$IP/Pit_Stats$G

Pit_Stats_prev <- Pit_Stats

Pit_Stats_prev$Season <- last_season

Pit_Stats_prev <- Pit_Stats_prev |>
  mutate(
    IP = floor(IP) + (IP - floor(IP)) * 10 / 3,
    IP.G = IP / G
  )

Pit_Stats_prev <- Pit_Stats_prev |>
  select(Player, G, IP.G)

colnames(Pit_Stats_prev) <- c("Player", "G_last", "IP.G_last")

Pit_Stats_prev <- left_join(Pit_Stats_prev, Pit_xwOBA_prev,
                            by = c("Player" = "pitcher"))

Pit_Stats_prev <- na.omit(Pit_Stats_prev)



#get last season bullpen stats
url <- paste0("https://www.mlb.com/stats/team/pitching/whip/",
              last_season, "?split=rp&sortState=asc")

table <- read_html(url)

BP_Stats_prev <- data.frame(html_table(html_nodes(table, "table")[[1]],
                                       fill =TRUE))

BP_Stats_prev <- BP_Stats_prev[,c(1, 12)]

colnames(BP_Stats_prev) <- c("Team", "IP_last")

BP_Stats_prev <- BP_Stats_prev |>
  mutate(
    IP_last = floor(IP_last) + (IP_last - floor(IP_last)) * 10 / 3,
  )

for(i in 1:nrow(BP_Stats_prev)){
  if(grepl("Diamondbacks",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Arizona Diamondbacks"
  }
  else if(grepl("Braves",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Atlanta Braves"
  }
  else if(grepl("Orioles",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Baltimore Orioles"
  }
  else if(grepl("Cubs",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Chicago Cubs"
  }
  else if(grepl("White Sox",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Chicago White Sox"
  }
  else if(grepl("Reds",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Cincinnati Reds"
  }
  else if(grepl("Guardians",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Cleveland Guardians"
  }
  else if(grepl("Rockies",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Colorado Rockies"
  }
  else if(grepl("Tigers",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Detroit Tigers"
  }
  else if(grepl("Astros",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Houston Astros"
  }
  else if(grepl("Royals",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Kansas City Royals"
  }
  else if(grepl("Angels",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Los Angeles Angels"
  }
  else if(grepl("Dodgers",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Los Angeles Dodgers"
  }
  else if(grepl("Marlins",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Miami Marlins"
  }
  else if(grepl("Brewers",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Milwaukee Brewers"
  }
  else if(grepl("Twins",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Minnesota Twins"
  }
  else if(grepl("Mets",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "New York Mets"
  }
  else if(grepl("Yankees",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "New York Yankees"
  }
  else if(grepl("Athletics",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Athletics"
  }
  else if(grepl("Phillies",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Philadelphia Phillies"
  }
  else if(grepl("Pirates",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Pittsburgh Pirates"
  }
  else if(grepl("Padres",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "San Diego Padres"
  }
  else if(grepl("Mariners",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Seattle Mariners"
  }
  else if(grepl("Giants",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "San Francisco Giants"
  }
  else if(grepl("Cardinals",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "St. Louis Cardinals"
  }
  else if(grepl("Rays",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Tampa Bay Rays"
  }
  else if(grepl("Rangers",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Texas Rangers"
  }
  else if(grepl("Blue Jays",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Toronto Blue Jays"
  }
  else if(grepl("Nationals",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Washington Nationals"
  }
  else if(grepl("Red Sox",BP_Stats_prev[i,1])){
    BP_Stats_prev$Team[i] <- "Boston Red Sox"
  }
}

colnames(BP_Stats_prev)[1] <- "TeamName"

BP_Stats_prev <- left_join(BP_Stats_prev, key,
                           by = "TeamName")

BP_Stats_prev <- left_join(BP_Stats_prev, BP_xwOBA_prev,
                           by = "Team")

BP_Stats_prev <- na.omit(BP_Stats_prev)

colnames(BP_Stats_prev)[4] <- "RxwOBA_last"



#get GIDP stats
url <- paste0("https://www.baseball-reference.com/leagues/majors/",
              last_season, "-situational-batting.shtml")

table <- read_html(url)

GIDP_Stats_prev <- data.frame(html_table(html_nodes(table, "table")[[1]],
                                         fill =TRUE))

GIDP_Stats_prev <- GIDP_Stats_prev[,c(1, 24:25)]

colnames(GIDP_Stats_prev) <- c("Tm", "GIDP_opp_last", "GIDP_last")

GIDP_Stats_prev <- GIDP_Stats_prev |>
  filter(Tm != "" & Tm != "Tm" & Tm != "League Average") |>
  mutate(across(-Tm, as.numeric)) |>
  mutate(GIDP_prob_last = GIDP_last/GIDP_opp_last)

GIDP_Stats_prev$Tm <- ifelse(GIDP_Stats_prev$Tm == "Oakland Athletics",
                             "Athletics", GIDP_Stats_prev$Tm)

GIDP_Stats_prev <- GIDP_Stats_prev |>
  mutate(across(-Tm, as.numeric))



#get batting data from this season
Bat_ratios <- tryCatch({
  
  url <- paste0(
    "https://www.baseball-reference.com/leagues/majors/",
    this_season,
    "-standard-batting.shtml"
  )
  
  table <- read_html(url)
  
  Team_Stats <- data.frame(
    html_table(html_nodes(table, "table")[[1]], fill = TRUE)
  ) |>
    filter(Tm != "" & Tm != "Tm" & Tm != "League Average") |>
    select(Tm, G, PA, H, X2B, X3B, HR, SB, CS, BB, OBP) |>
    mutate(across(-Tm, as.numeric))
  
  Team_Stats |>
    mutate(
      Walk       = BB / PA,
      Homerun    = HR / PA,
      Double     = X2B / PA,
      Triple     = X3B / PA,
      Out        = 1 - OBP,
      Single     = (H - X2B - X3B - HR) / PA,
      Tot        = Single + Double + Triple + Homerun + Walk + Out,
      Reach_Error = 1 - Tot
    ) |>
    select(Tm, G, Out, Single, Double, Triple, Homerun, Walk, Reach_Error)
  
}, error = function(e) {
  
  # Fallback: empty data frame with correct columns
  tibble(
    Tm = character(),
    G = numeric(),
    Out = numeric(),
    Single = numeric(),
    Double = numeric(),
    Triple = numeric(),
    Homerun = numeric(),
    Walk = numeric(),
    Reach_Error = numeric()
  )
  
})

Bat_ratios$Tm <- ifelse(Bat_ratios$Tm == "Oakland Athletics",
                        "Athletics", Bat_ratios$Tm)



#get baserunning data from this season
Bsr_ratios <- tryCatch({
  
  url <- paste0(
    "https://www.baseball-reference.com/leagues/majors/",
    this_season,
    "-baserunning-batting.shtml"
  )
  
  table <- read_html(url)
  
  Bsr_Stats <- data.frame(
    html_table(html_nodes(table, "table")[[1]], fill = TRUE)
  ) |>
    filter(Tm != "" & Tm != "Tm" & Tm != "League Average") |>
    select(
      Tm, SBO, SB2, CS2, SB3, CS3,
      X1stS, X1stS2, X1stS3,
      X1stD, X1stD3, X1stDH,
      X2ndS, X2ndS3, X2ndSH
    ) |>
    mutate(across(-Tm, as.numeric)) |>
    mutate(
      Steal_2nd = SB2 + CS2,
      Steal_3rd = SB3 + CS3,
      Steal_Prop = ifelse(Steal_3rd == 0, 1, round(Steal_2nd / Steal_3rd, 0)),
      Steal = Steal_2nd + Steal_3rd,
      Steal_Prob = ifelse(SBO == 0, 0, Steal / SBO),
      Steals_3rd = Steal_Prob / (Steal_Prop + 1),
      Steals_2nd = ifelse(Steals_3rd == 0, Steal_Prob, Steals_3rd * Steal_Prop),
      Success_2nd = ifelse(SB2 == 0, 0, SB2 / (SB2 + CS2)),
      Success_3rd = ifelse(SB3 == 0, 0, SB3 / (SB3 + CS3)),
      
      First_to_2nd_single = ifelse(X1stS == 0, 0, X1stS2 / X1stS),
      First_to_3rd_single = ifelse(X1stS == 0, 0, X1stS3 / X1stS),
      First_to_Out_single = 1 - First_to_2nd_single - First_to_3rd_single,
      
      First_to_3rd_double = ifelse(X1stD == 0, 0, X1stD3 / X1stD),
      First_to_Score_double = ifelse(X1stD == 0, 0, X1stDH / X1stD),
      First_to_Out_double = 1 - First_to_3rd_double - First_to_Score_double,
      
      Second_to_3rd_single = ifelse(X2ndS == 0, 0, X2ndS3 / X2ndS),
      Second_to_Score_single = ifelse(X2ndS == 0, 0, X2ndSH / X2ndS),
      Second_to_Out_single = 1 - Second_to_3rd_single - Second_to_Score_single
    ) |>
    select(
      Tm, Steals_2nd, Success_2nd, Steals_3rd, Success_3rd,
      First_to_2nd_single, First_to_3rd_single, First_to_Out_single,
      First_to_3rd_double, First_to_Score_double, First_to_Out_double,
      Second_to_3rd_single, Second_to_Score_single, Second_to_Out_single
    )
  
}, error = function(e) {
  
  # Empty fallback with correct structure
  tibble(
    Tm = character(),
    Steals_2nd = numeric(),
    Success_2nd = numeric(),
    Steals_3rd = numeric(),
    Success_3rd = numeric(),
    First_to_2nd_single = numeric(),
    First_to_3rd_single = numeric(),
    First_to_Out_single = numeric(),
    First_to_3rd_double = numeric(),
    First_to_Score_double = numeric(),
    First_to_Out_double = numeric(),
    Second_to_3rd_single = numeric(),
    Second_to_Score_single = numeric(),
    Second_to_Out_single = numeric()
  )
  
})

Bsr_ratios$Tm <- ifelse(Bsr_ratios$Tm == "Oakland Athletics",
                        "Athletics", Bsr_ratios$Tm)

ratios <- left_join(Bat_ratios, Bsr_ratios, by = "Tm")



plays <- DBI::dbGetQuery(
  con,
  paste0("SELECT game_date, player_name, pitcher, events, p_throws, home_team,
  away_team, inning, inning_topbot, game_pk, at_bat_number, pitch_number,
  home_score, away_score, bat_score, fld_score, post_home_score, post_away_score,
  post_bat_score, post_fld_score, estimated_woba_using_speedangle
          FROM mlb_pbp_", this_season)
)

events <- plays |>
  filter(is.na(events) == FALSE)

events <- events |>
  mutate(pitcher = str_trim(paste(
    str_split_fixed(player_name, ",", 2)[,2],
    str_split_fixed(player_name, ",", 2)[,1]
  )))

events$pitcher <- stri_trans_general(events$pitcher, "Latin-ASCII")

events$bat_team <- ifelse(events$inning_topbot == "Top",
                          events$away_team, events$home_team)

events$pitch_team <- ifelse(events$inning_topbot == "Top",
                            events$home_team, events$away_team)

events <- events |>
  arrange(game_pk, pitch_team, inning, pitch_number) |>
  group_by(game_pk, pitch_team) |>
  mutate(
    starter_pitcher = first(pitcher),
    pitcher_role = if_else(pitcher == starter_pitcher, "starter", "bullpen")
  ) |>
  ungroup()

Pit_xwOBA <- events |>
  arrange(game_pk, pitch_team, inning, pitch_number) |>
  group_by(game_pk, pitcher) |>
  summarize(
    pitch_team = first(pitch_team),
    date = first(game_date),
    BF = n(),
    xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
    starter_pitcher = first(starter_pitcher),
    pitcher_role = first(pitcher_role),
    .groups = "drop"
  ) |>
  arrange(pitcher, date)

Pit_xwOBA$pitcher <- ifelse(Pit_xwOBA$pitcher_role == "starter",
                            Pit_xwOBA$pitcher,
                            paste(Pit_xwOBA$pitch_team,
                                  Pit_xwOBA$pitcher_role,
                                  sep = "_"))

av_xwOBA <- sum(Pit_xwOBA$xwOBA * Pit_xwOBA$BF, na.rm = TRUE) /
  sum(Pit_xwOBA$BF, na.rm = TRUE)

BP_xwOBA <- Pit_xwOBA |>
  filter(grepl("_bullpen", pitcher))

BP_xwOBA <- BP_xwOBA |>
  group_by(pitch_team) |>
  summarize(
    xwOBA = sum(xwOBA * BF, na.rm = TRUE) / sum(BF, na.rm = TRUE)
  ) |>
  ungroup()

colnames(BP_xwOBA)[1] <- "Team"

Pit_xwOBA <- Pit_xwOBA |>
  filter(!grepl("_bullpen", pitcher))

Pit_xwOBA <- Pit_xwOBA |>
  group_by(pitcher) |>
  summarize(
    Team = last(pitch_team),
    xwOBA = sum(xwOBA * BF, na.rm = TRUE) / sum(BF, na.rm = TRUE)
  ) |>
  ungroup()



#get pitcher data from this season
Pit_Stats <- tryCatch({
  
  url <- paste0(
    "https://www.baseball-reference.com/leagues/majors/",
    this_season,
    "-standard-pitching.shtml"
  )
  
  table <- read_html(url)
  
  Pit_Stats <- data.frame(
    html_table(html_nodes(table, "table")[[2]], fill = TRUE)
  ) |>
    select(Player, Team, G, IP) |>
    mutate(across(-c(Player, Team), as.numeric)) |>
    group_by(Player) |>
    mutate(
      multi_tm = Team %in% c("2TM", "3TM", "4TM", "5TM"),
      last_real_team = if (any(!multi_tm)) {
        last(Team[!multi_tm])
      } else {
        NA_character_
      },
      Team = if_else(multi_tm, last_real_team, Team)
    ) |>
    select(-multi_tm, -last_real_team) |>
    ungroup() |>
    distinct(Player, .keep_all = TRUE) |>
    mutate(
      Hand = if_else(grepl("\\*", Player), "L", "R"),
      Player = stri_trans_general(Player, "Latin-ASCII"),
      Player = gsub("\\*", "", Player),
      Player = gsub("\\#", "", Player),
      IP.G = IP / G
    )
  
}, error = function(e) {
  
  # Empty fallback with correct structure
  tibble(
    Player = character(),
    Team = character(),
    G = numeric(),
    IP = numeric(),
    Hand = character(),
    IP.G = numeric()
  )
  
})

Pit_Stats$Season <- this_season

Pit_Stats <- Pit_Stats |>
  mutate(
    IP = floor(IP) + (IP - floor(IP)) * 10 / 3,
    IP.G = IP / G
  )

Pit_Stats <- Pit_Stats |>
  select(Player, G, IP.G)

Pit_Stats <- left_join(Pit_Stats, Pit_xwOBA,
                       by = c("Player" = "pitcher"))

Pit_Stats <- na.omit(Pit_Stats)



#get this season bullpen stats
BP_Stats <- tryCatch({
  
  url <- paste0(
    "https://www.mlb.com/stats/team/pitching/whip/",
    this_season,
    "?split=rp&sortState=asc"
  )
  
  table <- read_html(url)
  
  BP_Stats <- data.frame(
    html_table(html_nodes(table, "table")[[1]], fill = TRUE)
  )
  
  BP_Stats <- BP_Stats[, c(1, 12)]
  colnames(BP_Stats) <- c("Team", "IP")
  
  BP_Stats <- BP_Stats |>
    mutate(
      IP = floor(IP) + (IP - floor(IP)) * 10 / 3
    )
  
  for (i in 1:nrow(BP_Stats)) {
    if (grepl("Diamondbacks", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Arizona Diamondbacks"
    } else if (grepl("Braves", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Atlanta Braves"
    } else if (grepl("Orioles", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Baltimore Orioles"
    } else if (grepl("Cubs", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Chicago Cubs"
    } else if (grepl("White Sox", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Chicago White Sox"
    } else if (grepl("Reds", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Cincinnati Reds"
    } else if (grepl("Guardians", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Cleveland Guardians"
    } else if (grepl("Rockies", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Colorado Rockies"
    } else if (grepl("Tigers", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Detroit Tigers"
    } else if (grepl("Astros", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Houston Astros"
    } else if (grepl("Royals", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Kansas City Royals"
    } else if (grepl("Angels", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Los Angeles Angels"
    } else if (grepl("Dodgers", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Los Angeles Dodgers"
    } else if (grepl("Marlins", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Miami Marlins"
    } else if (grepl("Brewers", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Milwaukee Brewers"
    } else if (grepl("Twins", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Minnesota Twins"
    } else if (grepl("Mets", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "New York Mets"
    } else if (grepl("Yankees", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "New York Yankees"
    } else if (grepl("Athletics", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Athletics"
    } else if (grepl("Phillies", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Philadelphia Phillies"
    } else if (grepl("Pirates", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Pittsburgh Pirates"
    } else if (grepl("Padres", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "San Diego Padres"
    } else if (grepl("Mariners", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Seattle Mariners"
    } else if (grepl("Giants", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "San Francisco Giants"
    } else if (grepl("Cardinals", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "St. Louis Cardinals"
    } else if (grepl("Rays", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Tampa Bay Rays"
    } else if (grepl("Rangers", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Texas Rangers"
    } else if (grepl("Blue Jays", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Toronto Blue Jays"
    } else if (grepl("Nationals", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Washington Nationals"
    } else if (grepl("Red Sox", BP_Stats[i, 1])) {
      BP_Stats$Team[i] <- "Boston Red Sox"
    }
  }
  
  BP_Stats
  
}, error = function(e) {
  
  # Empty fallback with correct schema
  tibble(
    Team = character(),
    IP = numeric()
  )
  
})

colnames(BP_Stats)[1] <- "TeamName"

BP_Stats <- left_join(BP_Stats, key,
                      by = "TeamName")

BP_Stats <- left_join(BP_Stats, BP_xwOBA,
                      by = "Team")

BP_Stats <- na.omit(BP_Stats)

colnames(BP_Stats)[4] <- "RxwOBA"



#get GIDP stats for this season
GIDP_Stats <- tryCatch({
  
  url <- paste0(
    "https://www.baseball-reference.com/leagues/majors/",
    this_season, "-situational-batting.shtml"
  )
  
  table <- read_html(url)
  
  GIDP_Stats <- data.frame(
    html_table(html_nodes(table, "table")[[1]], fill = TRUE)
  )
  
  GIDP_Stats <- GIDP_Stats[, c(1, 24:25)]
  colnames(GIDP_Stats) <- c("Tm", "GIDP_opp", "GIDP")
  
  GIDP_Stats |>
    filter(Tm != "" & Tm != "Tm" & Tm != "League Average") |>
    mutate(across(-Tm, as.numeric)) |>
    mutate(GIDP_prob = GIDP / GIDP_opp)
  
}, error = function(e) {
  
  # Return an EMPTY but STRUCTURALLY CORRECT dataframe
  data.frame(
    Tm = character(),
    GIDP_opp = numeric(),
    GIDP = numeric(),
    GIDP_prob = numeric()
  )
  
})

GIDP_Stats$Tm <- ifelse(GIDP_Stats$Tm == "Oakland Athletics",
                        "Athletics", GIDP_Stats$Tm)

GIDP_Stats <- GIDP_Stats |>
  mutate(across(-Tm, as.numeric))



#if first game of season, use last season's numbers
if(nrow(ratios) < 30){
  ratios <- ratios_prev
} else { #use a share of last season's numbers to help shape this season
  ratios$Single <- (ratios$Single*ratios$G + ratios_prev$Single*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$Double <- (ratios$Double*ratios$G + ratios_prev$Double*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$Triple <- (ratios$Triple*ratios$G + ratios_prev$Triple*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$Homerun <- (ratios$Homerun*ratios$G + ratios_prev$Homerun*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$Walk <- (ratios$Walk*ratios$G + ratios_prev$Walk*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$Reach_Error <- (ratios$Reach_Error*ratios$G +
                           ratios_prev$Reach_Error*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$Out <- 1 - (ratios$Single + ratios$Double + ratios$Triple +
                       ratios$Homerun + ratios$Walk + ratios$Reach_Error)
  
  ratios$Steals_2nd <- (ratios$Steals_2nd*ratios$G +
                          ratios_prev$Steals_2nd*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$Success_2nd <- (ratios$Success_2nd*ratios$G +
                           ratios_prev$Success_2nd*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$Steals_3rd <- (ratios$Steals_3rd*ratios$G +
                          ratios_prev$Steals_3rd*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$Success_3rd <- (ratios$Success_3rd*ratios$G +
                           ratios_prev$Success_3rd*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$First_to_2nd_single <- (ratios$First_to_2nd_single*ratios$G +
                                   ratios_prev$First_to_2nd_single*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$First_to_3rd_single <- (ratios$First_to_3rd_single*ratios$G +
                                   ratios_prev$First_to_3rd_single*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$First_to_Out_single <- 1 - (ratios$First_to_2nd_single +
                                       ratios$First_to_3rd_single)
  ratios$First_to_3rd_double <- (ratios$First_to_3rd_double*ratios$G +
                                   ratios_prev$First_to_3rd_double*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$First_to_Score_double <- (ratios$First_to_Score_double*ratios$G +
                                     ratios_prev$First_to_Score_double*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$First_to_Out_double <- 1 - (ratios$First_to_3rd_double +
                                       ratios$First_to_Score_double)
  ratios$Second_to_3rd_single <- (ratios$Second_to_3rd_single*ratios$G +
                                    ratios_prev$Second_to_3rd_single*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$Second_to_Score_single <- (ratios$Second_to_Score_single*ratios$G +
                                      ratios_prev$Second_to_Score_single*ratios_prev$G)/
    (ratios$G + ratios_prev$G)
  ratios$Second_to_Out_single <- 1 - (ratios$Second_to_3rd_single +
                                        ratios$Second_to_Score_single)
}



#set average xwOBA
if(nrow(Pit_Stats) < 10){
  av_xwOBA <- av_xwOBA_prev
} else{
  av_xwOBA <- (av_xwOBA*max(ratios$G) + av_xwOBA_prev*20)/(max(ratios$G)+20)
}

Pit_Stats <- Pit_Stats |>
  select(-Team)

Pit_Stats_prev <- Pit_Stats_prev |>
  select(-Team)

#if first start of season use stats from last year
if(nrow(Pit_Stats) < 30){
  Pit_Stats <- Pit_Stats_prev
  Pit_Stats$xwOBA_adj <- Pit_Stats$xwOBA_last
  Pit_Stats$IP.G_adj <- Pit_Stats$IP.G_last
  New <- data.frame(Player = "New", G_last = 5, xwOBA_last = av_xwOBA,
                    IP.G_last = 5, xwOBA_adj = av_xwOBA, IP.G_adj = 5)
  
  New <- New |>
    distinct()
  
  Pit_Stats <- rbind(Pit_Stats, New)
} else{
  Pit_Stats <- left_join(Pit_Stats, Pit_Stats_prev, by = "Player")
  Pit_Stats$xwOBA_last <- ifelse(is.na(Pit_Stats$xwOBA_last),
                                 av_xwOBA, Pit_Stats$xwOBA_last)
  Pit_Stats$IP.G_last <- ifelse(is.na(Pit_Stats$IP.G_last),
                                5, Pit_Stats$IP.G_last)
  Pit_Stats$G_last <- 10
  Pit_Stats$xwOBA_adj <- (Pit_Stats$xwOBA_last*Pit_Stats$G_last +
                            Pit_Stats$xwOBA*Pit_Stats$G)/
    (Pit_Stats$G_last + Pit_Stats$G)
  Pit_Stats$IP.G_adj <- (Pit_Stats$IP.G_last*Pit_Stats$G_last +
                           Pit_Stats$IP.G*Pit_Stats$G)/
    (Pit_Stats$G_last + Pit_Stats$G)
  
  New <- data.frame(Player = "New", G = "30", xwOBA = av_xwOBA, IP.G = 5,
                    G_last = 5, xwOBA_last = av_xwOBA, IP.G_last = 5,
                    xwOBA_adj = av_xwOBA, IP.G_adj = 5)
  
  New <- New |>
    distinct()
  
  Pit_Stats <- rbind(Pit_Stats, New)
}



#if first start of season use stats from last year
if(nrow(BP_Stats) < 30){
  BP_Stats <- BP_Stats_prev
  BP_Stats$RxwOBA_adj <- BP_Stats$RxwOBA_last
} else{
  BP_Stats <- left_join(BP_Stats, BP_Stats_prev, by = c("Team", "TeamName"))
  BP_Stats$RxwOBA_last <- ifelse(is.na(BP_Stats$RxwOBA_last),
                                 av_xwOBA, BP_Stats$RxwOBA_last)
  BP_Stats$IP_last <- 50
  BP_Stats$RxwOBA_adj <- (BP_Stats$RxwOBA_last*BP_Stats$IP_last +
                            BP_Stats$RxwOBA*BP_Stats$IP)/
    (BP_Stats$IP_last + BP_Stats$IP)
}



#if first start of season use stats from last year
if(nrow(GIDP_Stats) < 30){
  GIDP_Stats <- GIDP_Stats_prev
  GIDP_Stats$GIDP_adj <- GIDP_Stats$GIDP_prob_last
} else{
  GIDP_Stats <- left_join(GIDP_Stats, GIDP_Stats_prev, by = "Tm")
  GIDP_Stats$GIDP_opp_last <- 100
  GIDP_Stats$GIDP_adj <- (GIDP_Stats$GIDP_prob_last*GIDP_Stats$GIDP_opp_last +
                            GIDP_Stats$GIDP_prob*GIDP_Stats$GIDP_opp)/
    (GIDP_Stats$GIDP_opp_last + GIDP_Stats$GIDP_opp)
}

GIDP_Stats <- GIDP_Stats |>
  select(Tm, GIDP_adj)



playball <- function(ID, numgames){
  #testing
  #ID <- game number
  #numgames <- 1
  
  Game <- matchups |>
    filter(game_id == ID)
  
  Away_Team <- as.character(Game$away_team)
  Home_Team <- as.character(Game$home_team)
  
  away_ratios <- ratios |>
    filter(Tm == Away_Team)
  
  home_ratios <- ratios |>
    filter(Tm == Home_Team)
  
  starters <- pitchers |>
    filter(game_id == ID)
  
  away_starter <- starters |>
    filter(team == Away_Team) |>
    select(probablePitcher)
  
  away_starter <- away_starter$probablePitcher
  
  home_starter <- starters |>
    filter(team == Home_Team) |>
    select(probablePitcher)
  
  home_starter <- home_starter$probablePitcher
  
  away_starter <- ifelse(away_starter %in% Pit_Stats$Player |
                           away_starter %in% Pit_Stats_prev$Player,
                         away_starter, "New")
  
  home_starter <- ifelse(home_starter %in% Pit_Stats$Player |
                           home_starter %in% Pit_Stats_prev$Player,
                         home_starter, "New")
  
  Pit_Stats$G <- as.numeric(Pit_Stats$G)
  Pit_Stats$G_last <- as.numeric(Pit_Stats$G_last)
  
  if(away_starter %in% Pit_Stats$Player &
     away_starter %in% Pit_Stats_prev$Player){
    check <- Pit_Stats |>
      filter(Player == away_starter)
    
    away_starter <- ifelse(check$G + check$G_last > 5,
                           away_starter, "New")
  }
  
  if(home_starter %in% Pit_Stats$Player &
     home_starter %in% Pit_Stats_prev$Player){
    check <- Pit_Stats |>
      filter(Player == home_starter)
    
    home_starter <- ifelse(check$G + check$G_last > 5,
                           home_starter, "New")
  }
  
  if(!(away_starter %in% Pit_Stats$Player) &
     away_starter %in% Pit_Stats_prev$Player){
    check <- Pit_Stats_prev |>
      filter(Player == away_starter)
    
    away_starter <- ifelse(check$G_last > 5,
                           away_starter, "New")
  }
  
  if(!(home_starter %in% Pit_Stats$Player) &
     home_starter %in% Pit_Stats_prev$Player){
    check <- Pit_Stats_prev |>
      filter(Player == home_starter)
    
    home_starter <- ifelse(check$G_last > 5,
                           home_starter, "New")
  }
  
  if(away_starter %in% Pit_Stats$Player &
     !(away_starter %in% Pit_Stats_prev$Player)){
    check <- Pit_Stats |>
      filter(Player == away_starter)
    
    away_starter <- ifelse(check$G > 5,
                           away_starter, "New")
  }
  
  if(home_starter %in% Pit_Stats$Player &
     !(home_starter %in% Pit_Stats_prev$Player)){
    check <- Pit_Stats |>
      filter(Player == home_starter)
    
    home_starter <- ifelse(check$G > 5,
                           home_starter, "New")
  }
  
  if(away_starter %in% Pit_Stats$Player){
    away_pitch <- Pit_Stats |>
      filter(Player == away_starter)
  } else{
    away_pitch <- Pit_Stats_prev |>
      filter(Player == away_starter) |>
      mutate(xwOBA_adj = xwOBA_last, IP.G_adj = IP.G_last)
  }
  
  if(home_starter %in% Pit_Stats$Player){
    home_pitch <- Pit_Stats |>
      filter(Player == home_starter)
  } else{
    home_pitch <- Pit_Stats_prev |>
      filter(Player == home_starter) |>
      mutate(xwOBA_adj = xwOBA_last, IP.G_adj = IP.G_last)
  }
  
  away_pitch$xwOBA_prop <- away_pitch$xwOBA_adj/av_xwOBA
  
  #away starter adjustment
  away_st_adj <- away_pitch$xwOBA_prop
  
  #average innings pitched by away starter
  away_st_ip <- away_pitch$IP.G_adj
  
  home_pitch$xwOBA_prop <- home_pitch$xwOBA_adj/av_xwOBA
  
  #home starter adjustment
  home_st_adj <- home_pitch$xwOBA_prop
  
  #average innings pitched by home starter
  home_st_ip <- home_pitch$IP.G_adj
  
  away_bp <- BP_Stats |>
    filter(TeamName == Away_Team)
  
  away_bp$RxwOBA_prop <- away_bp$RxwOBA_adj/av_xwOBA
  
  #away bullpen adjustment
  away_bp_adj <- away_bp$RxwOBA_prop
  
  home_bp <- BP_Stats |>
    filter(TeamName == Home_Team)
  
  home_bp$RxwOBA_prop <- home_bp$RxwOBA_adj/av_xwOBA
  
  #home bullpen adjustment
  home_bp_adj <- home_bp$RxwOBA_prop
  
  away_bat <- away_ratios |>
    select(Tm, Out, Single, Double, Triple, Homerun, Walk, Reach_Error)
  
  home_bat <- home_ratios |>
    select(Tm, Out, Single, Double, Triple, Homerun, Walk, Reach_Error)
  
  hit_events <- c("Single", "Double", "Triple", "Homerun", "Walk", "Reach_Error")
  
  away_bat_st <- away_bat |>
    # Step 1: adjust OUT probability on the logit scale
    mutate(
      Out_adj = inv_logit(logit(Out) - log(home_st_adj))
    ) |>
    rowwise() |>
    mutate(
      # Step 2: total non-out probability after adjustment
      hit_mass = 1 - Out_adj,
      # original hit mass
      hit_mass_orig = sum(c_across(all_of(hit_events))),
      # Step 3: rescale hit events proportionally
      across(
        all_of(hit_events),
        ~ .x / hit_mass_orig * hit_mass
      ),
      # Step 4: replace Out
      Out = Out_adj
    ) |>
    ungroup() |>
    select(-Out_adj, -hit_mass, -hit_mass_orig)
  
  away_bat_st <- left_join(away_bat_st, GIDP_Stats, by = "Tm")
  
  home_bat_st <- home_bat |>
    # Step 1: adjust OUT probability on the logit scale
    mutate(
      Out_adj = inv_logit(logit(Out) - log(away_st_adj))
    ) |>
    rowwise() |>
    mutate(
      # Step 2: total non-out probability after adjustment
      hit_mass = 1 - Out_adj,
      # original hit mass
      hit_mass_orig = sum(c_across(all_of(hit_events))),
      # Step 3: rescale hit events proportionally
      across(
        all_of(hit_events),
        ~ .x / hit_mass_orig * hit_mass
      ),
      # Step 4: replace Out
      Out = Out_adj
    ) |>
    ungroup() |>
    select(-Out_adj, -hit_mass, -hit_mass_orig)
  
  home_bat_st <- left_join(home_bat_st, GIDP_Stats, by = "Tm")
  
  away_bat_bp <- away_bat |>
    # Step 1: adjust OUT probability on the logit scale
    mutate(
      Out_adj = inv_logit(logit(Out) - log(home_bp_adj))
    ) |>
    rowwise() |>
    mutate(
      # Step 2: total non-out probability after adjustment
      hit_mass = 1 - Out_adj,
      # original hit mass
      hit_mass_orig = sum(c_across(all_of(hit_events))),
      # Step 3: rescale hit events proportionally
      across(
        all_of(hit_events),
        ~ .x / hit_mass_orig * hit_mass
      ),
      # Step 4: replace Out
      Out = Out_adj
    ) |>
    ungroup() |>
    select(-Out_adj, -hit_mass, -hit_mass_orig)
  
  away_bat_bp <- left_join(away_bat_bp, GIDP_Stats, by = "Tm")
  
  home_bat_bp <- home_bat |>
    # Step 1: adjust OUT probability on the logit scale
    mutate(
      Out_adj = inv_logit(logit(Out) - log(away_bp_adj))
    ) |>
    rowwise() |>
    mutate(
      # Step 2: total non-out probability after adjustment
      hit_mass = 1 - Out_adj,
      # original hit mass
      hit_mass_orig = sum(c_across(all_of(hit_events))),
      # Step 3: rescale hit events proportionally
      across(
        all_of(hit_events),
        ~ .x / hit_mass_orig * hit_mass
      ),
      # Step 4: replace Out
      Out = Out_adj
    ) |>
    ungroup() |>
    select(-Out_adj, -hit_mass, -hit_mass_orig)
  
  home_bat_bp <- left_join(home_bat_bp, GIDP_Stats, by = "Tm")
  
  away_bsr <- away_ratios |>
    select(Tm, Steals_2nd, Success_2nd, Steals_3rd, Success_3rd,
           First_to_2nd_single, First_to_3rd_single, First_to_Out_single,
           First_to_3rd_double, First_to_Score_double, First_to_Out_double,
           Second_to_3rd_single, Second_to_Score_single, Second_to_Out_single)
  
  home_bsr <- home_ratios |>
    select(Tm, Steals_2nd, Success_2nd, Steals_3rd, Success_3rd,
           First_to_2nd_single, First_to_3rd_single, First_to_Out_single,
           First_to_3rd_double, First_to_Score_double, First_to_Out_double,
           Second_to_3rd_single, Second_to_Score_single, Second_to_Out_single)
  
  #begin game setup
  TotalRunsA <- 0
  TotalRunsH <- 0
  Home <- rep(0, numgames)
  Away <- rep(0, numgames)
  InningsPlayed <- rep(0,numgames)
  
  for(j in 1:numgames){
    RunsH <- 0
    RunsA <- 0
    Innings <- 1
    MaxInnings <- 9
    
    #start game
    while(Innings <= MaxInnings){
      #away team bats
      Outs <- 0
      RunnersA <- data.frame(t(c(0,0,0)))
      colnames(RunnersA) <- c("B1", "B2", "B3")
      
      if(Innings > 9){
        RunnersA$B2 <- 1
      }
      
      while(Outs < 3){
        ratios <- away_bat_st
        
        if(RunsA > 4 & Innings > 3){
          ratios <- away_bat_bp
        } else if(RunsA > 3 & Innings > home_st_ip){
          ratios <- away_bat_bp
        } else if(RunsA > 1 & Innings > home_st_ip + 1){
          ratios <- away_bat_bp
        } else if(Innings > 7){
          ratios <- away_bat_bp
        }
        roll <- runif(1,0,1)
        if(roll <= ratios$Out){
          #Out
          Outs <- Outs + 1
          if(RunnersA$B1 > 0 & Outs < 3){
            DP <- runif(1,0,1)
            if(DP < ratios$GIDP_adj/ratios$Out){
              Outs <- Outs + 1
            }
          }
        } else if(roll > ratios$Out & roll <= ratios$Out + ratios$Walk){
          #Walk
          if(RunnersA$B1 > 0){
            RunnersA$B2 <- RunnersA$B2 + 1
            RunnersA$B1 <- 0
          }
          if(RunnersA$B2 > 1){
            RunnersA$B3 <- RunnersA$B3 + 1
            RunnersA$B2 <- 1
          }
          if(RunnersA$B3 > 1){
            RunsA <- RunsA + 1
            RunnersA$B3 <- 1
          }
          RunnersA$B1 <- RunnersA$B1 + 1
        } else if(roll > ratios$Out + ratios$Walk &
                  roll <= ratios$Out + ratios$Walk + ratios$Homerun){
          #Homerun
          RunsA <- RunsA + 1 + sum(RunnersA)
          RunnersA$B1 <- 0
          RunnersA$B2 <- 0
          RunnersA$B3 <- 0
        } else if(roll > ratios$Out + ratios$Walk + ratios$Homerun &
                  roll <= ratios$Out + ratios$Walk + ratios$Homerun +
                  ratios$Triple){
          #Triple
          RunsA <- RunsA + sum(RunnersA)
          RunnersA$B1 <- 0
          RunnersA$B2 <- 0
          RunnersA$B3 <- 1
        } else if(roll > ratios$Out + ratios$Walk + ratios$Homerun +
                  ratios$Triple &
                  roll <= ratios$Out + ratios$Walk + ratios$Homerun +
                  ratios$Triple + ratios$Double){
          #Double
          RunsA <- RunsA + RunnersA$B3 + RunnersA$B2
          double <- runif(1,0,1)
          if(RunnersA$B1 > 0 & double < away_bsr$First_to_3rd_double){
            RunnersA$B3 <- 1
            RunnersA$B1 <- 0
          } else if(RunnersA$B1 > 0 & double >= away_bsr$First_to_3rd_double &
                    double < away_bsr$First_to_Score_double +
                    away_bsr$First_to_3rd_double){
            RunsA <- RunsA + RunnersA$B1
            RunnersA$B1 <- 0
          } else if (RunnersA$B1 > 0){
            Outs <- Outs + 1
            RunnersA$B1 <- 0
          }
          RunnersA$B1 <- 0
          RunnersA$B2 <- 1
        } else if(roll > ratios$Out + ratios$Walk + ratios$Homerun +
                  ratios$Triple + ratios$Double &
                  roll <= 1){
          #Single
          single <- runif(1,0,1)
          RunsA <- RunsA + RunnersA$B3
          if(single < away_bsr$Second_to_Score_single & RunnersA$B2 > 0){
            RunsA <- RunsA + RunnersA$B2
            RunnersA$B2 <- 0
          } else if(single >= away_bsr$Second_to_Score_single & single <
                    away_bsr$Second_to_Score_single +
                    away_bsr$Second_to_3rd_single &
                    RunnersA$B2 > 0){
            RunnersA$B3 <- 1
            RunnersA$B2 <- 0
          } else if(RunnersA$B2 > 0){
            Outs <- Outs + 1
            RunnersA$B2 <- 0
          }
          if(RunnersA$B1 > 0 & RunnersA$B3 > 0){
            RunnersA$B2 <- 1
          }
          single2 <- runif(1,0,1)
          if(RunnersA$B1 > 0 & RunnersA$B3 < 1 &
             single2 < away_bsr$First_to_3rd_single){
            RunnersA$B3 <- 1
            RunnersA$B1 <- 0
          } else if(RunnersA$B1 > 0 & RunnersA$B3 < 1 & single2 >=
                    away_bsr$First_to_3rd_single &
                    single2 < away_bsr$First_to_3rd_single +
                    away_bsr$First_to_2nd_single){
            RunnersA$B2 <- 1
            RunnersA$B1 <- 0
          } else if(RunnersA$B1 > 0 & RunnersA$B3 < 1){
            Outs <- Outs + 1
            RunnersA$B1 <- 0
          }
          RunnersA$B1 <- 1
        }
        #steal bases
        if(RunnersA$B2 > 0 & RunnersA$B3 == 0){
          steal3 <- runif(1, 0, 1)
          if(steal3 <= away_bsr$Steals_3rd & RunnersA$B1 > 0){
            success <- runif(1, 0, 1)
            RunnersA$B1 <- 0
            if(success <= away_bsr$Success_3rd){
              RunnersA$B3 <- 1
            } else if(success > away_bsr$Success_3rd){
              Outs <- Outs + 1
            }
          } else if(steal3 <= away_bsr$Steals_3rd & RunnersA$B1 == 0){
            success <- runif(1, 0, 1)
            if(success <= away_bsr$Success_3rd){
              RunnersA$B2 <- 0
              RunnersA$B3 <- 1
            } else if(success > away_bsr$Success_3rd){
              RunnersA$B2 <- 0
              Outs <- Outs + 1
            }
          }
        }
        if(RunnersA$B1 > 0 & RunnersA$B2 == 0){
          steal2 <- runif(1, 0, 1)
          if(steal2 <= away_bsr$Steals_2nd){
            success <- runif(1, 0, 1)
            RunnersA$B1 <- 0
            if(success <= away_bsr$Success_2nd){
              RunnersA$B2 <- 1
            } else if(success > away_bsr$Success_2nd){
              Outs <- Outs + 1
            }
          }
        }
      }
      #Home Team Bats
      Outs <- 0
      RunnersH <- data.frame(t(c(0,0,0)))
      colnames(RunnersH) <- c("B1", "B2", "B3")
      
      if(Innings > 9){
        RunnersH$B2 <- 1
      }
      if(Innings >= 9 & RunsH > RunsA){
        Outs <- 3
      }
      while(Outs < 3){
        ratios <- home_bat_st
        
        if(RunsH > 4 & Innings > 3){
          ratios <- home_bat_bp
        } else if(RunsH > 3 & Innings > away_st_ip){
          ratios <- home_bat_bp
        } else if(RunsH > 1 & Innings > away_st_ip + 1){
          ratios <- home_bat_bp
        } else if(Innings > 7){
          ratios <- home_bat_bp
        }
        
        roll <- runif(1,0,1)
        if(roll <= ratios$Out){
          #Out
          Outs <- Outs + 1
          if(RunnersH$B1 > 0 & Outs < 3){
            DP <- runif(1,0,1)
            if(DP < ratios$GIDP_adj/ratios$Out){
              Outs <- Outs + 1
            }
          }
        } else if(roll > ratios$Out & roll <= ratios$Out + ratios$Walk){
          if(RunnersH$B1 > 0){
            #Walk
            RunnersH$B2 <- RunnersH$B2 + 1
            RunnersH$B1 <- 0
          }
          if(RunnersH$B2 > 1){
            RunnersH$B3 <- RunnersH$B3 + 1
            RunnersH$B2 <- 1
          }
          if(RunnersH$B3 > 1){
            RunsH <- RunsH + 1
            RunnersH$B3 <- 1
          }
          RunnersH$B1 <- RunnersH$B1 + 1
        } else if(roll > ratios$Out + ratios$Walk &
                  roll <= ratios$Out + ratios$Walk + ratios$Homerun){
          #Homerun
          RunsH <- RunsH + 1 + sum(RunnersH)
          RunnersH$B1 <- 0
          RunnersH$B2 <- 0
          RunnersH$B3 <- 0
        } else if(roll > ratios$Out + ratios$Walk + ratios$Homerun &
                  roll <= ratios$Out + ratios$Walk + ratios$Homerun +
                  ratios$Triple){
          #Triple
          RunsH <- RunsH + sum(RunnersH)
          RunnersH$B1 <- 0
          RunnersH$B2 <- 0
          RunnersH$B3 <- 1
        } else if(roll > ratios$Out + ratios$Walk + ratios$Homerun +
                  ratios$Triple &
                  roll <= ratios$Out + ratios$Walk + ratios$Homerun +
                  ratios$Triple + ratios$Double){
          #Double
          RunsH <- RunsH + RunnersH$B3 + RunnersH$B2
          double <- runif(1,0,1)
          if(RunnersH$B1 > 0 & double < home_bsr$First_to_3rd_double){
            RunnersH$B3 <- 1
            RunnersH$B1 <- 0
          } else if(RunnersH$B1 > 0 & double >= home_bsr$First_to_3rd_double & 
                    double < home_bsr$First_to_Score_double +
                    home_bsr$First_to_3rd_double){
            RunsH <- RunsH + RunnersH$B1
            RunnersH$B1 <- 0
          } else if (RunnersH$B1 > 0){
            Outs <- Outs + 1
            RunnersH$B1 <- 0
          }
          RunnersH$B1 <- 0
          RunnersH$B2 <- 1
        } else if(roll > ratios$Out + ratios$Walk + ratios$Homerun +
                  ratios$Triple + ratios$Double &
                  roll <= 1){
          #Single
          single <- runif(1,0,1)
          RunsH <- RunsH + RunnersH$B3
          if(single < home_bsr$Second_to_Score_single &
             RunnersH$B2 > 0){
            RunsH <- RunsH + RunnersH$B2
            RunnersH$B2 <- 0
          } else if(single >= home_bsr$Second_to_Score_single &
                    single < home_bsr$Second_to_Score_single +
                    home_bsr$Second_to_3rd_single &
                    RunnersH$B2 > 0){
            RunnersH$B3 <- 1
            RunnersH$B2 <- 0
          } else if(RunnersH$B2 > 0){
            Outs <- Outs + 1
            RunnersH$B2 <- 0
          }
          if(RunnersH$B1 > 0 & RunnersH$B3 > 0){
            RunnersH$B2 <- 1
          }
          single2 <- runif(1,0,1)
          if(RunnersH$B1 > 0 & RunnersH$B3 < 1 &
             single2 < home_bsr$First_to_3rd_single){
            RunnersH$B3 <- 1
            RunnersH$B1 <- 0
          } else if(RunnersH$B1 > 0 & RunnersH$B3 < 1 & single2 >=
                    home_bsr$First_to_3rd_single & single2 <
                    home_bsr$First_to_3rd_single + home_bsr$First_to_2nd_single){
            RunnersH$B2 <- 1
            RunnersH$B1 <- 0
          } else if(RunnersH$B1 > 0 & RunnersH$B3 < 1){
            Outs <- Outs + 1
            RunnersH$B1 <- 0
          }
          RunnersH$B1 <- 1
        }
        #steal bases
        if(RunnersH$B2 > 0 & RunnersH$B3 == 0){
          steal3 <- runif(1, 0, 1)
          if(steal3 <= home_bsr$Steals_3rd & RunnersH$B1 > 0){
            success <- runif(1, 0, 1)
            RunnersH$B1 <- 0
            if(success <= home_bsr$Success_3rd){
              RunnersH$B3 <- 1
            } else if(success > home_bsr$Success_3rd){
              Outs <- Outs + 1
            }
          } else if(steal3 <= home_bsr$Steals_3rd & RunnersH$B1 == 0){
            success <- runif(1, 0, 1)
            if(success <= home_bsr$Success_3rd){
              RunnersH$B2 <- 0
              RunnersH$B3 <- 1
            } else if(success > home_bsr$Success_3rd){
              RunnersH$B2 <- 0
              Outs <- Outs + 1
            }
          }
        }
        if(RunnersH$B1 > 0 & RunnersH$B2 == 0){
          steal2 <- runif(1, 0, 1)
          if(steal2 <= home_bsr$Steals_2nd){
            success <- runif(1, 0, 1)
            RunnersH$B1 <- 0
            if(success <= home_bsr$Success_2nd){
              RunnersH$B2 <- 1
            } else if(success > home_bsr$Success_2nd){
              Outs <- Outs + 1
            }
          }
        }
      }
      if(RunsH - RunsA == 0 & Innings == MaxInnings){
        MaxInnings <- MaxInnings + 1
      }
      Innings <- Innings + 1
      
      if(Innings >= 9 & RunsH > RunsA){
        Outs <- 3
      }
    }
    TotalRunsA <- TotalRunsA + RunsA
    TotalRunsH <- TotalRunsH + RunsH
    Home[j] <- RunsH
    Away[j] <- RunsA
    InningsPlayed[j] <- MaxInnings
  }
  diff <- data.frame(Home - Away)
  Arpg <- TotalRunsA/numgames
  Hrpg <- TotalRunsH/numgames
  Away2 <- diff[diff$Home...Away <= -2,]
  prob_away2 <- length(Away2)/numgames
  
  Home2 <- diff[diff$Home...Away >= 2,]
  prob_home2 <- length(Home2)/numgames
  
  AwayW <- diff[diff$Home...Away < 0,]
  prob_away <- length(AwayW)/numgames
  
  HomeW <- diff[diff$Home...Away > 0,]
  prob_home <- length(HomeW)/numgames
  
  TotalRuns <- Home + Away
  
  RunQuant <- data.frame(t(quantile(TotalRuns)))
  
  Scores <- data.frame(cbind(Away, Home))
  awaytable <- cbind(Away_Team, Arpg, prob_away, prob_away2, away_starter)
  hometable <- cbind(Home_Team, Hrpg, prob_home, prob_home2, home_starter)
  table <- data.frame(rbind(awaytable, hometable))
  colnames(table) <- c("Team",
                       "RPG",
                       "Probability of Winning",
                       "Probability of Winning by 2",
                       "Starting Pitcher")
  
  return(c(value1 = table, value2 = RunQuant))
}



#sim games
Away <- data.frame(matrix(nrow=nrow(matchups), ncol=5))
Home <- data.frame(matrix(nrow=nrow(matchups), ncol=5))
RunQuant <- data.frame(matrix(nrow=nrow(matchups), ncol=5))

for(i in 1:nrow(matchups)){
  result <- data.frame(playball(matchups$game_id[i],
                                10000))
  Away[i,] <- result[1,c(1:5)]
  Home[i,] <- result[2,c(1:5)]
  RunQuant[i,] <- result[1, c(6:10)]
}
Games <- data.frame(matrix(nrow=(nrow(matchups)*2), ncol=5))

for (i in 1:nrow(Games)) {
  if (i %% 2 == 1) {
    Games[i,] <- Away[(i+1) %/% 2, ]
  } else {
    Games[i,] <- Home[i %/% 2, ]
  }
}
colnames(Games) <- c("Team", "RPG", "Win_Prob",
                     "Win_by_2", "Starter")

Games <- Games |>
  group_by(Team) |>
  mutate(team_count = row_number()) |>
  ungroup()



api_key <- "177638eca772df2a30bece6b3d564b4c"

url <- "https://api.the-odds-api.com/v4/sports/baseball_mlb/odds"

response <- GET(
  url,
  query = list(
    apiKey = api_key,
    regions = "us",
    markets = "h2h,spreads",
    oddsFormat = "american"
  )
)

data <- fromJSON(
  content(response, "text", encoding = "UTF-8"),
  simplifyVector = FALSE
)

odds_df <- map_dfr(data, function(game) {
  
  if (length(game$bookmakers) == 0) return(NULL)
  
  home_ml <- away_ml <- -Inf
  home_spread <- away_spread <- NA
  home_spread_price <- away_spread_price <- -Inf
  
  for (bm in game$bookmakers) {
    
    for (market in bm$markets) {
      
      # MONEYLINE
      if (market$key == "h2h") {
        for (outcome in market$outcomes) {
          if (outcome$name == game$home_team) {
            home_ml <- max(home_ml, outcome$price, na.rm = TRUE)
          } else {
            away_ml <- max(away_ml, outcome$price, na.rm = TRUE)
          }
        }
      }
      
      # RUN LINE
      if (market$key == "spreads") {
        for (outcome in market$outcomes) {
          if (outcome$name == game$home_team) {
            if (outcome$price > home_spread_price) {
              home_spread <- outcome$point
              home_spread_price <- outcome$price
            }
          } else {
            if (outcome$price > away_spread_price) {
              away_spread <- outcome$point
              away_spread_price <- outcome$price
            }
          }
        }
      }
    }
  }
  
  data.frame(
    game_id = game$id,
    commence_time = game$commence_time,
    home_team = game$home_team,
    away_team = game$away_team,
    home_ml = home_ml,
    away_ml = away_ml,
    home_spread = home_spread,
    away_spread = away_spread,
    home_spread_price = home_spread_price,
    away_spread_price = away_spread_price
  )
})

check <- odds_df |>
  mutate(
    # 1. Parse the raw string as UTC (force it correctly)
    commence_time_utc = ymd_hms(commence_time, tz = "UTC"),
    # 2. Convert to Central time
    commence_time_ct = with_tz(commence_time_utc, "America/Chicago"),
    # 3. Extract date
    game_date = as.Date(commence_time_ct, tz = "America/Chicago")
  ) |>
  filter(game_date == today)

home_odds <- odds_df |>
  select(home_team, home_ml, home_spread, home_spread_price)

colnames(home_odds) <- c("Team", "ML_Price", "Spread", "RL_Price")

away_odds <- odds_df |>
  select(away_team, away_ml, away_spread, away_spread_price)

colnames(away_odds) <- c("Team", "ML_Price", "Spread", "RL_Price")

odds <- rbind(home_odds, away_odds)

odds <- odds |>
  group_by(Team) |>
  mutate(team_count = row_number()) |>
  ungroup()

Table <- left_join(Games, odds, by = c("Team", "team_count"))

Table <- Table |>
  select(-team_count)

Table$ML_vegas <- round(ifelse(Table$ML_Price > 0,
                               100 / (Table$ML_Price + 100),
                               abs(Table$ML_Price) / (abs(Table$ML_Price) + 100)), 4)

Table$RL_vegas <- round(ifelse(Table$RL_Price > 0,
                               100 / (Table$RL_Price + 100),
                               abs(Table$RL_Price) / (abs(Table$RL_Price) + 100)), 4)

Table$Hold <- 0

for(i in 1:nrow(Table)){
  if(i %% 2 == 1){
    Table$Hold[i] <- 1 - as.numeric(Table$Win_by_2[i+1])
  }
  else{
    Table$Hold[i] <- 1 - as.numeric(Table$Win_by_2[i-1])
  }
}

Table$Cover <- ifelse(Table$Spread < 0, Table$Win_by_2, Table$Hold)


Table$ML_Edge <- round(as.numeric(Table$Win_Prob) - as.numeric(Table$ML_vegas),
                       2)

Table$RL_Edge <- round(ifelse(as.numeric(Table$Spread) > 0,
                              as.numeric(Table$Cover) - as.numeric(Table$RL_vegas),
                              as.numeric(Table$Win_by_2) - as.numeric(Table$RL_vegas)),
                       2)

Table$ML_Pick <- ifelse(Table$ML_Edge > 0,
                        "Free", 0)
Table$ML_Pick <- ifelse(Table$ML_Edge > 0.03,
                        "Pro", Table$ML_Pick)
Table$ML_Pick <- ifelse(Table$ML_Edge > 0.05,
                        "Elite", Table$ML_Pick)

Table$RL_Pick <- ifelse(Table$RL_Edge > 0,
                        "Free", 0)
Table$RL_Pick <- ifelse(Table$RL_Edge > 0.03,
                        "Pro", Table$RL_Pick)
Table$RL_Pick <- ifelse(Table$RL_Edge > 0.05,
                        "Elite", Table$RL_Pick)



#write predictions to DB
DBI::dbWriteTable(
  con,
  "mlb_game_predictions_xwOBA",
  Table,
  overwrite = TRUE
)



report <- Table |>
  select(-c("ML_Price", "RL_Price", "ML_Edge", "RL_Edge", "ML_Pick", "RL_Pick"))


kable(report)
