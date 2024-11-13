library(hoopR)
library(tidyverse)

#Read in file with all 2024 possible tourney matchups

matchups_24 <- read_csv("final_deduplicated.csv")

#load team name data from last 4 seasons
teams_2024 <- espn_mbb_teams(year = 2024)
teams_2023 <- espn_mbb_teams(year = 2023)
teams_2022 <- espn_mbb_teams(year = 2022)
teams_2021 <- espn_mbb_teams(year = 2021)

#Filter each data set to only include teams that made the tournament

tourney_teams_21 <- c("Abilene Christian", "Alabama", "Appalachian State", "Arkansas", "Baylor", "Cleveland State", 
                      "Colgate", "Colorado", "Creighton","Drake", "Drexel", "Eastern Washington", "Florida", "Florida State", 
                      "Georgia Tech", "Gonzaga", "Grand Canyon", "Hartford", "Houston", "Illinois", "Iona", "Iowa", "Kansas", 
                      "Kansas State", "Liberty", "Loyola Chicago", "LSU", "Maryland", "Michigan", "Michigan State", "Missouri", 
                      "Morehead State", "Mount St. Mary's", "North Carolina", "North Texas", "Northeastern", "Ohio", "Oklahoma", 
                      "Oklahoma State", "Oral Roberts", "Oregon", "Oregon State", "Purdue","Rutgers", "St. Bonaventure", "San Diego State", 
                      "Syracuse", "Tennessee", "Texas", "Texas Southern", "Texas Tech", "UCLA", "USC", "Utah State", "VCU", 
                      "Villanova", "Virginia", "Virginia Tech", "West Virginia", "Western Kentucky", "Wisconsin", "Winthrop", "Wright State", "Xavier")

tourney_teams_22 <- c("Gonzaga", "Georgia State", "Boise State", "Memphis", "UConn", "New Mexico State", "Arkansas", "Vermont",
                      "Alabama", "Notre Dame", "Texas Tech", "Montana State", "Michigan State", "Davidson", "Duke", "Cal State Fullerton",
                      "Baylor", "Norfolk State", "North Carolina", "Marquette", "Saint Mary's", "Indiana", "UCLA", "Akron", "Texas",
                      "Virginia Tech", "Purdue", "Yale", "Murray State", "San Francisco", "Kentucky", "Saint Peter's",
                      "Arizona", "Wright State", "Seton Hall", "TCU", "Houston", "UAB", "Illinois", "Chattanooga", "Colorado State", "Michigan",
                      "Tennessee", "Longwood", "Ohio State", "Loyola Chicago", "Villanova", "Delaware", "Kansas", "Texas Southern",
                      "San Diego State", "Creighton", "Iowa", "Richmond", "Providence", "South Dakota State", "LSU", "Iowa State", 
                      "Wisconsin", "Colgate", "USC", "Miami", "Auburn", "Jacksonville State")

tourney_teams_23 <- c("Alabama", "Texas A&M-Corpus Christi", "Maryland", "West Virginia", "San Diego State", "Charleston", "Virginia", "Furman",
                      "Creighton", "NC State", "Baylor", "UC Santa Barbara", "Missouri", "Utah State", "Arizona", "Princeton", 
                      "Purdue", "Fairleigh Dickinson", "Memphis", "Florida Atlantic", "Duke", "Oral Roberts", "Tennessee", "Louisiana", "Kentucky", "Providence",
                      "Kansas State", "Montana State", "Michigan State", "USC", "Marquette", "Vermont", "Houston", "Northern Kentucky", 
                      "Iowa", "Auburn", "Miami", "Drake", "Indiana", "Kent State", "Iowa State", "Pittsburgh", 
                      "Xavier", "Kennesaw State", "Texas A&M", "Penn State", "Texas", "Colgate", "Kansas", "Howard", "Arkansas", "Illinois",
                      "Saint Mary's", "VCU", "UConn", "Iona", "TCU", "Arizona State", "Gonzaga", "Grand Canyon", "Northwestern", "Boise State", "UCLA", "UNC Asheville")

tourney_teams_24 <- c("UConn", "Stetson", "Florida Atlantic", "Northwestern", "San Diego State", "UAB", "Auburn", "Yale", "BYU", "Duquesne", "Illinois", "Morehead State",
                      "Washington State", "Drake", "Iowa State", "South Dakota State", "North Carolina", "Mississippi State", "Michigan State", "Saint Mary's",
                      "Grand Canyon", "Alabama", "Charleston", "Clemson", "New Mexico", "Baylor", "Colgate", "Dayton", "Nevada", "Arizona", "Long Beach State",
                      "Houston", "Longwood", "Nebraska", "Texas A&M", "Wisconsin", "James Madison", "Duke", "Vermont", "Texas Tech", "NC State", "Kentucky", 
                      "Oakland", "Florida", "Marquette", "Western Kentucky", "Purdue", "Utah State", "TCU", "Gonzaga", "McNeese", "Kansas", "Samford", 
                      "South Carolina", "Oregon", "Creighton", "Akron", "Texas", "Tennessee", "Saint Peter's", "Howard", "Wagner", "Colorado State",
                      "Virginia", "Grambling", "Montana State", "Colorado", "Boise State")

teams_2021_new <- teams_2021 %>% 
  filter(team %in% tourney_teams_21) %>% 
  select(team_id, team, conference_short_name) %>% 
  mutate(year = 2021)

teams_2022_new <- teams_2022 %>% 
  filter(team %in% tourney_teams_22) %>% 
  select(team_id, team, conference_short_name) %>% 
  mutate(year = 2022)

teams_2023_new <- teams_2023 %>% 
  filter(team %in% tourney_teams_23) %>% 
  select(team_id, team, conference_short_name) %>% 
  mutate(year = 2023)
  
teams_2024_new <- teams_2024 %>% 
  filter(team %in% tourney_teams_24) %>% 
  select(team_id, team, conference_short_name) %>% 
  mutate(year = 2024)

#Create data frame of team stats from each season 2021-24

demo <- espn_mbb_team_stats(team_id = 333, year = 2024)

team_stats_24 <- data.frame(matrix(NA, nrow = 68, ncol = 94))

# Loop through teams and fill in the rows
for (i in 1:68) {
  team_id <- teams_2024_new[i,1]
  team_stats <- espn_mbb_team_stats(team_id = team_id, year = 2024)
  
  # Assign team_stats to the corresponding row in team_stats_24
  team_stats_24[i, ] <- team_stats
}

colnames(team_stats_24) <- colnames(demo)


team_stats_23 <- data.frame(matrix(NA, nrow = 64, ncol = 94))

# Loop through teams and fill in the rows
for (i in 1:64) {
  team_id <- teams_2023_new[i,1]
  team_stats <- espn_mbb_team_stats(team_id = team_id, year = 2023)
  
  # Assign team_stats to the corresponding row in team_stats_24
  team_stats_23[i, ] <- team_stats
}

colnames(team_stats_23) <- colnames(demo)


team_stats_22 <- data.frame(matrix(NA, nrow = 64, ncol = 94))

# Loop through teams and fill in the rows
for (i in 1:64) {
  team_id <- teams_2022_new[i,1]
  team_stats <- espn_mbb_team_stats(team_id = team_id, year = 2022)
  
  # Assign team_stats to the corresponding row in team_stats_24
  team_stats_22[i, ] <- team_stats
}

colnames(team_stats_22) <- colnames(demo)


team_stats_21 <- data.frame(matrix(NA, nrow = 63, ncol = 94))

# Loop through teams and fill in the rows
for (i in 1:63) {
  team_id <- teams_2021_new[i,1]
  team_stats <- espn_mbb_team_stats(team_id = team_id, year = 2021)
  
  # Assign team_stats to the corresponding row in team_stats_24
  team_stats_21[i, ] <- team_stats
}

colnames(team_stats_21) <- colnames(demo)

#Make a list of variable names to drop
vars_to_drop <- c("team_guid","team_uid","team_sdr","team_slug","team_location","team_nickname",
                  "team_abbreviation","team_display_name","team_color","team_alternate_color",
                  "is_active","is_all_star","logo_href","logo_dark_href","general_ejections","general_avg_ejections",
                  "general_disqualifications","general_avg_disqualifications","general_games_played","general_games_started",
                  "general_double_double","general_triple_double","offensive_second_chance_points","offensive_fast_break_points")

#Subset team data sets

cleaned_stats_21 <- team_stats_21 %>% 
  select(!all_of(vars_to_drop)) %>% 
  mutate(season = 2021) 

cleaned_stats_22 <- team_stats_22 %>% 
  select(!all_of(vars_to_drop)) %>% 
  mutate(season = 2022)

cleaned_stats_23 <- team_stats_23 %>% 
  select(!all_of(vars_to_drop)) %>% 
  mutate(season = 2023)

cleaned_stats_24 <- team_stats_24 %>% 
  select(!all_of(vars_to_drop)) %>% 
  mutate(season = 2024)

#Find scores of all tournament games from 2021-23
vars_to_keep <-c("season","season_slug","game_id","home_team_id","home_team_location",
                 "home_score","home_win","away_team_id","away_team_location","away_score","away_win")

results_2023 <- espn_mbb_scoreboard(2023) %>%
  filter(season_slug == "post-season") %>% 
  select(all_of(vars_to_keep)) %>% 
  slice(20:n()) #Takes out conference tourney games, only including 63 NCAA tourney games

results_2022 <- espn_mbb_scoreboard(2022) %>%
  filter(season_slug == "post-season") %>% 
  select(all_of(vars_to_keep)) %>% 
  slice(20:n())

results_2021 <- espn_mbb_scoreboard(2021) %>%
  filter(season_slug == "post-season",
         away_team_location != "VCU") %>% #VCU forfeited due to COVID outbreak
  select(all_of(vars_to_keep))%>% 
  slice(12:n()) #Less conference tourney games in Covid-year

#Make separate dataframes for team stats for home/away matches for each year

#2021
results_home_21 <- results_2021 %>% 
  select(c("season","season_slug","game_id","home_team_id","home_team_location",
           "home_score","home_win")) %>% 
  rename(team_id = home_team_id)

results_away_21 <- results_2021 %>% 
  select(c("season","season_slug","game_id","away_team_id",
            "away_team_location","away_score","away_win")) %>% 
  rename(team_id = away_team_id)


#2022 
results_home_22 <- results_2022 %>% 
  select(c("season","season_slug","game_id","home_team_id","home_team_location",
           "home_score","home_win")) %>% 
  rename(team_id = home_team_id)

results_away_22 <- results_2022 %>% 
  select(c("season","season_slug","game_id","away_team_id",
           "away_team_location","away_score","away_win")) %>% 
  rename(team_id = away_team_id)

#2023
results_home_23 <- results_2023 %>% 
  select(c("season","season_slug","game_id","home_team_id","home_team_location",
           "home_score","home_win")) %>% 
  rename(team_id = home_team_id)

results_away_23 <- results_2023 %>% 
  select(c("season","season_slug","game_id","away_team_id",
           "away_team_location","away_score","away_win")) %>% 
  rename(team_id = away_team_id)


#join stats/results datasets

#2021
home_21_total <- right_join(cleaned_stats_21,results_home_21, by = "team_id")
home_21_total <- arrange(home_21_total, by = game_id)
home_factors_21 <- home_21_total %>% 
  select(-team_id,-team_name,-team_short_display_name,-season.x,-season.y,
         -season_slug,-home_team_location,-home_score,-home_win) #Only want team stats so we can find difference 

away_21_total <- right_join(cleaned_stats_21,results_away_21, by = "team_id")
away_21_total <- arrange(away_21_total, by = game_id)
away_factors_21 <- away_21_total %>% 
  select(-team_id,-team_name,-team_short_display_name,-season.x,-season.y,
         -season_slug,-away_team_location,-away_score,-away_win)

#2022
home_22_total <- right_join(cleaned_stats_22,results_home_22, by = "team_id")
home_22_total <- arrange(home_22_total, by = game_id)
home_factors_22 <- home_22_total %>% 
  select(-team_id,-team_name,-team_short_display_name,-season.x,-season.y,
         -season_slug,-home_team_location,-home_score,-home_win)

away_22_total <- right_join(cleaned_stats_22,results_away_22, by = "team_id")
away_22_total <- arrange(away_22_total, by = game_id)
away_factors_22 <- away_22_total %>% 
  select(-team_id,-team_name,-team_short_display_name,-season.x,-season.y,
         -season_slug,-away_team_location,-away_score,-away_win)

#2023
home_23_total <- right_join(cleaned_stats_23,results_home_23, by = "team_id")
home_23_total <- arrange(home_23_total, by = game_id)
home_factors_23 <- home_23_total %>% 
  select(-team_id,-team_name,-team_short_display_name,-season.x,-season.y,
         -season_slug,-home_team_location,-home_score,-home_win)

away_23_total <- right_join(cleaned_stats_23,results_away_23, by = "team_id")
away_23_total <- arrange(away_23_total, by = game_id)
away_factors_23 <- away_23_total %>% 
  select(-team_id,-team_name,-team_short_display_name,-season.x,-season.y,
         -season_slug,-away_team_location,-away_score,-away_win)


#Find differences in each dataset

diff_21 <- away_factors_21 - home_factors_21
diff_21$home <- home_21_total$home_team_location
diff_21$away <- away_21_total$away_team_location
diff_21$upset <- away_21_total$away_win
diff_21$season <- home_21_total$season.x

diff_22 <- away_factors_22 - home_factors_22
diff_22$home <- home_22_total$home_team_location
diff_22$away <- away_22_total$away_team_location
diff_22$upset <- away_22_total$away_win
diff_22$season <- home_22_total$season.x

diff_23 <- away_factors_23 - home_factors_23
diff_23$home <- home_23_total$home_team_location
diff_23$away <- away_23_total$away_team_location
diff_23$upset <- away_23_total$away_win
diff_23$season <- home_23_total$season.x

#Combine differences from each season
total_diff <- rbind(diff_21,diff_22,diff_23)

#Remove outliers and NAs (from 2021)

total_diff <- total_diff %>% 
  filter(abs(general_avg_minutes) < 100)


#Exploratory Plots

hist(total_diff$offensive_avg_team_turnovers)
#Use total_diff for logistic regression model to predict upsets

Basic_Upset_Model <- glm(upset ~ offensive_field_goal_pct + offensive_avg_assists + 
                            offensive_avg_offensive_rebounds,
                         data = total_diff,
                         family = "binomial")

#add more variables
Basic_Upset_Model2 <- glm(upset ~ general_avg_rebounds + offensive_avg_assists + 
                            offensive_field_goal_pct + offensive_free_throw_pct + offensive_three_point_field_goal_pct +
                            general_assist_turnover_ratio + general_block_foul_ratio + offensive_avg_estimated_possessions,
                          data = total_diff,
                          family = "binomial")

#Add one way interactions
Basic_Upset_Model3 <- glm(upset ~ offensive_avg_offensive_rebounds + offensive_avg_assists + offensive_scoring_efficiency +
                            offensive_field_goal_pct + offensive_free_throw_pct + offensive_three_point_field_goal_pct +
                            general_assist_turnover_ratio + general_block_foul_ratio + offensive_avg_estimated_possessions +
                            offensive_avg_assists:offensive_field_goal_pct + general_assist_turnover_ratio:offensive_free_throw_pct +
                            offensive_scoring_efficiency:offensive_avg_estimated_possessions,
                          data = total_diff,
                          family = "binomial")

#Add one way interactions
Basic_Upset_Model4 <- glm(upset ~ offensive_avg_offensive_rebounds + offensive_avg_assists + offensive_scoring_efficiency +
                            offensive_field_goal_pct + offensive_free_throws + offensive_free_throw_pct + offensive_three_point_field_goal_pct +
                            general_assist_turnover_ratio + general_steal_foul_ratio + offensive_avg_estimated_possessions +
                            offensive_points_per_estimated_possessions + offensive_avg_assists:offensive_field_goal_pct  +
                            offensive_scoring_efficiency:offensive_avg_estimated_possessions + 
                            offensive_free_throws:offensive_free_throw_pct + offensive_three_point_field_goal_pct:offensive_three_point_field_goals_attempted,
                          data = total_diff,
                          family = "binomial")


#Check fitted values in Models to actual upsets

model_check <- as.data.frame(cbind(total_diff$home,total_diff$away,total_diff$season,total_diff$upset,
                                   Basic_Upset_Model$fitted.values,Basic_Upset_Model2$fitted.values,
                                   Basic_Upset_Model3$fitted.values,Basic_Upset_Model4$fitted.values))

colnames(model_check) <- c("Home", "Away", "Season", "Upset", "WinProb1", "WinProb2", "WinProb3", "WinProb4")


#Adjust team names so they match submission file

pred_stats_24 <- cleaned_stats_24 %>% 
  mutate(team_short_display_name = recode("Boise St" = "Boise St.",
                                          "Charleston" = "College of Charleston",
                                          "Colorado St" = "Colorado St.",
                                          "UConn" = "Connecticut",
                                          "FAU" = "Florida Atlantic",
                                          "Iowa State" = "Iowa St.",
                                          "Long Beach St" = "Long Beach St.",
                                          "McNeese" = "McNeese St.",
                                          "Michigan St" = "Michigan St.",
                                          "Mississippi St" = "Mississippi St.",
                                          "Montana St" = "Montana St.",
                                          "Morehead St" = "Morehead St.",
                                          "NC State" = "North Carolina St.",
                                          "San Diego St" = "San Diego St.",
                                          "S Dakota St" = "South Dakota St.",
                                          "Utah State" = "Utah St.", 
                                          "Washington St" = "Washington St.",
                                          "Western KY" = "Western Kentucky",
                                          .x = team_short_display_name),
         #Fix values for McNeese and St. Peter's because they were incorrect
         
         offensive_scoring_efficiency = case_when(
           offensive_scoring_efficiency == 2612.000000 ~ 1.3,
           offensive_scoring_efficiency == 2377.000000 ~ 1.3,
           # Add more conditions as needed
           TRUE ~ offensive_scoring_efficiency  # Keep the original value if it doesn't match any condition
         ),
         offensive_field_goal_pct = case_when(
           offensive_field_goal_pct == 0.691460 ~ 49.3,
           offensive_field_goal_pct == 0.713256 ~ 39.7,
           TRUE ~ offensive_field_goal_pct),
         
         offensive_free_throw_pct = case_when(
         offensive_free_throw_pct ==  694.00000 ~ 71.325648,
         offensive_free_throw_pct == 726.00000 ~ 69.146007,
         TRUE ~ offensive_free_throw_pct),
         
         offensive_free_throws = case_when(
           offensive_free_throws == 71.325648 ~ 0.71325648,
           offensive_free_throws == 69.146007 ~ 0.69146007,
           TRUE ~ offensive_free_throws),
         
         offensive_three_point_field_goal_pct = case_when(
           offensive_three_point_field_goal_pct ==  516.00000 ~ 33.2,
           offensive_three_point_field_goal_pct == 698.00000 ~ 38.8,
           TRUE ~ offensive_three_point_field_goal_pct),
         
         offensive_avg_estimated_possessions = case_when(
           offensive_avg_estimated_possessions == 0.9963785 ~ 66.5,
           offensive_avg_estimated_possessions == 1.2105455 ~ 66.5,
         TRUE ~ offensive_avg_estimated_possessions)) %>% 
  
  select(team_short_display_name,team_id,offensive_avg_offensive_rebounds,offensive_avg_assists,offensive_scoring_efficiency,
         offensive_field_goal_pct,offensive_free_throws,offensive_free_throw_pct,offensive_three_point_field_goal_pct,
           general_assist_turnover_ratio,general_steal_foul_ratio,offensive_avg_estimated_possessions,
           offensive_points_per_estimated_possessions,offensive_three_point_field_goals_attempted) %>% 
  rename(team_name = team_short_display_name)
  
#Create separate data sets for team 1 (home) and team 2 (away)

#Team 1

team1 <- as.data.frame(matchups_24$team1)
team1 <- team1 %>% 
  rename(team_name = `matchups_24$team1`)
team1$GameID <- matchups_24$GameID

team1_stats <- right_join(pred_stats_24,team1, by = "team_name")

team1_factors <- team1_stats %>% 
  select(-team_name,-team_id)
team1_factors <- arrange(team1_factors, by = GameID)


team2 <- as.data.frame(matchups_24$team2)
team2 <- team2 %>% 
  rename(team_name = `matchups_24$team2`)
team2$GameID <- matchups_24$GameID

team2_stats <- right_join(pred_stats_24,team2, by = "team_name")
team2_stats <- arrange(team2_stats, by = "GameID") 
team2_factors <- team2_stats %>% 
  select(-team_name,-team_id)
team2_factors <- arrange(team2_factors, by = GameID)

diffs_24 <- team1_factors - team2_factors
#Make sure rows in team 1 and team 2 are aligned


#Apply Model Coefficients to Predict Matchups

diffs_24 <- diffs_24 %>% 
  mutate(Team1WinPercentage = exp(-7.175134e-01 + -4.349620e-01*offensive_avg_offensive_rebounds + 5.734939e-01*offensive_avg_assists +
           -2.355360e+01*offensive_scoring_efficiency + -1.971580e-02*offensive_field_goal_pct + -1.707104e+05*offensive_free_throws +
           1.707125e+03*offensive_free_throw_pct + -9.731328e-02*offensive_three_point_field_goal_pct + -5.746634e+00*general_assist_turnover_ratio +
           -5.678939e-01*general_steal_foul_ratio + -4.805487e-02*( offensive_avg_estimated_possessions) + 3.368375e+01*offensive_points_per_estimated_possessions+
           -4.130015e-02*(offensive_avg_assists*offensive_field_goal_pct) + 8.738166e-01*(offensive_scoring_efficiency*offensive_avg_estimated_possessions) +
           1.161845e-01*(offensive_free_throws*offensive_free_throw_pct) + -4.333735e-04*(offensive_three_point_field_goal_pct*offensive_three_point_field_goals_attempted)) / 
             (1 + exp(-7.175134e-01 + -4.349620e-01*offensive_avg_offensive_rebounds + 5.734939e-01*offensive_avg_assists +
                       -2.355360e+01*offensive_scoring_efficiency + -1.971580e-02*offensive_field_goal_pct + -1.707104e+05*offensive_free_throws +
                       1.707125e+03*offensive_free_throw_pct + -9.731328e-02*offensive_three_point_field_goal_pct + -5.746634e+00*general_assist_turnover_ratio +
                       -5.678939e-01*general_steal_foul_ratio + -4.805487e-02*( offensive_avg_estimated_possessions) + 3.368375e+01*offensive_points_per_estimated_possessions+
                       -4.130015e-02*(offensive_avg_assists*offensive_field_goal_pct) + 8.738166e-01*(offensive_scoring_efficiency*offensive_avg_estimated_possessions) +
                       1.161845e-01*(offensive_free_throws*offensive_free_throw_pct) + -4.333735e-04*(offensive_three_point_field_goal_pct*offensive_three_point_field_goals_attempted))))

#Create Final Data Frame

WinProbs <- diffs_24 %>% 
  select(Team1WinPercentage)
WinProbs$GameID = seq(1,2016,1)

Model_Results <- right_join(matchups_24, WinProbs, by = "GameID")

#Convert NA values to 0

Model_Results <- Model_Results %>% 
  mutate(Team1WinPercentage = case_when(
    Team1WinPercentage == NA ~ 0,
    Team1WinPercentage == NaN ~ 0,
    TRUE ~ Team1WinPercentage
  ))

Model_Results_2 <- Model_Results %>% 
  mutate(Team1WinPercentage = ifelse(is.na(Team1WinPercentage) | is.nan(Team1WinPercentage), 0, Team1WinPercentage))
#Export DataSet as CSV 

write.csv(Model_Results_2, file = "ModelPredictions.csv", row.names = FALSE)

