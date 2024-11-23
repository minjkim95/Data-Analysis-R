library(Lahman)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stats)
library(readr)
library(ggrepel)
library(psych)

# Teams Table containing seasonal stats fro major league teams
# going back to the first professional season in 1871
data("Teams")
head(Teams,6)
tail(Teams, 3) #3 last lines

# We want to see the relationship between runs and wins by 
# relating the proportion of wins with the 1. runs scored and 2. run allowed for all of the teams
my_teams = Teams%>% 
  filter(yearID > 2001) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA)
my_teams %>%
  tail()

# Calculate run differential and winning percentage (winning proportion) and 
# create columns for the two calculations

my_teams = my_teams %>%
  mutate(RD = R-RA, Wpct = W/(W+L))

# Scatterplot of run differential and winning percentage
run_diff = ggplot(my_teams, aes(x = RD, y = Wpct)) +
  geom_point() +
  scale_x_continuous("Run Differential")+
  scale_y_continuous("Winning Percentage") +
  ggtitle("Run Differential vs. Winning Percentage")
run_diff

# Linear Regression to predict a team's winning percentage using runs scored and runs allowed
linfit = lm(Wpct~RD, data = my_teams)
summary(lm(Wpct~RD, data = my_teams))
run_diff + 
  geom_smooth(method = 'lm', se = FALSE, color = "blue")

# Calculate predicted values from the linear model 
library(broom)
my_teams_aug = augment(linfit, data = my_teams)
my_teams_aug
# Plot the residuals: actual and estimated winning percentages vs. against run differential
library(ggrepel)
base_plot = ggplot(my_teams_aug, aes(x = RD, y = .resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = 3) +
  xlab("Run Differential") +
  ylab("Residual")

highlight_teams = my_teams_aug %>%
  arrange(desc(abs(.resid))) %>%
  head(4)
# With four teams with large residuals labeled
base_plot +
  geom_point(data = highlight_teams, color = "blue") +
  geom_text_repel(data = highlight_teams, color = "blue",
                  aes(label = paste(teamID, yearID)))

resid_summary = my_teams_aug %>%
  summarize(N = n(), avg = mean(.resid),
            RMSE = sqrt(mean(.resid^2)))
resid_summary
head(Teams)

#Pythagorean Formula for Winning percentage 
# formula by Bill James
my_teams = my_teams %>%
  mutate(Wpct_pyt = R^2 /(R^2 + RA^2))

# Calculate the residual: actual winning percentage
# and expected winning percentage calculated from Pythagorean formula 
my_teams = my_teams %>% 
  mutate(residuals_pyt = Wpct-Wpct_pyt)

# Residual Mean squared error calculated on the Pythagorean predictions is similar 
# in value to one calculated with the linear predictions
my_teams %>% 
  summarize(rmse = sqrt(mean(residuals_pyt^2)))

# Why the exponent in the Pythagorean model is 2? 
my_teams = my_teams %>%
  mutate(logWratio = log(W / L),
         logRratio = log(R / RA))
pytFit = lm(logWratio ~ 0 + logRratio, data = my_teams)
pytFit

# game log headers
glheaders = read_csv("C:/Users/minjk/Downloads/game_log_header.csv")
View(glheaders)

# Game information on every game played in the 2011 season
gl2011 = read_csv("C:/Users/minjk/Downloads/gl2011.txt", 
                  col_names = names(glheaders),
                  na = character())
# Red Sox games only 
BOS2011 = gl2011 %>%
  filter(HomeTeam == "BOS"|VisitingTeam == "BOS") %>%
  select(VisitingTeam, HomeTeam,
         VisitorRunsScored, HomeRunsScore)
head(BOS2011)

# Calculate the run differentials both for games won and lost 
# Add a column 'W' indicating whether the Red Sox won the game
BOS2011 = BOS2011 %>%
  mutate(Scorediff = ifelse(HomeTeam == "BOS",
                            HomeRunsScore - VisitorRunsScored,
                            VisitorRunsScored- HomeRunsScore),
         W = Scorediff > 0)

# Compute the summary statistics on the run differentials for games won and for games lost 
library(skimr)
BOS2011 %>%
  group_by(W) %>%
  skim(Scorediff)

# A team can overperform its Pythagorean winning percentage by winning a disproportionate number of close games
results = gl2011 %>%
  select(VisitingTeam, HomeTeam,
         VisitorRunsScored, HomeRunsScore) %>%
  mutate(winner = ifelse(HomeRunsScore > VisitorRunsScored,
                         HomeTeam, VisitingTeam),
         diff = abs(VisitorRunsScored - HomeRunsScore))

# Create a dataframe containing only the games decide by one-run victories 
one_run_wins = results %>%
  filter(diff == 1) %>%
  group_by(winner) %>%
  summarize(one_run_w = n())

# Relationship between the Pythagorean residuals and number of one-run victories 
# Team LA Angels
teams2011 = my_teams %>%
  filter(yearID ==2011) %>%
  mutate(teamID = ifelse(teamID == "LAA","ANA",
                         as.character(teamID))) %>%
  inner_join(one_run_wins, by = c("teamID" = "winner"))

ggplot(data = teams2011, aes(x = one_run_w, y = residuals_pyt)) +
  geom_point() +
  geom_text_repel(aes(label = teamID)) +
  xlab("One run wins") + ylab('Pythagorean residuals')

# Pitching Data
data(Pitching)
View(Pitching)

# select the pitcher seasons where more than 50 games finished by a pitcher with ERA lower than 2.50
top_closers = Pitching %>%
  filter(GF > 50 & ERA < 2.5) %>%
  select(playerID, yearID, teamID)

top_closers

# Merge top_closers with my_teams dataset, creating a dataset that contains 
# the teams featuring a top closer

my_teams %>%
  inner_join(top_closers) %>%
  pull(residuals_pyt) %>%
  summary()