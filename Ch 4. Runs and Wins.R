library(Lahman)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stats)
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
