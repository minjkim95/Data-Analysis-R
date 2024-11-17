library(readr)
library(dplyr)
library(Hmisc)
install.packages("Hmisc")
install.packages("psych")
library(psych)
library(ggplot2)
#Warren Spahn Topps Data
spahn <- read_csv("C:/Users/minjk/Downloads/spahn.csv")
View(spahn)

# Display first three rows and columns 1 through 10 of the spahn dataframe
# %>% pass the left hand side operator to the first argument of the right hand side of the operator 
spahn %>% slice(1:3) %>% select(1:10)

#
#Display Age, Wins, Loses, ERA for the first 10 seasons
spahn %>% slice(1:10) %>% select(Age, W, L, ERA)

#Descriptive statistics of ERA
describe(spahn$ERA)

spahn %>% 
  summarize(LO = min(ERA),
            QL = quantile(ERA, 0.25), QU = quantile(ERA, 0.75),
            M = median(ERA), HI = max(ERA))

# Find age when Spahn had his lowest ERA
spahn %>% filter(ERA == min(ERA)) %>% select(Age)

#Add new sabermetric FIP(Fielding independent pitching) variable to spahn's dataset

spahn %>% 
  mutate(FIP = (13 * HR + 3 * BB - 2 * SO)/IP) -> spahn

# Sort the dataset by FIP measure
spahn %>%
  arrange(FIP) %>% 
  select(Year,Age, W, L, ERA, FIP) %>%
  head()

# We want to compare Spahn's pitching for Boston and Milwaukee
# subset spahn1 for two teams 
spahn %>% filter(Tm == 'BSN' | Tm == "MLN") -> spahn1

# Redefine Team 
spahn1 = spahn1 %>%
  mutate(Tm = factor(Tm, levels = c("BSN", "MLN")))

#Spahn's statistics summary grouped by two teams 
spahn1 %>%
  group_by(Tm) %>%
  summarize(mean_W.L = mean(W.L, na.rm = TRUE),
            mean_ERA = mean(ERA),
            mean_WHIP = mean(WHIP),
            mean_FIP = mean(FIP))

# World series game record with fewer than 8 games played
library(Lahman)
data("SeriesPost")
SeriesPost = as.data.frame(SeriesPost)
View(SeriesPost)
ws = filter(SeriesPost, yearID >= 1903, round == "WS", wins+losses <8)

# Bar graph of the number of games played in best of seven World Series since 1903
ggplot(ws,aes(x = wins+losses)) + 
  geom_bar(fill = "#00abff") +
  labs(x = "Number of games", y = "Frequency")
