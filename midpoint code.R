setwd("~/College/Year 4/Project/R Code/Data")
#load required libraries
library(tidyverse)
# selection: Load and examine data
#update file paths to ensure compatibility
file_2004_05 <- read.csv("./2004-05.csv", stringsAsFactors = FALSE, encoding = "latin1")
file_2019_20 <- read.csv("./2019-20.csv", stringsAsFactors = FALSE, encoding = "latin1")
# Summarize data structure and first few rows
summary(file_2004_05)
summary(file_2019_20)
head(file_2004_05)
head(file_2019_20)
#select relevant columns (goals, results, and referee decisions)
data_2004 <- file_2004_05 %>% select(Div, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HR, AR, HY, AY, HF, AF, Referee)
data_2019 <- file_2019_20 %>% select(Div, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HR, AR, HY, AY, HF, AF, Referee)
#preprocessing: Handle missing data
#check for missing values
data_2004_clean <- data_2004 %>% drop_na()
data_2019_clean <- data_2019 %>% drop_na()
#convert date to proper format
data_2004_clean$Date <- as.Date(data_2004_clean$Date, format="%d/%m/%y")
data_2019_clean$Date <- as.Date(data_2019_clean$Date, format="%d/%m/%y")
#transformation:
#calculate goal differences
data_2004_clean <- data_2004_clean %>% mutate(GoalDiff = FTHG - FTAG)
data_2019_clean <- data_2019_clean %>% mutate(GoalDiff = FTHG - FTAG)
#add total red cards column
data_2004_clean <- data_2004_clean %>% mutate(TotalRedCards = HR + AR)
data_2019_clean <- data_2019_clean %>% mutate(TotalRedCards = HR + AR)
#investigate red card totals and matches
# Total yellow and red cards
cat("Pre-VAR Total Yellow Cards:", sum(data_2004_clean$HY + data_2004_clean$AY), "\n")
cat("Pre-VAR Total Red Cards:", sum(data_2004_clean$HR + data_2004_clean$AR), "\n")
cat("Post-VAR Total Yellow Cards:", sum(data_2019_clean$HY + data_2019_clean$AY), "\n")
cat("Post-VAR Total Red Cards:", sum(data_2019_clean$HR + data_2019_clean$AR), "\n")
#check match counts
cat("Pre-VAR Matches:", nrow(data_2004_clean), "\n")
cat("Post-VAR Matches:", nrow(data_2019_clean), "\n")
#identify potential outliers (matches with no red cards but high yellow cards)
outliers_pre_var <- data_2004_clean %>% filter(TotalRedCards == 0 & (HY + AY) > 10)
outliers_post_var <- data_2019_clean %>% filter(TotalRedCards == 0 & (HY + AY) > 10)
cat("Pre-VAR Outliers:\n")
print(outliers_pre_var)
cat("Post-VAR Outliers:\n")
print(outliers_post_var)
#yellow card trends per season
#aggregate yellow cards by date for both datasets
data_2004_clean <- data_2004_clean %>% mutate(Season = "2004-05")
data_2019_clean <- data_2019_clean %>% mutate(Season = "2019-20")
yellow_card_trends <- bind_rows(
  data_2004_clean %>% group_by(Date) %>% summarize(TotalYellowCards = sum(HY + AY), Season = first(Season)),
  data_2019_clean %>% group_by(Date) %>% summarize(TotalYellowCards = sum(HY + AY), Season = first(Season))
)
#plot yellow card trends
ggplot(yellow_card_trends, aes(x = Date, y = TotalYellowCards, color = Season)) +
  geom_line() +
  labs(title = "Yellow Card Trends Per Season", x = "Date", y = "Total Yellow Cards", color = "Season") +
  theme_minimal()
#red card trends per season
# aggregate red cards by date for both datasets
red_card_trends <- bind_rows(
  data_2004_clean %>% group_by(Date) %>% summarize(TotalRedCards = sum(HR + AR), Season = first(Season)),
  data_2019_clean %>% group_by(Date) %>% summarize(TotalRedCards = sum(HR + AR), Season = first(Season))
)
# Plot red card trends
ggplot(red_card_trends, aes(x = Date, y = TotalRedCards, color = Season)) +
  geom_line() +
  labs(title = "Red Card Trends Per Season", x = "Date", y = "Total Red Cards", color = "Season") +
  theme_minimal()
# Plot cumulative yellow cards
ggplot(cumulative_cards, aes(x = Date, y = CumulativeYellowCards, color = Season)) +
  geom_line() +
  labs(title = "Cumulative Yellow Cards Per Season", x = "Date", y = "Cumulative Yellow Cards", color = "Season") +
  theme_minimal()
# Plot cumulative red cards
ggplot(cumulative_cards, aes(x = Date, y = CumulativeRedCards, color = Season)) +
  geom_line() +
  labs(title = "Cumulative Red Cards Per Season", x = "Date", y = "Cumulative Red Cards", color = "Season") +
  theme_minimal()
#pie Chart: distribution of Yellow and Red Cards Pre- and Post-VAR
#aggregate totals for pie chart
total_yellow_red <- data.frame(
  CardType = c("Yellow Cards (Pre-VAR)", "Red Cards (Pre-VAR)",
               "Yellow Cards (Post-VAR)", "Red Cards (Post-VAR)"),
  Count = c(
    sum(data_2004_clean$HY + data_2004_clean$AY),  # Pre-VAR Yellow Cards
    sum(data_2004_clean$HR + data_2004_clean$AR),  # Pre-VAR Red Cards
    sum(data_2019_clean$HY + data_2019_clean$AY),  # Post-VAR Yellow Cards
    sum(data_2019_clean$HR + data_2019_clean$AR)   # Post-VAR Red Cards
  )
)
#create pie chart
ggplot(total_yellow_red, aes(x = "", y = Count, fill = CardType)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Yellow and Red Cards Pre- and Post-VAR",
       fill = "Card Type") +
  theme_void() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, size = 14))
#export Final Cleaned Datasets
#save pre-VAR cleaned dataset
write.csv(data_2004_clean, "data_2004_clean.csv", row.names = FALSE)
#save post-VAR cleaned dataset
write.csv(data_2019_clean, "data_2019_clean.csv", row.names = FALSE)
cat("Cleaned datasets exported: data_2004_clean.csv and data_2019_clean.csv
")
#final Cleaned Datasets
write.csv(data_2004_clean, "data_2004_clean.csv", row.names = FALSE)
#save post-VAR cleaned dataset
write.csv(data_2019_clean, "data_2019_clean.csv", row.names = FALSE)
cat("Cleaned datasets exported: data_2004_clean.csv and data_2019_clean.csv
")
#save post-VAR cleaned dataset
write.csv(data_2019_clean, "data_2019_clean.csv", row.names = FALSE)
cat("Cleaned datasets exported: data_2019_clean.csv
")
write.csv(data_2019_clean, "data_2019_clean.csv", row.names = FALSE)
savehistory(file = "R_session_history.txt")
