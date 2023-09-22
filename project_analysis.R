#Zack Lasek & Max Stevens
#5.1.2023
#Project Code

rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)

#Load in merged and cleaned file that was created in cleaned and scrapped R script
cbb3<-read.csv("cbb3.csv", na.strings = c(""," ", "NA"))

### Q1: Which team has the highest winning percentage over the entire dataset, and how many times did they win the championship, if at all?

cbb3$POSTSEASON <- ifelse(is.na(cbb3$POSTSEASON), 0, cbb3$POSTSEASON)
cbb3$win_pct <- cbb3$W / cbb3$G

# Group the data by Team and calculate mean win percentage for each team
team_summary <- cbb3 %>%
  group_by(School) %>%
  summarize(win_pct = mean(win_pct), 
            num_championships = sum(POSTSEASON == "Champions"))

# Sort the data by win percentage in descending order and keep only the top 5 teams
top_5_teams <- team_summary %>%
  arrange(desc(win_pct)) %>%
  head(5) %>%
  mutate(win_pct = paste0(round(win_pct*100, 2), "%"))

# Print the summary table - USING AS VISUAL
print(top_5_teams)


### Q2: Are there any conferences that have a noticeable difference in refereeing (Free Throws Attempted & Free Throws Allowed)?

# Group the data by conference and calculate the mean FTR for each conference
conference_summary <- cbb3 %>%
  group_by(CONF) %>%
  summarize(avg_ftr = mean(FTR, na.rm = TRUE))

# Sort the data by avg_ftr in descending order and keep only the top 5 conferences
top_10_conferences_ft <- conference_summary %>%
  arrange(desc(avg_ftr)) %>%
  head(10)

print(top_10_conferences_ft)
#VISUALIZATION
ggplot(top_10_conferences_ft, aes(x = reorder(CONF, -avg_ftr), y = avg_ftr)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Average FTR by Conference") +
  xlab("Conference") +
  ylab("Average FTR")


### Q3: Which conference has done the best in the NCAA Tournament (has the lowest avg POSTSEASON exit round)?

conference_champs <- cbb3 %>%
  filter(POSTSEASON == "Champions") %>%
  group_by(CONF) %>%
  summarize(num_champs = n())%>%
  arrange(desc(num_champs))

print(conference_champs)
#VISUALIZATION
ggplot(conference_champs, aes(x = reorder(CONF, -num_champs), y = num_champs)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Number of Championships Appearances by Conference") +
  xlab("Conference") +
  ylab("") +
  geom_text(aes(label = num_champs), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 360, vjust = 0.5, hjust=1)) 

#FINAL4
conference_final4 <- cbb3 %>%
  filter(POSTSEASON == "F4" | POSTSEASON == "Champions") %>%
  group_by(CONF) %>%
  summarize(num_final4 = n())%>%
  arrange(desc(num_final4))

print(conference_final4)
#VISUALIZATION
ggplot(conference_final4, aes(x = reorder(CONF, -num_final4), y = num_final4)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Number of Final 4 Appearances by Conference") +
  xlab("Conference") +
  ylab("") +
  geom_text(aes(label = num_final4), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 360, vjust = 0.5, hjust=1)) 

#ELITE 8
conference_elite8 <- cbb3 %>%
  filter(POSTSEASON == "E8" | POSTSEASON == "F4" | POSTSEASON == "Champions") %>%
  group_by(CONF) %>%
  summarize(num_elite8 = n())%>%
  arrange(desc(num_elite8))

print(conference_elite8)

#VISUALIZATION
ggplot(conference_elite8, aes(x = reorder(CONF, -num_elite8), y = num_elite8)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Number of Elite 8 Appearances by Conference") +
  xlab("Conference") +
  ylab("") +
  geom_text(aes(label = num_elite8), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 360, vjust = 0.5, hjust=1)) 

#SWEET16
conference_sweet16 <- cbb3 %>%
  filter(POSTSEASON == "S16" | POSTSEASON == "E8" | POSTSEASON == "F4" | POSTSEASON == "Champions") %>%
  group_by(CONF) %>%
  summarize(num_sweet16 = n())%>%
  arrange(desc(num_sweet16))

print(conference_sweet16)
#VISUALIZATION
ggplot(conference_sweet16, aes(x = reorder(CONF, -num_sweet16), y = num_sweet16)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Number of Sweet 16 Appearances by Conference") +
  xlab("Conference") +
  ylab("") +
  geom_text(aes(label = num_sweet16), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 360, vjust = 0.5, hjust=1)) 

### Q4: Did certain conferences have a much higher SRS and SOS among their teams compared to others, meaning they are overall better conferences?

# Group the data by conference and calculate the mean FTR for each conference
conference_srs <- cbb3 %>%
  group_by(CONF) %>%
  summarize(avg_srs = mean(SRS, na.rm = TRUE))

# Sort the data by avg_ftr in descending order and keep only the top 5 conferences
top_10_conferences_srs <- conference_srs %>%
  arrange(desc(avg_srs)) %>%
  head(10)

print(top_10_conferences_srs)

#VISUALIZATION
ggplot(top_10_conferences_srs, aes(x = reorder(CONF, -avg_srs), y = avg_srs)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Average SRS by Conference") +
  xlab("Conference") +
  ylab("Average SRS")

conference_sos <- cbb3 %>%
  group_by(CONF) %>%
  summarize(avg_sos = mean(SOS, na.rm = TRUE))

top_10_conferences_sos <- conference_sos %>%
  arrange(desc(avg_sos)) %>%
  head(10)

print(top_10_conferences_sos)

#VISUALIZATION
ggplot(top_10_conferences_sos, aes(x = reorder(CONF, -avg_sos), y = avg_sos)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Average SOS by Conference") +
  xlab("Conference") +
  ylab("Average SOS")


### Q5: Do teams with higher scoring offenses and play at a faster pace lead to more turnovers for those teams? Do both matter, one or the other, or neither?

efficient_summary <- cbb3 %>% 
  group_by(School) %>% 
  summarize(Avg_ADJOE = mean(ADJOE, na.rm = TRUE)) %>% # corrected code
  arrange(desc(Avg_ADJOE)) %>%   # sort by Avg_ADJOE in descending order
  slice(1:10)    

efficient_summary

ggplot(efficient_summary, aes(x = reorder(School, Avg_ADJOE), y = Avg_ADJOE)) +
  geom_col(fill = "steelblue") +
  labs(x = "Team", y = "Avg_ADJOE") +
  theme_minimal() +
  coord_flip()

turnover_summary <- cbb3 %>% 
  group_by(School) %>% 
  summarize(Avg_TOV = mean(TOV, na.rm = TRUE)) %>%
  arrange(desc(Avg_TOV)) %>%  
  slice(1:10)    

ggplot(turnover_summary, aes(x = reorder(School, Avg_TOV), y = Avg_TOV)) +
  geom_col(fill = "steelblue") +
  labs(x = "Team", y = "Avg_TOV") +
  theme_minimal() +
  coord_flip()
