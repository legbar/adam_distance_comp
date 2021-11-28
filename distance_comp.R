# renv stands for "R environment": It is a package manager
renv::status()
renv::hydrate()

library(readxl) # this will load our excel file
library(tidyverse) # this is a suite of packages that make working with R more user-friendly

response <- readxl::read_xlsx("Distance Comp Test Responses.xlsx") # read in the excel file

response_scored <- response %>%
  rename("Team" = `What Team are you?`) %>%
  pivot_longer(starts_with("How Far"), 
               names_to = "Question", 
               values_to = "Distance") %>% # pivot the question columns into question and value
  drop_na() %>% # remove NA rows (team-team member mismatch)
  mutate(`Team Member` = str_extract(Question, "(?<=Did )[:alpha:]+[:space:][:alpha:]+(?=[:space:])"), # extract team member name (It is preceded by the word Did with a space, is made of alpha characters, has a space in the middle and is followed by a space)
         Sport = str_to_sentence(str_extract(Question, "(?<=[:space:])[:alpha:]+(?=\\?)")), # extract the sport (it is preceded by a space and followed by a question mark)
         `Weighted Score` = ifelse(Sport == "Run", 
                                   Distance * 1, 
                                   ifelse(Sport == "Cycle", 
                                          Distance * 0.4, 
                                          Distance * 5))) # calculate the weighted score based on the sport (ifelse statement)

team_ranking <- response_scored %>%
  group_by(Team) %>% # group by team
  summarise(`Total Weighted Score` = sum(`Weighted Score`)) %>% # calculate the sum of weighted scores per team
  arrange(desc(`Total Weighted Score`)) # sort by total weighted score

individual_ranking <- response_scored %>%
  group_by(`Team Member`) %>%
  summarise(`Total Weighted Score` = sum(`Weighted Score`)) %>% # calculate the sum of weighted scores per individual
  arrange(desc(`Total Weighted Score`)) # sort by total weighted score (I see what you did there)

response_scored %>%
  group_by(Team, Sport) %>% # group by team
  summarise(`Total Distance` = sum(Distance)) %>% # calculate the sum of weighted scores per team
  pivot_wider(names_from = Sport, 
              values_from = `Total Distance`)

library(googledrive)

# trigger auth on purpose --> store a token in the specified cache
drive_auth(cache = ".secrets")

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

gargle::gargle_oauth_cache()

# now use googledrive with no need for explicit auth
drive_find(n_max = 5)

library(googlesheets4)

# sheets reauth with specified token and email address
gs4_auth(cache = ".secrets",
         email = TRUE)

read_sheet("https://docs.google.com/spreadsheets/d/12GxM17VqfjeNK_1zDi7mJ1_WU6FnkyW2jCm99iwaLTI/edit#gid=498276186")

# group ID to member ID ------

teams_to_members <- read_sheet("https://docs.google.com/spreadsheets/d/1xLIn8EYz43Ns7ST0bA4CPtPlDKaUWr3JITp5MCCN7uM/edit?resourcekey#gid=1308628846") %>%
  select(c(2:6)) %>%
  pivot_longer(-`What is your team name?`, 
               names_to = "Question", 
               values_to = "Answer") %>%
  rename(Team = `What is your team name?`, 
         Member = Answer) %>%
  select(-Question)

teams_to_members %>%
  pull(Team) %>%
  unique

read_sheet("https://docs.google.com/spreadsheets/d/12GxM17VqfjeNK_1zDi7mJ1_WU6FnkyW2jCm99iwaLTI/edit#gid=498276186") %>% 
  rename("Team" = `What Team are you?`) %>%
  pivot_longer(starts_with("How Far"), 
               names_to = "Question", 
               values_to = "Distance") %>% # pivot the question columns into question and value
  drop_na() %>% # remove NA rows (team-team member mismatch)
  mutate(`Team Member` = str_extract(Question, "(?<=Did )[:alpha:]+[:space:][:alpha:]+(?=[:space:])"))

