renv::status()
renv::hydrate()

library(readxl)
library(tidyverse)

response <- readxl::read_xlsx("Distance Comp Test Responses.xlsx")

response_scored <- response %>%
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
  group_by(`What Team are you?`) %>% # group by team
  summarise(`Total Weighted Score` = sum(`Weighted Score`)) %>% # calculate the sum of weighted scores per team
  arrange(desc(`Total Weighted Score`)) # sort by total weighted score

individual_ranking <- response_scored %>%
  group_by(`Team Member`) %>%
  summarise(`Total Weighted Score` = sum(`Weighted Score`)) %>% # calculate the sum of weighted scores per individual
  arrange(desc(`Total Weighted Score`)) # sort by total weighted score (I see what you did there)

