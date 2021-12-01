#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(googlesheets4)
library(shinyMatrix)
library(DT)

# sheets reauth with specified token and email address
gs4_auth(cache = ".secrets",
         email = TRUE)

# Define UI for application that draws a histogram

header <- dashboardHeader(title = "Equicall Distance Competition 2022", 
                          # Set height of dashboardHeader
                          tags$li(class = "dropdown",
                                  tags$style(".main-header .navbar {
    margin-left: 400px;
}"), 
                                  tags$style(".main-header .logo {
    width: 400px;
}")))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overall Rankings", tabName = "overall_rankings", icon = icon("dashboard")),
    menuItem("Running Rankings", tabName = "running_rankings", icon = icon("dashboard")),
    menuItem("Cycling Rankings", tabName = "cycling_rankings", icon = icon("dashboard")),
    menuItem("Swimming Rankings", tabName = "swimming_rankings", icon = icon("dashboard"))
  )
)


body <- dashboardBody(tabItems(
  # First tab content
  tabItem(tabName = "overall_rankings",
          # fluidRow(
          #   box(title = "DEBUG",
          #       verbatimTextOutput("debug"))
          # ),
          fluidRow(
            box(title = "Team Overall Ranking",
                DTOutput("team_overall_ranking")),
            box(title = "Individual Overall Ranking",
                DTOutput("individual_overall_ranking"))
          )),
  tabItem(tabName = "running_rankings",
          fluidRow(
            box(title = "Team Running Ranking",
                DTOutput("team_running_ranking")),
            box(title = "Individual Running Ranking",
                DTOutput("individual_running_ranking"))
          )),
  tabItem(tabName = "cycling_rankings",
          fluidRow(
            box(title = "Team Cycling Ranking",
                DTOutput("team_cycling_ranking")),
            box(title = "Individual Cycling Ranking",
                DTOutput("individual_cycling_ranking"))
          )),
  tabItem(tabName = "swimming_rankings",
          fluidRow(
            box(title = "Team Swimming Ranking",
                DTOutput("team_swimming_ranking")),
            box(title = "Individual Swimming Ranking",
                DTOutput("individual_swimming_ranking"))
          ))
))

ui <- dashboardPage(
  header, sidebar, body
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # # import the team registration document and produce a table of teams to members
  # teams_to_members <- read_sheet("https://docs.google.com/spreadsheets/d/1xLIn8EYz43Ns7ST0bA4CPtPlDKaUWr3JITp5MCCN7uM/edit?resourcekey#gid=1308628846") %>%
  #   select(c(2:6)) %>%
  #   pivot_longer(-`What is your team name?`, 
  #                names_to = "Question", 
  #                values_to = "Answer") %>%
  #   rename(Team = `What is your team name?`, 
  #          Member = Answer) %>%
  #   select(-Question)
  # 
  # # create a reactive variable of available teams
  # available_teams <- reactive({
  #   req(teams_to_members)
  #   teams_to_members %>%
  #     pull(Team) %>%
  #     unique
  # })
  # 
  # # update the team selection input with the available teams
  # observeEvent(available_teams(), {
  #   updateSelectizeInput(inputId = "team_selection", 
  #                        choices = available_teams())
  # })
  # 
  # # print the currently selected team
  # output$selected_team <- renderPrint({
  #   print(input$team_selection)
  # })
  # 
  # # create a reactive list of available members based on the selected team
  # available_members <- reactive({
  #   req(available_teams, 
  #       input$team_selection)
  #   teams_to_members %>%
  #     filter(Team == input$team_selection) %>%
  #     pull(Member)
  # })
  # 
  # # print the available members
  # output$available_members <- renderPrint({
  #   print(available_members())
  # })
  # 
  # # update the member selection input with the available members
  # observeEvent(available_members(), {
  #   updateSelectizeInput(inputId = "member_selection", 
  #                        choices = available_members())
  # })
  
  
  responses <- reactive({
    response <- read_sheet("https://docs.google.com/spreadsheets/d/12GxM17VqfjeNK_1zDi7mJ1_WU6FnkyW2jCm99iwaLTI/edit#gid=498276186") %>%
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
                                              Distance * 5)))
  })
  
  sport_team_rankings <- reactive({
    req(responses())
    object <- 
      responses() %>%
      group_by(Sport, Team) %>%
      summarise(`Total Distance` = sum(`Distance`)) %>%
      arrange(desc(`Total Distance`)) %>%
      group_split()
    
    names(object) <- 
      lapply(object, function(x){
        x$Sport %>% unique
      })
    
    object <- lapply(object, 
                                  function(x){
                                    x %>%
                                      select(-Sport) %>%
                                      ungroup %>%
                                      mutate(.before = everything(), 
                                             Rank = row_number())
                                  })
  })
  
  sport_individual_rankings <- reactive({
    req(responses())
    object <-
      responses() %>%
      group_by(Sport, `Team Member`) %>%
      summarise(`Total Distance` = sum(`Distance`)) %>%
      arrange(desc(`Total Distance`)) %>%
      group_split()
    
    names(object) <-
      lapply(object, function(x) {
        x$Sport %>% unique
      })
    
    object <- lapply(object,
                     function(x) {
                       x %>%
                         select(-Sport) %>%
                         ungroup %>%
                         mutate(.before = everything(), 
                                Rank = row_number())
                     })
  })
  
  
  # render the table of overall team rankings
  output$team_overall_ranking <- renderDT({
    req(responses())
    responses() %>%
      group_by(Team) %>% # group by team
      summarise(`Total Weighted Score` = sum(`Weighted Score`)) %>% # calculate the sum of weighted scores per team
      arrange(desc(`Total Weighted Score`)) %>% 
      ungroup %>%
      mutate(.before = everything(), 
             Rank = row_number()) %>%
      datatable(options = list(dom = 't'), rownames = F)
  })
  
  # render the table of overall individual rankings
  output$individual_overall_ranking <- renderDT({
    req(responses())
    responses() %>%
      group_by(`Team Member`) %>% # group by team
      summarise(`Total Weighted Score` = sum(`Weighted Score`)) %>% # calculate the sum of weighted scores per team
      arrange(desc(`Total Weighted Score`)) %>% 
      ungroup %>%
      mutate(.before = everything(), 
             Rank = row_number()) %>%
      datatable(options = list(dom = 't'), rownames = F)
  })
  
  # render the table of running team rankings
  output$team_running_ranking <- renderDT({
    req(sport_team_rankings())
    sport_team_rankings()[["Run"]] %>%
      datatable(options = list(dom = 't'), rownames = F)
  })
  
  # render the table of overall individual rankings
  output$individual_running_ranking <- renderDT({
    req(sport_individual_rankings())
    sport_individual_rankings()[["Run"]] %>%
      datatable(options = list(dom = 't'), rownames = F)
  })
  
  # render the table of running team rankings
  output$team_cycling_ranking <- renderDT({
    req(sport_team_rankings())
    sport_team_rankings()[["Cycle"]] %>%
      datatable(options = list(dom = 't'), rownames = F)
  })
  
  # render the table of overall individual rankings
  output$individual_cycling_ranking <- renderDT({
    req(sport_individual_rankings())
    sport_individual_rankings()[["Cycle"]] %>%
      datatable(options = list(dom = 't'), rownames = F)
  })
  
  # render the table of running team rankings
  output$team_swimming_ranking <- renderDT({
    req(sport_team_rankings())
    sport_team_rankings()[["Swim"]] %>%
      datatable(options = list(dom = 't'), rownames = F)
  })
  
  # render the table of overall individual rankings
  output$individual_swimming_ranking <- renderDT({
    req(sport_individual_rankings())
    sport_individual_rankings()[["Swim"]] %>%
      datatable(options = list(dom = 't'), rownames = F)
  })
  

  
  output$debug <- renderPrint({
    print(sport_individual_rankings())
  })
  
  
}






# Run the application 
shinyApp(ui = ui, server = server)
