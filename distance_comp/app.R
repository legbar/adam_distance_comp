#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(googlesheets4)
library(shinyMatrix)

# sheets reauth with specified token and email address
gs4_auth(cache = ".secrets",
         email = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Equicall Distance Competition 2022"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          selectizeInput("team_selection", 
                         "Choose your team", 
                         choices = NULL, 
                         multiple = F),
          
          tags$hr(), # horizontal line break

          # 
          # selectizeInput("member_selection", 
          #                "Select the Team Member", 
          #                choices = NULL, 
          #                multiple = F),
          # 
          # tags$hr(), # horizontal line break
          # 
          # selectizeInput("sport_selection", 
          #                "Select the Sport", 
          #                choices = c("Running", 
          #                            "Cycling", 
          #                            "Swimming", 
          #                            "Rowing"), 
          #                multiple = F),
          # 
          # tags$hr(), # horizontal line break
          # 
          # numericInput("distance", 
          #                "Enter the Distance", 
          #              0),
          # 
          verbatimTextOutput("selected_team"),
          verbatimTextOutput("available_members")
          
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30)
        ),
    
    

        # Show a plot of the generated distribution
        mainPanel(
          matrixInput("matrix2",
                      rows = list(
                        names = TRUE,
                        editableNames = TRUE),
                      value = matrix(data = 0,
                                     nrow = 4, 
                                     ncol = 4, 
                                     dimnames = list(c("Running", 
                                                       "Cycling", 
                                                       "Swimming", 
                                                       "Rowing"), 
                                                     c("A", 
                                                       "B", 
                                                       "C", 
                                                       "D")))),
          tableOutput("team_ranking")
           # plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # import the team registration document and produce a table of teams to members
  teams_to_members <- read_sheet("https://docs.google.com/spreadsheets/d/1xLIn8EYz43Ns7ST0bA4CPtPlDKaUWr3JITp5MCCN7uM/edit?resourcekey#gid=1308628846") %>%
    select(c(2:6)) %>%
    pivot_longer(-`What is your team name?`, 
                 names_to = "Question", 
                 values_to = "Answer") %>%
    rename(Team = `What is your team name?`, 
           Member = Answer) %>%
    select(-Question)
  
  # create a reactive variable of available teams
  available_teams <- reactive({
    req(teams_to_members)
    teams_to_members %>%
      pull(Team) %>%
      unique
  })
  
  # update the team selection input with the available teams
  observeEvent(available_teams(), {
    updateSelectizeInput(inputId = "team_selection", 
                         choices = available_teams())
  })
  
  # print the currently selected team
  output$selected_team <- renderPrint({
    print(input$team_selection)
  })
  
  # create a reactive list of available members based on the selected team
  available_members <- reactive({
    req(available_teams, 
        input$team_selection)
    teams_to_members %>%
      filter(Team == input$team_selection) %>%
      pull(Member)
  })
  
  # print the available members
  output$available_members <- renderPrint({
    print(available_members())
  })
  
  # update the member selection input with the available members
  observeEvent(available_members(), {
    updateSelectizeInput(inputId = "member_selection", 
                         choices = available_members())
  })
  
  
  
  
  
  
  
  # render the table of overall team rankings
  output$team_ranking <- renderTable({
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
                                              Distance * 5))) %>%
      group_by(Team) %>% # group by team
      summarise(`Total Weighted Score` = sum(`Weighted Score`)) %>% # calculate the sum of weighted scores per team
      arrange(desc(`Total Weighted Score`))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
