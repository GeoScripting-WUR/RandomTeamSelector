# Script for randomly assigning teams of 3 into an exercise discussion group.
# Output is integers listing the discussion group number.
# License: CC0 (https://creativecommons.org/publicdomain/zero/1.0/)

library(shiny)
library(stringr)

## Update these values!
## ====================
RepoLink = "https://github.com/geoscripting-2021"

DeliverableNames = c(paste("Exercise", 1:3),
                     "Assignment 1",
                     paste("Exercise", 4:7),
                     "Assignment 2",
                     paste("Exercise", 8:11),
                     "Assignment 3",
                     "Project proposal",
                     "Project (Gaia 2)",
                     "Project (Lumen 1)",
                     "Project (Lumen 2)")

TeamNames = c(
  "IBBizzle",
  "roaring bold fox",
  "Baguette",
  "Yneke en Suzan",
  "lovely straight bull",
  "Team Zeeland",
  "Puzzles",
  "Imperfect GeoCircle",
  "V and B",
  "Ultimate gold",
  "Geowizards",
  "Andrew & Katherine",
  "lush convivial owl",
  "Chicago bulls",
  "Geoscripting Geckoes",
  "smoky prehistoric basilisk",
  "enchanted remarkable goshawk",
  "arcane unique groundhog",
  "Team MeMo",
  "The horse",
  "The Toxic Pythons",
  "spectacular wonderful finch",
  "glaring meaty mouflon",
  "TeamScripting",
  "The Geodudes",
  "cautious groovy asp",
  "enthusiastic crystal bug",
  "heavenly defiant lion",
  "arrogant cyber crow"
)
TeamList = rep(list(TeamNames), length(DeliverableNames)) 
## After there are any changes to teams over time, overwrite the slots like this:
# TeamList[Dstart:Dend] = rep(list(newTeamNames), rangeLen)
## Where `Dstart` and `Dend` are the indices of the first and last deliverable in 
## the range to which the new team names apply.
## `rangeLen` is the length of the range (length of `Dstart:Dend`)

# Set project teams; Change if team changes occur for the project
ProjectTeams = c(
  "IBBizzle",
  "roaring bold fox",
  "Baguette",
  "Yneke en Suzan",
  "lovely straight bull",
  "Team Zeeland",
  "Puzzles",
  "Imperfect GeoCircle",
  "V and B",
  "Ultimate gold",
  "Geowizards",
  "Andrew & Katherine",
  "lush convivial owl",
  "Chicago bulls",
  "Geoscripting Geckoes",
  "smoky prehistoric basilisk",
  "enchanted remarkable goshawk",
  "arcane unique groundhog",
  "Team MeMo",
  "Pygasus",
  "The Toxic Pythons",
  "spectacular wonderful finch",
  "glaring meaty mouflon",
  "TeamScripting",
  "The Geodudes",
  "cautious groovy asp",
  "enthusiastic crystal bug",
  "heavenly defiant lion"
)

# Set project presentation subgroups; fill when division is known
# Arrays need to have some value, to preserve the length of TeamList
ProjectGaia2 = c('[No teams assigned]')
ProjectLumen1 = c('[No teams assigned]')
ProjectLumen2 = c('[No teams assigned]')

# Set team lists for project deliverables (split by subgroup)
TeamList[[length(DeliverableNames) - 3]] = ProjectTeams
TeamList[[length(DeliverableNames) - 2]] = ProjectGaia2
TeamList[[length(DeliverableNames) - 1]] = ProjectLumen1
TeamList[[length(DeliverableNames)]] = ProjectLumen2

Year = 2021
Seed = 0xfedbeef
## ====================

ExerciseGroupSize = 3 # We want groups of 3 teams
AssignmentGroupSize = 3 # But only one for graded assignments

# https://github.com/geoscripting-2021/Exercise_1_Starter/pulls?utf8=%E2%9C%93&q=is%3Apr+Teamname
DeliverableURLs = paste0(RepoLink, "/", c(
    paste0("Exercise_", 1:3, "_"),
    "Assignment_1_",
    paste0("Exercise_", 4:7, "_"),
    "Assignment_2_",
    paste0("Exercise_", 8:11, "_"),
    "Assignment_3_",
    "Project-",
    "Project-",
    "Project-",
    "Project-"
    ), "Starter-")

ExerciseIDs = grep("Exercise", DeliverableNames)
FreeDays = which(DeliverableNames == "Exercise 8" | DeliverableNames == "Exercise 9")

# Sanity checks
stopifnot(length(DeliverableNames) == length(TeamList)) # We need one set of teams for each deliverable
stopifnot(length(DeliverableNames) == length(DeliverableURLs))
if (any(sapply(TeamList, anyDuplicated)))
    stop("Duplicate names found!")

names(TeamList) = DeliverableNames

# Exclude teams from specific exercises
# * teamName (character): name of the team
# * exerciseNames (character vector): vector of exercise names
excludeTeams <- function(teamName, exerciseNames) {
  for (exercise in exerciseNames) {
    TeamList[[exercise]] = TeamList[[exercise]][TeamList[[exercise]] != teamName]
  }
  return(TeamList)
}
TeamList <- excludeTeams('Team Zeeland', c('Exercise 3'))
TeamList <- excludeTeams("roaring bold fox", c('Exercise 10', 'Exercise 11', 'Assignment 3'))

# Web UI
ui = fluidPage(
  titlePanel("Geoscripting review group generator"),
  sidebarLayout(
    sidebarPanel(
      tags$p("This Shiny app tells you which teams you should go to in order to discuss the answers to the exercise of the day, or which team to review for the assignments of the week.",
      tags$b("Please do not write these down as teams may change throughout the course! Always rerun this app!")),
      tags$p(""),
      selectInput("DeliverableName", label = "Assignment or exercise to review:", choices=DeliverableNames),
      selectInput("MyTeam", 
        label = "Your team name:",
        choices = c("SELECT YOUR TEAM NAME", sort(TeamList[[1]]))),
      helpText("Hint: if you click on the boxes above and press Backspace, you can search for your team name by typing it."),
      tags$div(class="header", checked=NA,
               tags$a(href="https://github.com/GeoScripting-WUR/RandomTeamSelector", "Source code of this Shiny app")
      )
    ),

    mainPanel(
      htmlOutput("Result")
    )
  )
)

# Actual random algorithm
server = function(input, output, session)
{
  output$Result <- renderUI({
    DeliverableID = which(DeliverableNames == input$DeliverableName) # Alternatively could use TeamList[[input$DeliverableName]] everywhere
    updateSelectInput(session, "MyTeam", choices=c("SELECT YOUR TEAM NAME", sort(TeamList[[DeliverableID]])), selected=ifelse(input$MyTeam %in% TeamList[[DeliverableID]], input$MyTeam, "SELECT YOUR TEAM NAME"))
    
    if (input$MyTeam == "SELECT YOUR TEAM NAME")
        tags$p("Please select the exercise or assignment number by using the first drop-down box, and then select your team name from the second drop-down box.")
    else if (DeliverableID %in% FreeDays)
        tags$p("There is no exercise that needs to be reviewed today.")
    else
    {
        GroupSize = if (DeliverableID %in% ExerciseIDs) ExerciseGroupSize else AssignmentGroupSize
        TeamNames = TeamList[[DeliverableID]]
        names(TeamNames) = TeamNames
        
        if (!input$MyTeam %in% TeamNames)
            paste0("Your team (", input$MyTeam, ") was not registered to ", input$DeliverableName, ", please contact the course coordinators to check if you should review or participate in the discussions.")
        else
        {
            set.seed(DeliverableID+Year+Seed)
            Weights = runif(length(TeamNames))
            # names(weights) = TeamNames
            GroupNumbers = ceiling(order(Weights) / GroupSize)
            MyGroup = GroupNumbers[TeamNames == input$MyTeam]
            OtherTeams = TeamNames[GroupNumbers == MyGroup]
            OtherTeams = OtherTeams[OtherTeams != input$MyTeam]
            OtherTeamsFiltered = str_replace_all(OtherTeams, "[^a-zA-Z0-9\\-_]", "_")
            TeamURLs = paste0(DeliverableURLs[DeliverableID], sapply(OtherTeamsFiltered, URLencode, reserved=TRUE), '/issues')
            stopifnot(length(MyGroup) == 1)
        
            switch(length(OtherTeams)+1,
                paste0("For ", input$DeliverableName, ", your team (", input$MyTeam, ") is all alone! You can discuss with staff or review whatever group you prefer."),
                tags$p(paste0("For ", input$DeliverableName, ", your team (", input$MyTeam, ") should review the work of team"), tags$a(OtherTeams[1], href=TeamURLs[1], target="_blank"), "."),
                tags$p(paste0("For ", input$DeliverableName, ", your team (", input$MyTeam, ") should review the work of teams"), tags$a(OtherTeams[1], href=TeamURLs[1], target="_blank"), "and", tags$a(OtherTeams[2], href=TeamURLs[2], target="_blank"), ".")
            )
        }
    }
  })
}

shinyApp(ui = ui, server = server)

# Run with shiny::runGist("a5e7ed83ab16c0125ca11079adfbaf50")
