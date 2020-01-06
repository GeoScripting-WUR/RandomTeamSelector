# Script for randomly assigning teams of 3 into an exercise discussion group.
# Output is integers listing the discussion group number.
# License: CC0 (https://creativecommons.org/publicdomain/zero/1.0/)

library(shiny)

## Update these values!
## ====================
TeamNames = c(
"Mandalorians",
"Fried Plantain",
"De 2J's",
"MGI Old Version",
"Marijn en Daan",
"aMEEzing Geoscripting team",
"Alena and Isra",
"Geo Scripting Masters",
"Gothic Concrete Tractors",
"BubblyCoders",
"Chloe and Agata",
"de koffiedrinkers",
"Apples and oranges",
"GitGud",
"Los Codificadores",
"GeoScriptors ",
"Lleida Gang",
"Amandine & Tara",
"JR",
"RaffeJanssen",
"Llama",
"Wil and Rich",
"Whirling Thunder Anacondas ",
"TheBuggers",
"Leroy and Stefan",
"Way, No Way",
"team rocket",
"Jorinator",
"Hot chocolate",
"GozdeYosef",
"Pyrates (R!)",
"LaiyaKuaihuoya"
)
TeamList = rep(list(TeamNames), 16) ## After there are any changes to teams over time, overwrite the slots like this:
# ProjectTeams = c(
# "MMXIX",
# "Guardians of the Galaxy",
# "Mindwrap_Error",
# "The Happy Penguins",
# "R-mateys",
# "gisrsartists",
# "Team Sheet",
# "WHAT?!",
# "soilandwater",
# "The Appies",
# "Nimma Gang",
# "Team Coffee",
# "Firebrand",
# "VB13",
# "Green Cobra",
# "GreenGrain",
# "Spectacular Bathduck",
# "Stan + Guus",
# "Blossom",
# "Reflective Duck",
# "DduDduDdu",
# "CreativeTeamName",
# "Building B Hallway 1",
# "Querulist",
# "Team Sierra",
# "Spatial Technical Dissemination (STD)",
# "ErRor",
# '"Hello World!"',
# "Sushi",
# "Scripting team",
# "Souvlaki",
# "ArraysStartAt1",
# "Mr_Robot",
# "Black@white",
# "Kliffa"
# )
# TeamList[c(8, 15)] = rep(list(ProjectTeams), 2)
#TeamList[5:15] = rep(list(paste("Team", LETTERS[1:10])), 11)
#TeamList[10:15] = rep(list(c(TeamNames, paste("Team", LETTERS[1:10]))), 6)

Year = 2020
Seed = 0xfedbeef
## ====================

ExerciseGroupSize = 3 # We want groups of 3 teams
AssignmentGroupSize = 2 # But only one for graded assignments

DeliverableNames = c(paste("Exercise", 1:4),
"Assignment 1",
paste("Exercise", 5:8),
"Assignment 2",
paste("Exercise", 9:11),
"Assignment 3",
"Exercise 12",
"Project")

ExerciseIDs = grep("Exercise", DeliverableNames)
FreeDays = which(DeliverableNames == "Exercise 8" | DeliverableNames == "Exercise 9")

# Sanity checks
stopifnot(length(DeliverableNames) == length(TeamList)) # We need one set of teams for each deliverable
if (any(sapply(TeamList, anyDuplicated)))
    stop("Duplicate names found!")

names(TeamList) = DeliverableNames

# Web UI
ui = fluidPage(
  titlePanel("Geoscripting review group generator"),
  sidebarLayout(
    sidebarPanel(
      tags$p("This Shiny app tells you which teams you should go to in order to discuss the answers to the exercise of the day, or which team to review for the assignments of the week.",
      tags$b("Please do not write these down as teams may change throughout the course! Always rerun this app using runGist()!")),
      tags$p(""),
      selectInput("DeliverableName", label = "Assignment or exercise to review:", choices=DeliverableNames),
      selectInput("MyTeam", 
        label = "Your team name:",
        choices = c("SELECT YOUR TEAM NAME", TeamList[[1]])),
      helpText("Hint: if you click on the boxes above and press Backspace, you can search for your team name by typing it."),
      tags$div(class="header", checked=NA,
               tags$a(href="https://gist.github.com/GreatEmerald/a5e7ed83ab16c0125ca11079adfbaf50", "Source code of this Shiny app")
      )
    ),

    mainPanel(
      textOutput("YourTeam")
    )
  )
)

# Actual random algorithm
server = function(input, output, session)
{
  output$YourTeam <- renderText({
    DeliverableID = which(DeliverableNames == input$DeliverableName) # Alternatively could use TeamList[[input$DeliverableName]] everywhere
    updateSelectInput(session, "MyTeam", choices=c("SELECT YOUR TEAM NAME", TeamList[[DeliverableID]]), selected=ifelse(input$MyTeam %in% TeamList[[DeliverableID]], input$MyTeam, "SELECT YOUR TEAM NAME"))
    
    if (input$MyTeam == "SELECT YOUR TEAM NAME")
        "Please select the exercise or assignment number by using the first drop-down box, and then select your team name from the second drop-down box."
    else if (DeliverableID %in% FreeDays)
        "There is no exercise that needs to be reviewed today."
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
            stopifnot(length(MyGroup) == 1)
        
            switch(length(OtherTeams)+1,
                paste0("For ", input$DeliverableName, ", your team (", input$MyTeam, ") is all alone! You can discuss with staff or review whatever group you prefer."),
                paste0("For ", input$DeliverableName, ", your team (", input$MyTeam, ") should review the work of team \"", OtherTeams[1], "\"."),
                paste0("For ", input$DeliverableName, ", your team (", input$MyTeam, ") should discuss your answers with teams \"", OtherTeams[1], "\" and \"", OtherTeams[2], "\".")
            )
        }
    }
  })
}

shinyApp(ui = ui, server = server)

# Run with shiny::runGist("a5e7ed83ab16c0125ca11079adfbaf50")
