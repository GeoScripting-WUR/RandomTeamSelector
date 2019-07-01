# Script for randomly assigning teams of 3 into an exercise discussion group.
# Output is integers listing the discussion group number.
# License: CC0 (https://creativecommons.org/publicdomain/zero/1.0/)

library(shiny)

## Update these values!
## ====================
GroupSize = 3 # We want groups of 3 teams
TeamNames = c("MMXIX",
"Guardians of the Galaxy",
"Mindwrap_Error",
"The Happy Penguins",
"R-mateys",
"gisrsartists",
"Team Sheet",
"WHAT?!",
"soilandwater",
"Pepsi",
"The Appies",
"VB13",
"Nimma Gang",
"Team Coffee",
"Firebrand",
"Green Cobra",
"GreenGrain",
"Spectacular Bathduck",
"Stan + Guus",
"Blossom",
"Reflective Duck",
"DduDduDdu",
"CreativeTeamName",
"Building B Hallway 1",
"Querulist",
"Team Sierra",
"Spatial Technical Dissemination (STD)",
"ErRor",
'"Hello World!"',
"Sushi",
"Scripting team",
"Souvlaki",
"ArraysStartAt1",     ## Put initial names here
"Mr_Robot"
)
TeamList = rep(list(TeamNames), 15) ## After there are any changes to teams over time, overwrite the slots like this:
ProjectTeams = c(
"MMXIX",
"Guardians of the Galaxy",
"Mindwrap_Error",
"The Happy Penguins",
"R-mateys",
"gisrsartists",
"Team Sheet",
"WHAT?!",
"soilandwater",
"The Appies",
"Nimma Gang",
"Team Coffee",
"Firebrand",
"VB13",
"Green Cobra",
"GreenGrain",
"Spectacular Bathduck",
"Stan + Guus",
"Blossom",
"Reflective Duck",
"DduDduDdu",
"CreativeTeamName",
"Building B Hallway 1",
"Querulist",
"Team Sierra",
"Spatial Technical Dissemination (STD)",
"ErRor",
'"Hello World!"',
"Sushi",
"Scripting team",
"Souvlaki",
"ArraysStartAt1",
"Mr_Robot",
"Black@white",
"Kliffa"
)
TeamList[c(8, 15)] = rep(list(ProjectTeams), 2)
#TeamList[5:15] = rep(list(paste("Team", LETTERS[1:10])), 11)
#TeamList[10:15] = rep(list(c(TeamNames, paste("Team", LETTERS[1:10]))), 6)

Year = 2020
Seed = 0xfedbeef
## ====================
# TODO: mention that the list is subject to change

# Sanity check
if (any(sapply(TeamList, anyDuplicated)))
    stop("Duplicate names found!")

# Web UI
ui = fluidPage(
  titlePanel("Geoscripting discussion group generator"),
  sidebarLayout(
    sidebarPanel(
      helpText("This Shiny app tells you which teams you should go to in order to discuss the answers to the exercise of the day."),
      tags$div(class="header", checked=NA,
               tags$a(href="https://gist.github.com/GreatEmerald/2556d79d899f253c46ee830a3b1c5c15", "Source code")
      ),
      sliderInput("Assignment",
                  "Exercise number:",
                  min = 1,
                  max = 15,
                  value = 1),
      selectInput("MyTeam", 
        label = "Your team name",
        choices = c("SELECT YOUR TEAM NAME", TeamList[[1]]))
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
    updateSelectInput(session, "MyTeam", choices=c("SELECT YOUR TEAM NAME", TeamList[[input$Assignment]]), selected=ifelse(input$MyTeam %in% TeamList[[input$Assignment]], input$MyTeam, "SELECT YOUR TEAM NAME"))
    
    if (input$MyTeam == "SELECT YOUR TEAM NAME")
        "Please select the exercise number by sliding the slider on the left, and then select your team name from the drop-down box."
    else if (input$Assignment == 10)
        "This exercise is on DataCamp, no need to discuss it!"
    else if (input$Assignment == 13 || input$Assignment == 14)
        "No exercise for today either, so you do not need to discuss anything."
    else
    {
        TeamNames = TeamList[[input$Assignment]]
        names(TeamNames) = TeamNames
        
        if (!input$MyTeam %in% TeamNames)
            paste0("Your team (", input$MyTeam, ") was not registered to exercise ", input$Assignment, ", please contact the course coordinators to check if you should participate in the discussions.")
        else
        {
            set.seed(input$Assignment+Year+Seed)
            Weights = runif(length(TeamNames))
            # names(weights) = TeamNames
            GroupNumbers = ceiling(order(Weights) / GroupSize)
            MyGroup = GroupNumbers[TeamNames == input$MyTeam]
            OtherTeams = TeamNames[GroupNumbers == MyGroup]
            OtherTeams = OtherTeams[OtherTeams != input$MyTeam]
            stopifnot(length(MyGroup) == 1)
        
            paste0("For exercise ", input$Assignment, ", your team (", input$MyTeam, ") should discuss your answers with teams \"", OtherTeams[1], "\" and \"", OtherTeams[2], "\".")
        }
    }
  })
}

shinyApp(ui = ui, server = server)

# Run with shiny::runGist()
