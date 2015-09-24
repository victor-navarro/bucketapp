library(shiny)

shinyUI(fluidPage(
    titlePanel(h1('Bucket Task results')),
    fluidRow(
        column(12,
            wellPanel(
                uiOutput('instructions'),
                uiOutput('hide')
               )
        )
    ),
    fluidRow(
        column(3,
                wellPanel(
                    h4('Main options'),
                    radioButtons("mode", label = "Mode", choices = c("Group comparison", "Group", "Individual")),
                    radioButtons("phase", "Phase", choices = c("Training 1", "Equal Work", "Training 2", "Reverse")),
                    radioButtons("moment", label = "Moment", choices = c("All sessions", "Individual sessions", "Blocks of sessions"))
                    )
               ),
        column(3,
               wellPanel(
                   h4('Secondary options'),
                   conditionalPanel(condition = "input.mode == 'Group' | input.mode == 'Individual'", radioButtons("group", label = "Group", choices = c("Closest", "Farthest"))),
                   uiOutput('birds'),
                   uiOutput('block_s'),
                   uiOutput('rangecheck')
               )
        ),         
        column(6, 
                wellPanel(plotOutput('plot'),
                          conditionalPanel(condition = "input.moment == 'Blocks of sessions' | input.moment == 'Individual sessions'", uiOutput('slider'))
                )
        )
    )
))