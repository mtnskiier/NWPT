

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next Word Prediction Tool"),
  
    sidebarPanel(
       textInput("inptext", "",  placeholder = "English text"),
       radioButtons("numwds", "Number of potential matches:", c("One" = "1",
                                                                "Five" = "5",
                                                                "Ten" = "10"),
                    selected = "5"),
       submitButton(text="Submit Text"),
       tableOutput("resTable")
       
    ),
    mainPanel(
            tabsetPanel(
                    tabPanel("Word Cloud", plotOutput("plot") ),
                    tabPanel("Usage", htmlOutput("usage")),
                    tabPanel("Overview", htmlOutput("overview"))
            )
           
    )
  )
)
