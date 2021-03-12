#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Stat2Data)
library(tidyverse)
library(ggcorrplot)
library(shinythemes)
require("shinyjs")
data("Backpack")



infoPanel<- tabPanel(title = "About this Shiny",
                     mainPanel(p("In this shiny app....")))

dataPanel <- tabPanel("Data",fluidPage( 
        p("Here we can see the whole data set"),
        dataTableOutput("data")
    ))
    

plotPanel <- tabPanel("Histogram",
                      sidebarLayout(position = "right",
                                    sidebarPanel(
                                        selectInput("var", label = h3("Select the variable"), 
                                                    choices = c("BackpackWeight","BodyWeight","Ratio","Year","Units"),
                                                    selected = 1),
                                        sliderInput("n_bins", label = h3("Number of bins"), min = 1, 
                                                    max = 20, value = 5)
                                    ), # sidebarPanel
                                    mainPanel(
                                        p("In this histogram, we can select one out of the five continous variables
                                          and plot it in a histogram, being able to select the number of bins as well."),
                                        plotOutput(outputId = "plot")
                                    ) # mainPanel
                      ) # sidebarLayout
) #  tabPanel


# Define UI for application that draws a histogram
ui <- navbarPage("shiny App",
                 theme = shinytheme("united"),
                 infoPanel,
                 dataPanel,
                 plotPanel
                
)


# Define server logic required to draw a histogram
server <- function(input, output) { 
    output$data <- renderDataTable(Backpack)
    output$plot = renderPlot({
        ggplot(data = Backpack, aes_string(x = input$var)) +
            geom_histogram(bins = input$n_bins, fill = "mediumturquoise", color="grey97") +
            theme_bw()
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)