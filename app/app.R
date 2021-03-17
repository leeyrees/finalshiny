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
library(ggplot2)
library(shinythemes)
library(plotly)
require("shinyjs")
data("Backpack")



infoPanel<- tabPanel(title = "About this Shiny",
                     mainPanel(p("In this shiny app....")))

dataPanel <- tabPanel("Data",fluidPage( 
        p("Here we can see the whole data set"),
        dataTableOutput("data")
    ))
    



plotPanel=tabPanel("Plot",fluidPage(
         sidebarLayout(position = "right",
             sidebarPanel(
                 radioButtons(
                     inputId="Item",
                     label="Choose your plot",
                     choices=list("Histogram","Boxplot"),
                     selected='Histogram'),
                 #only show this when Item 1 is selected
                 #user choose either 1 or 2 to display to display
                 conditionalPanel(
                     condition="input.Item == 'Histogram' ",
                     selectInput("var", label = h3("Select the variable"), 
                                 choices = c("BackpackWeight","BodyWeight","Ratio","Year","Units"),
                                 selected = 1),
                     sliderInput("n_bins", label = h3("Number of bins"), min = 1, 
                                 max = 20, value = 5)
                 ),
                 #only show this when Item 2 is selected
                 conditionalPanel(
                     condition="input.Item == 'Boxplot' ",
                     selectInput("var1", label = h3("Select the qualitative variable "),
                                 choices = c("BackProblem", "Major","Sex","Status"),
                                 selected = "BackProblem"),
                     selectInput("var2", label = h3("Select the numerical variable"), 
                                 choices = c("BackpackWeight","BodyWeight","Ratio","Year","Units"),
                                 selected = "BackpackWeight")
                 ),
                 
             ),
             mainPanel(conditionalPanel(
                 condition = "input.view == 'Boxplot'",
                 plotOutput("boxplot")
             ),
             conditionalPanel(
                 condition = "input.view == 'Histogram'",
                 plotOutput("histplot")
             )
         )
),
)
)
dinPanel <- tabPanel( "Plotly Graph",
                     useShinyjs(),
                     cellWidths = 300,
                     cellArgs = list(style = "padding: 6px"),
                     h3("In this section we can visualize the variables through a dymanic graph"),
                     br(),
                     br(),
                     column(6,plotlyOutput(outputId = "plotly1"),height="600px"),
                     
                     
) #

# Define UI for application that draws a histogram
ui <- navbarPage("shiny App",
                 theme = shinytheme("united"),
                 infoPanel,
                 dataPanel,
                 plotPanel,
                 dinPanel
                 
)


# Define server logic required to draw a histogram
server <- function(input, output) { 
    datasetInput<- reactive(Backpack)
    output$data <- renderDataTable(Backpack)
    output$histplot = renderPlot({
        ggplot(data = Backpack, aes_string(x = input$var)) +
            geom_histogram(bins = input$n_bins, fill = "mediumturquoise", color="grey97") +
            theme_bw()
    })
    output$boxplot <- renderPlot({ggplot(data = Backpack, aes(x=input$var1, y=input$var2)) + 
        geom_boxplot()
    })   
    output$plotly1 <- renderPlotly({
        plot_ly(Backpack, x = ~BackpackWeight, y = ~BodyWeight, type = "scatter", mode = "markers",
                color = ~BackProblems) %>% 
            layout(title = "Back problems in function of body weight and ",
                   xaxis = list(title = "Total_Trans_Amt"), 
                   yaxis = list(title = "Total_Trans_Ct"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)