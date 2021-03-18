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
Backpack$BackProblems = as.factor(Backpack$BackProblems)
Backpack$Major = as.factor(Backpack$Major)
Backpack$Sex = as.factor(Backpack$Sex)
Backpack$Status = as.factor(Backpack$Status)

infoPanel<- tabPanel(title = "About this Shiny",
                     mainPanel(p("In this shiny app, we are going to analyse the data set Backpack, which contains information about students at California Polytechnic State University (San Luis Obispo) in order to
investigate the question of whether back aches might be due to carrying heavy backpacks. We will be able to analyse visually the variables and also through a regression model.")))

dataPanel <- tabPanel("Data",fluidPage( 
        p("Here we can see the whole data set"),
        dataTableOutput("data")
    ))
    
plotPanel = tabPanel("Plots of the variables",
                     fluidPage(
                         sidebarLayout(position = "right",
                                       sidebarPanel(
                                           selectInput("var1", label = h3("Select the qualitative variable "),
                                                       choices = c("BackProblems", "Major","Sex","Status"),
                                                       selected = "BackProblems"),
                                           selectInput("var2", label = h3("Select the numerical variable"), 
                                                       choices = c("BackpackWeight","BodyWeight","Ratio","Year","Units"),
                                                       selected = "BackpackWeight"),
                                           selectInput("var", label = h3("Select the variable for the histogram"), 
                                                       choices = c("BackpackWeight","BodyWeight","Ratio","Year","Units"),
                                                       selected = 1),
                                           sliderInput("n_bins", label = h3("Number of bins"), min = 1, 
                                                       max = 20, value = 5)
                                           
                                       ),
                                       mainPanel( tabsetPanel(type = "tabs",
                                                              tabPanel("Box-plot", plotOutput("boxplot")),
                                                              tabPanel("Histogram", plotOutput("histplot"))))
                             
                         )
                     ),
                     )



dinPanel <- tabPanel( "Plotly Graph",
                     useShinyjs(),
                     cellWidths = 300,
                     cellArgs = list(style = "padding: 6px"),
                     h3("In this section we can visualize the variables through a dymanic graph"),
                     br(),
                     column(6,plotlyOutput(outputId = "plotly1"),height="600px"),
                     
                     
)
regPanel <- tabPanel(title = "A simple regression",
                    useShinyjs(),
                    sidebarLayout(
                        sidebarPanel(
                            radioButtons("y", label = "Select the target variable:",  choices = c("BackpackWeight","BodyWeight","Ratio","Year","Units")),
                            radioButtons("x", label = "Select the predictor variable:", choices = c("BackpackWeight","BodyWeight","Ratio","Year","Units")),

                            downloadButton("report", "Generate report")
                        ),
                        mainPanel(
                            plotlyOutput("reg")
                        )
                    )
)
refPanel <- tabPanel("References",
                     p(tags$button(
                         class="btn btn-default", 
                         `data-toggle`="collapse", 
                         `data-target`="#collapseExample",
                         "References")),
                     div(class="collapse", id="collapseExample",
                         div(class="card card-body",
                             includeMarkdown("References.md")
                         )
                     )
)
# Define UI for application that draws a histogram
ui <- navbarPage("shiny App",
                 theme = shinytheme("united"),
                 infoPanel,
                 dataPanel,
                 plotPanel,
                 dinPanel, 
                 regPanel,
                 refPanel
                 
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
    
    output$boxplot <- renderPlot({
        ggplot(data=Backpack, aes_string( input$var1, input$var2)) + 
            geom_boxplot(aes_string(fill = input$var2), color = "coral1", fill = "orange", alpha = 0.2)+  
            theme(legend.position="none") 
            
    })
    output$plotly1 <- renderPlotly({
        plot_ly(Backpack, x = ~BackpackWeight, y = ~BodyWeight, type = "scatter", mode = "markers",
                color = ~BackProblems) %>% 
            layout(title = "Back problems in function of body weight and ",
                   xaxis = list(title = "BackpackWeight"), 
                   yaxis = list(title = "BodyWeight"))
    })
    output$reg <- renderPlotly({
         model<- as.formula(paste(input$y, " ~ ", input$x))
        fit <- glm(model, data = Backpack)
        ggplot(data = Backpack, aes_string(x = input$x, y = input$y)) +
            geom_point() + 
            geom_smooth(method = "glm") +
            theme_bw()})
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.pdf",
        content = function(file) {
           
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(
                x = isolate(input$x), 
                y = isolate(input$y)
            )
            
            rmarkdown::render(tempReport, 
                              output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
            
}

# Run the application 
shinyApp(ui = ui, server = server)