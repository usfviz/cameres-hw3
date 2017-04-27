library(shiny)
packages <- c('ggplot2', 'GGally', 'corrplot')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(GGally)
library(corrplot)

facebook <- read.csv('facebook.csv', sep = ';')
nums <- sapply(facebook, is.numeric)
num_facebook <- facebook[nums]
num_cols <- names(nums[nums == T])

# whoops, it doesn't like creating multiple
# only the first one is accessible by server 
# `input` var
col_input <- selectInput(
  "columns", "Columns",
  num_cols,
  selected=num_cols[1:5],
  multiple=T)

# custom input for Heatmap
heatmap_conditional <- function() {
  corrplot_methods <- c("circle", "square", "ellipse", "number", "shade",
                        "color", "pie")
  corrplot_types <- c("full", "lower", "upper")
  
  method_input <- selectInput(
    "method", "Method",
    choices=corrplot_methods)
  
  types_input <- selectInput(
    "type", "Type",
    corrplot_types)
  
  conditionalPanel(
    condition="input.tabs=='Heatmap'",
    method_input,
    types_input
  )
}

scatter_conditional <- function() {
  conditionalPanel(
    condition="input.tabs=='Scatter'"
  )
}

cordinate_conditional <- function() {
  spline_input <- sliderInput('spline',
                              'Spline Factor',
                              min=1, 
                              max=10,
                              value=1,
                              step=1)
  conditionalPanel(
    condition="input.tabs=='Cordinates'",
    spline_input
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarPanel(
    # create three conditional panels for of the plots
    conditionalPanel(
      condition="input.tabs=='Cordinates' | input.tabs=='Scatter' | input.tabs=='Heatmap'",
      col_input,
      heatmap_conditional(),
      scatter_conditional(),
      cordinate_conditional()
    )
  ),
  mainPanel(
    tabsetPanel(
      id='tabs',
      tabPanel("Heatmap", plotOutput('heatmap')),
      tabPanel("Scatter", plotOutput('scatter')),
      tabPanel("Cordinates", plotOutput('coordinates'))
    )
  )
)

server <- function(input, output) {
  output$heatmap <- renderPlot({
    subset_facebook <- num_facebook[input$columns]
    m <- cor(subset_facebook)
    corrplot(m, 
             method=input$method, 
             type=input$type,
             title='Correlations for Columns Selected') 
  })
  output$scatter <- renderPlot({
    subset_facebook <- num_facebook[input$columns]
    ggpairs(subset_facebook, 
            columns = input$columns)
  })
  output$coordinates <- renderPlot({
    subset_facebook <- num_facebook[input$columns]
    ggparcoord(subset_facebook, 
               title="Parallel Cordinates",
               splineFactor=input$spline)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

