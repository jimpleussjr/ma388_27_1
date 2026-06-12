#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    h6("This is some really helpful explanation of what you're looking at."),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            h6("I couldn't think of anything good to add here, so I'm switching data sets:"),
            radioButtons("cylinders", 
                         label = h3("Select a number of cylinders to explore in mtcars:"),
                         choices = list("4 cylinders" = 4, 
                                        "6 cylinders" = 6, 
                                        "8 cylinders" = 8), 
                         selected = 4)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("distPlot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    output$distPlot2 <- renderPlot({
      mtcars |> 
        filter(cyl == input$cylinders) |> 
        ggplot(aes(x = mpg)) + 
        geom_histogram(bins = 6) + 
        labs(x = 'MPG',
             title = paste('Histogram of MPG for vehicles with', 
                           input$cylinders, 
                           "cylinders"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
