#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
load("KrogerShiny.RData")

library(shiny)
library(regclass)
library(shinythemes)
library(sqldf)

getwd()


sqldf("select max(Spent) from KROGER")
sqldf("select min(Spent) from KROGER")
sqldf("select avg(Spent) from KROGER")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),

    # Application title
    titlePanel("Kroger Customer Analytics"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("metric", label = h3("Select a Metric"),
                        choices = list("Visits", "Spent", "AmountPerVisit"),
                        selected = "Visits"),
            radioButtons("va", label = h3("Your Vertical Axis"),
                         choices = list("Visits", "Spent", "AmountPerVisit"), 
                         selected = "AmountPerVisit"),
            selectInput("dem", label = h3("Select the Demographic"),
                         choices = list("Age", "MaritalStatus", "Income"),
                         selected = "Income"),
            radioButtons("ha", label = h3("Your Horizontal Axis"),
                         choices = list("Visits", "Spent", "AmountPerVisit"), 
                         selected = "Spent"), 
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
            
        ),

        # Show a plot of the generated distribution
        mainPanel( 
            p("This Shiny App was created by Iva Dedic, as a part of BAS479 at the Haslam College of Business. Go Vols!"),
            img(src='krogerImage.png',height="20%", width="30%", align = "right"),
            mainPanel(
                plotOutput("boxplot"), 
                textOutput("usefullabel"),
                verbatimTextOutput("aggregate"),
                plotOutput("scatter"),
                p("Below, we see a histogram of the amount of money spent per household in total."),
                plotOutput("distPlot")
         
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input,output,session) {
output$boxplot <- renderPlot({
    boxplot(formula(paste(input$metric,"~",input$dem)), data= KROGER)
})


output$usefullabel <- renderText({
    paste("Average value of", input$metric, "for each level of", input$dem)
})

output$aggregate <- renderPrint({
    aggregate(formula(paste(input$metric,"~",input$dem)),data= KROGER, FUN= mean)
})

output$scatter <- renderPlot({
    plot(formula(paste(input$va,"~",input$ha)),data= KROGER)
})

output$distPlot <- renderPlot({
    x    <- KROGER[, 3]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'dodgerblue4', border = 'white')
})
}

# Run the application 
shinyApp(ui = ui, server = server)
