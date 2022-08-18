library(shiny)

data <- read.csv('Train+Week1&2.csv')

#Setting up the page layout
ui <- fluidPage(

    titlePanel("User Internet Usage"),   #Title of the page

    #Sidebar panel consisting of an input widget and 2 simple variable outputs
    sidebarLayout(
        sidebarPanel(
            selectInput("ID", 
                        label = "Select User ID",
                        choices = unique(data$subscriber_ecid),
                        selected = "-06FoayTOXJ8-"),
            p("Mean:"),
            verbatimTextOutput("M"),
            p("Standard Deviation:"),
            verbatimTextOutput("SD")
        ),

        #Main panel consisting of an image, 2 plots and a text output
        mainPanel(
           img(src = "Legend.png", height = 59, width = 343, align = "center"),
           plotOutput("tsPlot"),
           plotOutput("normPlot"),
           p("Intensity Level of User's Internet Usage is:"),
           textOutput("UsageLevel")
        )
    )
)

#Setting up the server; getting the input (User ID) and producing the outputs
server <- function(input, output) {
    
    #Printing user's average internet usage in the sidebar
    output$M <- renderText({
        UserUsage <-  data$data_usage_volume[data$subscriber_ecid==input$ID]
        mean(UserUsage)
    })
    
    #Printing user's standard deviation of internet usage in the sidebar
    output$SD <- renderText({
        UserUsage <-  data$data_usage_volume[data$subscriber_ecid==input$ID]
        sd(UserUsage)
    })
    
    #Plotting the user's internet usage time series 
    ##User's average usage and total average usage added to the plot
    ###If there's any abnormal activity observed in user's behavior, those days are also showcased on the plot
    output$tsPlot <- renderPlot({
        UserUsage <-  data$data_usage_volume[data$subscriber_ecid==input$ID]

        plot(UserUsage,
             main = "Daily Internet Usage",
             xlab = "Days",
             ylab = "Volume")
        
        UserMean <-  mean(UserUsage)
        UserSD <- sd(UserUsage)
        TotMean <- mean(data$data_usage_volume)
        
        par(xpd=FALSE)
        abline(h=TotMean, pch=18, col="blue", lty=2)
        abline(h=UserMean, pch=18, col="green", lty=2)
        
        ind <- c()
        for (i in 1:length(UserUsage)) {
            if (UserUsage[i] > UserMean + 3*UserSD) ind[i]<-i
        }
        ind <- ind[!is.na(ind)]
        
        points(ind, UserUsage[ind],col="red", pch=13)
    })
    
    #Drawing a Normal Distribution with the total Mean and SD of all users
    ##The Average usage of the given user is also shown on the curve with a vertical red line
    output$normPlot <- renderPlot ({
            curve(dnorm(x, 1.33, 3.42), from=-8.67, to=11.33, xlab="Internet Usage", ylab="Probability Density")
            axis(1, at=1.33, labels = 1.33)
            UserUsage <-  data$data_usage_volume[data$subscriber_ecid==input$ID]
            UserMean <-  mean(UserUsage)
            axis(3, at=UserMean, labels = UserMean)
            abline(v=UserMean, pch=18, col="red", lty=2)
    })
    
    #This panel is used to determine the level of internet usage of the specific user compared to all other users    
    output$UsageLevel <- renderText({
            UserUsage <-  data$data_usage_volume[data$subscriber_ecid==input$ID]
            UserMean <-  mean(UserUsage)
            if (UserMean > 2) print("Heavy") else{
                if (UserMean < 0.5) print("Light") else{
                    print("Moderate")
                }
            }
    })

}

#Running the app
shinyApp(ui = ui, server = server)