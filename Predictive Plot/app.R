library(shiny)
require(forecast)

#Primary loading and functions
data <- read.csv('Train+Week1&2.csv')
real <- read.csv('Week3.csv')
DModel <- read.csv('DModel.csv')

TModel <- readRDS('TModel.RData')
AModel <- readRDS('AModel.RData')
SModel <- readRDS('SModel.RData')
BModel <- readRDS('BModel.RData')

id <- unique(data$subscriber_ecid)
lt <- length(real$subscriber_ecid)
ld <- length(data$subscriber_ecid)
m <- aggregate(data_usage_volume ~ subscriber_ecid, data = data, FUN = "mean")
OM <- rep(0, lt)
real <- cbind(real, OM)

for (j in 1:lt) {
    for (i in id) {
        if (real$subscriber_ecid[j] == i) real$OM[j] <- m[m$subscriber_ecid==i, 2]
    }
}

u <- data.frame()
c <- 1
for (i in id) {
    u[c,1] <- i
    u[c,2] <- c
    c <- c+1
}

rmse <- function(actual, pred){
    s <- 0
    l <- length(pred)
    for (i in 1:l) {
        s <- s + (pred[i] - actual[i])^2
    }
    return(sqrt(s/l))
}

#Setting up the page layout
ui <- fluidPage(

    titlePanel("User Internet Usage Prediction"),

    #Sidebar panel consisting of 2 input widgets and a variable output
    sidebarLayout(
        sidebarPanel(
            selectInput("ID", 
                        label = "Select User ID",
                        choices = unique(data$subscriber_ecid),
                        selected = "-06FoayTOXJ8-"),
            radioButtons("Model",
                         label = "Choose a Model",
                         choices = c("Benchmark", "TBATS", "ARIMA", "SES", "Bagged", "Neural Net", "Ensemble"),
                         selected = "Benchmark"),
            p("RMSE(Model)="),
            verbatimTextOutput("RMSE")
        ),

        #Main panel consisting of a plot and a downloadable table
        mainPanel(
           plotOutput("tsPlot"),
           p("* The vertical line separates the training and test sets."),
           tableOutput("predTable"),
           downloadButton("downloadTable", label = "Download")
        )
    )
)

#Setting up the server; getting the input (User ID) and producing the outputs
server <- function(input, output) {
    
        #Wrapping up a reactive table based on the input user ID into a reactive object for further use.
        ##The table is consisted of the predicted values of various models and the real ones.
        table_reactive <- reactive({
            id <- u$V2[u$V1==input$ID]
            r <- real$data_usage_volume[real$subscriber_ecid==input$ID]
            l <- length(r)
            pred_benchmark <- real$OM[real$subscriber_ecid==input$ID]
            pred_tbats <- forecast(TModel[[id]], h = l)
            pred_arima <- forecast(AModel[[id]], h = l)
            pred_ses <- forecast(SModel[[id]], h = l)
            pred_bagged <- forecast(BModel[[id]], h = l)
            pred_nn <- DModel$DNN[DModel$subscriber_ecid==input$ID]
            pred_ensemble <- c()
            for (k in 1:l) {
                pred_ensemble[k] <- mean(c(pred_tbats$mean[k], pred_arima$mean[k], pred_nn[k]))
            }
            d<-rep(0,l)
            t <- data.frame(d, r, pred_benchmark, pred_tbats$mean, pred_arima$mean, pred_ses$mean, pred_bagged$mean, pred_nn, pred_ensemble)
            for (i in 1:l) {
                t[i,1] <- paste0("Day+",i)
            }
            colnames(t) <- c("Days", "Real", "Benchmark", "TBATS", "ARIMA", "SES", "Bagged", "Neural Net", "Ensemble")
            t
        })
        
        #Calculating the RMSE of the selected model and printing it in the sidebar.
        output$RMSE <- renderText({
            t <- table_reactive()
            m1 <- c("Benchmark", "TBATS", "ARIMA", "SES", "Bagged", "Neural Net", "Ensemble")
            m2 <- 3:9
            m<-rbind(m1,m2)
            p<-m[2,m[1,]==input$Model]
            rmse(t[,as.numeric(p)],t[,2])
        })
    
        #Drawing the time-series plot of the selected user
        ##Splitting the training and test sets with a vertical line
        ###Adding the predicted values of the selected model to the plot.
        output$tsPlot <- renderPlot({
            UserUsage <-  data$data_usage_volume[data$subscriber_ecid==input$ID]
            e <- length(UserUsage)
            RealUsage <- real$data_usage_volume[real$subscriber_ecid==input$ID]
            l <- length(RealUsage)

            plot(UserUsage,
                 main = "Daily Internet Usage Prediction from Various Models",
                 xlab = "Days",
                 ylab = "Volume",
                 xlim = c(1,(e+l)))
        
            points((e+1):(e+l), RealUsage,col="black", pch=1)
        
            par(xpd=FALSE)
            abline(v=e+0.5, pch=18, col="red", lty=2)
        
            t <- table_reactive()
            m1 <- c("Benchmark", "TBATS", "ARIMA", "SES", "Bagged", "Neural Net", "Ensemble")
            m2 <- 3:9
            m<-rbind(m1,m2)
            p<-m[2,m[1,]==input$Model]
            points((e+1):(e+l), t[,as.numeric(p)] ,col="red", pch=13)
        })
        
        #Printing the contents of the reactive table created previously.
        output$predTable <- renderTable({
            table_reactive()
        })
        
        #Making the created table downloadable.
        output$downloadTable <- downloadHandler(
            filename = function() {
                paste0(input$ID, ".csv", sep='')
            },
            content = function(con) {
                t <- table_reactive()
                write.csv(t, con, row.names = FALSE)
            }
        )
}

#Running the app
shinyApp(ui = ui, server = server)