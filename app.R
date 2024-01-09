#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(dplyr)

load("EnergySales.RData")
data<- MunsterSalesGER

#UI
ui <- fluidPage(
  titlePanel("Sales Predictions with Adjusted Budget"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("x1", "Sport Sponsoring Multiplier", min = 0, max = 1, value = 0.14),
      sliderInput("x2", "Print Magazine Multiplier", min = 0, max = 1, value = 0.14),
      sliderInput("x3", "POS Display Multiplier", min = 0, max = 1, value = 0.14),
      sliderInput("x4", "Out of Home Multiplier", min = 0, max = 1, value = 0.14),
      sliderInput("x5", "TV Ads Local Multiplier", min = 0, max = 1, value = 0.14),
      sliderInput("x6", "Youtube Multiplier", min = 0, max = 1, value = 0.14),
      sliderInput("x7", "SEA Google Multiplier", min = 0, max = 1, value = 0.16),
      actionButton("update", "Update")
    ),
    mainPanel(
      plotOutput("salesPlot"),
      textOutput("sumWarning")
    )
  )
)

#Server logic
server <- function(input, output, session) {
  observeEvent(input$update, {
    if(sum(input$x1, input$x2, input$x3, input$x4, input$x5, input$x6, input$x7) != 1) {
      output$sumWarning <- renderText("The sum of multipliers must equal 1.")
    } else {
      output$sumWarning <- renderText("")
      output$salesPlot <- renderPlot({
        data <- MunsterSalesGER
        data$SportSponsoring <- data$SportSponsoring * input$x1
        data$PrintMagazine <- data$PrintMagazine * input$x2
        data$PosDisplay <- data$PosDisplay * input$x3
        data$OutofHome <- data$OutofHome * input$x4
        data$TVAdsLocal <- data$TVAdsLocal * input$x5
        data$Youtube <- data$Youtube * input$x6
        data$SEAGoogle <- data$SEAGoogle * input$x7
        model_optimal <- lm(Sales ~ SportSponsoring + PrintMagazine + PosDisplay + OutofHome + TVAdsLocal + Youtube, data = data)
        predicted_sales_original <- predict(model_optimal, newdata = data)
        data$Predicted_Sales_Restructured <- predicted_sales_original
        
        ggplot(data, aes(x = Week)) +
          geom_line(aes(y = Sales, colour = "Actual Sales")) +
          geom_line(aes(y = Predicted_Sales_Restructured, colour = "Predicted Sales")) +
          labs(title = "Actual vs Predicted Sales by Week", x = "Week", y = "Sales") +
          scale_colour_manual(values = c("Actual Sales" = "red", "Predicted Sales" = "blue"),
                              name = "Legend",
                              breaks = c("Actual Sales", "Predicted Sales"),
                              labels = c("Actual Sales" = ": Actual Sales", "Predicted Sales" = ": Predicted Sales")) +
          theme_minimal() +
          theme(legend.position = "bottom")
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)