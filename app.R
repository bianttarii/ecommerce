#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Install dan load library Shiny
#install.packages("shiny")
library(shiny)

# input data
ecommerce_data = data.frame(
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1    = c(150000, 160000, 170000, 180000, 190000, 200000, 
            210000, 220000, 230000, 240000, 250000, 260000),
  x2    = c(8000, 9500, 10000, 10500, 11000, 9000,
            11500, 12000, 12500, 13000, 14000, 15000),
  x3    = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4    = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5    = c(20000, 22000, 25000, 23000, 30000, 28000, 
            27000, 35000,40000, 45000, 50000, 60000),
  y     = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350))

# Mengubah tipe data variabel x4 menjadi numerik (jika belum numerik)
ecommerce_data$x1 <- as.numeric(as.character(ecommerce_data$x1))

# Mendefinisikan UI
ui <- fluidPage(
  titlePanel("E-commerce Sales Analysis"),
  sidebarLayout(
    sidebarPanel(
      numericInput("x1", "Website Visitors", value = 1000),
      numericInput("x2", "Monthly Transactions", value = 500),
      numericInput("x3", "Avg. Items per Transaction", value = 2.5, step = 0.1),
      sliderInput("x4", "Customer Satisfaction", min = 1, max = 10, value = 8.5, step = 0.1),
      numericInput("x5", "Ads per Month", value = 20)
    ),
    mainPanel(
      verbatimTextOutput("prediction"),
      plotOutput("scatterplot"),
      tableOutput("datatable")
    )
  )
)

# Mendefinisikan server
server <- function(input, output) {
  # Menambahkan model regresi sebagai variabel global
  global_model <- reactive({
    lm(y ~ x1 + x3 + x4, data = ecommerce_data)
  })
  
  # Output prediksi
  output$prediction <- renderText({
    new_data <- data.frame(
      x1 = input$x1,
      x2 = input$x2,
      x3 = input$x3,
      x4 = input$x4,
      x5 = input$x5
    )
    
    # Menggunakan model regresi global
    prediction <- predict(global_model(), newdata = new_data)
    
    paste("Predicted Sales: $", round(prediction, 2))
  })
  
  # Output scatterplot interaktif
  output$scatterplot <- renderPlot({
    plot(ecommerce_data$x1, ecommerce_data$y, main = "Scatterplot",
         xlab = "Website Visitors", ylab = "Monthly Sales Volume",
         col = "blue", pch = 19)
  })
  
  # Output tabel interaktif
  output$datatable <- renderTable({
    ecommerce_data
  })
}

# Menjalankan aplikasi
shinyApp(ui, server)