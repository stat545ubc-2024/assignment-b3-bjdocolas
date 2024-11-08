# This is a Shiny web application that was created for STAT545B at University of British Columbia (UBC).
# The app uses an open source dataset found on Kaggle by Sooter Saalu titled "Amazon Top 50 Bestselling Books 2009-2019".
# You can run the application by clicking the 'Run App' button above.

# **There are many different features in this shiny app code, however I will highlight and comment on 3 of them.**

# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Load the dataset downloaded from Kaggle
books <- read.csv("dataset/amazon_bestsellers.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
  titlePanel("Amazon's Top 50 Bestselling Books from 2009-2019"),
  sidebarLayout(
    sidebarPanel(
      h4("Need a good book to cozy up with? This app will help you find the right book to read! Just use the filters below ..."),
      br(),
      sliderInput("priceInput", "Price", 0, 35, c(0, 35), pre = "$"),
      radioButtons("genreInput", "Genre",
                   choices = c("Fiction", "Non Fiction"),
                   selected = "Fiction"),
      checkboxInput("filterYear", "Filter by year", FALSE),
      conditionalPanel(
        condition = "input.filterYear",
        uiOutput("yearSelectorOutput")),
      hr(),
      span("Source: dataset created by Sooter Saalu on ",
           tags$a("Kaggle",
                  href = "https://www.kaggle.com/datasets/sootersaalu/amazon-top-50-bestselling-books-2009-2019")),
      br(), br()
    ),

    mainPanel(
      h3(textOutput("summaryText")),

      # This feature using downloadButton() allows the user to download the results table as a .csv file to save their results based on the filtering they select from the side panel.
      downloadButton("download", "Download results"),

      br(),

      # This feature separates the produced plot and data table into separate tabs on the main panel so that the user can toggle between the two of them.
      tabsetPanel(
        tabPanel("Graph", plotOutput("coolplot")),
        tabPanel("Table", DT::dataTableOutput("results"))),
    )
  )
)


server <- function(input, output) {
  output$yearSelectorOutput <- renderUI({
    selectInput("yearInput", "Year",
                sort(unique(books$Year)),
                selected = "2019")
  })

  # This feature shows the number of results produced depending on the filters used, and automatically updates whenever the filters change, telling the user how many observations are found depending on their selections.
  output$summaryText <- renderText({
    numOptions <- nrow(filtered())
    if (is.null(numOptions)) {
      numOptions <- 0
    }
    paste0("We found ", numOptions, " options for you")
  })

  filtered <- reactive({
    data <- books
    data <- data %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Genre == input$genreInput)

    if (input$filterYear && !is.null(input$yearInput)) {
      data <- data %>%
        filter(Year == input$yearInput)
    }

    if (nrow(data) == 0) {
      return(NULL)
    }

    data

  })

  output$coolplot <- renderPlot({
    if (is.null(filtered())){
      return(NULL)
    }

    ggplot(filtered(), aes(x = Price, fill = Genre)) +
      geom_histogram(colour = "black", bins = 20) +
      scale_fill_manual(values = c("Fiction" = "pink", "Non Fiction" = "lightblue")) +
      theme_classic(20) +
      labs(title = "Book Prices by Genre",
           x = "Price ($)",
           y = "Count")
  })

  output$results <- DT::renderDataTable({
    filtered()
  })

  output$download <- downloadHandler(
    filename = function() {
      "amazon_bestsellers_filtered.csv"
    },
    content = function(con) {
      write.csv(filtered(), con, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
