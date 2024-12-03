# This is a Shiny web application that was created for STAT545B at University of British Columbia (UBC).
# The app uses an open source dataset found on Kaggle by Sooter Saalu titled "Amazon Top 50 Bestselling Books 2009-2019".
# You can run the application by clicking the 'Run App' button above.

# **There are many different features in this shiny app code, however have commented on 3 labelled as Asssignment b3 and 3 labelled for Assignment B4.**

# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinythemes)
library(shinydashboard)

# Load the dataset downloaded from Kaggle
books <- read.csv("dataset/amazon_bestsellers.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
  # Assignment B4 feature: using shinytheme() to alter the theme and appearance of the app to make it more aesthetically pleasing to the user
  theme = shinytheme("sandstone"),
  titlePanel("Amazon's Top 50 Bestselling Books from 2009-2019"),
  sidebarLayout(
    sidebarPanel(
      h4("Find a good book to read! Just use the filters below ..."),
      br(),
      sliderInput("priceInput", "Price", 0, 35, c(0, 35), pre = "$"),
      radioButtons("genreInput", "Genre",
                   choices = c("Fiction", "Non Fiction"),
                   selected = "Fiction"),
      checkboxInput("filterYear", "Filter by year", FALSE),
      conditionalPanel(
        condition = "input.filterYear",
        uiOutput("yearSelectorOutput")),
      br(),
      # Assignment B4 feature: This NEW feature adds a link to Amazon so the user can order any of the books they find based on using their filters in the app!
      span("Want to order any of these books? Check them out on ",
           tags$a("Amazon",
                  href = "https://www.amazon.com/")),
      hr(),
      span("Source: dataset created by Sooter Saalu on ",
           tags$a("Kaggle",
                  href = "https://www.kaggle.com/datasets/sootersaalu/amazon-top-50-bestselling-books-2009-2019")),
      br(), br()
    ),

    mainPanel(
      h3(textOutput("summaryText")),

      # Assignment b3 feature: This feature using downloadButton() allows the user to download the results table as a .csv file to save their results based on the filtering they select from the side panel.
      downloadButton("download", "Download results"),

      br(), br(),

      # Assignment b3 feature: This feature separates the produced plot and data table into separate tabs on the main panel so that the user can toggle between the two of them.
      tabsetPanel(
        tabPanel("Graph", plotOutput("coolplot")),
        tabPanel("Table", DT::dataTableOutput("results"))),
    )
  )
)


server <- function(input, output) {
  # Assignment B4 feature: This modalDialog() feature lists instructions for how the user can navigate the app. Using showModal allows the window to pop-up at first load of the app page.
  observe({
    showModal(
      modalDialog(
        title = "Need to find your next good read? You've come to the right app!",
        p("Here’s how to use this app:"),
        tags$ul(
          tags$li("Filter the book selection based on price, genre, and year"),
          tags$li("View your results by GRAPH, or select the TABLE tab for more information on each book"),
          tags$li("Use the DOWNLOAD RESULTS button to save your next reads!")
        ),
        footer = modalButton("Got it!")
      )
    )
  })

  output$yearSelectorOutput <- renderUI({
    selectInput("yearInput", "Year",
                sort(unique(books$Year)),
                selected = "2019")
  })

  # Assignment b3 feature: This feature shows the number of results produced depending on the filters used, and automatically updates whenever the filters change, telling the user how many observations are found depending on their selections.
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
      scale_fill_manual(values = c("Fiction" = "tan", "Non Fiction" = "lightblue")) +
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
