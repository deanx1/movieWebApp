# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("DT")
# install.packages("tools")
# install.packages("Rcpp")
# install.packages("httpuv")

library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(Rcpp)
library(httpuv)

print("starting..")

# Data to use
ml_link_film <- read_csv("ml-20m/ml_link_film.csv")

genres <- ml_link_film$genres %>%  str_split("\\|") %>% unlist() %>% unique()
genreCountTemp <-
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)

# Make dataframes per genre
genreSubset <- list()
for(i in 1:length(genres)) {
  # Doesn't work for unkown reasons???
  #genreSubset[[i]] <- ml_link_film %>% filter(str_detect(ml_link_film$genres, genres[i]))
  genreStr <- genres[i]
  genreSubset[[i]] <- ml_link_film %>% filter(str_detect(ml_link_film$genres, genreStr))
  cat(genres[i], nrow(genreSubset[[i]]), "\n")
}

genreRatingsYear <- list()
years <- vector()
for (i in 1:length(genres)) {
  # Filter the rows further in intervals of 10 years
  start <- min(ml_link_film$year)
  end <- max(ml_link_film$year)
  count <- 1
  tempVector <- vector()
  genreStr <- genres[i]
  while (start <= end + 3) {
    tempSet <-
      genreSubset[[i]] %>% filter(year >= start, year < start + 10)
    tempVector[count] = round(abs(mean(tempSet$avgRatingMl) - mean(tempSet$ratingTmdb)), 2)
    years[count] = start
    start <- start + 10
    count <- count + 1
  }
  print(genreStr)
  print(tempVector)
  genreRatingsYear[[i]] <- tempVector
}

# Total genre year rating
totalGenreRatingYear <- vector()
# Per timeframe
for (i in 1:length(years)) {
  sum <- 0
  count <- 0
  # Per genre
  for (x in 1:length(genres)) {
    tempGenre <- genreRatingsYear[[x]]
    if (!is.na(tempGenre[i])) {
      count <- count + 1
      sum <- sum + tempGenre[i]
    }
  }
  totalGenreRatingYear[i] <- sum / count
}

# Define UI for application that plots features of movies
ui <- fluidPage(
  theme = shinytheme("readable"),

  titlePanel("Movie overview", windowTitle = "Movies"),

  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs
    sidebarPanel(
      width = 3,

      h3("Plotting"),
      # Third level header: Plotting

      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c(
          "IMDB rating" = "ratingTmdb",
          "IMDB number of votes" = "voteCount",
          "ML rating" = "avgRatingMl",
          "Year" = "year",
          "Budget" = "budget",
          "Revenue" = "revenue",
          "Runtime" = "runtime",
          "IMDB Populairty" = "popularity"
        ),
        selected = "ratingImdb"
      ),

      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c(
          "IMDB rating" = "ratingTmdb",
          "IMDB number of votes" = "voteCount",
          "ML rating" = "avgRatingMl",
          "Year" = "year",
          "Budget" = "budget",
          "Revenue" = "revenue",
          "Runtime" = "runtime",
          "IMDB Populairty" = "popularity"
        ),
        selected = "avgRatingMl"
      ),

      # Select variable for color
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c("Adult" = "adult",
                    "Year" = "year"),
        selected = "year"
      ),

      # Set alpha level
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0,
        max = 1,
        value = 0.5
      ),

      # Set point size
      sliderInput(
        inputId = "size",
        label = "Size:",
        min = 0,
        max = 5,
        value = 2
      ),

      # Enter text for plot title
      textInput(
        inputId = "plot_title",
        label = "Plot title",
        placeholder = "Enter text to be used as plot title"
      ),

      # Only show complete casus
      checkboxInput(
        inputId = "complete_cases",
        label = "Show complete casus only",
        value = F
      ),

      # Select sample size
      numericInput(
        inputId = "n_samp",
        label = "Sample size:",
        min = 1,
        max = nrow(ml_link_film),
        value = 100
      ),
      # Header
      h4("Genres"),


      # Only show complete casus
      checkboxInput(
        inputId = "select_all",
        label = "Select all genres",
        value = T
      ),

      # Select which types of movies to plot
      checkboxGroupInput(
        inputId = "selected_genres",
        label = "Select movie genre(s):",
        choices = genres,
        selected = "Comedy"
      ),

      # Built with Shiny by RStudio
      br(),
      h5("Built with Shiny by Dean van Doorn")

    ),

    # Output:
    mainPanel(
      width = 9,

      tabsetPanel(
        id = "tabspanel",
        type = "tabs",
        tabPanel(
          title = "Interactive Plot",
          fluidRow(column(
            width = 8,
            plotOutput(
              outputId = "scatterplot",
              brush = "plot_brush",
              height = "600px",
              width = "680px"
            ),
            br()
          ),

          column(width = 4,
                 h4(
                   uiOutput(outputId = "n")
                 ))),
          verbatimTextOutput("brush_data")
        ),
        tabPanel(
          title = "Genre Ratings Over Time",
          plotOutput(
            outputId = "time_plot",
            brush = "plot_brush",
            height = "600px",
            width = "660px"
          )
        )

      )
    )
  )
)

# Server
server <- function(input, output, session) {
  print("starting server")

  # Filter data by genres
  movies_subset <- reactive({
    req(input$selected_genres) # ensure availablity of value before proceeding
    ml_link_film$genres <-
      str_replace_all(ml_link_film$genres, "\\|", " ")
    if (input$complete_cases)
      ml_link_film <- filter(ml_link_film, budget > 0, revenue > 0)
    ml_link_film %>% filter(str_detect(ml_link_film$genres, input$selected_genres))
  })

  # Select all genres
  observe({
    updateCheckboxGroupInput(session,
                             'selected_genres',
                             choices = genres,
                             selected = if (input$select_all)
                               genres)
  })

  # Update the maximum allowed n_samp for selected type movies
  observe({
    updateNumericInput(
      session,
      inputId = "n_samp",
      value = min(100, nrow(movies_subset())),
      max = nrow(movies_subset())
    )
  })

  # Create new df that is n_samp obs from selected type movies
  movies_sample <- reactive({
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(movies_subset(), input$n_samp)
  })

  # Plot interactive plot
  output$scatterplot <- renderPlot({
    ggplot(data = movies_sample(), aes_string(
      x = input$x,
      y = input$y,
      color = input$z
    )) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(
        x = toTitleCase(str_replace_all(input$x, "_", " ")),
        y = toTitleCase(str_replace_all(input$y, "_", " ")),
        color = toTitleCase(str_replace_all(input$z, "_", " ")),
        title = toTitleCase(input$plot_title)
      )
  })

  # Plot genre ratings over time
  output$time_plot <- renderPlot({
    ggplot(mapping = aes(x = years, y = totalGenreRatingYear)) +
      geom_line() +
      geom_point() +
      labs(y = "Total Difference Rating between Movielens and TMDB")
  })

  # Data by brushing
  output$brush_data <- renderPrint({
    rows <-
      brushedPoints(movies_sample(), input$plot_brush) %>% select(-movieId,-tmdbId,-adult)
    cat("Brushed points:\n")
    print(rows)
  })

}

# Create Shiny app object
shinyApp(ui = ui, server = server)