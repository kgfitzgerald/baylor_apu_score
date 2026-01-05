library(shiny)
library(tidyverse)

# Load the data
batter_war <- read_csv("./batter_stats.csv") %>% 
  select(player_name, age, war) |> 
  mutate(war_type = "batter")
pitcher_war <- read_csv("./pitcher_stats.csv") %>% 
  select(player_name, age, war) |> 
  mutate(war_type = "pitcher")

all_players_war <- rbind(batter_war, pitcher_war)

ymin <- floor(min(all_players_war$war))
ymax <- ceiling(max(all_players_war$war))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("MLB Player's WAR Stats by Age"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Dropdown menu for player name with server-side selectize ----
      selectizeInput(
        inputId = "player_name",
        label = "Player Name",
        choices = NULL
      )
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  # Update selectize input choices dynamically
  updateSelectizeInput(session, "player_name", choices = all_players_war$player_name, server = TRUE)
  
  output$distPlot <- renderPlot({
    # Access the input value using input$player_name
    player_name <- input$player_name
    
    if (!is.null(player_name) && player_name != "") {
      player_data <- all_players_war %>% 
        filter(player_name == !!player_name)
      
      if (nrow(player_data) > 2) {
        min_age <- min(player_data$age)
        max_age <- max(player_data$age)
        
        ggplot(player_data, aes(x = age, y = war, 
                                linetype = war_type)) +
          geom_point() +
          geom_line() +
          # Shade the region above y = 6 as light purple
          annotate("rect", xmin = -Inf, xmax = Inf, ymin = 6, ymax = Inf, alpha = 0.2, fill = "purple") +
          # Shade the region from y = 3 to y = 6 as light blue
          annotate("rect", xmin = -Inf, xmax = Inf, ymin = 3, ymax = 6, alpha = 0.2, fill = "blue") +
          # Label the light blue region as "All-star level"
          annotate("text", x = min_age + 1, y = 4.5, label = "All-star level", color = "blue", size = 5) +
          # Label the light purple region as "MVP level"
          annotate("text", x = min_age + 1, y = 7, label = "MVP level", color = "purple", size = 5) +
          theme_minimal() +
          ylim(ymin, ymax) +
          scale_x_continuous(breaks = (min_age-2):(max_age + 2)) +
          labs(
            x = "Age",
            y = "WAR",
            title = paste(player_name, "'s WAR trajectory", sep = ""),
            subtitle = "Data from 2014 - 2023 seasons"
          ) +
          theme(
            text = element_text(size = 16),
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 16),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 16),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16)
          )
      } else {
        plot.new()
        text(0.5, 0.5, "Not enough data available for selected player", cex = 1.5)
      }
    } else {
      plot.new()
      text(0.5, 0.5, "Select a player name to see their WAR Stats Over time", cex = 1.5)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

