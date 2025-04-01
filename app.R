# Install and load the necessary packages
library(shiny)
library(tidyverse)
library(rsconnect)

rugby <- read.csv("results.csv")
# Create a sample data frame
ugby <- rugby %>% 
  #creates variables win_score and lose_score that indicates which score was of the winning team and the losing team, respectfully
  mutate(Win_Score = pmax(home_score, away_score),
         Lose_Score = pmin(home_score, away_score)) %>% 
  #creates variables winning_team and losing_team that shows the winning and losing team
  #if tied, then NA
  mutate(Winning_Team = ifelse(home_score > away_score, home_team, 
                               ifelse(home_score < away_score, away_team, NA)),
         Losing_Team = ifelse(home_score < away_score, home_team, 
                              ifelse(home_score > away_score, away_team, NA))) %>% 
  #parses the date into just the year
  mutate(Year = year(date)) %>% 
  #turns True and False into 1's and 0's, where 1 is True for world_cup
  mutate(world_cup = ifelse(world_cup == "True", 1, 0)) %>% 
  #removes colums neutral, home_score and away_score
  select(-neutral, -home_score, -away_score, -competition, -stadium, -city)


# Define the user interface (UI)
ui <- fluidPage(
  titlePanel("International Rugby Union Games from 1871-2023"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Select input for numerical variable
      selectInput("category", "Select Categorical Variable:",
                  choices = c("none" = "None", "Winning Team" = "Winning_Team", "Losing Team" = "Losing_Team")),
      
      
      selectInput("numery", "Select Numerical Variable:",
                  choices = c("none" = "None", "Year", "Scores of Winning Team"= "Win_Score", "Scores of Losing Team"= "Lose_Score")),
      
      h2("Click for data during Rugby World Cup or Reset", style = "font-size: 14px;"), 
      actionButton("worldcup", "Rugby World Cup"),
      actionButton("clearance", "Clear"),
      
      h2(), 
      
      radioButtons("colors","Color (changes color of graph)",
                   choices=c("None"='grey',"Blue"="blue","Red"='darkred',"Pink"='pink'),
                   selected = 1),
      
      sliderInput("bins", "Number of bins (Grouping Numerical Data into Larger or Smaller Groups):",
                  min = 5, max = 50, value = 30),
      
      img(src = "https://keyassets.timeincuk.net/inspirewp/live/wp-content/uploads/sites/7/2023/03/GettyImages-1474385820-scaled.jpg",
          width = 345, height = 230, style = "margin-top: 10px;"),
      p("Dymock, Alan. “The 2023 Rugby World Cup Warm-Ups Results – the Full Rundown.” Rugby World, 29 Aug. 2023, www.rugbyworld.com/tournaments/rugby-world-cup/rugby-world-cup-warm-up-fixtures-149076. Accessed 1 Dec. 2023.")
      
      
    ),
    
    # Main panel for displaying the plot
    mainPanel(
      plotOutput("distplot"),
      
      h2("Description of the Graph Outputed", style = "font-size: 16px;"),
      verbatimTextOutput("output_description"),
      
      h2("Summary of the Selected Numerical Variable (5 Num Summary)", style = "font-size: 16px;"),
      verbatimTextOutput("sum")
    )
  )
)

# Define the server function
server <- function(input, output) {

  worldCupFilter <- reactiveVal(FALSE)
  
  observeEvent(input$worldcup, {
    worldCupFilter(TRUE)
  }) 
  
  observeEvent(input$clearance, {
    worldCupFilter(FALSE)
  }) 
  
  
  truth <- reactive({
    if (worldCupFilter()) {
      return(ugby %>% filter(world_cup == 1))
    } else {
      return(ugby)
    }
  })
  
  output$distplot <- renderPlot({
    if(input$category != "None" && input$numery == "None"){
      truth() %>% 
        filter(is.na(Winning_Team) == FALSE) %>% 
        ggplot(aes_string(x = input$category))+
        geom_bar(fill = ifelse(is.null(input$colors) == FALSE, input$colors, "grey"))+
        labs(title = paste("Barplot of ", input$category, sep = ""), y = "Count")
    }
    
    else if(input$category == "None" && input$numery != "None"){
      truth() %>% 
        filter(is.na(Winning_Team) == FALSE) %>% 
        ggplot(aes_string(x = input$numery))+
        geom_histogram(fill = ifelse(is.null(input$colors) == FALSE, input$colors, "grey"), color = "black",
                       bins = input$bins)+
        labs(title = paste("Histogram of ", input$numery, sep = ""), y = "Count")
    }
    else if (input$category != "None" && input$numery != "None") {
      truth() %>%
        filter(!is.na(Winning_Team)) %>%
        ggplot(aes_string(x = input$category, y = input$numery)) +
        geom_boxplot(fill = ifelse(!is.null(input$colors), input$colors, "grey"), color = "black") +
        labs(title = paste("Boxplot of", input$category, "and", input$numery, sep = " "), 
             y = paste(input$numery), x = paste(input$category))
    }
    
  })
  
  output$sum <- renderPrint({
    if(input$category == "None" && input$numery != "None"){
      summary(ugby[, input$numery])
    }else{
      print("Choose Numerical Variable Only")
    }
  })
  
  
  output$output_description <- renderText({
    if (worldCupFilter()){
      x <- "Rugby World Cup Matches"
    }else{
      x <- "International Rugby Matches"
    }
    
    if (input$category != "None" && input$numery == "None") {
      if(input$category == "Winning_Team"){
        return(paste("This shows the Number of Wins per Country for", x, sep = " "))
      }
      if(input$category == "Losing_Team"){
        return(paste("This shows the Number of Losses per Country for", x, sep = " "))
      }
      
    } else if (input$category == "None" && input$numery != "None") {
      if(input$numery == "Year"){
        return(paste("This shows the Number of games each Year for", x, sep = " "))
      }
      if(input$numery == "Win_Score"){
        return(paste("This shows the distribution of scores of the winning team for", x, sep = " "))
      }
      if(input$numery == "Lose_Score"){
        return(paste("This shows the distribution of scores of the losing team for", x, sep = " "))
      }
      
    } else if (input$category != "None" && input$numery != "None"){
      return(paste("This shows the relationship between", input$category,
             "and", input$numery, "for", x, sep = " "))
    } else {
      return("No specific plot selected.")
    }
  })
  
}

# Run the shiny app
shinyApp(ui = ui, server = server)
