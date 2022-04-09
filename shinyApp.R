# Install Packages 
library(shiny)
library(shinyWidgets)
library(shinythemes)
library("tidyverse")
library(ggplot2)
library(readr)

# Read in data 
clean_deaths <- readr::read_csv("data//deaths_with_AllCountries.csv")
country_list <- unique(clean_deaths$Entity)
year_list <- unique(clean_deaths$Year)
cause_list <- unique(clean_deaths$Causes_name)

# Helper Functions to create graphs 
create_Percent_Graph <- function(selectedCountries, selectedYearRange) {
  p <- clean_deaths %>%
    filter(Entity %in% selectedCountries, 
           Year %in% selectedYearRange) %>%
    group_by(Entity, Year) %>%
    mutate(Total_Deaths = sum(Death_Numbers, na.rm = TRUE), 
           Percent_Deaths = 100*Death_Numbers/Total_Deaths, 
           Year = factor(Year)) %>%
    ungroup() %>%
    ggplot() +
    geom_col(position = "dodge",
             aes(x = Percent_Deaths,
                 y = reorder(Causes_name, Percent_Deaths),
                 fill=Entity)) +
    facet_wrap(~Year) +
    theme_minimal() +
    theme(legend.position="bottom") +
    labs(x="Percent of Deaths", y = "Cause of Death") +
    guides(fill=guide_legend(title="Country")) + 
    scale_x_continuous(expand = c(0, 0))
  return(p)
}

create_Number_Graph <- function(selectedCountries, selectedYearRange) {
  p <- clean_deaths %>%
    filter(Entity %in% selectedCountries, 
           Year %in% selectedYearRange) %>%
    mutate(Year = factor(Year)) %>%
    ggplot() +
    geom_col(position = "dodge",
             aes(x = Death_Numbers,
                 y = reorder(Causes_name, Death_Numbers),
                 fill=Entity)) + 
    facet_wrap(~Year) +
    theme_minimal() +
    theme(legend.position="bottom") +
    labs(x="Number of Deaths", y = "Cause of Death") +
    guides(fill=guide_legend(title="Country")) + 
    scale_x_continuous(expand = c(0, 0))
  return(p)
}

create_Number_Causes_Graph <- function(selectedCountries, selectedCauses) {
  p <- clean_deaths %>%
    filter(Entity %in% selectedCountries, 
           Causes_name %in% selectedCauses) %>%
    ggplot() + 
    geom_line(aes(x = Year, y = Death_Numbers, col = Entity)) + 
    facet_wrap(~Causes_name) + 
    theme_minimal() +
    theme(legend.position="bottom") +
    labs(y="Number of Deaths", x = "Year") +
    guides(col=guide_legend(title="Country"))
  return(p)
}

create_Percent_Causes_Graph <- function(selectedCountries, selectedCauses) {
  p <- clean_deaths %>%
    filter(Entity %in% selectedCountries, 
           Causes_name %in% selectedCauses) %>%
    group_by(Entity, Causes_name) %>%
    mutate(Total_Deaths = sum(Death_Numbers, na.rm = TRUE), 
           Percent_Deaths = 100*Death_Numbers/Total_Deaths) %>%
    ungroup() %>%
    ggplot() + 
    geom_line(aes(x = Year, y = Percent_Deaths, col = Entity)) + 
    facet_wrap(~Causes_name) + 
    theme_minimal() +
    theme(legend.position="bottom") +
    labs(y="Percent of Deaths", x = "Year") +
    guides(col=guide_legend(title="Country"))
  return(p)
}

# Shiny App 
ui = fluidPage(
  theme = shinytheme("superhero"),
  
  # Create Tabs in the NavBar 
  navbarPage("Causes of Death",
             
             # Tab 1: Deaths by Country 
             tabPanel("Deaths by Country",
                      sidebarLayout(
                        # Sidebar 
                        sidebarPanel(
                          
                          # Select Countries 
                          selectizeInput(
                            selected = "All Countries", 
                            inputId = "country", 
                            label = h3("Choose a Country:"),
                            choices = country_list, 
                            multiple = TRUE,
                            options = list(create = FALSE), 
                          ), 
                          
                          # Select two years to facet by. 
                          sliderInput("yearRange", 
                                      label = h3("Year Range:"),
                                      min = year_list[1], 
                                      max = year_list[length(year_list)], 
                                      value = c(year_list[1], year_list[length(year_list)]),
                                      sep=""
                          ), 
                          
                          # Toggle between Number and Percent Graphs 
                          switchInput(
                            inputId = "yStyle",
                            onLabel = "Number", 
                            offLabel = "Percent", 
                            size = "mini"
                          )
                        ), 
                        
                        # Main Panel: 
                        mainPanel(
                          plotOutput("barChart")
                        )
                      )
             ), 
             
             # Tab 2: Deaths by Cause
             tabPanel("Deaths by Cause",
                      sidebarLayout(
                        sidebarPanel(
                          # Select Countries 
                          selectizeInput(
                            selected = "All Countries", 
                            inputId = "country2", 
                            label = h3("Choose a Country:"),
                            choices = country_list, 
                            multiple = TRUE,
                            options = list(create = FALSE), 
                          ),
                          
                          # Select Causes of Death 
                          selectizeInput(
                            selected = "Acute hepatitis",
                            inputId = "causes",
                            label = h3("Choose a Cause:"),
                            choices = cause_list,
                            multiple = TRUE,
                            options = list(create = FALSE),
                          ), 
                          # Toggle between Number and Percent Graphs 
                          switchInput(
                            inputId = "yStyle2",
                            onLabel = "Number", 
                            offLabel = "Percent", 
                            size = "mini"
                          )
                        ),
                        mainPanel(
                          plotOutput("causeChart")
                        )
                      )
             )
  )
)

server = function(input, output) {
  output$barChart <- renderPlot({
    if(input$yStyle){
      create_Number_Graph(input$country, input$yearRange)
    }else{
      create_Percent_Graph(input$country, input$yearRange)
    }
  })
  
  output$causeChart <- renderPlot({
    if(input$yStyle2){
      create_Number_Causes_Graph(input$country2, input$causes)
    }else{
      create_Percent_Causes_Graph(input$country2, input$causes)
    }
  })
}

shinyApp(ui = ui, server = server)


