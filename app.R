# Install Packages 
library(shinyWidgets)
library(shinythemes)
library("tidyverse")
library(countrycode)
library(rworldmap)
library(RColorBrewer)

th <- theme_minimal() + 
  theme(
    rect = element_blank(), 
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#4d5d6c", color = "#4d5d6c"),
    plot.background = element_rect(fill = "#2b3e4f", color = "#2b3e4f"),
    axis.text = element_text(size = 14, color = "#ffffff"),
    axis.title = element_text(size = 16, color = "#ffffff"),
    strip.text = element_text(color = "#ffffff"), 
    legend.text = element_text(color = "#ffffff"),
    legend.title = element_text(color = "#ffffff"), 
    legend.position = "top", 
    panel.spacing.x = unit(10, "mm")
  ) 
theme_set(th)

# Color Palettes 
color_theme = "Paired"
palette = colorRampPalette(brewer.pal(n=7, name='Oranges'))(7)

# Read in data 
clean_deaths <- readr::read_csv("data//deaths_with_AllCountries.csv")
country_list <- unique(clean_deaths$Entity)
year_list <- unique(clean_deaths$Year)
cause_list <- unique(clean_deaths$Causes_name)

top_causes_list <- clean_deaths %>%
  filter(Entity == "All Countries", Year == 2019) %>%
  slice_max(order_by = Death_Numbers, n = 20)

# Adding Country Code for map 
map_data <- clean_deaths %>%
  filter(Entity != "All Countries") %>%
  mutate(Entity = ifelse(Entity == "America", "United States", Entity), 
         CountryCode = countrycode(Entity , origin='country.name' , destination='iso3c'))

map_regions <- c("World", levels(unique(getMap()$REGION)))

# Helper Functions to create graphs 
PercentDeathGraph <- function(selectedCountries, selectedYearRange) {
  p <- clean_deaths %>%
    filter(Entity %in% selectedCountries, 
           Year %in% selectedYearRange) %>%
    group_by(Entity, Year) %>%
    mutate(Total_Deaths = sum(Death_Numbers, na.rm = TRUE), 
           Percent_Deaths = 100*Death_Numbers/Total_Deaths, 
           Year = factor(Year)) %>%
    ungroup() %>% 
    filter(Causes_name %in% top_causes_list$Causes_name) %>%
    
    ggplot() +
    geom_col(position = "dodge",
             aes(x = Percent_Deaths,
                 y = reorder(Causes_name, Percent_Deaths),
                 fill=Entity)) +
    facet_wrap(~Year, ncol = 2) +
    labs(x="Percent of Deaths", y = "Cause of Death") +
    guides(fill=guide_legend(title="Country")) + 
    scale_x_continuous(expand = c(0, 0))+ 
    scale_fill_brewer(palette = color_theme) +
    th
  return(p)
}

CountDeathGraph <- function(selectedCountries, selectedYearRange) {
  p <- clean_deaths %>%
    filter(Entity %in% selectedCountries, 
           Year %in% selectedYearRange) %>%
    mutate(Year = factor(Year)) %>%
    
    ggplot() +
    geom_col(position = "dodge",
             aes(x = Death_Numbers,
                 y = reorder(Causes_name, Death_Numbers),
                 fill=Entity)) + 
    facet_wrap(~Year, ncol = 2) +
    labs(x="Number of Deaths", y = "Cause of Death") +
    guides(fill=guide_legend(title="Country")) + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_fill_brewer(palette = color_theme) +
    th
  return(p)
}

CountCausesGraph <- function(selectedCountries, selectedCauses) {
  p <- clean_deaths %>%
    filter(Entity %in% selectedCountries, 
           Causes_name %in% selectedCauses) %>%
    
    ggplot() + 
    geom_line(aes(x = Year, y = Death_Numbers, col = Entity)) + 
    facet_wrap(~Causes_name, ncol = 2) + 
    labs(y="Number of Deaths", x = "Year") +
    guides(col=guide_legend(title="Country")) + 
    scale_color_brewer(palette = color_theme) +
    th
  return(p)
}

PercentCausesGraph <- function(selectedCountries, selectedCauses) {
  p <- clean_deaths %>%
    filter(Entity %in% selectedCountries,) %>%
    group_by(Entity, Year) %>%
    mutate(Total_Deaths = sum(Death_Numbers, na.rm = TRUE), 
           Percent_Deaths = 100*Death_Numbers/Total_Deaths) %>%
    ungroup() %>%
    filter(Causes_name %in% selectedCauses) %>%
    
    ggplot() + 
    geom_line(aes(x = Year, y = Percent_Deaths, col = Entity)) + 
    facet_wrap(~Causes_name, ncol = 2) + 
    labs(y="Percent of Deaths", x = "Year") +
    guides(col=guide_legend(title="Country")) + 
    scale_color_brewer(palette = color_theme) +
    th
  return(p)
}

# Create World Map 
worldmap <- function(year, Cause, region){
  
  #Filter and Join Data to map 
  new_data <- map_data %>%
    filter(Year == year) %>%
    group_by(Entity) %>%
    mutate(Percent_Deaths = 100*Death_Numbers/sum(Death_Numbers, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(Causes_name == Cause) %>%
    joinCountryData2Map(joinCode = "ISO3" , nameJoinColumn = "CountryCode") 
  
  # Create 7 Even Color Palette Groups 
  max_val <- max(new_data$Percent_Deaths, na.rm = TRUE)
  iter_num <- max_val/6 
  
  # Create Map 
  mapParams <- mapCountryData(new_data, 
                 nameColumnToPlot='Percent_Deaths', 
                 mapRegion = region, 
                 catMethod = if(max_val == 0) c(0, 1) else (seq(0, max_val+iter_num, iter_num)), 
                 colourPalette = palette,
                 oceanCol='light blue', 
                 missingCountryCol='dark grey', 
                 mapTitle=paste(Cause, "-", year, "\nby Percent of Deaths"),
                 addLegend=TRUE,
  )
  mapParams$legendText 
}

# TEXT 
motivation <- "This interface was created to answer that question. We wanted to create a general interface so that every user could use it, regardless of their country. 
If a reader wants to know what causes of death are more prevalent in their country, they can focus on adjusting their lifestyle and risk factors to prevent those causes of death. 
Additionally, users can look at the temporal trends of specific causes of deaths throughout each country."
DeathsByCountry <- "The Deaths by Country tab allows users to investigate how the top causes of death in selected countries and years."
DeathsByCause <- "The Deaths by Country tab allows users to investigate trendlines of selected causes of death in selected countries between 1990 and 2019. "
WorldMapDesc <- "The World Map tab visualizes the countries with the most deaths from the selected cause of death."


# Shiny App 
ui = fluidPage(
  theme = shinytheme("superhero"),
  
  # Create Tabs in the NavBar 
  navbarPage("Causes of Death",
             
             # Tab 0: Overview
             tabPanel("Overview", 
               titlePanel("Project Overview"),
               h3("How does the cause of death vary and change across countries and years?"), 
               p(motivation), 
               
               hr(), 
               
               h4("Deaths by Country"), 
               p(DeathsByCountry), 
               h4("Deaths by Cause"), 
               p(DeathsByCause), 
               h4("World Map"), 
               p(WorldMapDesc), 
               
               hr(), 
               
               h3("Data Sources"), 
               p("Dataset from Causes of Death - Our World In Data at https://www.kaggle.com/datasets/ivanchvez/causes-of-death-our-world-in-data"), 
             ),
             
             # Tab 1: Deaths by Country 
             tabPanel("Deaths by Country",
                      
                      fluidRow(
                        
                        # Select Countries 
                        column( width = 4, 
                          selectizeInput(
                            selected = "All Countries", 
                            inputId = "country", 
                            label = h3("Choose a Country:"),
                            choices = country_list, 
                            multiple = TRUE,
                            options = list(create = FALSE), 
                          )
                          ), 
                          
                        # Select years to facet by. 
                        column( width = 4, 
                                selectizeInput(
                                  selected = c(year_list[1], year_list[length(year_list)]), 
                                  inputId = "yearRange", 
                                  label = h3("View Years:"),
                                  choices = year_list, 
                                  multiple = TRUE,
                                  options = list(create = FALSE), 
                                )
                        ), 
                          
                        # Toggle between Number and Percent Graphs 
                        column( width = 4, 
                                h3("Switch y-axis: "), 
                                switchInput(
                                  inputId = "yStyle",
                                  onLabel = "Number", 
                                  offLabel = "Percent", 
                                  size = "normal"
                                )
                        )
                      ), 
                      
                      # Deaths Barchart: 
                      fluidRow(width = 11, 
                               plotOutput("deathsChart")
                      )
             ), 
             
             # Tab 2: Deaths by Cause
             tabPanel("Deaths by Cause",
                      fluidRow(
                        
                        # Select Countries 
                        column( width = 4,
                                selectizeInput(
                                selected = "All Countries", 
                                inputId = "country2", 
                                label = h3("Choose a Country:"),
                                choices = country_list, 
                                multiple = TRUE,
                                options = list(create = FALSE),
                                )
                        ), 
                        
                        # Select Causes of Death 
                        column( width = 4, 
                                selectizeInput(
                                  selected = "Acute hepatitis",
                                  inputId = "causes",
                                  label = h3("Choose a Cause:"),
                                  choices = cause_list,
                                  multiple = TRUE,
                                  options = list(create = FALSE),
                                )
                        ), 
                        
                        # Toggle between Number and Percent Graphs 
                         column( width = 4, 
                                 h3("Switch y-axis: "), 
                                 switchInput(
                                   inputId = "yStyle2",
                                   onLabel = "Number", 
                                   offLabel = "Percent", 
                                   size = "normal"
                                 )
                         )), 
                      
                      fluidRow( width = 11, 
                               plotOutput("causeChart")
                      )
             ), 
             
             # Tab 3: World map by cause
             tabPanel("World Map",
                      fluidRow(
                        
                        # Select Causes of Death 
                        column(width = 4, 
                               selectizeInput(
                                 selected = "Acute hepatitis",
                                 inputId = "mapCause",
                                 label = h3("Choose a Cause:"),
                                 choices = cause_list,
                                 multiple = FALSE,
                                 options = list(create = FALSE),
                                 )
                               ), 
                        
                        # Select Year Shown 
                        column(width = 4, 
                               selectizeInput(
                                 selected = max(year_list),
                                 inputId = "mapYear",
                                 label = h3("Choose a Year:"),
                                 choices = year_list,
                                 multiple = FALSE,
                                 options = list(create = FALSE),
                                 )
                               ), 
                        
                        # Select Region View 
                        column(width = 4, 
                               selectizeInput(
                                 selected = "World",
                                 inputId = "region",
                                 label = h3("Zoom into a Region:"),
                                 choices = map_regions,
                                 multiple = FALSE,
                                 options = list(create = FALSE),
                                 )
                               )
                      ), 
                      
                      # Plot World Map 
                      fluidRow( width = 11, plotOutput("map"))
             )
  )
)

server = function(input, output) {
  # Calculate height of plot
  plotHeight <- reactive({550*(length(input$yearRange) + length(input$yearRange) %%2)/2})
  plotHeight2 <- reactive({550*(length(input$causes) + length(input$causes) %%2)/2})
  
  # Render Tab 1 Deaths Plot 
  output$deathsChart <- renderPlot({
    if(input$yStyle){
      CountDeathGraph(input$country, input$yearRange)
    }else{
      PercentDeathGraph(input$country, input$yearRange)
    }
  }, height = function(){plotHeight()})
  
  # Render Tab 2 Causes Plot 
  output$causeChart <- renderPlot({
    if(input$yStyle2){
      CountCausesGraph(input$country2, input$causes)
    }else{
      PercentCausesGraph(input$country2, input$causes)
    }
  }, height = function(){plotHeight2()})
  
  # Render Tab 3 World Map
  output$map <- renderPlot({
    worldmap(input$mapYear, input$mapCause, input$region)
  }, height = 550)
}

shinyApp(ui = ui, server = server)


