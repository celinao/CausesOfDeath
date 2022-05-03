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

# Overview Text 
keyQuestion <- "How does the cause of death vary and change across regions, countries, and years?"
introduction <- "This interface was created to answer that question. We wanted to create a general interface so that every user could use it, regardless of their country. If a reader wants to know what causes of death are more prevalent in their country, they can focus on adjusting their lifestyle and risk factors to prevent those causes of death. Additionally, users can look at the temporal trends of specific causes of deaths throughout each country."
WorldMapCard <- "The World Map tab allows you to see which countries are most impacted by each cause of death. You can select a cause of death and a year and a heat map will be generate showing which countries have the highest percentage of deaths for each cause. \nYou can use this data see where different causes are most common or to follow the spread of an epidemic across the years as it spreads to neighboring countries."
CauseByTimeCard <- "The Deaths by Cause tab allows you to see how different disease trend through the years. You can select countries and causes and see how the percentage (or number) of deaths have increased or decreased through the years. You can use this to see spikes in deaths or to compare the number of deaths in different countries or between different causes. "
DeathsByCountryCard <- "The Deaths by Country tab allows you to view a general overview of the most common causes of deaths in each country. It allows you to select countries and years and it displays the percent (or number) of deaths for twenty most common deaths. "

# Where to Start
commonWorldMap <- "The world map is very good at seeing what causes of death were most common in different regions. The most common cause of death for every year between 1990 and 2019 was Cardiovascular Disease, followed by Neoplasms. Try looking at those diseases to see if they take a higher percentage of deaths on some regions more than others."
regionalWorldMap <- "Try looking at Tuberculosis. The map shows that it is most prevelant in Central Africa and South/Southeast Asia. However, if you change you year you can see that, while it is concentrated in Somolia and the Central African Republic right now, it used to be a much bigger issue in Southeast Asia. \nThe TB vaccine was created in 1921 so the disease hasn't spread much to other countries even with immigration and has instead slowly decreased in the regions it is present in."
HIV1 <- "HIV/AIDS is currently very common in South Africa. However, if you look back into the early 1990s you can see that it used to much bigger in other countries like Uganda and Botswana which implies that it spread from those countries to South Africa."
HIV2 <- "If you examine the HIV/AIDS trend in the Cause by Time tab you can see more information on how it spread. By selecting the countries we saw in the world map (South Africa, Botswana, Tanzania, Uganda, Zambia, Zimbabwe) we can see that the Uganda's percent deaths was almost twice that of other countries in 1990. However, other countries quickly overtook Uganda until they too, declined, leaving South Africa as the current epicenter. "
HIV3 <- "However, you may remember that the US had an HIV/AIDS epidemic in the early 90s. Where is that on the graph? Since the population of the US is much larger than that of other countries the percent of people who died from HIV is much lower than that of countries in Africa. However, if we change the y-axis to show deaths by numbers we can see that while, it does have a higher number of deaths in the early 90s than countries like Zambia, Botswanwa the number has nothing on the number of people dying in South Africa right now. "
generalDeathsByCountry <- "If you examine two countries like the United States and South Africa you can see that they have share the most common causes of death. However, you can also see that the causes of deaths in the US is highly concentrated on Cardiovascular Diseases and Neoplasms whereas, the deaths in South Africa are more evenly distributed amongst the top twenty causes."

# Shiny App 
ui = fluidPage(
  theme = shinytheme("superhero"),
  
  # Create Tabs in the NavBar 
  navbarPage("Causes of Death",
             
             # Tab 1: Overview
             tabPanel("Overview"
                      , 
               titlePanel("Project Overview"),
               h3(keyQuestion), 
               p(introduction), 
               
               hr(), 
               
               fluidRow(
                 column(width = 4,
                        div(class = "card border-primary mb-3",
                            h3("World Map"), 
                            div(class = "card-body", 
                                p(class = "card-text", WorldMapCard)
                                ), 
                            )
                        ), 
                 
                 column(width = 4,
                        div(class = "card border-primary mb-3",
                            h3("Cause By Time"), 
                            div(class = "card-body", 
                                p(class = "card-text", CauseByTimeCard)
                            ), 
                        )
                 ), 
                 
                 column(width = 4,
                        div(class = "card border-primary mb-3", 
                            h3("Deaths By Country"), 
                            div(class = "card-body", 
                                p(class = "card-text", DeathsByCountryCard)
                            ), 
                        )
                 )
               ), 
               
               hr(), 
               
               h2("Where to Start"), 
               h3("Examining Maps"),
               fluidRow(
                 column(width = 4, 
                        img(src = 'commonWorldMap.JPG', width = 400), 
                        p(commonWorldMap), 
                        ), 
                 column(width = 4, 
                        img(src = 'TB1990.JPG', width = 400), 
                        p(regionalWorldMap)
                        ), 
                 column(width = 4, 
                        img(src = 'DeathsByCountryGeneral.JPG', width = 400), 
                        p(generalDeathsByCountry))
               ), 
               
               h3("Examining One Cause of Death: HIV/AIDS"), 
               fluidRow(
                 column(width = 4, 
                        img(src = 'HIV1990.JPG', width = 400), 
                        img(src = 'HIV2019.JPG', width = 400), 
                        p(HIV1), 
                 ), 
                 column(width = 4, 
                        img(src = 'HIV_Time.JPG', width = 400), 
                        p(HIV2)
                 ), 
                 column(width = 4, 
                        img(src = 'HIV_Time_Count.JPG', width = 400), 
                        p(HIV3))
               ), 
               
               hr(), 
               
               h3("Data Sources"), 
               p("Dataset from Causes of Death - Our World In Data at https://www.kaggle.com/datasets/ivanchvez/causes-of-death-our-world-in-data"), 
             ),
             
             # Tab 2: World map by cause
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
             ), 
             
             # Tab 3: Cause by Time
             tabPanel("Cause by Time",
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
             
             # Tab 4: Deaths by Country 
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
             
  )
)

server = function(input, output) {
  # Calculate height of plot
  plotHeight <- reactive({550*(length(input$yearRange) + length(input$yearRange) %%2)/2})
  plotHeight2 <- reactive({550*(length(input$causes) + length(input$causes) %%2)/2})
  
  # Render Tab 4 Deaths Plot 
  output$deathsChart <- renderPlot({
    if(input$yStyle){
      CountDeathGraph(input$country, input$yearRange)
    }else{
      PercentDeathGraph(input$country, input$yearRange)
    }
  }, height = function(){plotHeight()})
  
  # Render Tab 3 Causes Plot 
  output$causeChart <- renderPlot({
    if(input$yStyle2){
      CountCausesGraph(input$country2, input$causes)
    }else{
      PercentCausesGraph(input$country2, input$causes)
    }
  }, height = function(){plotHeight2()})
  
  # Render Tab 2 World Map
  output$map <- renderPlot({
    worldmap(input$mapYear, input$mapCause, input$region)
  }, height = 550)
}

shinyApp(ui = ui, server = server)


