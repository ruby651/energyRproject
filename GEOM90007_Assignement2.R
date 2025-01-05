#Import Library
library(shiny)         # library for interactive web applications
library(leaflet)       # library for map
library(RColorBrewer)  # library for colour
library(rnaturalearth) # library for long and lat map
library(sf)            # library for Chloropleth map
library(ggplot2)       # library for draw graph
library(ggiraph)       # library for interactive graph
library(tidyr)         # library for restructuring data
library(dplyr)         # library for manipulate the dataset


# Import Dataset
energy <- read.csv('owid-energy-data.csv')

# Filter the year from 2002 to 2022 and the country in Asia 
Asia_to_filter <- c('China','India','Kazakhstan','Saudi Arabia',
                    'Iran','Mongolia', 'Indonesia', 'Pakistan', 
                    'Turkey', 'Myanmar','Afghanistan', 'Yemen', 
                    'Thailand','Turkmenistan', 'Uzbekistan', 
                    'Iraq', 'Japan','Vietnam', 'Malaysia', 'Oman',
                    'Philippines', 'Laos','Kyrgyzstan', 'Syria', 
                    'Bangladesh','Brunei', 'Cambodia', 'Nepal', 
                    'Tajikistan', 'North Korea', 'South Korea', 'Jordan',
                    'United Arab Emirates', 'Azerbaijan', 'Georgia', 
                    'Sri Lanka', 'Bhutan', 'Taiwan', 'Armenia', 
                    'Israel', 'Kuwait', 'East Timor', 'Qatar', 'Lebanon', 
                    'Cyprus', 'Palestine','Brunei', 'Hong Kong', 
                    'Bahrain', 'Maldives', 'Macao','Singapore')
energy <- energy[energy$year >= 2002 & energy$year <= 2022 & energy$country %in% Asia_to_filter, ]

#Dataset of energy consumption from different sources over by year
  #The following code was based on the code at https://dplyr.tidyverse.org/reference/across.html
cols <- c('population','biofuel_consumption', 'coal_consumption', 
          'gas_consumption', 'hydro_consumption',
          'nuclear_consumption', 'other_renewable_consumption', 
          'solar_consumption',
          'wind_consumption', 'oil_consumption')
energy_pri_cst <- energy %>%
  select(c('year', cols)) %>%
  group_by(year) %>%
  summarise(across(all_of(cols), sum, na.rm = TRUE))

#calculate the total Amount in year 
#The following code was based on the code at https://www.statology.org/rowsums-function-in-r/
energy_pri_cst$total_Amount <- rowSums(energy_pri_cst[, -c(1, 2)], na.rm = TRUE)
#Change the structure of data set
energy_pri_cst_long <- energy_pri_cst %>% gather(key=primary_energy, value = Amount, 
                                                                                                  -year, -population, -total_Amount)
#str(energy_pri_cst)
#Dataset for capita comsumption
#Change the dataset structure
energy_long <- energy %>% gather(key=primary_energy, value=Amount, -country,
                                 -year, -iso_code, -population, -gdp )

pri_pc <- c('biofuel_cons_per_capita', 'coal_cons_per_capita',
            'gas_energy_per_capita', 'hydro_energy_per_capita', 
            'nuclear_energy_per_capita',
            'other_renewables_energy_per_capita', 
            'solar_energy_per_capita',
            'wind_energy_per_capita', 'oil_energy_per_capita')

energy_pri_pc <- energy_long[energy_long$primary_energy %in% pri_pc, ]

### import latitude and longtitude map data
#The following code was based on the code at https://cran.r-project.org/web/packages/rnaturalearth/vignettes/rnaturalearth.html

# Get the latitude and longitude data of Asian countries
asia_countries <- ne_countries(scale = "medium", returnclass = "sf")
# Merge the energy consumption data with geographic data
merged_data <- energy_pri_pc %>%
  left_join(asia_countries, by = c("country" = "name")) %>%
  st_as_sf()  # The reference of this line at https://github.com/r-spatial/sf/issues/2429

# Ensure that Amount is numeric
merged_data$Amount <- as.numeric(merged_data$Amount)

#Popup in map
makemapPopup <- function(row) {
  ifelse(row$Amount == "NA", 
         "No data", 
         paste0(strong(row$country), 
                '<br>Amount per Capita: ', prettyNum(round(row$Amount,2), big.mark = ","), ' kWH'))
  
  
}

merged_data$Popup <- by(merged_data, seq_len(nrow(merged_data)), makemapPopup)

# Remove scientific notation
options(scipen=999)

#Color palette for this graph
# The following code was based on the code at chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf
dark2_palette <- brewer.pal(8, 'Dark2')

dark2_extended <-c("#007BA7", dark2_palette )

# 
##################
# USER INTERFACE #
##################

#Overall tab
trendenergy_tab <- tabPanel(
  title='The Overview of Energy Consumption',
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", 
              "Year Range", 
              min = min(energy_pri_cst_long$year), 
              max = max(energy_pri_cst_long$year),
              value = c(min(energy_pri_cst_long$year), max(energy_pri_cst_long$year)),
              step = 1,
              sep = ""
              ),
      selectInput("energySource",
                  "Energy Source",
                  choices = c("Total Energy Consumption"= 'total_Amount',
                              "Biofuel" = 'biofuel_consumption', 
                              "Coal" = 'coal_consumption', 
                              "Gas" = 'gas_consumption', 
                              "Hydropower" = 'hydro_consumption', 
                              "Nuclear" = 'nuclear_consumption', 
                              "Other Renewables" = 'other_renewable_consumption', 
                              "Solar" = 'solar_consumption', 
                              "Wind Energy" = 'wind_consumption' , 
                              "Oil" = 'oil_consumption'),
                  selected = 'total_Amount',
                  multiple = FALSE
      )
  ),
  
    mainPanel(
      girafeOutput('trend_energy_line'),
      girafeOutput('scatter_plot')
    )
  )
)

#Per capita 
energybycountry_tab <- tabPanel(
  title='Per Capita Energy Map',
  sidebarLayout(
    sidebarPanel(
      selectInput("energyElement",
                  "Energy Source",
                  choices = c("Biofuel" = 'biofuel_cons_per_capita', 
                              "Coal" = 'coal_cons_per_capita', 
                              "Gas" = 'gas_energy_per_capita', 
                              "Hydropower" = 'hydro_energy_per_capita', 
                              "Nuclear" = 'nuclear_energy_per_capita', 
                              "Other Renewables" = 'other_renewables_energy_per_capita', 
                              "Solar" = 'solar_energy_per_capita', 
                              "Wind Energy" = 'wind_energy_per_capita' , 
                              "Oil" = 'oil_energy_per_capita'),
                  selected = 'coal_cons_per_capita',
                  multiple = FALSE
      ),
      sliderInput("yearInput", 
                  "Year", 
                  min = min(energy_pri_pc$year), 
                  max = max(energy_pri_pc$year),
                  value = max(energy_pri_pc$year),
                  step = 1,
                  sep = "",
                  animate =
                    animationOptions(interval = 1000, loop = TRUE)
      )
      
      
    ),
    
    mainPanel(
      leafletOutput("energyMap"),
      girafeOutput('pc_country')
      
    )
  )
)  


ui <- navbarPage(
    id='mypage',
    title='Energy Consumption for Electricity in Asia',
    trendenergy_tab,   #Overview tab for the energy consumption of  Asia
    energybycountry_tab #The energy consumption per capita
  )

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  #Trend
  #Filter function
  getFilteredenergy <- reactive({
    filter_pri <- filter(energy_pri_cst_long, year >= input$year_range[1] & year <= input$year_range[2])
    
  #Rearrange the dataset from High to low of the Amount of energy consumption
    #The following code was based on the code at https://dplyr.tidyverse.org/reference/across.html
    total_cons <- filter_pri %>%
      group_by(primary_energy) %>%
      summarise(total = sum(Amount)) %>%
      arrange(desc(total)) %>% 
      pull(primary_energy)
    
    
    
    filter_pri <- filter_pri %>%
      mutate(primary_energy = factor(primary_energy, levels = total_cons))
    
  })
  
  
  #Main graph
  output$trend_energy_line <- renderGirafe({
    p<- ggplot(data = getFilteredenergy(), aes(x = year, y = Amount, 
                                               fill = primary_energy,
                                               color = primary_energy)) +
      geom_area_interactive(aes(fill = primary_energy)) +
      geom_line_interactive(aes(color = primary_energy), position = "stack",size = 2)+
      #The prettyNum(round(Amount,2) code was based on the code at 
      #https://stackoverflow.com/questions/3838774/comma-separator-for-numbers-in-r
      geom_point_interactive(position = "stack", 
                             aes(color = primary_energy, 
                                 tooltip = paste("Year:", year, 
                                                 "<br>Consumption:", prettyNum(round(Amount,2), big.mark = ","), "(TWh)")), 
                             size = 2) +
      #labs(title = "Asian Primary Energy Consumption by Source") +
      scale_x_continuous(
        breaks = seq(min(getFilteredenergy()$year), 
                     max(getFilteredenergy()$year), by = 2)) +
      scale_color_manual(values = dark2_extended, 
                         name = "Energy Source",
                         #direction = -1,
                         labels = c(
                           "biofuel_consumption" = "Biofuel", 
                           "coal_consumption" = "Coal", 
                           "gas_consumption" = "Gas", 
                           "hydro_consumption" = "Hydropower", 
                           "nuclear_consumption" = "Nuclear", 
                           "other_renewable_consumption" = "Other Renewables", 
                           "solar_consumption" = "Solar", 
                           "wind_consumption" = "Wind", 
                           "oil_consumption" = "Oil")) +
      scale_fill_manual(values = dark2_extended, 
                        name = "Energy Source",
                        #direction = -1,
                        labels = c(
                          "biofuel_consumption" = "Biofuel", 
                          "coal_consumption" = "Coal", 
                          "gas_consumption" = "Gas", 
                          "hydro_consumption" = "Hydropower", 
                          "nuclear_consumption" = "Nuclear", 
                          "other_renewable_consumption" = "Other Renewables", 
                          "solar_consumption" = "Solar", 
                          "wind_consumption" = "Wind", 
                          "oil_consumption" = "Oil")) +
      theme(panel.background=element_blank(),
            panel.grid.major.y=element_line(color='#e2e2e2'),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_text(size = 14, face = "bold"),  
            legend.text = element_text(size = 12),
            legend.key.size = unit(1.5, "cm")) +
    ggtitle(paste("The Primary Energy Consumption by Source in Asia", input$year_range[1] ,
                  "-", input$year_range[2]))
    girafe(ggobj=p, height_svg=8, width_svg=12)
  })
  
  
  #scatter plot
  getFilteredyear <- reactive({
    filter(energy_pri_cst, year >= input$year_range[1] & year <= input$year_range[2])
  })
  output$scatter_plot <- renderGirafe({
    p <- ggplot(data = getFilteredyear(), aes(x = population, 
                                              y=  round(.data[[input$energySource]]))) +
      geom_point_interactive(aes(tooltip = paste("Year: ", year,
                                                 "<br>Population: ", prettyNum(population, big.mark = ","),
                                                 "<br>Consumption:", prettyNum(round(.data[[input$energySource]],2), big.mark = ","), "(TWh)")), 
                              size = 6, color = '#de2d26')+
      labs(title = "The Relationship Between Population and the Energy Consumption",
           x = "Population", y = "Energy Consumption (TWh)") +
      theme(panel.background=element_blank(),
            panel.grid.major.y=element_line(color='#e2e2e2'),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 14, face = "bold"),  
            legend.text = element_text(size = 12))
    girafe(ggobj=p, height_svg=8, width_svg=12)
  })
  
  
  
  # Filter and process data based on the selected year
  getfilter_energy_pri_pc <- reactive({
    filtered_pc <- filter(energy_pri_pc, year == input$yearInput)


    
    # Calculate total consumption per country and arrange in ascending order
    total_consumption <- filtered_pc %>%
      group_by(country) %>%
      summarise(total_amount = sum(Amount)) %>%
      arrange(total_amount) %>%
      slice_head(n = 10) %>% 
      pull(country)
    
    
    # Reorder country from largest to smallest Amount
    filtered_pc <- filtered_pc %>%
      filter(country %in% total_consumption) %>%
      mutate(country = factor(country, levels = total_consumption))
    
    # Calculate total consumption per primary energy and arrange in ascending order
    total_amount <- filtered_pc %>%
      group_by(primary_energy) %>%
      summarise(total_amount = sum(Amount)) %>%
      arrange(total_amount) %>%
      pull(primary_energy)
    
    # Reorder primary energy from largest to smallest Amount
    filtered_pc <- filtered_pc %>%
      mutate(primary_energy = factor(primary_energy, levels = total_amount))
    
    
    filtered_pc
  })
  
  #Stacked bar
    
    output$pc_country <- renderGirafe({
      p <- ggplot(getfilter_energy_pri_pc()) +
        #aes(x = country, y = Amount, fill = primary_energy) +
        geom_bar_interactive(aes(x = country, y = Amount, fill = primary_energy,
                            tooltip = paste(prettyNum(round(Amount,2), big.mark = ","), "(kWh)")), 
                             size = 1.5, stat = 'identity', position = "stack") +
        scale_fill_manual(values = rev(dark2_extended),
                          name = "Energy Source", 
                          #direction = -1,
                          labels = c('biofuel_cons_per_capita' = "Biofuel", 
                                     'coal_cons_per_capita' = "Coal", 
                                     'gas_energy_per_capita' = "Gas", 
                                     'hydro_energy_per_capita' = "Hydropower",
                                     'nuclear_energy_per_capita' = "Nuclear", 
                                     'oil_energy_per_capita' = "Oil",
                                     'other_renewables_energy_per_capita' = "Other Renewables", 
                                     'solar_energy_per_capita' = "Solar", 
                                     'wind_energy_per_capita' = "Wind"
                                     )
                          ) +
        coord_flip() +
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(color = '#e2e2e2'),
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.text.x = element_text(size = 12),
              legend.title = element_text(size = 14, face = "bold"),  
              legend.text = element_text(size = 12),
              legend.key.size = unit(1.5, "cm")) +
        ggtitle(paste("Top 10 Countries in Primary Energy Consumption Per Capita", input$yearInput))
      
      girafe(ggobj = p, height_svg = 10, width_svg = 12)
    })
      
  #Map:
    
    
    output$energyMap <- renderLeaflet({
      filtered_data <- merged_data %>%
        filter(primary_energy == input$energyElement, year == input$yearInput)
    #main map  
      # Some of the code below refer the code at: chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://cran.r-project.org/web/packages/leaflet/leaflet.pdf
      # Prepare color palette
      pal <- colorBin(palette = "Reds", 
                      domain =  filtered_data$Amount, 
                          pretty = TRUE, bins =4, na.color = "white")
      
      # Leaflet map with Choropleth
      leaflet() %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>%  # Add OpenStreetMap tiles
        setView(lng = 90, lat = 30, zoom = 3)%>%
        addPolygons(
          data = filtered_data,  
          fillColor = ~pal(Amount),
          fillOpacity = 0.7,
          color = "grey", 
          weight = 1,
          highlight = highlightOptions(weight = 2, color = "white", fillOpacity = 0.7),
          popup = ~Popup,
          label = ~paste(country) 
        ) %>%
        addLegend(position = "bottomleft", 
                  pal = pal,
                  values = filtered_data$Amount,
                  na.label = "No data",
                  opacity = 0.7
                  )
                  
      })
}

#############
# RUN SHINY #
#############

shinyApp(ui, server)