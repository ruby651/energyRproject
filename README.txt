#Overview of the design
The purpose of this visualisation is to raise the awareness among people living in the Asia about energy consumption trends in the region. This visualisation provides an interactive platform for public to analyse and discover how the different energy resources were consumed across Asia recently. Based on the insights provided, the platform encourages people to discuss about the challenges and opportunities for addressing future energy demand and the impact on environmental sustainability.
#Prerequisites
1. Install R (https://cran.r-project.org/) and RStudio (https://posit.co/download/rstudio-desktop/#download) (if not already installed):
2. Before running the application, ensure you have the following packages installed:
* shiny
* leaflet
* RColorBrewer
* rnaturalearth
* sf
* ggplot2
* ggiraph
* tidyr
* dplyr
       To install the required packages, run the following R code:
       install.packages(c("shiny", "leaflet", "RColorBrewer", "rnaturalearth", "sf", "ggplot2", "ggiraph", "tidyr", "dplyr"))
3. The application uses a dataset (owid-energy-data.csv) which contains energy consumption data from various countries. Ensure that this CSV file is located in your working directory.
#User Interface and Shiny server section
There are two main tabs: 
The first tab provides the overview of energy consumption across Asia for the year. It includes two graphs: “The Primary Energy Consumption by Source in Asia” and “The Relationship Between Population and the Energy Consumption”. Additionally, there are two controls tab – selectInput for energy source selection and slicerInput for year range selection.
The second tab focuses on the energy consumption per person on different countries. It includes one graph and one map. The map shows the energy consumption per person in different countries by specific energy source for a year. The stacked bar chart shows top 10 countries in term of energy consumptions per person in different resources in top 10 country. This tab also has two controls: slicer Input for year selection and the other is selectInput for the energy source selection.
- The Primary Energy Consumption by Source in Asia graph:
Description of graph: The graph shows the trend of energy consumption over the years, emphasising the amount of energy consumptions from difference source.
Visual variable: The energy source is a nominal variable; thus, colour hue is chosen to visualise different categories. To compare the difference in energy usage from difference sources and the change in energy usage over the years, the area chart was used for easier comparison for two dimensions. 
Gestalt Laws: Similarity and Continuity law has been applied in this chart. Data from same energy source is represented by the same colour (Similarity). The amount of energy consumption from different energy sources in specific period also ranks in ordered. (Continuity)
- The relationship between population and the Amount of Energy Consumption:
Description of graph: The graph is about the relationship between the population of Asia and the amount of energy usage in general as well as in different energy sources (use energy selection button)
Visual variable: To show the relationship, a scatterplot (position in visual variable) has been chosen for this graph because both data variables (population and energy consumption) are numerical. The colour red is used to indicate that the energy usage has negative impact on environment.
- The energy consumption per person map:
Description of map: The map illustrates the amount of energy consumption per person in different countries.
Type of map: The choropleth map has been chosen for this visualisation as it effectively shows how much of energy consumed in different countries. In addition, the data variable represents the average energy per person by country which has been normalised. Therefore, it is easier to compare energy consumption between countries
Map visual variable: Brightness is used in the map to show the amount of energy usage per person with darker shades indicating higher energy usage. Red colour is used to indicate that the energy usage has negative impact on environment. Therefore, the sequential red colour is applied in this map. 
The colour range is divided using equal interval method which makes it easier for target audience – public users – to understand. Additionally, the range of colour is divided into four bins which allow for clearer difference between them. White colour in colour range indicates the data is Null. Therefore, a ‘0’ value is not used white.
- Top 10 Countries in Primary Energy Consumption Per Person:
Description of graph: The stacked bar graph displays the top 10 countries with the highest energy consumption per person, categorizing the total energy amount by different energy sources.
Visual variable	The energy source is nominal variable; thus, colour hue is chosen to visualise different categories. The bar chart (shape) is used to demonstrate the amount from difference energy sources and to compare energy usage between countries.
Gestalt Laws: Similarity and Continuity law has been used in this chart. The data from same energy source has same colour (Similarity). The amount of energy consumption from different energy sources during a specific period is ranked in ordered. The countries are also ranked in ordered based on the amount of energy they were used. (Continuity) 
Data Density: By showing top ten countries, this graph helps users easily identify the key players in energy consumption whose are major contributors to the negative impact on the environment. 

#Appendix.
Data source: https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-06-06/owid-energy.csv
The dataset has the period time from 1900 to 2023 in the World. I focused on visualising the data from 2002 and 2022 in Asia to highlight the current trends of energy consumption to identify the challenges and opportunities (Data Correspondence). I did not expand the period to 2024 because, dataset was published in the mid-2023, the data in that year is not fully complete (Data Integrity).
The main columns I use in this visualisation are 'country', 'year', 'population', 'biofuel_consumption', 'coal_consumption', 'gas_consumption', 'hydro_consumption', 'nuclear_consumption', 'other_renewable_consumption', 'solar_consumption', 'wind_consumption', 'oil_consumption', 'biofuel_cons_per_capita', 'coal_cons_per_capita', 'gas_energy_per_capita', 'hydro_energy_per_capita', 'nuclear_energy_per_capita', 'other_renewables_energy_per_capita', 'solar_energy_per_capita', 'wind_energy_per_capita', 'oil_energy_per_capita'

