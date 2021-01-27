library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(maps)

## Read in and wrangle Johns Hopkins COVID-19 data
# Read in Johns Hopkins global data for confirmed cases of COVID-19
jhu = read_csv("time_series_covid19_confirmed_global.csv")

# Read in Johns Hopkins UID lookup table
# Source: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv
uid_lookup_table = read_csv("UID_ISO_FIPS_LookUp_Table.csv")

# wrangling code
# converting to long format
jhu_dat <- jhu %>% gather(date, cases, -Lat, -Long, -`Province/State`, -`Country/Region`)
# adjusting date, renaming variables for ease
jhu_dat <- jhu_dat %>% mutate(date = mdy(date)) %>% rename(Country_Region = `Country/Region`) %>% rename(Province_State = `Province/State` )
# Calculating total cases/day/country
sum_jhu <- jhu_dat %>% group_by(Country_Region, date) %>%
  summarize(cases=sum(cases)) %>% ungroup()
# Keeping only the date, cases, and country variables
sum_jhu <- sum_jhu %>% 
  select(Country_Region, date, cases)
# dates appear to be ordered, so I group by country, then calculate the lag (new cases per day)
lag_dat <- sum_jhu %>% group_by(Country_Region) %>% mutate(new_cases = cases - lag(cases))
head(lag_dat)
# use the new cases per day variable to calculate a rolling average over 7 days
rollavg_dat <- lag_dat %>% group_by(Country_Region) %>% mutate(rollavg = rollmean(new_cases, k = 7, fill = NA))
# creating new data frame with only required variables
uid_lookup <- uid_lookup_table %>% select(Country_Region, Population) %>% ungroup()

uid_lookup <- uid_lookup %>% group_by(Country_Region) 
uid_sum <- uid_lookup %>%
  summarize(sum_pop = sum(Population, na.rm = TRUE)) %>% ungroup()
uid_sum %>% filter(Country_Region=="US")

# merging the two data frames 
merge_dat <- left_join(uid_sum, rollavg_dat)
head(merge_dat)

full_dat <- merge_dat %>% mutate(avg_pop = (rollavg/sum_pop)*1000000)
# Pull out world map data frame
world_map = map_data("world")

world_map <- world_map %>%  rename(Country_Region = region)

merge_dat$Country_Region <- recode(merge_dat$Country_Region, "Antigua and Barbuda" = "Antigua", 
                                   "Burma" = "Myanmar", 
                                   "Cabo Verde" = "Cape Verde", 
                                   "Congo (Kinshasa)" = 
                                     "Democratic Republic of the Congo",  
                                   "Congo (Brazzaville)" = 
                                     "Republic of Congo",  
                                   "Cote d'Ivoire" = "Ivory Coast", 
                                   "Czechia" = "Czech Republic", 
                                   "Eswatini" = "Swaziland", 
                                   "Holy See" = "Vatican", 
                                   "Korea, South" = "South Korea", 
                                   "North Macedonia" = "Macedonia", 
                                   "Saint Kitts and Nevis" = "Saint Kitts", 
                                   "Saint Vincent and the Grenadines" = 
                                     "Saint Vincent", 
                                   "Taiwan*" = "Taiwan", 
                                   "Trinidad and Tobago" = "Trinidad", 
                                   "United Kingdom" = "UK", 
                                   "US" = "USA")

full_dat$Country_Region <- recode(full_dat$Country_Region, "Antigua and Barbuda" = "Antigua", 
                                  "Burma" = "Myanmar", 
                                  "Cabo Verde" = "Cape Verde", 
                                  "Congo (Kinshasa)" = 
                                    "Democratic Republic of the Congo",  
                                  "Congo (Brazzaville)" = 
                                    "Republic of Congo",  
                                  "Cote d'Ivoire" = "Ivory Coast", 
                                  "Czechia" = "Czech Republic", 
                                  "Eswatini" = "Swaziland", 
                                  "Holy See" = "Vatican", 
                                  "Korea, South" = "South Korea", 
                                  "North Macedonia" = "Macedonia", 
                                  "Saint Kitts and Nevis" = "Saint Kitts", 
                                  "Saint Vincent and the Grenadines" = 
                                    "Saint Vincent", 
                                  "Taiwan*" = "Taiwan", 
                                  "Trinidad and Tobago" = "Trinidad", 
                                  "United Kingdom" = "UK", 
                                  "US" = "USA")



table_df <- full_dat %>% rename("Rolling Average of Cases per 1,000,000" = avg_pop, "Rolling Average of Cases" = rollavg)


# Define UI 
ui <- fluidPage(
  headerPanel("Global Covid-19 Cases"),
  fluidRow(
    column(4, 
           selectizeInput("m", "Choose up to 6 Countries", multiple = TRUE, choices = merge_dat$Country_Region, options = list(maxOptions = 6))
    ),
    column(4, 
           radioButtons(inputId = "rate", label = "Select a 7-day rolling average rate",
                        choices = c("Total Covid-19 cases", "Covid-19 cases per million"))
    ),
    column(4, 
           selectizeInput("date", "Choose a date in yyyy-mm-dd format", multiple = TRUE, choices = merge_dat$date, options = list(maxOptions = 1), selected = "2020-02-01")
    )
  ),
  fluidRow(
    column(4, tableOutput("table")),
    column(8, plotOutput("plot_map"))
  ),
  mainPanel(
    # Plot
    plotOutput("plot")
  )
)

# Define server 
server <- function(input, output) { 
  
  output$table <- renderTable(table_df %>% filter(date == input$date)
                              %>% filter(Country_Region %in% input$m)
                              %>% select(Country_Region, "Rolling Average of Cases per 1,000,000", "Rolling Average of Cases"))
  
  output$plot = renderPlot({
    if (input$rate == "Total Covid-19 cases"){
      full_dat %>% filter(Country_Region %in% input$m) %>%
        ggplot(aes(date, rollavg, color=Country_Region)) + 
        geom_line() +
        geom_vline(xintercept = as.Date(input$date)) +
        xlab("Time") + 
        ylab("Average of Cases")
      
    } else if (input$rate == "Covid-19 cases per million"){
      
      full_dat %>% filter(Country_Region %in% input$m) %>%
        ggplot(aes(date, avg_pop, color=Country_Region)) + 
        geom_line() +
        geom_vline(xintercept = as.Date(input$date)) +
        xlab("Time") + 
        ylab("7-Day Rolling Average of Cases")
      }
    }
  )
  
  output$plot_map = renderPlot({
    temp_dat <- merge_dat %>% filter(date == input$date)
    map_df <- full_join(temp_dat, world_map)
    temp_dat2 <- full_dat %>% filter(date == input$date)
    map_df2 <- full_join(world_map, temp_dat2)
    
    if (input$rate == "Total Covid-19 cases"){
      ggplot(map_df2, aes(x = long, y = lat, group = group)) +
        geom_polygon(data = map_df2, aes(fill=rollavg), color = "white") + 
        theme(panel.grid.major = element_blank(), 
              panel.background = element_blank(),
              axis.title = element_blank(), 
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        scale_fill_viridis_c() +
        scale_fill_gradientn(trans="log10", limits=c(NA,60000), colors = terrain.colors(10))
    }
        
   else if (input$rate == "Covid-19 cases per million"){
      print(map_df2$avg_pop)
      ggplot(map_df2, aes(x = long, y = lat, group = group)) +
        geom_polygon(data = map_df2, aes(fill = avg_pop), color = "white") + 
        theme(panel.grid.major = element_blank(), 
              panel.background = element_blank(),
              axis.title = element_blank(), 
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
      scale_fill_gradientn(colors = terrain.colors(10), trans="log10", limits=c(NA,1000))
      }
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


