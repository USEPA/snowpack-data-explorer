# Working App - DO NOT EDIT ####

# Set base parameters #### --- --- --- --- --- --- --- --- --- --- --- ---

# Import libraries
library(rgdal)
# library(Cairo)
library(ggh4x)
library(shiny)
library(bslib)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(data.table)
library(plotly)
library(ggthemes)
library(ggnewscale)
library(ggrepel)


# Seaglass color palet
sg_teal_glow <- "#a1d894"
sg_teal_glow2 <- "#a0e49a"
sg_cobalt_glow <- "#68a3e5"
sg_cobalt_glow2 <- "#59a3f2"
sg_cobalt_dark <- "#1953bd"


# Get huc4s
huc4s_simple <- readOGR("huc4s.shp",
                        layer = "huc4s",
                        GDAL1_integer64_policy = TRUE)

labels_huc4 <- huc4s_simple$name

# 
# colors_hist   <- scale_fill_manual(values = c("darkgrey", "darkgrey", sg_cobalt_glow2, sg_teal_glow2))
# colors_line_1 <- scale_color_manual(values = c("darkgrey", "darkgrey", "#f0f0f0", "#f0f0f0"))
# colors_line_2 <- scale_color_manual(values = c("darkgrey", "darkgrey", sg_cobalt_glow2, sg_teal_glow2))

# Technical Details 
technical_Details <- paste0("<p>The Snowpack Data Explorer tool provides historical (1982 - 2005) and projected (2070 - 2099) snowpack and precipitation ",
                            "conditions for HUC4 watersheds in the western United States. HUC4 daily values for precipitation and snowpack were ",
                            "calculated by averaging the daily HUC8 values. HUC8 values were calculated using five models from the Bureau of Reclamation ",
                            "(BOR) LOCA Coupled Model Intercomparison Project Phase 5 (CMIP5) dataset, all using Representative Concentration Pathway (RCP) 8.5. ",
                            "The five models were:</p><p style = \"text-indent: 40px\"><ul><li>National Center for Atmospheric Research (CCSM4)</li><p><p style = \"text-indent: 40px\"><li>NASA Goddard Institute for Space Studies (GISS-E2-R)</li></p><p style = \"text-indent: 40px\">",
                            "<li>Canadian Centre for Climate Modeling and Analysis (CanESM2)</li></p><p style = \"text-indent: 40px\"><li>Met Office Hadley Centre (HadGEM2-ES)</li></p><p style = \"text-indent: 40px\">",
                            "<li>Atmosphere and Ocean Research Institute, National Institute for Environmental Studies, and Japan Agency for Marine-Earth Science and Technology (MIROC5)</li></ul></p><p>",
                            "Model selection rationale described in \"Multi-Model Framework for Quantitative Sectoral Impacts Analysis: A Technical Report for the Fourth National Climate ",
                            "Assessment. U.S. Environmental Protection Agency, EPA 430-R-17-001.\" More about the CMIP5 projections can be found ", 
                            "<a href=\"https://gdo-dcp.ucllnl.org/downscaled_cmip_projections/dcpInterface.html\">here</a>.</p><p>",
                            "Snowpack plots display the 90th and 10th percentiles of daily snowpack values,for both the baseline (historical) and future (projected) ",
                            "time periods. The daily 90th and 10th percentile values were calculated using all model runs for each date, ",
                            "corresponding to 120 historical snowpack values (5 models * 24 years) and 150 projected snowpack values (5 models * 30 years) for every date of the year.</p><p>",
                            "Precipitation plots display the distribution of annual precipitation totals within three categories: drier years, normal years, and wetter years. ",
                            "As with snowpack data, there are 120 historical annual precipitation values (5 models * 24 years) and 150 projected annual precipitation values (5 models * 30 years). ",
                            "Normal years are those with total precipitation between the 25th and 75th percentiles of historical annual precipitation totals.</p><p>",
                            "To view the EPA GIT Hub, visit <a href=\"https://github.com/USEPA/snowpack-data-explorer\">here</a>.</p>")


sg_teal_glow2 <- "#a0e49a"
colors_fill <- c("grey", "#1953bd")


# Define functions ####
leaflet_map <- function() {
  
  # Set fill color
  fill_color <- "#28A088" # Match plotly higlight color
    
    # Set label options
    label_options <- labelOptions(direction = "top",
                                  style = list(
                                      "color" = "grey",
                                      "font-size" = "15px",
                                      "border-color" = "rgba(0,0,0,0.5)"
                                  ))
    
    map <-
      
      # Set basic map parameters --- --- --- ---
        leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
      
        # Set initial view
        setView(lng = -114,
                lat = 43,
                zoom = 4) %>%
        
        # Set map boundaries
        setMaxBounds(
            lng1 = -140,
            lng2 = -85,
            lat2 = 55,
            lat1 = 20
        ) %>%
        
        # Create map and add max and min zoom
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(minZoom = 4,
                                                       maxZoom = 5)) %>%
        
        # Set z index to ensure selected polygons are on top
        addMapPane("base_map", zIndex = 420) %>%
        addMapPane("selected", zIndex = 430) %>%
      
      # Add polygons to map --- --- --- ---
        
        # Add HUC4 polygons
        addPolygons(
            data = huc4s_simple,
            color = "#A9ADBB",
            weight = 1,
            smoothFactor = 0.5,
            opacity = 1.0,
            fillOpacity = 0.4,
            highlightOptions = highlightOptions(
                color = "white",
                fillColor = fill_color, 
                weight = 2,
                bringToFront = TRUE
            ),
            group = "huc4",
            layerId = huc4s_simple$name,
            label = ~ paste(name, "Watershed"),
            
            labelOptions = label_options,
            
            options = pathOptions(pane = "base_map")
            
        ) %>%
        
        # Add HUC4 polygons for selection
        addPolygons(
            data = huc4s_simple,
            color = "white",
            fillColor = fill_color,
            weight = 2,
            smoothFactor = 0.5,
            opacity = 1.0,
            fillOpacity = 0.6,
            layerId = ~ OBJECTID,
            group = ~ name,
            label = ~ paste(name, "Watershed"),
            labelOptions = label_options,
            options = pathOptions(pane = "selected")
        ) %>%
        
        hideGroup(group = huc4s_simple$name)
    
    return(map)
}

# Define plots
snowpack_by_month <- function(.data, percentile_max) {
  
  
  baseline_max <- percentile_max %>% filter(period=="Baseline")
  future_max <- percentile_max %>% filter(period=="Future")
  
  # This is used to try to set how far about the graph sets the labels from each other
  if(future_max$maxp90>1) {
    future_y_nudge = 1
    baseline_y_nudge = -0.25
  } else if (future_max$maxp90<1 & future_max$maxp90>=0.5){
    future_y_nudge = -0.3
    baseline_y_nudge = 1
  } else {
    future_y_nudge = -.1
    baseline_y_nudge = 1
  }

  # Get snowpack data for plot
  snowpack <- .data %>%
    
    # Get data for chart
    filter(type == "snowpack")
  
  # Create ggplot object
  plot <- snowpack %>%
    
    # Make plot
    ggplot(aes(x = month_day, fill = period, color = period, text = "")) +
    
    # One line for max snowpack
    geom_line(aes(y = pct90), size = 1) +
    # Another line for min snowpack
    geom_line(aes(y = pct10), size = 1) +
    
    
    # stat_difference() from ggh4x package applies the conditional fill
    stat_difference(aes(ymin = pct10,
                        ymax = pct90),
                    alpha = 0.6) +
    
    scale_fill_manual("Period", values = colors_fill) +
    scale_color_manual("Period", values = colors_fill) +
    
    
    # Adds a point corresponding to the maximum baseline value
    geom_point(data=baseline_max,
               aes(x=date90, y = maxp90, fill=period, color = period),
               size = 4) +

    # Adding static labels
    geom_label_repel(data=baseline_max,
                     aes(x=date90, y = maxp90,
                         label = paste("Max ",period, " Peak",
                                       "\nDate: ", format(date90, "%B %d"), sep=""),
                         size=20),
                     nudge_x = 150,
                     nudge_y = baseline_y_nudge,
                     segment.size=.6,
                     fill = alpha(c("grey"),0.4),
                     segment.color = "black",
                     # fontface="bold",
                     color="black",
                     show.legend = FALSE,
                     label.padding = 0.5,
                     force=10,
                     label.size = 0) +

    # Adds a point corresponding to the maximum future value
    geom_point(data=future_max,
               aes(x=date90, y = maxp90, fill=period, color = period),
               size = 4) +

    # Adding static lablels
    geom_label_repel(data=future_max,
                     aes(x=date90, y = maxp90,
                         label = paste0("Max ",period, " Peak",
                                       "\nDate: ", format(date90, "%B %d")
                                       # ,"\n% of Max Baseline Peak: ",
                                       # round(maxp90*100, 0), "%",sep=""
                                       ),
                         size=20),
                     nudge_x = 200,
                     nudge_y = future_y_nudge,
                     fill=alpha(c("#1953bd"),0.6),
                     # alpha=.7,
                     segment.color="black",
                     color="white",
                     # fontface="bold",
                     show.legend = FALSE,
                     label.padding = 0.5) +
    
    # Adds a point corresponding to 10th percentile maximums for future and baseline
    geom_point(data=percentile_max,
               aes(x=date10, y = maxp10, fill=period, color = period),
               size = 4) +
    
    # Set theme
    theme_minimal() +
    
    theme(panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 14, family = "sans"),
          # axis.title = element_text(size = 15, family = "sans"),
          axis.title = element_blank(),
          legend.text = element_text(size=14, family = "sans"),
          legend.title = element_blank(),
          legend.position = "top",
          legend.justification = "left",
          legend.direction = "horizontal",
          plot.title = element_text(size=18, family = "sans")) + # Remove vertical major grid lines
    
    # Scale axes
    
    # Set x axis as month name
    scale_x_date(date_labels = "%B") +
    # 
    # xlab("Date") +
    # ylab("Percent") +
    ggtitle("Maximum baseline snowpack") +
    
    # Set y axis as percent; max y axis = max pct10 (baseline or future, whichever is higher)
    scale_y_continuous(labels = scales::percent,
                       limits = c(-0.01, max(snowpack$pct90)))  # Axis set to -0.01 to avoid clipping chart
  
  
  return(plot)
  
}

# Build Precip Charts
precip_chart <- function(.data) {
  
  .data[.data=="Dry"] = "Drier"
  .data[.data=="Wet"] = "Wetter"
  
  plot <-  .data %>%
    
    # Make plot -- the text element is what will show up for the hovering
    ggplot(aes(x = `Type of Year`, y = `Percent of Years`, fill = Period, color = Period)) +
    geom_bar(stat="identity", position = "dodge2", alpha=0.6) +
    
    # Add Static labels to the precip plot
    geom_label(aes(label = paste0(" ",round(`Percent of Years`,0), "%")),
               size=5,
               label.size=0,
               color = "white",
               position=position_dodge(.91),
               vjust=1.3,
               fontface = "bold",
               show.legend = FALSE,
               label.padding = unit(0.25, "lines")) +
    
    # Set colors in charts
    scale_fill_manual(values = c("#808080", "#1953bd")) +
    scale_color_manual(values = c("#808080", "#1953bd")) +
    
    # Labeling the x and y axis
    scale_y_continuous(labels = scales::percent_format(accuracy=1, scale = 1)) +
    scale_x_discrete(labels = c("Drier", "Normal", "Wetter")) +
    
    # Setting different elements of plot
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 14, family = "sans"),
          # axis.title = element_text(size = 15, family = "sans"),
          axis.title = element_blank(),
          legend.text = element_text(size=14, family = "sans"),
          legend.title = element_blank(),
          legend.position = "top",
          legend.justification = "left",
          legend.direction = "horizontal",
          plot.title = element_text(size=18, family = "sans")) +
    
    ggtitle("Distribution of Drier/Wetter Years")
  
  
  
  return(plot)
}

# Import data ####
data <- as.data.frame(fread("huc4_for_RShiny_2022-04-21.csv", header = TRUE) %>%  #"huc4_daily_averages_2022-03-22.csv"
  mutate(month_day = as.Date(month_day)))

yearPrecipClassification <- as.data.frame(fread("year_precip_class_2022-04-21.csv", header = TRUE)) %>% 
                                            mutate(`Percent of Years` = round(percent, 1))
                                            
names(yearPrecipClassification)[names(yearPrecipClassification)=='wet_dry_normal'] <- 'Type of Year'

percentileMax <- as.data.frame(fread("percentile_maximums_2022-04-21.csv", header=TRUE))

# Define UI ####
ui <- fluidPage(tagList(
  navbarPage(
    # Set name and theme
    "Snowpack Data Explorer",
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    
    
    # Main Page
    tabPanel("Regional Snowpack",
             
             
             # Row for page
             fluidRow(
               
               # Page title  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
               fluidRow(
                 
                 column(12,
                        
                        # WS Name and HUC
                        h5(htmlOutput("watershed_name_huc")),
                        
                        # Break
                        tags$hr(),
                        
                        
                 )),
               
               # Leaflet Map --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
               column(5,
                      
                      # Set map height
                      tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
                      leafletOutput("map")),
               
               
               # Snowpack and Precipitation Tab Panel --- --- --- --- --- --- --- --- --- --- --- --- --- ---
               column(7,
                      
                      fluidRow(
                        column(12,
                        
                        tabBox(
                          
                          width = NULL,
                          
                          # Make snowpack tab pannel
                          tabPanel("Snowpack", 
                                   
                                   
                                   # Add narrative text for watershed (snowpack)
                                   p(htmlOutput("narrative")),
                                   
                                   # Set plot height, show plot
                                   tags$style(type = "text/css", "#chart_snowpack {height: calc(100vh - 380px) !important;}"),
                                   
                                   # Generate chart
                                   plotOutput("chart_snowpack")),
                          
                          
                          tabPanel("Precipitation", 
                              
                                   p(htmlOutput("precip_narrative")),
                                   # 
                                   tags$style(type = "text/css", "#chart_precip {height: calc(100vh - 420px) !important;}"),

                                   plotOutput("chart_precip")
                                   
                                   )
                          
                          # This will alos need the set plot height code, change plot name to whatever
                          # the new plot is called (e.g. "#chart_precipitation {height: ...})
                        )
                        
                      )))
             )),
    
    # Technical Details  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---  --- --- --- ---
    tabPanel("Technical Details", 
             
             # Page header
             h4("Snowpack Technical Details"),
             
             # Break
             tags$hr(),
             
             p(HTML(technical_Details)))
  )
))


# Define server ####
server <- function(input, output) {
  
  # Create leaflet map
  output$map <- renderLeaflet({
    leaflet_map()
  })
  
  #define leaflet proxy for second regional level map
  proxy <- leafletProxy("map")
  
  
  #create empty vector to hold all click ids
  selected <- reactiveValues(groups = vector())
  
  # Set initial messages
  output$watershed_name_huc <- renderText(paste0("The Snowpack Data Explorer provides snowpack and precipitation data for HUC4 watersheds",
                                                 " in the Western U.S. Click on your watershed of interest to view plots and compare the ranges of ",
                                                 "snowpack data for baseline (historical data from 1982-2005) and future (model projections for 2070-2099) ",
                                                 "time periods. Use the tabs to toggle between views. Use the tabs to toggle between views. View the \"Technical Details\" for ",
                                                 "more information on the data sources."))
  output$narrative <- renderText("")
  output$precip_narrative <- renderText("")
  
  
  # Get click for map
  observeEvent(input$map_shape_click, {
    
    # Get current selection --- --- --- --- --- --- --- ---
    
    # Check if click type is integer. If yes, do nothing.
    if (typeof(input$map_shape_click$id) != "integer") {
      
      # If click is integer, save to list and generate output

      # For the first click
      if (length(selected$groups) < 1) {
        # Add click to vector
        selected$groups <-
          c(selected$groups, input$map_shape_click$id)
        
        # Update map with the first click
        proxy %>% showGroup(selected$groups)
        
        # For all other clicks
      } else {
        # Shorten vector to most recent entry; add new click
        selected$groups <- tail(selected$groups, 1)
        selected$groups <-
          c(selected$groups, input$map_shape_click$id)
        
        # Get last and second to last items
        last <- selected$groups[length(selected$groups)]
        second_last <-
          selected$groups[length(selected$groups) - 1]
        
        # Update map by showing the last group and hiding the second to last group
        proxy %>%
          showGroup(last) %>%
          hideGroup(second_last)
        
      }
      
      # Get watershed information --- --- --- --- --- --- --- ---
      
      # Current click = HUC4 name    
      huc_name <- input$map_shape_click$id                           
      huc_code <- huc4s_simple@data %>%  # Get HUC as vector
        subset(name == huc_name) %>%
        pull(huc4)
      
      # Filter data to current selection
      
      data_current <- data %>% subset(HUC4 == huc_code)
      
      data_percentile <- percentileMax %>% subset(HUC4 == huc_code)
      
      precip_data <- yearPrecipClassification %>% subset(HUC4 == huc_code)
      
      # Make the chart with renderPlotly()
      output$chart_snowpack <- renderPlot({
        snowpack_by_month(data_current, data_percentile)
      })
      
      output$chart_precip <- renderPlot({
        precip_chart(precip_data)
      })
      
      # Print messages to app --- --- --- --- --- --- --- ---
      
      # Generate messages
      dateDif <- data_percentile$date90[data_percentile$period=="Future"] - data_percentile$date90[data_percentile$period=="Baseline"]
      beforeAfter<-ifelse(dateDif>0,ifelse(dateDif==1," day later", " days later"), ifelse(dateDif==-1, " day earlier", " days earlier"))
      
      normalMoreLess <- ifelse(precip_data$percent[precip_data$Period=="Future" & precip_data$`Type of Year`=="Normal"] > 50, "an increase", 
                         ifelse(precip_data$percent[precip_data$Period=="Future" & precip_data$`Type of Year`=="Normal"] == 50, "no change", "a decrease"))
      
      wetterMoreLess <- ifelse(precip_data$percent[precip_data$Period=="Future" & precip_data$`Type of Year`=="Wet"] > 25, "an increase", 
                               ifelse(precip_data$percent[precip_data$Period=="Future" & precip_data$`Type of Year`=="Wet"] == 25, "no change", "a decrease"))
      
      drierMoreLess <- ifelse(precip_data$percent[precip_data$Period=="Future" & precip_data$`Type of Year`=="Dry"] > 25, "an increase", 
                               ifelse(precip_data$percent[precip_data$Period=="Future" & precip_data$`Type of Year`=="Dry"] == 25, "no change", "a decrease"))
      
      watershed_name_huc <- paste(huc_name, " Watershed |", "HUC4 Code: ", huc_code)
      watershed_huc <- paste("HUC4 Code: ", huc_code)
      narrative <- paste0("Snowpack is displayed as a seasonal time series of the ratio of snow-water equivalent (SWE) to the baseline peak SWE. ",
                         "The shaded ranges depict the range from 10th to 90th percentile for each time period in the ",
                         huc_name, " watershed.  Models project that the future peak SWE may be ",
                         round(data_percentile$maxp90[data_percentile$period=="Future"]*100,0), "% of the baseline peak and could occur ",
                         abs(dateDif), beforeAfter, ".")
      

      precip_narrative <- paste0("Precipitation is displayed as the fraction of years in each time period that are wetter or drier than the \"normal\" years in the baseline data. ",
                                 "\"Normal\" years are those years in the baseline with annual total precipitation between the 25th and 75th percentile of all baseline years. ",
                                 "Shifts between the distribution of years in the future time period, based on projected total annual precipitation, describe how often drier and " ,
                                 "wetter years could be expected by end of century in the ", huc_name, " watershed. Models project that in the future period, there will be ", 
                                 drierMoreLess, " in drier years, ", normalMoreLess, " in normal years, and ", wetterMoreLess, " in wetter years.")
      
      # Message for app
      output$watershed_name_huc <- renderText(watershed_name_huc)
      output$narrative <- renderText(narrative)
      output$precip_narrative <- renderText(precip_narrative)
      
    }
    
  })
  
}

# Run the application ####
shinyApp(ui = ui, server = server)

