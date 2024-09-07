
library(leaflet)
library(shiny)
library(bslib)
library(readr)
library(stringr)
library(sf)
library(jsonlite)
library(dplyr)
library(leaflet.extras)

some_upper_geojson_data <- st_as_sf(data.frame(
  name = "Upper District",
  geometry = st_sfc(st_polygon(list(matrix(c(
    -84.5, 44.5,
    -84.5, 44.6,
    -84.4, 44.6,
    -84.4, 44.5,
    -84.5, 44.5
  ), byrow = TRUE, ncol = 2))))
))

some_lower_geojson_data <- st_as_sf(data.frame(
  name = "Lower District",
  geometry = st_sfc(st_polygon(list(matrix(c(
    -84.4, 44.4,
    -84.4, 44.5,
    -84.3, 44.5,
    -84.3, 44.4,
    -84.4, 44.4
  ), byrow = TRUE, ncol = 2))))
))

build_district_layers <- function(upper) {
  if (upper == 1) {
    return(some_upper_geojson_data)
  } else {
    return(some_lower_geojson_data)
  }
}

build_marker_layer <- function(LARA_C) {
  if (LARA_C == 0) {
    return(data.frame(lat = runif(10, 42, 45), lng = runif(10, -85, -83)))
  } else {
    return(data.frame(lat = runif(10, 42, 45), lng = runif(10, -85, -83)))
  }
}

ui <- fluidPage(
  titlePanel("Manufactured Housing Communities in Michigan"),
  h4("Project by ", 
     a("INFORMS", href = "https://informs.engin.umich.edu/", target = "_blank"), 
     " and ", 
     a("CTAC", href = "https://ginsberg.umich.edu/ctac", target = "_blank"),
     " at the University of Michigan"),
  sidebarLayout(
    sidebarPanel(
      h3("Background"),
      p("This app is a visualization tool designed to highlight the distribution of manufactured housing communities across Michigan."),
      p("Michigan Department of Licensing and Regulatory Affairs (LARA) data was obtained by a FOIA request in January 2024. MHVillage data was scraped in December 2023."),
      p("For more information, please visit MHAction.org"),
      p("\n"),
      h4("Definitions"),
      p("- MHC: Manufactured Housing Community"),
      p("- LARA: Michigan Department of Licensing and Regulatory Affairs. Michigan MHC's are required to register with LARA."),
      p("- MHVillage: Online marketplace for buying and selling manufactured homes. Data may be incomplete."),
      checkboxGroupInput("layerlist", "Choose layers:", 
                         choices = c(
                           "Legislative districts (Michigan State Senate)",
                           "Legislative districts (Michigan State House of Representatives)",
                           "Marker MHVillage",
                           "Marker LARA",
                           "Circle MHVillage (location only)",
                           "Circle LARA (location only)"
                         ))
    ),
    mainPanel(
      leafletOutput("leafletMap")
      # tableOutput("mhvillageTable"),
      # tableOutput("laraTable")
    )
  )
)

server <- function(input, output, session) {
  # Load the CSV file into a data frame
  mhvillage_df <- read_csv("MHVillageDec7_Legislative1.csv")
  mhvillage_df$Sites <- as.integer(mhvillage_df$Sites)
  lara_df <- read_csv("LARA_with_coord_and_legislativedistrict1.csv")
  lara_df$County <- str_to_title(lara_df$County)
  
  output$leafletMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -84.3615564, lat = 44.44343, zoom = 6) %>%
      addMarkers(lng = -83.735, lat = 42.280, popup = "Ann Arbor")
  })
  
  observe({
    the_map <- leafletProxy("leafletMap") %>% clearControls()  # Clear previous layers
    
    if ("Legislative districts (Michigan State Senate)" %in% input$layerlist) {
      upper_layers <- build_district_layers(1) 
      the_map %>% clearGroup("Upper") %>% addPolygons(data = upper_layers, group = "Upper")
    }
    
    if ("Legislative districts (Michigan State House of Representatives)" %in% input$layerlist) {
      lower_layers <- build_district_layers(0)
      the_map %>% clearGroup("Lower") %>% addPolygons(data = lower_layers, group = "Lower")
    }
    
    if ("Marker MHVillage" %in% input$layerlist) {
      mklist_mh <- build_marker_layer(0)
      the_map %>% clearGroup("MK_MH") %>% addMarkers(data = mklist_mh, ~lng, ~lat, group = "MK_MH")
    }
    
    if ("Marker LARA" %in% input$layerlist) {
      mklist_lara <- build_marker_layer(1)
      the_map %>% clearGroup("MK_LARA") %>% addMarkers(data = mklist_lara, ~lng, ~lat, group = "MK_LARA")
    }
    
    if ("Circle MHVillage (location only)" %in% input$layerlist) {
      circlelist_mh <- build_marker_layer(0) # Use the same function for example
      the_map %>% clearGroup("CIRC_MH") %>% 
        addCircles(data = circlelist_mh, ~lng, ~lat, radius = 500, group = "CIRC_MH")
    }
    
    if ("Circle LARA (location only)" %in% input$layerlist) {
      circlelist_lara <- build_marker_layer(1)
      the_map %>% clearGroup("CIRC_LARA") %>% 
        addCircles(data = circlelist_lara, ~lng, ~lat, radius = 500, group = "CIRC_LARA")
    }
    
    # Add layer control
    the_map %>% addLayersControl(
      overlayGroups = c("Upper", "Lower", "MK_MH", "MK_LARA", "CIRC_MH", "CIRC_LARA"),
      options = layersControlOptions(collapsed = FALSE)
    )
  })
  
  # Render mhvillage table in the UI
  output$mhvillageTable <- renderTable({
    mhvillage_df
  })
  
  # Render lara table in the UI
  output$laraTable <- renderTable({
    lara_df
  })
}

shinyApp(ui = ui, server = server)