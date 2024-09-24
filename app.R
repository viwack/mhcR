
library(leaflet)
library(shiny)
library(bslib)
library(readr)
library(stringr)
library(sf)
library(jsonlite)
library(dplyr)
library(leaflet.extras)
library(geojsonio)
library(ggplot2)
library(scales)
library(shinydashboard)
library(htmltools)
library(mapview)
library(tigris)
library(htmlwidgets)
library(terra)
library(mapdeck)

house <- st_read("Michigan_State_House_Districts_2021.json")
senate <- st_read("Michigan_State_Senate_Districts_2021.json")

house_districts <- read_sf("Michigan_State_House_Districts_2021.json")
senate_districts <- read_sf("Michigan_State_Senate_Districts_2021.json")

house_districts_j <- read_json("Michigan_State_House_Districts_2021.json")
senate_districts_j <- read_json("Michigan_State_Senate_Districts_2021.json")

mhvillage_df <- read_csv("MHVillageDec7_Legislative1.csv")
mhvillage_df$Sites <- as.integer(mhvillage_df$Sites)

lara_df <- read_csv("LARA_with_coord_and_legislativedistrict1.csv")
lara_df$County <- str_to_title(lara_df$County)

mhvillage_simple <- read_csv("MHVillage_simple.csv")
mhvillage_simple$Sites <- as.integer(mhvillage_simple$Sites)

lara_simple <- read_csv("LARA_simple.csv")
lara_simple$County <- str_to_title(lara_simple$County)

mi_counties <- counties(state = "MI")
mi_counties$NAME <- sort(mi_counties$NAME)

house_districts <- fromJSON("Michigan_State_House_Districts_2021.json")
senate_districts <- fromJSON("Michigan_State_Senate_Districts_2021.json")

build_infographics1 <- function(lara_df) {
  total_sites_by_county <- lara_df %>%
    filter(!is.na(`Total_#_Sites`)) %>%
    group_by(County) %>%
    summarise(Total_Sites = sum(`Total_#_Sites`, na.rm = TRUE)) %>%
    arrange(desc(Total_Sites))
  
  top_20 <- total_sites_by_county %>%
    slice_head(n = 20)  # Keep only top 20
  
  plot <- ggplot(top_20, aes(x = Total_Sites, y = reorder(County, Total_Sites))) +
    geom_bar(stat = "identity", fill = "cadetblue4", color = "cadetblue4") +
    geom_bar(stat = "identity", fill = "mediumpurple", color = "mediumpurple", alpha = 0.7) +
    labs(
      title = "Top 20 Counties by Number of MHCs (LARA)",
      x = "Total Number of Sites",
      y = "County"
    ) +
    theme_minimal() +
    scale_x_continuous(labels = scales::comma)
  
  return(plot)
}

build_infographics2 <- function(mhvillage_df) {
  # Calculate mean Average_rent by County
  total_sites_by_name <- mhvillage_df %>%
    group_by(County) %>%
    summarise(Average_rent = mean(Average_rent, na.rm = TRUE)) %>%
    filter(!is.na(Average_rent)) %>%
    arrange(Average_rent)
  
  # Create a clean DataFrame and count number of sites per county
  df_clean <- mhvillage_df %>%
    select(County, Average_rent) %>%
    filter(!is.na(Average_rent))
  
  county_counts <- df_clean %>%
    count(County)
  
  # Merge the DataFrames
  total_sites_by_name_count <- merge(total_sites_by_name, county_counts, by = "County")
  
  # Sort and select top 20 counties by count of sites
  total_sites_by_name_20 <- total_sites_by_name_count %>%
    arrange(desc(n)) %>%
    head(20) %>%
    arrange(desc(Average_rent))
  
  # Create the plot
  ax <- ggplot(total_sites_by_name_20, aes(x = Average_rent, y = reorder(County, Average_rent))) +
    geom_bar(stat = "identity", fill = "cadetblue4") +
    labs(x = "Average Rent ($)", y = "County", title = "Average rent by county (MHVillage)") +
    theme_minimal() +
    geom_text(aes(label = n), hjust = -0.2, color = "black", size = 4)
    geom_bar(stat = "identity", fill = "pink") +
    labs(x = "Average Rent ($)", y = "County", title = "Average rent by county (MHVillage)") +
    theme_minimal() +
    geom_text(aes(label = n), hjust = -0.1, color = "black", size = 4)
  
  # Print the plot
  print(ax)
}

build_marker_layer <- function(map, layer_name) {
  if (layer_name == "MHVillage Markers (includes site info)") {
    mhvillage_df$label <- paste(
      "Name: ", as.character(mhvillage_df$Name),
      br(),
      "Sites: ", as.character(mhvillage_df$Sites),
      br(),
      "Address: ", mhvillage_df$FullstreetAddress,
      br(),
      "House District: ", as.character(mhvillage_df$`House district`),
      br(),
      "Senate District: ", as.character(mhvillage_df$`Senate district`),
      br(),
      "Source: MHVillage"
    )
    # Add your specific code to build the Marker MHVillage layer
    map <- map %>% addMarkers(lng = as.numeric(mhvillage_df$longitude), 
                              lat = as.numeric(mhvillage_df$latitude),
                              group = "MHVillage Markers (includes site info)",
                              popup = mhvillage_df$label,
                              clusterOptions = markerClusterOptions())
  } else if (layer_name == "LARA Markers (includes site info)") {
    lara_df$label <- paste(
      "Name: ", ifelse(lara_df$DBA != "", lara_df$DBA, lara_df$`Owner / Community_Name`),
      br(),
      "Sites: ", as.character(lara_df$`Total_#_Sites`),
      br(),
      "Address: ", lara_df$Location_Address,
      br(),
      "House District: ", as.character(lara_df$`House district`),
      br(),
      "Senate District: ", as.character(lara_df$`Senate district`),
      br(),
      "Source: LARA"
    )
    # Add your specific code to build the Marker LARA layer
    map <- map %>% addMarkers(lng = as.numeric(lara_df$longitude), 
                              lat = as.numeric(lara_df$latitude), 
                              group = "LARA Markers (includes site info)",
                              popup = lara_df$label,
                              clusterOptions = markerClusterOptions())
  } else if (layer_name == "MHVillage Circles (location only)") {
    # Add your specific code to build the Circle MHVillage layer
    map <- map %>% addCircles(lng = as.numeric(mhvillage_df$longitude), 
                              lat = as.numeric(mhvillage_df$latitude),
                              color = "orange",
                              opacity = 0.9,
                              radius = 4,
                              group = "MHVillage Circles (location only)")
  } else if (layer_name == "LARA Circles (location only)") {
    # Add your specific code to build the Circle LARA layer
    map <- map %>% addCircles(lng = as.numeric(lara_df$longitude), 
                              lat = as.numeric(lara_df$latitude),
                              color = "blue",
                              opacity = 0.7,
                              group = "LARA Circles (location only)")
  }
  return(map)
}


ui <- navbarPage(
  imageOutput("mhaction_logo", inline = TRUE),
  # First tab with existing content
  
  # Additional tabs
  tabPanel(
    "Map Tool",
    fluidPage(
      titlePanel("Michigan Manufacturing Housing Communities Map Tool"),


ui <- navbarPage(
  "MHAction Manufactured Housing Communities",
  # First tab with existing content
  tabPanel(
    "About",
    page_fillable(
          titlePanel(
            tags$b("Manufactured Housing Communities in Michigan"),
          ),
          h5("A mapping project by ", 
             a("INFORMS", href = "https://informs.engin.umich.edu/", target = "_blank"), 
             " and ", 
             a("CTAC", href = "https://ginsberg.umich.edu/ctac", target = "_blank"),
             " at the University of Michigan")), br(),
      layout_columns(
        card(
          card_title("About"),
          p("The MHAction Mapping Tool is a visualization application designed to highlight the distribution of manufactured housing communities (MHC's) across the state of Michigan. The project began in late December 2023, initially between INFORMs and MHAction.
            In May 2024, INFORMs transitioned the project to the Community Technical Assistance Collaborative under the Ginsberg Center for Community Service and Learning. 
            The new development team aimed to improve the number of users allowed on the webpage, transitioning the webpage to GitHub pages. This allows for easy access to source data and code files in the future, as well as improved deployment capacity.
             Additional functional capabilities were added, including downloadable .csv files, user interface (UI) improvements, and data visibility."),
          p("Two sources of data were used to create this application. Michigan Department of Licensing and Regulatory Affairs (LARA) data was obtained by a FOIA request in January 2024. This contains a complete list of MHC's in the state of Michigan, per state regulatory guidelines for registration. 
            MHVillage data was scraped from the website in December 2023. The data sources can differ significantly, including by MHC name and management, and MHVillage data is typically incomplete. However, it is important to include data from both sources, as 
            this enhances the reliability and accuracy of the map visualization tool by allowing users to cross-verify information and identify discrepancies."),
          p("State House and Senate districting information is essential to help identify legislators and communities most significantly impacted by laws surroundign Manufactured Housing Communities. State districting information was based off of the 2021 Linden (State Senate) and Hickory (State House) maps. 
            These were drafted by the Michigan Independent Citizens Redistricting Commission following the decennial US Census results in 2020."),
          p("For more information, please visit ",
            a("MHAction.org", href = "https://www.mhaction.org/", target = "_blank"))
        )),
      layout_columns(
        card(
          card_title("Key Terms"),
          p("* MHC: Manufactured Housing Community"),
          p("* LARA: Michigan Department of Licensing and Regulatory Affairs. Michigan MHC's are required to register with LARA."),
          p("* MHVillage: Online marketplace for buying and selling manufactured homes. Data may be incomplete.")))),
  
    # Additional tabs
  tabPanel(
    "Map Tool",
    fluidPage(
      titlePanel("Map Tool"),
      fluidRow(
        column(
          width = 5,
          card(
            card_header(h6("Instructions")),
            HTML("
              <ol>
                <li>Select the layers you would like to view.
                  <ul>
                    <li>Markers will include additional information in a Pop-up message.</li>
                    <li>Circle layers will only pinpoint the coordinates of each MHC.</li>
                    <li>LARA Circles are selected as default.</li>
                  </ul>
                </li>
              </ol>
            "),
            selectizeInput("layerlist", "Choose a Layer:", choices = c("Base Map Only", 
                                                                      "LARA Circles (location only)",
                                                                      "LARA Markers (includes site info)",
                                                                      "MHVillage Circles (location only)",
                                                                      "MHVillage Markers (includes site info)"),
                                                                   
                           selected = "LARA Circles (location only)",
                           multiple = FALSE),
            HTML("
              <ol start='2'>
                <li>Select a district layer in the upper-right corner of the map.</li>
                <br> Note: To visualize circle sites clearly, please add a House or Senate districting layer before selecting a marker.
              </ol>
            "),
            h5("Instructions"),
            "Select the layers you would like to view below. Marker indicates that each MHC from the source you selected will include a popup with information about the MHC name, House and Senate district, and data source.", 
            "Circle layers will only pinpoint the coordinates of each MHC with no additional information. If you would like to see districting, the layer selection in the upper-right corner of the map allows selection of the geographic boundary.",
            selectInput("layerlist", "Choose layers:", choices = c(" ", 
                                                                   "Marker MHVillage",
                                                                   "Marker LARA",
                                                                   "Circle MHVillage (location only)",
                                                                   "Circle LARA (location only)"), 
                        multiple = TRUE),
            height = "500px"
          )
        ),
        column(
          width = 7,
          card(
            leafletOutput("leafletMap"),
            full_screen = TRUE,
            height = "800px")
        )
      )
    )),
  
  tabPanel(
    "About",
    page_fillable(
      titlePanel(
        "Manufactured Housing Communities in Michigan"
      ),
      p("By ", 
        a("INFORMS", href = "https://informs.engin.umich.edu/", target = "_blank"), 
        " and ", 
        a("CTAC", href = "https://ginsberg.umich.edu/ctac", target = "_blank"),
        " at the University of Michigan")), br(),
    layout_columns(
      card(
        card_title("About"),
        p("The MHAction Mapping Tool is a visualization application designed to highlight the distribution of manufactured housing communities (MHC's) across the state of Michigan. The project began in late December 2023, initially between INFORMs and MHAction.
            In May 2024, INFORMs transitioned the project to the Community Technical Assistance Collaborative under the Ginsberg Center for Community Service and Learning. 
            The new development team aimed to improve the number of users allowed on the webpage, transitioning the webpage to GitHub pages. This allows for easy access to source data and code files in the future, as well as improved deployment capacity.
             Additional functional capabilities were added, including downloadable .csv files, user interface (UI) improvements, and data visibility. Package versioning was not optimized under the Python Shiny GitHub Pages website, so the app was re-written in RShiny using the automatic deployment tool."),
        p("Two sources of data were used to create this application. Michigan Department of Licensing and Regulatory Affairs (LARA) data was obtained by a FOIA request in January 2024. This contains a complete list of MHC's in the state of Michigan, per state regulatory guidelines for registration. 
            MHVillage data was scraped from the website in December 2023. The data sources can differ significantly, including by MHC name and management, and MHVillage data is typically incomplete. However, it is important to include data from both sources, as 
            this enhances the reliability and accuracy of the map visualization tool by allowing users to cross-verify information and identify discrepancies."),
        p("State House and Senate districting information is essential to help identify legislators and communities most significantly impacted by laws surrounding Manufactured Housing Communities. State districting information was based off of the 2021 Linden (State Senate) and Hickory (State House) maps. 
            These were drafted by the Michigan Independent Citizens Redistricting Commission following the decennial US Census results in 2020."),
        p("For more information, please visit ",
          a("MHAction.org", href = "https://www.mhaction.org/", target = "_blank"))
      )),
    layout_columns(
      card(
        card_title("Navigation"),
        "About: Background information for this MHAction mapping project.",
        br(), "Map Tool: Visualize Manufactured Housing Communities and view data for the state of Michigan.",
        br(), "Infographics: Static bar graphs demonstrating MHVillage and LARA dataset capabilities. Also allows user to download full table data.",
        br(), "Tables: Dynamic tables that output MHC rows based on data source, geographic boundary type, and boundary selections. Users can download a full table with County, House, and Senate districting numbers.",
        br(), "Other: Credits, source files, and additional information."
      )
    ),
    layout_columns(
      card(
        card_title("Key Terms"),
        "MHC: Manufactured Housing Community",
        br(), "LARA: Michigan Department of Licensing and Regulatory Affairs. Michigan MHC's are required to register with LARA.",
        br(), "MHVillage: Online marketplace for buying and selling manufactured homes. Data may be incomplete."))),
  
  
            "Map Output", 
            leafletOutput("leafletMap"))
        )
      )
    )),

  tabPanel(
    "Infographics",
    fluidPage(
      titlePanel("Infographics"),
      fluidRow(
        column(
          width = 4,
          card(
            "Infographics show selected county information from the LARA and MHVillage datasets. A csv file is available for all rows of data.",
            downloadLink("info1", "Download all LARA county site counts as .csv file."),
            downloadLink("info2", "Download all MHVillage average rents by county as .csv file.")
          )
        ),
        column(
          width = 8,
          card(
            plotOutput("infographic1"),
            plotOutput("infographic2")
          )
        )
      )
    )),
  
  tabPanel(
    "MHC Site List Tables",
    "Tables",
    fluidPage(
      titlePanel("Tables"),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            selectInput("datasource", "Select LARA or MHVillage Data Source:", choices = c("LARA", "MHVillage")),
            selectInput("main_category", "Select a Geographic Boundary Type:", choices = c("County", "House district", "Senate district")),
            uiOutput("sub_category_ui"),
            h6("Site List Summary"),
            tableOutput("site_list_summary"),
            downloadButton("site_list_download", "Download Site List"),
            br(), br(),
            h5("Interested in the full district communities?"),
            downloadLink("mhvillage_all", "Download MHVillage data"),
            br(),
            downloadLink("lara_all", "Download LARA data")

            selectInput("datasource", "Data Source:", choices = c(" ", "MHVillage", "LARA")),
            selectInput("main_category", "Select Geographic Boundary Type:", choices = c(" ", "County", "House district", "Senate district")),
            uiOutput("sub_category_ui"),
            h5("Site List Summary"),
            tableOutput("site_list_summary"),
            downloadLink("site_list_download", "Download Site List")
          )
        ),
        column(
          width = 8,
          tableOutput("site_list")
        )
      )
    )),
  
  tabPanel(
    "Other",
    page_fillable(
      titlePanel("Other Information"),
      layout_columns(
        card(
          card_title("Credits"),
          p("This website was built by Vicky Wang with the Community Technical Assistance Collaborative in partnership with MHAction."),
          p("A previous version, hosted on GitHub Pages with limited functionality is available ",
            a("here.", href = "https://viwaumich.github.io/mhc/", target = "_blank")),
          p("Inspired by a ",
            a("project ", href = "https://hessakh.shinyapps.io/michigan_housing1/", target = "_blank"),
            "created by INFORMs at the University of Michigan."),
          br())),
      layout_columns(
        card(card_title("Reference Files"),
             p(
               downloadLink("mhvillage_raw", "MHVillage Raw Data (.csv)"),
               br(),
               downloadLink("lara_raw", "LARA Raw Data (.csv)"),
               br(),
               downloadLink("house_geojson", "MI State House Districts 2021 (.json)"),
               br(),
               downloadLink("senate_geojson", "MI State Senate Districts 2021 (.json)")
             )
        )
      ),
      " Please reach out to Vicky Wang (viwa@umich.edu) with questions.",
      br(),
      imageOutput("ctac_logo",inline = TRUE),
      imageOutput("mhaction_logo_large", inline = TRUE),
      imageOutput("informs_logo", inline = TRUE)))
  )


server <- function(input, output, session) {

    tabPanel(
      "Other",
      page_fillable(
        titlePanel("Other Information"),
        layout_columns(
          card(
            card_header("Credits"),
            p("This project was built by the Community Technical Assistance Collaborative in partnership with MHAction."),
            p("Project lead: ",
              a("Hessa Al-thani", href = "mailto:hessakh@umich.edu", target = "_blank"), br(),
              "MHAction contact: ",
              a("Paul Terranova", href = "mailto:pterranova@mhaction.org", target = "_blank"), br(),
              "with support from ",
              a("Deb Campbell", href = "mailto:dcampbell@ionia-mi.net", target = "_blank"), br(),
              "Website development: ",
              a("Naichen Shi", href = "mailto:naichens@umich.edu", target = "_blank"), br(),
              "Web design: ",
              a("Vicky Wang", href = "mailto:viwa@umich.edu", target = "_blank"), br(),
              "Data scraping and collection: ",
              a("Bingqing Xiang", href = "mailto:xbq@umich.edu", target = "_blank"), br(),
              "In partnership with INFORMS at the University of Michigan"
            )
          )),
        layout_columns(
          card(card_header("Reference Files"),
               p(
                 downloadLink("mhvillage_raw", "MHVillage Raw Data (.csv)"),
                 br(),
                 downloadLink("lara_raw", "LARA Raw Data (.csv)"),
                 br(),
                 downloadLink("house_geojson", "MI State House Districts 2021 (.json)"),
                 br(),
                 downloadLink("senate_geojson", "MI State Senate Districts 2021 (.json)")
               )
          )
        ),
        
        p("This is an updated version from June 2024. The original app can be found",
          a("here", href = "https://hessakh.shinyapps.io/michigan_housing1/", target = "_blank"),
          ". Updated source code can be found on ",
          a("Git", href = "https://github.com/viwaumich/mhc", target = "_blank"),
          ". Please reach out to Vicky Wang (viwa@umich.edu) with questions.")
      )
    )
)
  


server <- function(input, output, session) {
  # Load the CSV file into a data frame
  mhvillage_df <- read_csv("MHVillageDec7_Legislative1.csv")
  mhvillage_df$Sites <- as.integer(mhvillage_df$Sites)

  lara_df <- read_csv("LARA_with_coord_and_legislativedistrict1.csv")
  lara_df$County <- str_to_title(lara_df$County)

  house_districts <- read_json("Michigan_State_House_Districts_2021.json")
  senate_districts <- read_json("Michigan_State_Senate_Districts_2021.json")
  
  circlelist_mh <- list()
  mklist_mh <- list()
  circlelist_lara <- list()
  mklist_lara <- list()

  output$mhvillage_all <- downloadHandler(
    filename = function() {
      paste("mhvillage_all", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(mhvillage_simple, file, row.names = FALSE)
    }
  )
  
  output$lara_all <- downloadHandler(
    filename = function() {
      paste("lara_all", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(lara_simple, file, row.names = FALSE)
    }
  )
  
  output$ctac_logo <- renderImage({
    
    list(src = "ctac_logo.png",
         height = 60)
    
  }, deleteFile = F)
  
  output$mhaction_logo <- renderImage({
    
    list(src = "mhaction_logo.png",
         height = 50)
    
  }, deleteFile = F)
  
  output$mhaction_logo_large <- renderImage({
    
    list(src = "mhaction_logo.png",
         height = 80)
    
  }, deleteFile = F)
  
  output$informs_logo <- renderImage({
    
    list(src = "informs_logo.png",
         height = 70)
    
  }, deleteFile = F)
  
  
  output$leafletMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -84.3615564, lat = 44.44343, zoom = 6) %>%
      addGeoJSON(
        house_districts_j,
        fillColor = "green",
        color = "darkgreen",
        house_districts,
        fillColor = "pink",
        fillOpacity = 0.5,
        weight = 1,
        group = "House Districts"
      ) %>%
      addGeoJSON(
        senate_districts_j,
        color = "blueviolet",
        fillColor = "purple",
        senate_districts,
        fillColor = "violet",
        fillOpacity = 0.5,
        weight = 1,
        group = "Senate Districts"
      ) %>%
      addLayersControl(
        overlayGroups = c("House Districts", "Senate Districts")
      ) %>%
      hideGroup(c("House Districts", "Senate Districts"))
  })
  
  observeEvent(input$layerlist, {
    leafletProxy("leafletMap") %>% 
      clearMarkers() %>% 
      clearShapes() %>%
      clearMarkerClusters()
    
    map <- leafletProxy("leafletMap")
    
    for (layer in input$layerlist) {
      if (layer != " ") {
        map <- build_marker_layer(map, layer)
      }
    }
  })
  
  

  output$infographic1 <- renderPlot({
    build_infographics1(lara_df)
  })
  
  total_sites_by_county <- lara_df %>%
    filter(!is.na(`Total_#_Sites`)) %>%
    group_by(County) %>%
    summarise(Total_Sites = sum(`Total_#_Sites`, na.rm = TRUE)) %>%
    arrange(desc(Total_Sites))
  
  output$info1 <- downloadHandler(
    filename = function() {
      paste("lara_mhc_counts_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(total_sites_by_name_count, file, row.names = FALSE)
    }
  )
  
  output$infographic2 <- renderPlot({
    build_infographics2(mhvillage_df)
  })
  
  total_sites_by_name <- mhvillage_df %>%
    group_by(County) %>%
    summarise(Average_rent = mean(Average_rent, na.rm = TRUE)) %>%
    arrange(Average_rent)
  
  df_clean <- mhvillage_df %>%
    select(County, Average_rent)
  
  county_counts <- df_clean %>%
    count(County)
  
  total_sites_by_name_count <- merge(total_sites_by_name, county_counts, by = "County")
  
  output$info2 <- downloadHandler(
    filename = function() {
      paste("mhvillage_avg_rents_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(total_sites_by_name_count, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$main_category, {
    if (input$datasource == 'MHVillage') {
      if (input$main_category == 'County') {
        subcategory_choices <- sort(unique(mhvillage_df$County))
      } else {
        subcategory_choices <- sort(unique(mhvillage_df[[input$main_category]]))
      }
    } else {
      if (input$main_category == 'County') {
        subcategory_choices <- sort(unique(lara_df$County))
      } else {
        subcategory_choices <- sort(unique(lara_df[[input$main_category]]))
      }
    }
    
    updateSelectInput(session, "sub_category", choices = subcategory_choices)
  })
  
  output$sub_category_ui <- renderUI({
    selectInput("sub_category", "Select County/District Boundary:", choices = c())
  })
  
  reactive_site_list <- reactive({
    req(input$datasource)
    req(input$main_category)
    req(input$sub_category)
    
    if (input$datasource == 'MHVillage') {
      if (input$main_category == 'County') {
        df <- mhvillage_df %>%
          filter(County == input$sub_category) %>%
          select(Name, Sites, FullstreetAddress) %>%
          rename(`Number of Sites` = Sites,
                 Address = FullstreetAddress)
        
        df <- df %>%
          arrange(desc(`Number of Sites`)) %>%
          mutate(`Number of Sites` = as.integer(`Number of Sites`))
      } else {
        df <- mhvillage_df %>%
          filter(!!sym(input$main_category) == as.integer(as.numeric(input$sub_category))) %>%
          select(Name, Sites, FullstreetAddress) %>%
          rename(`Number of Sites` = Sites,
                 Address = FullstreetAddress)
        
        df <- df %>%
          arrange(desc(`Number of Sites`)) %>%
          mutate(`Number of Sites` = as.integer(`Number of Sites`))
      }
      
    } else {
      if (input$main_category == 'County') {
        df <- lara_df %>%
          filter(County == input$sub_category) %>%
          select(DBA, `Owner / Community_Name`, `Total_#_Sites`, `Location_Address`)
      } else {
        house_district <- as.integer(as.numeric(input$sub_category))
        df <- lara_df %>%
          filter(!!sym(input$main_category) == house_district) %>%
          select(DBA, `Owner / Community_Name`, `Total_#_Sites`, `Location_Address`)
      }
      
      df <- df %>%
        mutate(Name = ifelse(!is.na(DBA) & DBA != '', DBA, `Owner / Community_Name`)) %>%
        select(-DBA, -`Owner / Community_Name`) %>%
        rename(`Number of Sites` = `Total_#_Sites`,
               Address = `Location_Address`)
      
      df <- df %>%
        arrange(desc(`Number of Sites`)) %>%
        mutate(`Number of Sites` = as.integer(`Number of Sites`)) %>%
        select(Name, `Number of Sites`, `Address`)
        mutate(`Number of Sites` = as.integer(`Number of Sites`))
    }
    
    return(df)
  })
  
  sub_categories <- reactiveVal(c())
  
  # Update sub_category_ui based on main_category
  observe({
    req(input$main_category) # Ensure that input$main_category is available
    if (input$main_category == "County") {
      # Example values, replace with your actual values
      sub_categories(mi_counties$NAME)
    } else if (input$main_category == "House district") {
      sub_categories(c("District 1", "District 2", "District 3"))
    } else if (input$main_category == "Senate district") {
      sub_categories(c("Senate 1", "Senate 2", "Senate 3"))
    }
    
    output$sub_category_ui <- renderUI({
      selectInput("sub_category", "Select a County, House, or Senate District: ", choices = sub_categories(), selected = sub_categories()[1])
    })
  })
  
  # Update table based on selected inputs
  output$site_list <- renderTable({
    req(input$sub_category)
=======
  output$site_list <- renderTable({
    reactive_site_list()
  })
  
  output$site_list_summary <- renderTable({
    df <- reactive_site_list()
    if (nrow(df) > 0) {
      summary_df <- data.frame(
        "Number of MHC's" = nrow(df),
        "Total Sites" = sum(df$`Number of Sites`, na.rm = TRUE)
      ) 
    } else {
      summary_df <- data.frame(
        "Number of MHC's" = 0,
        "Total Sites" = 0
      )
    }
    summary_df %>%
      rename_with(~ gsub("\\.", " ", .))
  })
  
  output$site_list_download <- downloadHandler(
    filename = function() {
      paste("site_list_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_site_list(), file, row.names = FALSE)
    }
  )
  
  output$mhvillage_raw <- downloadHandler(
    filename = "mhvillage_raw.csv",
    content = function(file) {
      write.csv(mhvillage_df, file, row.names = FALSE)
    }
  )
  
  output$lara_raw <- downloadHandler(
    filename = "lara_raw.csv",
    content = function(file) {
      write.csv(lara_df, file, row.names = FALSE)
    }
  )
  
  output$house_geojson <- downloadHandler(
    filename = "house.json",
    content = function(file) {
      write_json(house_districts, file, row.names = FALSE)
    }
  )
  
  output$senate_geojson <- downloadHandler(
    filename = "senate.json",
    content = function(file) {
      write_json(senate_districts, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)