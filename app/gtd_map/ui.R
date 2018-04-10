library(leaflet)

vars <- c(
  "(Please Select a Dataset)" = "NoneSelected",
  "Total Successful Incidents" = "LocationTotal",
  "Total Incidents By Decade" = "TotalSuccessfulIncidents_Decade",
  "Incidents With Extensive Property Damage" = "PropertyDamageText",
  "Custom Mapping" = "CustomMapping"
)

decades <- c(
  "1970s" = "1970s",
  "1980s" = "1980s",
  "1990s" = "1990s",
  "2000s" = "2000s",
  "2010s" = "2010s"
)

sizeOpts <- c(
  "Property Damage ($)" = "propvalue"
)

colorOpts <- c(
  "Attack Type" = "attacktype1_txt",
  "Target Type" = "targtype1"
)

navbarPage("Global Terrorism DB", id="nav",
  tabPanel("Interactive map",
    div(class="outer",
        tags$head(
          includeCSS("styles.css"),
          includeScript("gomap.js")
        ),
        
        leafletOutput("map", width="100%", height="100%"),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, height = "auto",
                      
                      h2("GTD Explorer"),
                      
                      selectInput("select_map", "Please select the data set:", vars, selected="NoneSelected"),
                      conditionalPanel("input.select_map == 'TotalSuccessfulIncidents_Decade'",
                                       # Only prompt for decade when selecting decade data
                                       selectInput("DecadeSelection", "Please select a decade:", decades, selected="1970s")
                                       ),
                      conditionalPanel("input.select_map == 'CustomMapping'",
                                       selectInput("colorBy", "Select Color Variable", colorOpts),
                                       selectInput("sizeBy", "Select Sizing Variable", sizeOpts))
                      )
    )
  )
)