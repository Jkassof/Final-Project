library(leaflet)
library(leaflet.extras)
library(viridis)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(bit64)

confirmed_db <- tbl(get_gtddb,"events")
confirmed_attacks <- confirmed_db %>%
  select(eventid,iyear,imonth,iday,country_txt,provstate,city,latitude,longitude,attacktype1_txt,targtype1,
         targtype1_txt,claimed,property,propextent,propvalue,doubtterr,success,gname) %>%
  #rename(Year=iyear,Country=country_txt) %>%
  filter(doubtterr==0 & success==1) %>%
  filter(!is.na(latitude)) %>%
  select(-doubtterr,-success)

total_attacks_by_country <- confirmed_attacks %>%
  group_by(country_txt) %>%
  tally() %>%
  rename(TotalSuccessfulAttacks = n)

total_attacks_by_location <- confirmed_attacks %>%
  filter(!is.na(latitude)) %>%
  group_by(latitude,longitude) %>%
  tally() %>%
  rename(LocationTotal = n)

total_attacks_by_decade <- confirmed_attacks %>%
  select(iyear,country_txt,latitude,longitude) %>%
  filter(!is.na(latitude)) %>%
  mutate(Decade = case_when(
    iyear >= 1970 & iyear < 1980 ~ "1970s",
    iyear >= 1980 & iyear < 1990 ~ "1980s",
    iyear >= 1990 & iyear < 2000 ~ "1990s",
    iyear >= 2000 & iyear < 2010 ~ "2000s",
    iyear >= 2010 ~ "2010s"
  )) %>%
  group_by(Decade,country_txt,latitude,longitude) %>%
  tally() %>%
  rename(TotalSuccessfulAttacks_Decade = n)

total_property_damage <- confirmed_attacks %>%
  select(eventid,iyear,imonth,iday,attacktype1_txt,property,propextent,propvalue,latitude,longitude,gname) %>%
  filter(property=="1" & propextent %in% c("1","2"))

get_totals <- collect(confirmed_attacks)
get_totals_by_country <- collect(total_attacks_by_country)
get_totals_by_decade <- collect(total_attacks_by_decade)
get_totals_property_damage <- collect(total_property_damage)
get_totals_property_damage_rev <- get_totals_property_damage %>%
  filter(!is.na(propvalue)) %>%
  filter(!is.na(latitude)) %>%
  mutate(
    AttackDate = case_when(
      iday == "0" ~ paste(iyear,"-", imonth, sep=""),
      iday != "0" ~ paste(iyear, "-", imonth, "-", iday, sep="")),
    PropertyDamageText = case_when(
      as.character(as.integer64(propvalue)) == "-99" & as.character(as.integer(propextent)) == "1" ~ "> $1000000000",
      as.character(as.integer64(propvalue)) == "-99" & as.character(as.integer(propextent)) == "2" ~ "Between $1000000 and $100000000",
      TRUE ~ paste("$",as.character(as.integer64(propvalue)),sep="")
    )
  )

get_totals_property_radius <- get_totals_property_damage_rev %>%
  mutate(
    PropertyDamageRadius = case_when(
      propvalue <= "0" ~ "1",
      propextent == "1" ~ as.character(as.numeric((propvalue) / 1000000)),
      propextent == "2" ~ as.character(as.numeric((propvalue) / 1000))
    )
  )
get_totals_by_location <- collect(total_attacks_by_location)

total_attacks_plus_location_total <- merge(x=get_totals,y=get_totals_by_location, by=c("latitude","longitude"), all.x=TRUE)

function(input, output, session) {
  
  ## Interactive Map ###########################################
    
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lat = 35.13, lng = -71.3394481, zoom = 3)
  })
 
  observe({
  
      if (input$select_map == "LocationTotal") {
        leafletProxy("map", data = total_attacks_plus_location_total) %>%
          clearShapes() %>%
          clearHeatmap() %>%
          clearMarkers() %>%
          clearMarkerClusters() %>%
          addHeatmap(~longitude, ~latitude, intensity = ~LocationTotal,
                     blur = 20, max = 0.05, radius = 15)
      } else if(input$select_map == "PropertyDamageText") {
        leafletProxy("map", data = get_totals_property_damage_rev) %>%
          clearShapes() %>%
          clearHeatmap() %>%
          clearMarkers() %>%
          clearMarkerClusters() %>%
          addMarkers(~longitude, ~latitude, layerId = ~eventid, 
                     popup = paste("<b>",get_totals_property_damage_rev$attacktype1_txt,"</b>",
                                   "</br>","Incident Date: ",get_totals_property_damage_rev$AttackDate,"</br>","Perpetrator/Group: ",get_totals_property_damage_rev$gname,
                                   "</br>","Estimated Damage: ",get_totals_property_damage_rev$PropertyDamageText),
            clusterOptions = markerClusterOptions()
          )
      } else if(input$select_map == "TotalSuccessfulIncidents_Decade") {
        leafletProxy("map", data = get_totals_by_decade[get_totals_by_decade$Decade==input$DecadeSelection,]) %>%
          clearShapes() %>%
          clearHeatmap() %>%
          clearMarkers() %>%
          clearMarkerClusters() %>%
          addHeatmap(~longitude, ~latitude, intensity = ~Decade,
                     blur = 20, max = 0.05, radius = 15)
      #} else if(input$select_map == "CustomMapping") {
      #   colorBy <- input$colorBy
      #   colorData <- unique(get_totals[[colorBy]])
      #   sizeBy <- input$sizeBy
      #   radius <- get_totals[[sizeBy]] / (max(get_totals[[sizeBy]], na.rm = TRUE)*.0000005)
      #   
      #   #leaflet() %>%
      #    # addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      #   #  setView(lat = 35.13, lng = -71.3394481, zoom = 3) %>%
      #   leafletProxy("map", data = get_totals) %>%
      #     clearShapes() %>%
      #     clearHeatmap() %>%
      #     clearMarkers() %>%
      #     addCircles(~longitude, ~latitude, radius=radius, layerId=NULL,
      #                stroke=FALSE, fillOpacity=0.4, fillColor=brewer.pal(length(colorData), "Paired")) # %>%
          #addLegend("bottomleft", pal=brewer.pal(length(colorData)), values=colorData, title=colorBy,
           #         layerId="colorLegend")
      } else {
        leafletProxy("map") %>%
          clearShapes() %>%
          clearHeatmap() %>%
          clearMarkers() %>%
          clearMarkerClusters()
      }
    })
  
  eventsInBounds <- reactive({
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    confirmed_attacks %>%
      filter(
        latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2]
      )
  })
  
  observe({
    
    
  })
  
  ## Data Explorer ###########################################
  
  # observe({
  #   cities <- if (is.null(input$states)) character(0) else {
  #     filter(cleantable, State %in% input$states) %>%
  #       `$`('City') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$cities[input$cities %in% cities])
  #   updateSelectInput(session, "cities", choices = cities,
  #                     selected = stillSelected)
  # })
  # 
  # observe({
  #   zipcodes <- if (is.null(input$states)) character(0) else {
  #     cleantable %>%
  #       filter(State %in% input$states,
  #              is.null(input$cities) | City %in% input$cities) %>%
  #       `$`('Zipcode') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
  #   updateSelectInput(session, "zipcodes", choices = zipcodes,
  #                     selected = stillSelected)
  # })
  # 
  # observe({
  #   if (is.null(input$goto))
  #     return()
  #   isolate({
  #     map <- leafletProxy("map")
  #     map %>% clearPopups()
  #     dist <- 0.5
  #     zip <- input$goto$zip
  #     lat <- input$goto$lat
  #     lng <- input$goto$lng
  #     showZipcodePopup(zip, lat, lng)
  #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  #   })
  # })
  # 
  # output$gtdtable <- DT::renderDataTable({
  #   df <- cleantable %>%
  #     filter(
  #       Score >= input$minScore,
  #       Score <= input$maxScore,
  #       is.null(input$states) | State %in% input$states,
  #       is.null(input$cities) | City %in% input$cities,
  #       is.null(input$zipcodes) | Zipcode %in% input$zipcodes
  #     ) %>%
  #     mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  #   action <- DT::dataTableAjax(session, df)
  #   
  #   DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  # })
}