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

# total_telecom_and_utility_attacks <- confirmed_attacks %>%
#  select(Year,imonth,iday,targtype1,targtype1_txt,gname,latitude,longitude,attacktype1_txt) %>%
#  rename(Month = imonth, Day = iday, TargetType=targtype1_txt, PerpetratorGroup = gname, AttackType = attacktype1_txt) %>%
#  filter(!is.na(latitude) & targtype1 %in% c("16","21")) %>%
#  mutate(AttackDate = case_when(
#    Day == "0" ~ paste(Year,"-", Month, sep=""),
#    Day != "0" ~ paste(Year, "-", Month, "-", Day, sep="")
#  )) %>%
#  tally() %>%
#  rename(TotalTelecomAndUtilityAttacks = n)

total_property_damage <- confirmed_attacks %>%
  select(eventid,iyear,imonth,iday,attacktype1_txt,property,propextent,propvalue,latitude,longitude,gname) %>%
  #rename(Month = imonth, Day = iday, PropertyDamage = propvalue, AttackType = attacktype1_txt, PerpetratorGroup = gname) %>%
  filter(property=="1" & propextent %in% c("1","2"))

get_totals <- collect(confirmed_attacks)
get_totals_by_country <- collect(total_attacks_by_country)
get_totals_by_decade <- collect(total_attacks_by_decade)
# get_totals_telecom_and_utility <- collect(total_telecom_and_utility_attacks)
get_totals_property_damage <- collect(total_property_damage)
get_totals_property_damage_rev <- get_totals_property_damage %>%
  #mutate(
    #PropertyDamage = if_else(propvalue == "", NA_character_, as.character(propvalue))
  #) %>%
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

# total_attacks_plus_cntry_total <- merge(x=get_totals,y=get_totals_by_country, by="Country", all.x=TRUE)
total_attacks_plus_location_total <- merge(x=get_totals,y=get_totals_by_location, by=c("latitude","longitude"), all.x=TRUE)
# total_attacks_plus_location_decade <- merge(x=get_totals_by_decade,y=get_totals_by_location, by="Country")

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Define color palette for attacks
  
 # pal <- colorFactor(
#    #palette = 'Dark2',
#    palette = c('red','blue','green','purple','orange','pink','yellow','brown','gray'),
#    domain = get_totals_property_damage$AttackType
#  )

#  prop_pal <- colorFactor(viridis(7), get_totals_property_radius$AttackType)
    
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
      } else {
        leafletProxy("map") %>%
          clearShapes() %>%
          clearHeatmap() %>%
          clearMarkers() %>%
          clearMarkerClusters()
      }
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