

shinyServer(function(input, output, session) {
    
    
    ##############################################################################
    #       FUNCTIONS
    ##############################################################################
    
    update_years <- function(data) {
      
      
      minCurrentYear <- min(data$anoinicio, na.rm = TRUE)
      maxCurrentYear <- max(data$anoinicio, na.rm = TRUE)

      
      
      if(!is.infinite(minCurrentYear)){
        minYear <<- minCurrentYear
        
        updateSliderTextInput(session, "yearStartSlider_choice",
                              choices = seq(minYear, maxYear, by=1),
                              selected = minYear)
      }
      
      if(!is.infinite(maxCurrentYear)){
        maxYear <<- maxCurrentYear
        
        
        updateSliderTextInput(session, "yearEndSlider_choice",
                              choices = seq(minYear, maxYear, by=1),
                              selected = maxYear)
      }
      
      
    }
    
    check_columns <- function(data, c_sequence){
      for (c in c_sequence) {
        if(!(c %in% colnames(data))){
          return(c(FALSE, c))
        }
      }
      return(c(TRUE,NA))
    }
    
    
    filter_data <- function(data){
      
      district_choice <- input$district_choice
      county_choice <- input$county_choice
      yearStart_choice <- input$yearStartSlider_choice
      yearEnd_choice <- input$yearEndSlider_choice
      location_choice <- input$location_choice
      
      if(is.null(data)){
        return(NULL)
      }
      
      if(controlDis){
        county_choice <- 'ALL'
      }
      
      if(location_choice == 'Origin'){
        
        if(county_choice == 'ALL'){
          
          if(district_choice == 'ALL'){
            
            currentFileOri <<- filter(data, (anoinicio>= yearStart_choice & anoinicio <= yearEnd_choice) | is.na(anoinicio))

            return(currentFileOri)
          }
          else{
            
            currentFileOri <<- filter(data, (anoinicio>= yearStart_choice & anoinicio <= yearEnd_choice | is.na(anoinicio)) & district_ori == district_choice)

            return(currentFileOri)
            
          }
          
        }
        else{
          
          currentFileOri <<- filter(data, (anoinicio>= yearStart_choice & anoinicio <= yearEnd_choice | is.na(anoinicio)) & 
                                      district_ori == district_choice & county_ori == county_choice)

          return(currentFileOri)
          
          
        }
        
      }
      
      if(location_choice == 'Residence'){
        
        if(county_choice == 'ALL'){
          
          if(district_choice == 'ALL'){
            
            currentFileRes <<- filter(data, (anoinicio>= yearStart_choice & anoinicio <= yearEnd_choice) | is.na(anoinicio))
            return(currentFileRes)
          }
          else{
            
            currentFileRes <<- filter(data, (anoinicio>= yearStart_choice & anoinicio <= yearEnd_choice | is.na(anoinicio)) & district_res == district_choice)
            return(currentFileRes)
            
          }
          
        }
        else{
          
          currentFileRes <<- filter(data, (anoinicio>= yearStart_choice & anoinicio <= yearEnd_choice | is.na(anoinicio)) & 
                                      district_res == district_choice & county_res == county_choice)
          return(currentFileRes)
          
          
        }
        
      }
      
    }
    
    draw_table <- function(data) {
      if(!is.null(data)){
        
        output$table_input=DT::renderDataTable({
          DT::datatable(data[,c(column_sequence)]
          )
        })
        
      }
    }
    
    draw_map <- function(data_filtered){
      
      valLoc <- input$location_choice
      
      if(is.null(data_filtered) || (is.data.frame(data_filtered) && nrow(data_filtered) == 0)){
        output$map <- leaflet::renderLeaflet({
          m <- leaflet() %>%
            setView(lng = -8, lat = 39.8, zoom = 6) %>%
            addProviderTiles("Esri.WorldStreetMap")
          m
        })
      }
      
      else{
        
        if(valLoc == 'Origin'){
          output$map <- leaflet::renderLeaflet({
            m <- leaflet() %>%
              leaflet(data = data_filtered) %>% addProviderTiles("Esri.WorldStreetMap") %>%
              addCircleMarkers(lng = ~ori_long, lat = ~ori_lat, color = "#6b2c2a", weight = 1, opacity = 0.3, fillColor = "#d9534f", fillOpacity = 0.6, label = ~county_ori)
            m
          })
          
        }
        
        if(valLoc == 'Residence'){
          output$map <- leaflet::renderLeaflet({
            m <- leaflet() %>%
              leaflet(data = data_filtered) %>% addProviderTiles("Esri.WorldStreetMap") %>%
              addCircleMarkers(lng = ~res_long, lat = ~res_lat, color = "#6b2c2a", weight = 1, opacity = 0.3, fillColor = "#d9534f", fillOpacity = 0.6, label = ~county_res)
            m
          })
        } 
        
        
      }
    }
    
    draw_UI <- function(data){
      draw_map(data)
      draw_table(data)
      create_geomaps_and_local_datasets_filtered(filter_data(data))
      create_barplots_and_local_datasets_filtered(filter_data(data))
      create_territorial_maps_district_filtered(filter_data(data))
      create_territorial_maps_county_filtered(filter_data(data))
      
      currentInterval <<- 0
      overtime_gvis(filter_data(data))
      
      
    }
    
    draw_wLocation <- function(){
      
      valLoc <- input$location_choice
      
      if (valLoc == 'Origin'){
        draw_UI(filter_data(disease_Ori))
      }
      
      if (valLoc == 'Residence'){
        draw_UI(filter_data(disease_Res))
      }
      
    }
    
    add_dates_symptoms <- function(data){
      
      diff <- vector(mode = "integer")
      
      for(row in 1:nrow(data)){
        if(!is.na(data[row, "anoinicio"][[1]]) & !is.na(data[row, "yearOfBirth"][[1]])){
          if((data[row, "anoinicio"][[1]]) > (data[row, "yearOfBirth"][[1]])){
            diff <- append(diff, (data[row, "anoinicio"][[1]])- (data[row, "yearOfBirth"][[1]]))
          }
          
        }
      }
      
      mean_diff <- mean(diff)
      
      for(row in 1:nrow(data)){
        if(is.na(data[row, "anoinicio"][[1]]) & !is.na(data[row, "yearOfBirth"][[1]])){
          
          data[row, "anoinicio"][[1]] <- data[row, "yearOfBirth"][[1]] + round(mean_diff)
          
        }
      }
      return(data)
      
    }
    
    add_decades <- function(data){
      
      data$decadeSymptoms <- NA
      data$decadeSymptoms <- as.double(data$decadeSymptoms)
      
      for(row in 1:nrow(data)){
        if(!is.na(data[row, "anoinicio"][[1]])){
          
          data[row, "decadeSymptoms"][[1]] <- (data[row, "anoinicio"][[1]] %/% 10) * 10
          
        }
      }
      
      return(data)
    }
    
    create_geomaps_and_local_datasets_all <- function(data){
      
      valLoc <- input$location_choice
      
      if(is.null(data) || (is.data.frame(data) && nrow(data) == 0)){
        
        output$mapIncidence <- leaflet::renderLeaflet({
          m <- leaflet() %>%
            setView(lng = -8, lat = 39.8, zoom = 6) %>%
            addProviderTiles("Esri.WorldStreetMap")
          m
        })
        return()
      }
      
      if(!is.null(data) & valLoc == 'Origin'){
        
        
        per_district_ori_all <<- data %>% 
          group_by(district_ori, county_ori) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        per_county_ori_all <<- data %>% 
          group_by(county_ori) %>% 
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        locations_ori_all <<- unique(data[,c('district_ori','county_ori','ori_long','ori_lat')])
        
        geo_per_district_ori_all <<- left_join(per_district_ori_all, 
                                               locations_ori_all, 
                                               by = c("district_ori" = "district_ori", "county_ori" = "county_ori"))
        
        quantileNamed <- round(quantile(geo_per_district_ori_all$count, 0.90),digits = 0)

        
        output$mapIncidence <- leaflet::renderLeaflet({
          m <- leaflet() %>%
            leaflet(data = geo_per_district_ori_all) %>% addProviderTiles("Esri.WorldTopoMap") %>%
            addCircleMarkers(lng = ~ori_long, lat = ~ori_lat, 
                             color = ifelse(geo_per_district_ori_all$count> quantileNamed, "#FF0000", "#0000FF"), 
                             radius = sqrt(geo_per_district_ori_all$count)/2, 
                             opacity = ifelse(geo_per_district_ori_all$count> quantileNamed, 0.5, 0.3), 
                             fillColor = ifelse(geo_per_district_ori_all$count> quantileNamed, "#FF0000", "#0000FF"), 
                             fillOpacity = ifelse(geo_per_district_ori_all$count> quantileNamed, 0.6, 0.2),
                             label = ~county_ori)
          m
        })
        
        
        
      }
      
      if(!is.null(data) & valLoc == 'Residence'){
        
        per_district_res_all <<- data %>% 
          group_by(district_res, county_res) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        per_county_res_all <<- data %>% 
          group_by(county_res) %>% 
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        locations_res_all <<- unique(data[,c('district_res','county_res','res_long','res_lat')])
        
        geo_per_district_res_all <<- left_join(per_district_res_all, 
                                               locations_res_all, 
                                               by = c("district_res" = "district_res", "county_res" = "county_res"))
        
        quantileNamed <- round(quantile(geo_per_district_res_all$count, 0.90),digits = 0)

        output$mapIncidence <- leaflet::renderLeaflet({
          m <- leaflet() %>%
            leaflet(data = geo_per_district_res_all) %>% addProviderTiles("Esri.WorldTopoMap") %>%
            addCircleMarkers(lng = ~res_long, lat = ~res_lat, 
                             color = ifelse(geo_per_district_res_all$count> quantileNamed, "#FF0000", "#0000FF"), 
                             radius = sqrt(geo_per_district_res_all$count)/2, 
                             opacity = ifelse(geo_per_district_res_all$count> quantileNamed, 0.5, 0.3), 
                             fillColor = ifelse(geo_per_district_res_all$count> quantileNamed, "#FF0000", "#0000FF"), 
                             fillOpacity = ifelse(geo_per_district_res_all$count> quantileNamed, 0.6, 0.2),
                             label = ~county_res)
          m
        })
        
      }  
      
    }
    
    create_geomaps_and_local_datasets_filtered <- function(data){
      
      if(is.null(data) || (is.data.frame(data) && nrow(data) == 0)){
        output$mapIncidenceSelected <- leaflet::renderLeaflet({
          m <- leaflet() %>%
            setView(lng = -8, lat = 39.8, zoom = 6) %>%
            addProviderTiles("Esri.WorldStreetMap")
          m
        })
        return()
      }
      
      valLoc <- input$location_choice
      
      if(!is.null(data) & valLoc == 'Origin'){

        per_district_ori_filtered <<- data %>% 
          group_by(district_ori, county_ori) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        per_county_ori_filtered <<- data %>% 
          group_by(county_ori) %>% 
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        locations_ori_filtered <<- unique(data[,c('district_ori','county_ori','ori_long','ori_lat')])
        
        geo_per_district_ori_filtered <<- left_join(per_district_ori_filtered, 
                                                    locations_ori_filtered, 
                                                    by = c("district_ori" = "district_ori", "county_ori" = "county_ori"))
        
        quantileNamed <- round(quantile(geo_per_district_ori_filtered$count, 0.90),digits = 0)
 
        
        output$mapIncidenceSelected <- leaflet::renderLeaflet({
          m <- leaflet() %>%
            leaflet(data = geo_per_district_ori_filtered) %>% addProviderTiles("Esri.WorldTopoMap") %>%
            addCircleMarkers(lng = ~ori_long, lat = ~ori_lat, 
                             color = ifelse(geo_per_district_ori_filtered$count>= quantileNamed, "#FF0000", "#0000FF"), 
                             radius = sqrt(geo_per_district_ori_filtered$count)/2, 
                             opacity = ifelse(geo_per_district_ori_filtered$count>= quantileNamed, 0.5, 0.3), 
                             fillColor = ifelse(geo_per_district_ori_filtered$count>= quantileNamed, "#FF0000", "#0000FF"), 
                             fillOpacity = ifelse(geo_per_district_ori_filtered$count>= quantileNamed, 0.6, 0.2),
                             label = ~county_ori)
          m
        })
        
        
        
      }
      
      if(!is.null(data) & valLoc == 'Residence'){
        
        per_district_res_filtered <<- data %>% 
          group_by(district_res, county_res) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        per_county_res_filtered <<- data %>% 
          group_by(county_res) %>% 
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        locations_res_filtered <<- unique(data[,c('district_res','county_res','res_long','res_lat')])
        
        geo_per_district_res_filtered <<- left_join(per_district_res_filtered, 
                                                    locations_res_filtered, 
                                                    by = c("district_res" = "district_res", "county_res" = "county_res"))
        
        quantileNamed <- round(quantile(geo_per_district_res_filtered$count, 0.90),digits = 0)
 
        output$mapIncidenceSelected <- leaflet::renderLeaflet({
          m <- leaflet() %>%
            leaflet(data = geo_per_district_res_filtered) %>% addProviderTiles("Esri.WorldTopoMap") %>%
            addCircleMarkers(lng = ~res_long, lat = ~res_lat, 
                             color = ifelse(geo_per_district_res_filtered$count>= quantileNamed, "#FF0000", "#0000FF"), 
                             radius = sqrt(geo_per_district_res_filtered$count)/2, 
                             opacity = ifelse(geo_per_district_res_filtered$count>= quantileNamed, 0.5, 0.3), 
                             fillColor = ifelse(geo_per_district_res_filtered$count>= quantileNamed, "#FF0000", "#0000FF"), 
                             fillOpacity = ifelse(geo_per_district_res_filtered$count>= quantileNamed, 0.6, 0.2),
                             label = ~county_res)
          m
        })
        
      }  
      
    }
    
    create_barplots_and_local_datasets_all <- function(data){
      
      
      if(is.null(data)){
        output$plotDistrictAllDecades <- renderPlot({})
        output$plotDistrictsAll <- renderPlot({})
        output$plotCountyAll <- renderPlot({})
        output$plotTimeSeriesAll <- renderPlot({})
      }
      
      valLoc <- input$location_choice
      
      if(!is.null(data) & valLoc == 'Origin'){
        
        per_decade_all <<- data %>% 
          group_by(decadeSymptoms ) %>%
          summarise(count = n()) %>% 
          arrange(decadeSymptoms)
        
        per_decade_all <- drop_na(per_decade_all, decadeSymptoms)
        
        output$plotDistrictAllDecades <- renderPlot({
          
          barplot(per_decade_all$count, names.arg = per_decade_all$decadeSymptoms,
                  col = "#d9534f", border = "white",
                  xlab = "Decade of symptoms",
                  ylab = "Incidence" )
          
        })
        
        per_each_district_ori_all <<- data %>% 
          group_by(district_ori) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        
        output$plotDistrictsAll <- renderPlot({
          
          barplot(per_each_district_ori_all$count, names.arg = per_each_district_ori_all$district_ori, 
                  col = "#d9534f", border = "white",
                  ylab = "Incidence",
                  las=2, cex.names =0.75)
          
        })
        
        per_each_county_ori_all <<- data %>% 
          group_by(district_ori,county_ori) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        per_each_county_ori_all <- filter(per_each_county_ori_all, count>0)
        per_each_county_ori_all <- per_each_county_ori_all[0:19,]
        
        output$plotCountyAll <- renderPlot({
          
          barplot(per_each_county_ori_all$count, names.arg = per_each_county_ori_all$county_ori, 
                  col = "#d9534f", border = "white",
                  ylab = "Incidence",
                   las=2, cex.names =0.5)
          
        })
        
        per_year_all <<- data %>% 
          group_by(anoinicio) %>%
          summarise(count = n()) %>% 
          arrange(anoinicio)
        
        per_year_all <- drop_na(per_year_all, anoinicio)
        
        per_year_all_2022 <- subset(per_year_all, anoinicio > 2022)
        
        
        output$plotTimeSeriesAll <- renderPlot({
          
          ggplot() + ylab("Incidence") + xlab("Year of Symptoms") + 
            geom_line(data=per_year_all, aes(x=anoinicio, y = count), color= "#d9534f") + 
            geom_line(data=per_year_all_2022, aes(x=anoinicio,y=count), color = "black") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5,face = "bold"))
        })
        
      }
      
      if(!is.null(data) & valLoc == 'Residence'){
        
        per_decade_all <<- data %>% 
          group_by(decadeSymptoms ) %>%
          summarise(count = n()) %>% 
          arrange(decadeSymptoms)
        
        per_decade_all <- drop_na(per_decade_all, decadeSymptoms)
        
        output$plotDistrictAllDecades <- renderPlot({
          
          barplot(per_decade_all$count, names.arg = per_decade_all$decadeSymptoms, 
                  col = "#d9534f", border = "white",
                  xlab = "Decade of symptoms",
                  ylab = "Incidence",
                   )
          
        })
        
        
        per_each_district_res_all <<- data %>% 
          group_by(district_res) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        
        output$plotDistrictsAll <- renderPlot({
          
          barplot(per_each_district_res_all$count, names.arg = per_each_district_res_all$district_res, 
                  col = "#d9534f", border = "white",
                  ylab = "Incidence",
                  las=2, cex.names =0.75)
          
        })
        
        per_each_county_res_all <<- data %>% 
          group_by(district_res,county_res) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        per_each_county_res_all <- filter(per_each_county_res_all, count>0)
        per_each_county_res_all <- per_each_county_res_all[0:19,]
        
        output$plotCountyAll <- renderPlot({
          
          barplot(per_each_county_res_all$count, names.arg = per_each_county_res_all$county_res, 
                  col = "#d9534f", border = "white",
                  ylab = "Incidence",
                  las=2, cex.names =0.5)
          
        })
        
        
        per_year_all <<- data %>% 
          group_by(anoinicio) %>%
          summarise(count = n()) %>% 
          arrange(anoinicio)
        
        per_year_all <- drop_na(per_year_all, anoinicio)
        
        per_year_all_2022 <- subset(per_year_all, anoinicio > 2022)
        
        output$plotTimeSeriesAll <- renderPlot({
          
          ggplot()  + ylab("Incidence") + xlab("Year of Symptoms") + 
            geom_line(data=per_year_all, aes(x=anoinicio, y = count), color= "#d9534f") + 
            geom_line(data=per_year_all_2022, aes(x=anoinicio,y=count), color = "black") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5,face = "bold"))
        })
        
      }
      
      
    }
    
    create_barplots_and_local_datasets_filtered <- function(data){
      
      
      if(is.null(data)){
        output$plotDistrictFilteredDecades <- renderPlot({})
        output$plotDistrictsFiltered <- renderPlot({})
        output$plotCountyFiltered <- renderPlot({})
        output$plotTimeSeriesFiltered <- renderPlot({})
        
      }
      
      valLoc <- input$location_choice
      
      if(!is.null(data) & valLoc == 'Origin'){
        
        
        per_decade_filtered <<- data %>% 
          group_by(decadeSymptoms ) %>%
          summarise(count = n()) %>% 
          arrange(decadeSymptoms)
        
        per_decade_filtered <- drop_na(per_decade_filtered, decadeSymptoms)
        
        output$plotDistrictFilteredDecades <- renderPlot({
          
          barplot(per_decade_filtered$count, names.arg = per_decade_filtered$decadeSymptoms, 
                  col = "#d9534f", border = "white",
                  xlab = "Decade of symptoms",
                  ylab = "Incidence",
                   )
          
        })
        
        
        per_each_district_ori_filtered <<- data %>% 
          group_by(district_ori) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        
        output$plotDistrictsFiltered <- renderPlot({
          
          barplot(per_each_district_ori_filtered$count, names.arg = per_each_district_ori_filtered$district_ori, 
                  col = "#d9534f", border = "white",
                  ylab = "Incidence",
                  las=2, cex.names =0.75)
          
        })
        
        
        
        per_each_county_ori_filtered <<- data %>% 
          group_by(district_ori,county_ori) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        per_each_county_ori_filtered <- filter(per_each_county_ori_filtered, count>0)
        per_each_county_ori_filtered <- per_each_county_ori_filtered[0:19,]
        
        output$plotCountyFiltered <- renderPlot({
          
          barplot(per_each_county_ori_filtered$count, names.arg = per_each_county_ori_filtered$county_ori, 
                  col = "#d9534f", border = "white",
                  ylab = "Incidence",
                  las=2, cex.names =0.5)
          
        })
        
        
        
        per_year_all <<- data %>% 
          group_by(anoinicio) %>%
          summarise(count = n()) %>% 
          arrange(anoinicio)
        
        per_year_all <- drop_na(per_year_all, anoinicio)
        
        per_year_all_2022 <- subset(per_year_all, anoinicio > 2022)
        
        
        output$plotTimeSeriesFiltered <- renderPlot({
          
          ggplot()  + ylab("Incidence") + xlab("Year of Symptoms") + 
            geom_line(data=per_year_all, aes(x=anoinicio, y = count), color= "#d9534f") + 
            geom_line(data=per_year_all_2022, aes(x=anoinicio,y=count), color = "black") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5,face = "bold"))
        })
        
      }
      
      if(!is.null(data) & valLoc == 'Residence'){
        
        per_decade_filtered <<- data %>% 
          group_by(decadeSymptoms ) %>%
          summarise(count = n()) %>% 
          arrange(decadeSymptoms)
        
        per_decade_filtered <- drop_na(per_decade_filtered, decadeSymptoms)
        
        output$plotDistrictFilteredDecades <- renderPlot({
          
          barplot(per_decade_filtered$count, names.arg = per_decade_filtered$decadeSymptoms, 
                  col = "#d9534f", border = "white",
                  xlab = "Decade of symptoms",
                  ylab = "Incidence",
                   )
          
        })
        
        
        per_each_district_res_filtered <<- data %>% 
          group_by(district_res) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        
        output$plotDistrictsFiltered <- renderPlot({
          
          barplot(per_each_district_res_filtered$count, names.arg = per_each_district_res_filtered$district_res, 
                  col = "#d9534f", border = "white",
                  ylab = "Incidence",
                   las=2, cex.names =0.75)
          
        })
        
        
        per_each_county_res_filtered <<- data %>% 
          group_by(district_res,county_res) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        per_each_county_res_filtered <- filter(per_each_county_res_filtered, count>0)
        per_each_county_res_filtered <- per_each_county_res_filtered[0:19,]
        
        output$plotCountyFiltered <- renderPlot({
          
          barplot(per_each_county_res_filtered$count, names.arg = per_each_county_res_filtered$county_res, 
                  col = "#d9534f", border = "white",
                  ylab = "Incidence",
                   las=2, cex.names =0.5)
          
        })
        
        per_year_all <<- data %>% 
          group_by(anoinicio) %>%
          summarise(count = n()) %>% 
          arrange(anoinicio)
        
        per_year_all <- drop_na(per_year_all, anoinicio)
        
        per_year_all_2022 <- subset(per_year_all, anoinicio > 2022)
        
        output$plotTimeSeriesFiltered <- renderPlot({
          
          ggplot() + ylab("Incidence") + xlab("Year of Symptoms") + 
            geom_line(data=per_year_all, aes(x=anoinicio, y = count), color= "#d9534f") + 
            geom_line(data=per_year_all_2022, aes(x=anoinicio,y=count), color = "black") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5,face = "bold"))
        })
        
      }
      
      
    }
    
    create_territorial_maps_district_all <- function(data){
      
      
      if(is.null(data)){
        output$terrMapDistrictAll <- renderPlot({})
      }
      
      
      valLoc <- input$location_choice
      
      gadm <- readRDS("./data/final_district_continental_adm1.rds")
      
      districtColors <- rep("#ffffff",20)
      districtColors <- setNames( districtColors, levels(as.factor(gadm@data$NAME_1)))
      
      colors = rainbow(20)
      mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(17)
      
      
      
      if(!is.null(data) & valLoc == 'Origin'){
        
        per_district_ori_count <- data %>% 
          group_by(district_ori) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        
        output$terrMapDistrictAll <- renderPlot({
          
          per_district_ori_count$district_color <- "#ffffff"
          districtColors <- rep("#ffffff",18) 
          districtColors <- setNames( districtColors, levels(as.factor(gadm@data$NAME_1)))
          
          for(p in 1:9){
            quantileNamedInterval <- round(quantile(per_district_ori_count$count, numbersPercentiles[p], names = FALSE),digits = 0)
            per_district_ori_count$district_color <- ifelse(per_district_ori_count$count >= quantileNamedInterval & per_district_ori_count$district_color == "#ffffff" ,
                                                            pallete_colors_red[p], per_district_ori_count$district_color)
          }
          
          for (row in 1:nrow(per_district_ori_count)){
            districtColors[per_district_ori_count$district_ori] = per_district_ori_count$district_color
          }
          plot(gadm, col = districtColors, 
               border = TRUE)
          text(coordinates(gadm), labels = gadm$NAME_1, cex = 0.75) +
            theme(rect = element_rect(fill = "transparent"))
          #title("Incidence of ALL cases for each District ")
          
          
        })
      }
      
      if(!is.null(data) & valLoc == 'Residence'){
        
        per_district_res_count <- data %>% 
          group_by(district_res) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        
        output$terrMapDistrictAll <- renderPlot({
          
          per_district_res_count$district_color <- "#ffffff"
          districtColors <- rep("#ffffff",18) 
          districtColors <- setNames( districtColors, levels(as.factor(gadm@data$NAME_1)))
          
          for(p in 1:9){
            quantileNamedInterval <- round(quantile(per_district_res_count$count, numbersPercentiles[p], names = FALSE),digits = 0)
            per_district_res_count$district_color <- ifelse(per_district_res_count$count >= quantileNamedInterval & per_district_res_count$district_color == "#ffffff" ,
                                                            pallete_colors_red[p], per_district_res_count$district_color)
          }
          
          for (row in 1:nrow(per_district_res_count)){
            districtColors[per_district_res_count$district_res] = per_district_res_count$district_color
          }
          plot(gadm, col = districtColors, 
               border = TRUE)
          text(coordinates(gadm), labels = gadm$NAME_1, cex = 0.75)
          #title("Incidence of ALL cases for each District ")
          
          
        })
        
        
      }
      
      
    }
    
    create_territorial_maps_district_filtered <- function(data){
      
      
      if(is.null(data)){
        output$terrMapDistrictFiltered <- renderPlot({})
      }
      
      
      valLoc <- input$location_choice
      
      gadm <- readRDS("./data/final_district_continental_adm1.rds")
      
      districtColors <- rep("#ffffff",20)
      districtColors <- setNames( districtColors, levels(as.factor(gadm@data$NAME_1)))
      
      colors = rainbow(20)
      mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(17)
      
      
      
      if(!is.null(data) & valLoc == 'Origin'){
        
        per_district_ori_count <- data %>% 
          group_by(district_ori) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        
        output$terrMapDistrictFiltered <- renderPlot({
          
          per_district_ori_count$district_color <- "#ffffff"
          districtColors <- rep("#ffffff",18) 
          districtColors <- setNames( districtColors, levels(as.factor(gadm@data$NAME_1)))
          
          for(p in 1:9){
            quantileNamedInterval <- round(quantile(per_district_ori_count$count, numbersPercentiles[p], names = FALSE),digits = 0)
            per_district_ori_count$district_color <- ifelse(per_district_ori_count$count >= quantileNamedInterval & per_district_ori_count$district_color == "#ffffff" ,
                                                            pallete_colors_red[p], per_district_ori_count$district_color)
          }
          
          for (row in 1:nrow(per_district_ori_count)){
            districtColors[per_district_ori_count$district_ori] = per_district_ori_count$district_color
          }
          plot(gadm, col = districtColors, 
               border = TRUE)
          text(coordinates(gadm), labels = gadm$NAME_1, cex = 0.75)
          #title("Incidence of FILTERED cases for each District ")
          
          
        })
        
        output$terrMapCountyFiltered <- renderPlot({
          
          
          
        })
        
        
        
      }
      
      if(!is.null(data) & valLoc == 'Residence'){
        
        per_district_res_count <- data %>% 
          group_by(district_res) %>%
          summarise(count = n()) %>% 
          arrange(desc(count))
        
        
        output$terrMapDistrictFiltered <- renderPlot({
          
          per_district_res_count$district_color <- "#ffffff"
          districtColors <- rep("#ffffff",18) 
          districtColors <- setNames( districtColors, levels(as.factor(gadm@data$NAME_1)))
          
          for(p in 1:9){
            quantileNamedInterval <- round(quantile(per_district_res_count$count, numbersPercentiles[p], names = FALSE),digits = 0)
            per_district_res_count$district_color <- ifelse(per_district_res_count$count >= quantileNamedInterval & per_district_res_count$district_color == "#ffffff" ,
                                                            pallete_colors_red[p], per_district_res_count$district_color)
          }
          
          for (row in 1:nrow(per_district_res_count)){
            districtColors[per_district_res_count$district_res] = per_district_res_count$district_color
          }
          plot(gadm, col = districtColors, 
               border = TRUE)
          text(coordinates(gadm), labels = gadm$NAME_1, cex = 0.75)
          #title("Incidence of FILTERED cases for each District ")
          
          
        })
        
        
      }
      
      
    }
    
    create_territorial_maps_county_all <- function(data){
      
      
      if(is.null(data)){
        output$terrMapCountyAll <- renderPlot({})
      }
      
      
      valLoc <- input$location_choice
      
      gadmCountys <- readRDS("./data/final_county_continental_adm1.rds")
      
      
      if(!is.null(data) & valLoc == 'Origin'){
        
        per_county_ori_count <- data %>%
          group_by(county_ori) %>%
          summarise(count = n()) %>%
          arrange(desc(count))
        
        output$terrMapCountyAll <- renderPlot({
          
          per_county_ori_count$county_color <- "#ffffff"
          countyColors <- rep("#ffffff",length(gadmCountys@data$NAME_2)) 
          countyColors <- setNames( countyColors, factor(gadmCountys@data$NAME_2, levels = gadmCountys@data$NAME_2))
          
          for(p in 1:9){
            quantileNamedInterval <- round(quantile(per_county_ori_count$count, numbersPercentiles[p], names = FALSE),digits = 0)
            per_county_ori_count$county_color <- ifelse(per_county_ori_count$count >= quantileNamedInterval & per_county_ori_count$county_color == "#ffffff" ,
                                                        pallete_colors_red[p], per_county_ori_count$county_color)
          }
          
          for (row in 1:nrow(per_county_ori_count)){
            countyColors[per_county_ori_count$county_ori] = per_county_ori_count$county_color
          }
          plot(gadmCountys, col = countyColors,
               border = TRUE)
          #title("Incidence for each county of ALL cases")
          
        })
      }
      
      if(!is.null(data) & valLoc == 'Residence'){
        
        per_county_res_count <- data %>%
          group_by(county_res) %>%
          summarise(count = n()) %>%
          arrange(desc(count))
        
        output$terrMapCountyAll <- renderPlot({
          
          per_county_res_count$county_color <- "#ffffff"
          countyColors <- rep("#ffffff",length(gadmCountys@data$NAME_2)) 
          countyColors <- setNames( countyColors, factor(gadmCountys@data$NAME_2, levels = gadmCountys@data$NAME_2))
          
          for(p in 1:9){
            quantileNamedInterval <- round(quantile(per_county_res_count$count, numbersPercentiles[p], names = FALSE),digits = 0)
            per_county_res_count$county_color <- ifelse(per_county_res_count$count >= quantileNamedInterval & per_county_res_count$county_color == "#ffffff" ,
                                                        pallete_colors_red[p], per_county_res_count$county_color)
          }
          
          for (row in 1:nrow(per_county_res_count)){
            countyColors[per_county_res_count$county_res] = per_county_res_count$county_color
          }
          plot(gadmCountys, col = countyColors,
               border = TRUE)
          #title("Incidence for each county of ALL cases")
          
        })
      }
    }
    
    create_territorial_maps_county_filtered <- function(data){
      
      
      if(is.null(data)){
        output$terrMapCountyFiltered <- renderPlot({})
      }
      
      
      valLoc <- input$location_choice
      
      gadmCountys <- readRDS("./data/final_county_continental_adm1.rds")
      
      
      if(!is.null(data) & valLoc == 'Origin'){
        
        per_county_ori_count <- data %>%
          group_by(county_ori) %>%
          summarise(count = n()) %>%
          arrange(desc(count))
        
        output$terrMapCountyFiltered <- renderPlot({
          
          per_county_ori_count$county_color <- "#ffffff"
          countyColors <- rep("#ffffff",length(gadmCountys@data$NAME_2)) 
          countyColors <- setNames( countyColors, factor(gadmCountys@data$NAME_2, levels = gadmCountys@data$NAME_2))
          
          for(p in 1:9){
            quantileNamedInterval <- round(quantile(per_county_ori_count$count, numbersPercentiles[p], names = FALSE),digits = 0)
            per_county_ori_count$county_color <- ifelse(per_county_ori_count$count >= quantileNamedInterval & per_county_ori_count$county_color == "#ffffff" ,
                                                        pallete_colors_red[p], per_county_ori_count$county_color)
          }
          
          for (row in 1:nrow(per_county_ori_count)){
            countyColors[per_county_ori_count$county_ori] = per_county_ori_count$county_color
          }
          plot(gadmCountys, col = countyColors,
               border = TRUE)
          #title("Incidence for each county of FILTERED cases")
          
        })
      }
      
      if(!is.null(data) & valLoc == 'Residence'){
        
        per_county_res_count <- data %>%
          group_by(county_res) %>%
          summarise(count = n()) %>%
          arrange(desc(count))
        
        output$terrMapCountyFiltered <- renderPlot({
          
          per_county_res_count$county_color <- "#ffffff"
          countyColors <- rep("#ffffff",length(gadmCountys@data$NAME_2)) 
          countyColors <- setNames( countyColors, factor(gadmCountys@data$NAME_2, levels = gadmCountys@data$NAME_2))
          
          for(p in 1:9){
            quantileNamedInterval <- round(quantile(per_county_res_count$count, numbersPercentiles[p], names = FALSE),digits = 0)
            per_county_res_count$county_color <- ifelse(per_county_res_count$count >= quantileNamedInterval & per_county_res_count$county_color == "#ffffff" ,
                                                        pallete_colors_red[p], per_county_res_count$county_color)
          }
          
          for (row in 1:nrow(per_county_res_count)){
            countyColors[per_county_res_count$county_res] = per_county_res_count$county_color
          }
          plot(gadmCountys, col = countyColors,
               border = TRUE)
          #title("Incidence for each county of FILTERED cases")
          
        })
      }
    }
    
    overtime_gvis <- function(data){
      
      
      
      gadm <<- readRDS("./data/final_district_continental_adm1.rds")
      gadmCountys <<- readRDS("./data/final_county_continental_adm1.rds")
      
      valLoc <- input$location_choice
      
      interval <<- input$interval
      
      valMaxYear <- input$yearEndSlider_choice
      valMinYear <- input$yearStartSlider_choice
      
      maxInterval <<- (valMaxYear - valMinYear) %/% interval
      
      
      if(is.null(data) || (is.data.frame(data) && nrow(data) == 0)){
        output$mapIncidenceInterval <- leaflet::renderLeaflet({
          m <- leaflet() %>%
            setView(lng = -8, lat = 39.8, zoom = 6) %>%
            addProviderTiles("Esri.WorldStreetMap")
          m
        })
        
        output$terrMapDistrictInterval <- renderPlot({
          
          plot(gadm, border = TRUE)
        })
        
        output$terrMapCountyInterval <- renderPlot({
          
          plot(gadmCountys, border = TRUE)
        })
        
      }
      
      if(!is.null(data) & valLoc == 'Origin'){
        
        data_holder <<- data
        
        
        if(currentInterval != maxInterval){
          data_holder <- filter(data_holder, anoinicio >= (valMinYear+(currentInterval*interval)) & anoinicio <(valMinYear+((currentInterval+1)*interval)) )
          data_holder <- drop_na(data_holder, anoinicio)
          
        }
        if(currentInterval == maxInterval){
          data_holder <- filter(data_holder, anoinicio >= (valMinYear+((currentInterval)*interval)))
          data_holder <- drop_na(data_holder, anoinicio)
          
        }
        
        if(currentInterval != maxInterval){
          output$txtInterval <- renderText({
            paste((valMinYear+(currentInterval*interval)),(valMinYear+((currentInterval+1)*interval)) ,sep = " - " )
          })
        }
        if(currentInterval == maxInterval){
          output$txtInterval <- renderText({
            paste((valMinYear+(currentInterval*interval)),valMaxYear ,sep = " - " )
          })
        }
        
        
        if(is.infinite(min(data_holder$anoinicio))){
          
          output$mapIncidenceInterval <- leaflet::renderLeaflet({
            m <- leaflet() %>%
              setView(lng = -8, lat = 39.8, zoom = 6) %>%
              addProviderTiles("Esri.WorldStreetMap")
            m
          })
          
          output$terrMapDistrictInterval <- renderPlot({
            
            plot(gadm, border = TRUE)
          })
          
          output$terrMapCountyInterval <- renderPlot({
            
            plot(gadmCountys, border = TRUE)
          })
          
          
          
          
          
        }
        
        
        if(!is.infinite(min(data_holder$anoinicio))){
          
          
          
          per_district_ori_all <- data_holder %>% 
            group_by(district_ori, county_ori) %>%
            summarise(count = n()) %>% 
            arrange(desc(count))
          
          
          
          per_county_ori_all <- data_holder %>% 
            group_by(county_ori) %>% 
            summarise(count = n()) %>% 
            arrange(desc(count))
          
          
          locations_ori_all <- unique(data_holder[,c('district_ori','county_ori','ori_long','ori_lat')])
          
          geo_per_district_ori_all <- left_join(per_district_ori_all, 
                                                locations_ori_all, 
                                                by = c("district_ori" = "district_ori", "county_ori" = "county_ori"))
          
          quantileNamed <- round(quantile(geo_per_district_ori_all$count, 0.90),digits = 0)
          
          output$mapIncidenceInterval <- leaflet::renderLeaflet({
            m <- leaflet() %>%
              leaflet(data = geo_per_district_ori_all) %>% addProviderTiles("Esri.WorldTopoMap") %>%
              addCircleMarkers(lng = ~ori_long, lat = ~ori_lat, 
                               color = ifelse(geo_per_district_ori_all$count> quantileNamed, "#FF0000", "#0000FF"), 
                               radius = sqrt(geo_per_district_ori_all$count)/2, 
                               opacity = ifelse(geo_per_district_ori_all$count> quantileNamed, 0.5, 0.3), 
                               fillColor = ifelse(geo_per_district_ori_all$count> quantileNamed, "#FF0000", "#0000FF"), 
                               fillOpacity = ifelse(geo_per_district_ori_all$count> quantileNamed, 0.6, 0.2),
                               label = ~county_ori)
            m
          })
          
          per_district_ori_count <- data_holder %>% 
            group_by(district_ori) %>%
            summarise(count = n()) %>% 
            arrange(desc(count))
          
          
          output$terrMapDistrictInterval <- renderPlot({
            
            per_district_ori_count$district_color <- "#ffffff"
            districtColors <- rep("#ffffff",18) 
            districtColors <- setNames( districtColors, levels(as.factor(gadm@data$NAME_1)))
            
            for(p in 1:9){
              quantileNamedInterval <- round(quantile(per_district_ori_count$count, numbersPercentiles[p], names = FALSE),digits = 0)
              per_district_ori_count$district_color <- ifelse(per_district_ori_count$count >= quantileNamedInterval & per_district_ori_count$district_color == "#ffffff" ,
                                                              pallete_colors_red[p], per_district_ori_count$district_color)
            }
            
            for (row in 1:nrow(per_district_ori_count)){
              districtColors[per_district_ori_count$district_ori] = per_district_ori_count$district_color
            }
            plot(gadm, col = districtColors, 
                 border = TRUE)
            text(coordinates(gadm), labels = gadm$NAME_1, cex = 0.75)

            
          })
          
          
          per_county_ori_count <- data_holder %>%
            group_by(county_ori) %>%
            summarise(count = n()) %>%
            arrange(desc(count))
          
          output$terrMapCountyInterval <- renderPlot({
            
            per_county_ori_count$county_color <- "#ffffff"
            countyColors <- rep("#ffffff",length(gadmCountys@data$NAME_2)) 
            countyColors <- setNames( countyColors, factor(gadmCountys@data$NAME_2, levels = gadmCountys@data$NAME_2))
            
            for(p in 1:9){
              quantileNamedInterval <- round(quantile(per_county_ori_count$count, numbersPercentiles[p], names = FALSE),digits = 0)
              per_county_ori_count$county_color <- ifelse(per_county_ori_count$count >= quantileNamedInterval & per_county_ori_count$county_color == "#ffffff" ,
                                                          pallete_colors_red[p], per_county_ori_count$county_color)
            }
            
            for (row in 1:nrow(per_county_ori_count)){
              countyColors[per_county_ori_count$county_ori] = per_county_ori_count$county_color
            }
            plot(gadmCountys, col = countyColors,
                 border = TRUE)
            #title("Incidence of FILTERED cases for each county")
            
          })
          
          
        }
      }
      
      if(!is.null(data) & valLoc == 'Residence'){
        
        data_holder <- data
        
        
        if(currentInterval != maxInterval){
          data_holder <- filter(data_holder, anoinicio >= (valMinYear+(currentInterval*interval)) & anoinicio <(valMinYear+((currentInterval+1)*interval)) )
          data_holder <- drop_na(data_holder, anoinicio)
          
        }
        if(currentInterval == maxInterval){
          data_holder <- filter(data_holder, anoinicio >= (valMinYear+((currentInterval)*interval)))
          data_holder <- drop_na(data_holder, anoinicio)
          
        }
        
        if(currentInterval != maxInterval){
          output$txtInterval <- renderText({
            paste((valMinYear+(currentInterval*interval)),(valMinYear+((currentInterval+1)*interval)) ,sep = " - " )
          })
        }
        if(currentInterval == maxInterval){
          output$txtInterval <- renderText({
            paste((valMinYear+(currentInterval*interval)),valMaxYear ,sep = " - " )
          })
        }
        
        if(is.infinite(min(data_holder$anoinicio))){
          
          output$mapIncidenceInterval <- leaflet::renderLeaflet({
            m <- leaflet() %>%
              setView(lng = -8, lat = 39.8, zoom = 6) %>%
              addProviderTiles("Esri.WorldStreetMap")
            m
          })
          
          output$terrMapDistrictInterval <- renderPlot({
            
            plot(gadm, border = TRUE)
          })
          
          output$terrMapCountyInterval <- renderPlot({
            
            plot(gadmCountys, border = TRUE)
          })
          
          
          
          
        }
        
        
        if(!is.infinite(min(data_holder$anoinicio))){
          
          
          
          per_district_res_all <- data_holder %>% 
            group_by(district_res, county_res) %>%
            summarise(count = n()) %>% 
            arrange(desc(count))
          
          
          
          per_county_res_all <- data_holder %>% 
            group_by(county_res) %>% 
            summarise(count = n()) %>% 
            arrange(desc(count))
          
          
          locations_res_all <- unique(data_holder[,c('district_res','county_res','res_long','res_lat')])
          
          geo_per_district_res_all <- left_join(per_district_res_all, 
                                                locations_res_all, 
                                                by = c("district_res" = "district_res", "county_res" = "county_res"))
          
          quantileNamed <- round(quantile(geo_per_district_res_all$count, 0.90),digits = 0)
          
          output$mapIncidenceInterval <- leaflet::renderLeaflet({
            m <- leaflet() %>%
              leaflet(data = geo_per_district_res_all) %>% addProviderTiles("Esri.WorldTopoMap") %>%
              addCircleMarkers(lng = ~res_long, lat = ~res_lat, 
                               color = ifelse(geo_per_district_res_all$count> quantileNamed, "#FF0000", "#0000FF"), 
                               radius = sqrt(geo_per_district_res_all$count)/2, 
                               opacity = ifelse(geo_per_district_res_all$count> quantileNamed, 0.5, 0.3), 
                               fillColor = ifelse(geo_per_district_res_all$count> quantileNamed, "#FF0000", "#0000FF"), 
                               fillOpacity = ifelse(geo_per_district_res_all$count> quantileNamed, 0.6, 0.2),
                               label = ~county_res)
            m
          })
          
          per_district_res_count <- data_holder %>% 
            group_by(district_res) %>%
            summarise(count = n()) %>% 
            arrange(desc(count))
          
          
          output$terrMapDistrictInterval <- renderPlot({
            
            per_district_res_count$district_color <- "#ffffff"
            districtColors <- rep("#ffffff",18) 
            districtColors <- setNames( districtColors, levels(as.factor(gadm@data$NAME_1)))
            
            for(p in 1:9){
              quantileNamedInterval <- round(quantile(per_district_res_count$count, numbersPercentiles[p], names = FALSE),digits = 0)
              per_district_res_count$district_color <- ifelse(per_district_res_count$count >= quantileNamedInterval & per_district_res_count$district_color == "#ffffff" ,
                                                              pallete_colors_red[p], per_district_res_count$district_color)
            }
            
            for (row in 1:nrow(per_district_res_count)){
              districtColors[per_district_res_count$district_res] = per_district_res_count$district_color
            }
            plot(gadm, col = districtColors, 
                 border = TRUE)
            text(coordinates(gadm), labels = gadm$NAME_1, cex = 0.75)
            #title("Incidence of FILTERED cases for each District ")
            
            
          })
          
          
          per_county_res_count <- data_holder %>%
            group_by(county_res) %>%
            summarise(count = n()) %>%
            arrange(desc(count))
          
          output$terrMapCountyInterval <- renderPlot({
            
            per_county_res_count$county_color <- "#ffffff"
            countyColors <- rep("#ffffff",length(gadmCountys@data$NAME_2)) 
            countyColors <- setNames( countyColors, factor(gadmCountys@data$NAME_2, levels = gadmCountys@data$NAME_2))
            
            for(p in 1:9){
              quantileNamedInterval <- round(quantile(per_county_res_count$count, numbersPercentiles[p], names = FALSE),digits = 0)
              per_county_res_count$county_color <- ifelse(per_county_res_count$count >= quantileNamedInterval & per_county_res_count$county_color == "#ffffff" ,
                                                          pallete_colors_red[p], per_county_res_count$county_color)
            }
            
            for (row in 1:nrow(per_county_res_count)){
              countyColors[per_county_res_count$county_res] = per_county_res_count$county_color
            }
            plot(gadmCountys, col = countyColors,
                 border = TRUE)
          })
          
          
        }
        
        
        
      }
      
    }
    
    
    ##############################################################################
    #       EVENTS
    ##############################################################################
    
    
    observeEvent(input$map_marker_click, { 
      p <- input$map_marker_click
    })
    
    observeEvent(input$sex_choice,{
      
      val <- input$sex_choice
      disease_Ori <<- disease_Ori_full
      disease_Res <<- disease_Res_full
      
      
      if(!is.null(disease_Ori) ){
        
        ifelse(val == "all", disease_Ori, disease_Ori <<- subset(disease_Ori, sex == val))

        
        draw_UI(filter_data(disease_Ori))
      }
      
      if(!is.null(disease_Res)){
        ifelse(val == "all", disease_Res, disease_Res <<- subset(disease_Res, sex == val))
        
        
        draw_UI(filter_data(disease_Res))
      }
      
      
      
    })
    
    output$downloadcsv <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(filter_data(disease_Ori), file)
      }
    )
    
    observeEvent(input$dateless,{
      
      val <- input$dateless
      


      if(val == FALSE){
        
        if(!is.null(disease_Ori)){
          disease_Ori <<- subset(disease_Ori, !is.na(anoinicio))
          
          
          draw_table(disease_Ori)
          draw_map(disease_Ori)
          updateSelectInput(session, "district_choice", choices = c("ALL", sort(unique(disease_Ori$district_ori))))
          update_years(disease_Ori)
          create_geomaps_and_local_datasets_all(disease_Ori)
          create_barplots_and_local_datasets_all(disease_Ori)
          create_territorial_maps_district_all(disease_Ori)
          create_territorial_maps_county_all(disease_Ori)
          
          create_geomaps_and_local_datasets_filtered(filter_data(disease_Ori))
          create_barplots_and_local_datasets_filtered(filter_data(disease_Ori))
          create_territorial_maps_district_filtered(filter_data(disease_Ori))
          create_territorial_maps_county_filtered(filter_data(disease_Ori))
          
          currentInterval <<- 0
          overtime_gvis(disease_Ori)

        }   
        if(!is.null(disease_Res)){
          disease_Res <<- subset(disease_Res, !is.na(anoinicio))
            
          draw_table(disease_Res)
          draw_map(disease_Res)
          updateSelectInput(session, "district_choice", choices = c("ALL", sort(unique(disease_Res$district_res))))
          update_years(disease_Res)
          create_geomaps_and_local_datasets_all(disease_Res)
          create_barplots_and_local_datasets_all(disease_Res)
          create_territorial_maps_district_all(disease_Res)
          create_territorial_maps_county_all(disease_Res)
          
          create_geomaps_and_local_datasets_filtered(filter_data(disease_Res))
          create_barplots_and_local_datasets_filtered(filter_data(disease_Res))
          create_territorial_maps_district_filtered(filter_data(disease_Res))
          create_territorial_maps_county_filtered(filter_data(disease_Res))
          
          currentInterval <<- 0
          overtime_gvis(disease_Res)
        }
        
        
      }
      
      if(val == TRUE){
        
        disease_Ori <<- disease_Ori_full
        disease_Res <<- disease_Res_full
        
        if(!is.null(disease_Ori)){
        
          draw_table(disease_Ori)
          draw_map(disease_Ori)
          updateSelectInput(session, "district_choice", choices = c("ALL", sort(unique(disease_Ori$district_ori))))
          update_years(disease_Ori)
          create_geomaps_and_local_datasets_all(disease_Ori)
          create_barplots_and_local_datasets_all(disease_Ori)
          create_territorial_maps_district_all(disease_Ori)
          create_territorial_maps_county_all(disease_Ori)
          
          create_geomaps_and_local_datasets_filtered(filter_data(disease_Ori))
          create_barplots_and_local_datasets_filtered(filter_data(disease_Ori))
          create_territorial_maps_district_filtered(filter_data(disease_Ori))
          create_territorial_maps_county_filtered(filter_data(disease_Ori))
          
          currentInterval <<- 0
          overtime_gvis(disease_Ori)
          
          
        }
        
        
        if(!is.null(disease_Res)){
          
          draw_table(disease_Res)
          draw_map(disease_Res)
          updateSelectInput(session, "district_choice", choices = c("ALL", sort(unique(disease_Res$district_res))))
          update_years(disease_Res)
          create_geomaps_and_local_datasets_all(disease_Res)
          create_barplots_and_local_datasets_all(disease_Res)
          create_territorial_maps_district_all(disease_Res)
          create_territorial_maps_county_all(disease_Res)
          
          create_geomaps_and_local_datasets_filtered(filter_data(disease_Res))
          create_barplots_and_local_datasets_filtered(filter_data(disease_Res))
          create_territorial_maps_district_filtered(filter_data(disease_Res))
          create_territorial_maps_county_filtered(filter_data(disease_Res))
          
          currentInterval <<- 0
          overtime_gvis(disease_Res)
          
        }
        
        
      }

    })
    
    
    observeEvent(input$intervalPrevious,{
      
      if(!currentInterval == 0){
        
        currentInterval <<- currentInterval - 1
        
      }
      
      valLoc <- input$location_choice
      
      if(valLoc == "Origin"){
        overtime_gvis(filter_data(disease_Ori))
      }
      
      if(valLoc == "Residence"){
        overtime_gvis(filter_data(disease_Res))
        
      }
    })
    
    observeEvent(input$intervalNext,{
      
      if(!currentInterval == maxInterval){
        
        currentInterval <<- currentInterval + 1
        
      }
      
      valLoc <- input$location_choice
      
      if(valLoc == "Origin"){
        overtime_gvis(filter_data(disease_Ori))
      }
      
      if(valLoc == "Residence"){
        overtime_gvis(filter_data(disease_Res))
        
      }
      
    })
    
    observeEvent(input$check_interval, { 
      i <- input$intervalo 
      
      currentInterval <<- 0
      maxInterval <- NULL
      
      
      valLoc <- input$location_choice
      
      if(valLoc == "Origin"){
        overtime_gvis(filter_data(disease_Ori))
      }
      
      if(valLoc == "Residence"){
        overtime_gvis(filter_data(disease_Res))
        
      }
      
    })
    
    
    
    observeEvent(input$mapIncidence_marker_click, { 
      p <- input$mapIncidence_marker_click  
    })
    
    observeEvent(input$yearStartTextMinus, {
      val <- input$yearStartSlider_choice
      updateSliderTextInput(session,"yearStartSlider_choice",
                            selected = ifelse(val == minYear,val,
                                              val-1)
      )
    })
    observeEvent(input$yearStartTextPlus, {
      val <- input$yearStartSlider_choice
      updateSliderTextInput(session,"yearStartSlider_choice",
                            selected = ifelse(val == maxYear,val,
                                              val+1)
      )
    })
    observeEvent(input$yearEndTextMinus, {
      val <- input$yearEndSlider_choice
      updateSliderTextInput(session,"yearEndSlider_choice",
                            selected = ifelse(val == minYear ,val,
                                              val-1)
      )
    })
    observeEvent(input$yearEndTextPlus,{
      val <- input$yearEndSlider_choice
      updateSliderTextInput(session,"yearEndSlider_choice",
                            selected = ifelse(val == maxYear,val,
                                              val+1)
      )
    })
    
    observeEvent(input$location_choice,{
      val <- input$location_choice
      if (val == 'Origin'){
        
        updatePrettyCheckbox(
          inputId = "dateless",
          value = TRUE,)
        
        draw_table(disease_Ori)
        draw_map(disease_Ori)
        updateSelectInput(session, "district_choice", choices = c("ALL", sort(unique(disease_Ori$district_ori))))
        update_years(disease_Ori)
        create_geomaps_and_local_datasets_all(disease_Ori)
        create_barplots_and_local_datasets_all(disease_Ori)
        create_territorial_maps_district_all(disease_Ori)
        create_territorial_maps_county_all(disease_Ori)
        
        create_geomaps_and_local_datasets_filtered(filter_data(disease_Ori))
        create_barplots_and_local_datasets_filtered(filter_data(disease_Ori))
        create_territorial_maps_district_filtered(filter_data(disease_Ori))
        create_territorial_maps_county_filtered(filter_data(disease_Ori))
        
        currentInterval <<- 0
        overtime_gvis(disease_Ori)
        
        
      }
      if (val == 'Residence'){
        
        updatePrettyCheckbox(
          inputId = "dateless",
          value = TRUE,)
        
        draw_table(disease_Res)
        draw_map(disease_Res)
        updateSelectInput(session, "district_choice", choices = c("ALL", sort(unique(disease_Res$district_res))))
        update_years(disease_Res)
        create_geomaps_and_local_datasets_all(disease_Res)
        create_barplots_and_local_datasets_all(disease_Res)
        create_territorial_maps_district_all(disease_Res)
        create_territorial_maps_county_all(disease_Res)
        
        create_geomaps_and_local_datasets_filtered(filter_data(disease_Res))
        create_barplots_and_local_datasets_filtered(filter_data(disease_Res))
        create_territorial_maps_district_filtered(filter_data(disease_Res))
        create_territorial_maps_county_filtered(filter_data(disease_Res))
        
        currentInterval <<- 0
        overtime_gvis(disease_Res)
        
      }
      
      
    })
    
    
    observeEvent(input$district_choice,{
      valLoc <- input$location_choice
      valDisc <- input$district_choice
      
      controlDis <<- TRUE
      
      valCount <- input$county_choice
      if(valDisc == "ALL"){
        if(valLoc == "Origin"){
          placeholderTibble <- disease_Ori
        }
        if(valLoc == "Residence"){
          placeholderTibble <- disease_Res
        }
      }
      
      if (valLoc == 'Origin'){
        if(!is.null(disease_Ori)){
          placeholderTibble <- filter(disease_Ori, district_ori == valDisc )
          updateSelectInput(session, "county_choice", choices = c("ALL", sort(unique(placeholderTibble$county_ori))), selected = 'ALL')
          
          if(valDisc == 'ALL'){
            updateSelectInput(session, "county_choice", choices = c("ALL", sort(unique(placeholderTibble$county_ori))), selected = 'ALL')
          }
          
          draw_UI(filter_data(disease_Ori))
          controlDis <<- FALSE
        }
        else{
          placeholderTibble <- NULL
          updateSelectInput(session, "county_choice", choices = c("ALL", sort(unique(placeholderTibble$county_ori))))
        }
        
      }
      
      if (valLoc == 'Residence'){
        if(!is.null(disease_Res)){
          placeholderTibble <- filter(disease_Res, district_res == valDisc )
          updateSelectInput(session, "county_choice", choices = c("ALL", sort(unique(placeholderTibble$county_res))))
          
          if(valDisc == 'ALL'){
            updateSelectInput(session, "county_choice", choices = c("ALL", sort(unique(placeholderTibble$county_res))), selected = 'ALL')
          }
          
          draw_UI(filter_data(disease_Res))
          controlDis <<- FALSE
        }
        else{
          placeholderTibble <- NULL
          updateSelectInput(session, "county_choice", choices = c("ALL", sort(unique(placeholderTibble$county_res))))
        }
      }
      
      
    })
    
    observeEvent(input$county_choice,{
      
      draw_wLocation()
      
      
    })
    
    observeEvent(input$yearStartSlider_choice,{
      
      s <- input$yearStartSlider_choice
      e <- input$yearEndSlider_choice
      
      if(s <= e){
        draw_wLocation()
      }
      
    })
    
    observeEvent(input$yearEndSlider_choice,{
      
      s <- input$yearStartSlider_choice
      e <- input$yearEndSlider_choice
      
      if(s <= e){
        draw_wLocation()
      }
      
    })
    
    
    
    observeEvent(input$fileOri,{
      currentFileOri <- input$fileOri
      
      ext <- tools::file_ext(currentFileOri$datapath)
      if(ext != "csv"){
        currentFileOri <- NULL
        showNotification(paste("You uploaded a wrong file type that contains a .",ext, " extension.", "Please upload a origin .csv instead"), 
                         type = 'error', duration = 10)
        
      }
      else{
        disease_Ori <<- read.csv(currentFileOri$datapath, na=c("NA"), header=TRUE)
        
        checked_value <- check_columns(disease_Ori,column_sequence)[1]
        checked_c <- check_columns(disease_Ori,column_sequence)[2]
        
        if (checked_value == FALSE){
          showNotification(paste("You uploaded a file that does not contain a column named ",checked_c, ".", "Please upload a correct origin .csv file."), 
                           type = 'error', duration = 10)
        }
        
        else{
          updateAwesomeRadio(session, "location_choice", selected = "Residence") #trick to change values on UI
          updateAwesomeRadio(session, "location_choice", selected = "Origin")
          
          disease_Ori <<- add_dates_symptoms(disease_Ori)
          
          disease_Ori <<- add_decades(disease_Ori)
          
          disease_Ori_full <<- disease_Ori
          
          draw_map(filter_data(disease_Ori))
          update_years(filter_data(disease_Ori))
          create_geomaps_and_local_datasets_all(disease_Ori)
          create_barplots_and_local_datasets_all(disease_Ori)
          create_territorial_maps_district_all(disease_Ori)
          create_territorial_maps_county_all(disease_Ori)
          
          
          
          create_geomaps_and_local_datasets_filtered(filter_data(disease_Ori))
          create_barplots_and_local_datasets_filtered(filter_data(disease_Ori))
          create_territorial_maps_district_filtered(filter_data(disease_Ori))
          create_territorial_maps_county_filtered(filter_data(disease_Ori))
          
          currentInterval <<- 0
          overtime_gvis(disease_Ori)
          
          
          updateTabsetPanel(session, "plot_tabs",
                            selected = "Static GVis")
          
          
        }
        
      }
    })
    
    observeEvent(input$fileRes,{
      currentFileRes <- input$fileRes
      ext <- tools::file_ext(currentFileRes$datapath)
      
      if(ext != "csv"){
        currentFileRes <- NULL
        showNotification(paste("You uploaded a wrong file type that contains a .",ext, " extension.", "Please upload a residence .csv instead."), 
                         type = 'error', duration = 10)
        
      }
      else{
        disease_Res <<- read.csv(currentFileRes$datapath, na=c("NA"), header=TRUE)
        
        checked_value <- check_columns(disease_Res,column_sequence)[1]
        checked_c <- check_columns(disease_Res,column_sequence)[2]
        
        if (checked_value == FALSE){
          showNotification(paste("You uploaded a file that does not contain a column named ",checked_c, ".", "Please upload a correct residence .csv file."), 
                           type = 'error', duration = 10)
        }
        
        else{
          updateAwesomeRadio(session, "location_choice", selected = "Origin") #trick to change values on UI
          updateAwesomeRadio(session, "location_choice", selected = "Residence")
          
          disease_Res <<- add_dates_symptoms(disease_Res)
          disease_Res <<- add_decades(disease_Res)
          
          disease_Res_full <<- disease_Res
          
          draw_map(filter_data(disease_Res))
          update_years(filter_data(disease_Res))
          create_geomaps_and_local_datasets_all(disease_Res)
          create_barplots_and_local_datasets_all(disease_Res)
          create_territorial_maps_district_all(disease_Res)
          create_territorial_maps_county_all(filter_data(disease_Res))
          
          create_geomaps_and_local_datasets_filtered(filter_data(disease_Res))
          create_barplots_and_local_datasets_filtered(filter_data(disease_Res))
          create_territorial_maps_district_filtered(filter_data(disease_Res))
          create_territorial_maps_county_filtered(filter_data(disease_Res))
          
          currentInterval <<- 0
          overtime_gvis(disease_Res)
          
          updateTabsetPanel(session, "plot_tabs",
                            selected = "Static GVis")
          
        }
        
      }
    })
    
    
    output$txtout <- renderText({
      paste(input$location_choice, input$district_choice ,input$yearStartSlider_choice, input$yearEndSlider_choice, input$level_choice, sep = " " )
    })
    
  }
)