
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$setting_description <- renderUI({
        HTML('1. Put <span style="color: #267ECA">blue marker</span> by tapping map</br> 2. Choose below settings</br>3. Hit <span style="color: #FB5666">right red button</span> to update map')
    })
    
    ################################################
    # List of cities based on region input
    ################################################
    
    
    cities <- eventReactive(req(input$region),{
        city %>%
            filter(region == input$region) %>%
            arrange(desc(lng)) %>%
            pull(name) %>%
            unique() 
    })
    
    output$region_button <- renderUI({
        radioGroupButtons(inputId = "region",
                          label = 'Region',
                          choices = region_list)
    })
    
    output$city_button <- renderUI({
        radioGroupButtons(inputId = "city",
                          label = 'Major city',
                          choices = cities()
        )
    })
    
    output$jump_button <- renderUI({
        actionBttn(inputId = 'go_button',
                   label = paste('Go to', input$city, '!'), 
                   color = 'warning')
    })
    

    ################################################
    # Default of map information
    ################################################
    map_info <- reactiveValues(iso_union = iso_union_default,
                               lng = lng_default,
                               lat = lat_default,
                               profile = profile_default)
    
    
    ################################################
    # Update clicked marker
    ################################################
    prev_marker <- reactiveVal('0')
    observeEvent(input$mainmap_click, {
        click <-  input$mainmap_click
        
        map_info$lng <- click$lng
        map_info$lat <- click$lat
        
        # Update map with new click information
        proxy <- leafletProxy('mainmap')
        proxy %>%
            addMarkers(layerId = as.character(1 - as.integer(prev_marker())),
                       lng = click$lng, 
                       lat = click$lat)
        
        status('')
        # Reset previously selected marker
        if(!is.null(prev_marker()))
        {
            proxy %>%
                removeMarker(layerId = prev_marker())
        }
        # set new value to reactiveVal 
        prev_marker(as.character(1 - as.integer(prev_marker())))
    })
    
    
    ################################################
    # Information of error message 
    ################################################
    status <- reactiveVal('')
    
    observeEvent(req(input$update_iso), {
        status('')
    })
    
    output$status_text <- renderText({
        status()
    })
    
    output$status <- renderUI({
        if(status() != ''){
            wellPanel(
                span(textOutput('status_text'),
                     style = "font-size: 18px; font-weight: bold; color: #D9534F"))
        }
    })
        
    
    
    
    ####################################################
    # Update information of iso_union by selected city
    ####################################################
    observeEvent(req(input$go_button), {

        map_info$lng <- city %>% filter(name == input$city) %>% pull(lng)
        map_info$lat <- city %>% filter(name == input$city) %>% pull(lat)

        tryCatch({
            iso <- mb_isochrone(location = c(map_info$lng, map_info$lat),
                                profile = input$profile,
                                time = seq(from = as.integer(input$by_time),
                                           to = as.integer(input$max_time),
                                           by = as.integer(input$by_time)))
            status('')
        }, error=function(e) {
            status('Error: choose other location!')
        }
        )

        if(status() == ''){
            iso %<>%
                st_as_sf()

            times <- sort(unique(iso$time))
            n <- length(times)
            time_label <- paste0(c(0, times[1:(n-1)]), ' - ', times)
            iso %<>%
                arrange(time) %>%
                mutate(time = factor(time_label, levels = time_label))

            suppressMessages({
                iso_union <- iso %>%
                    group_by(time) %>%
                    summarise() %>%
                    st_cast("MULTIPOLYGON") %>%
                    st_difference() %>%
                    arrange(desc(time))
            })

            map_info$iso_union <- iso_union
            map_info$profile <- input$profile
        }

    })
    
        
    ################################################
    # Update information of iso_union by click
    ################################################
    observeEvent(req(input$update_iso), {

        tryCatch({
            iso <- mb_isochrone(location = c(map_info$lng, map_info$lat),
                                profile = input$profile,
                                time = seq(from = as.integer(input$by_time), 
                                           to = as.integer(input$max_time), 
                                           by = as.integer(input$by_time))) 
            status('')
        }, error=function(e) {
            status('Error: choose other location!')
        }
        )
        
        if(status() == ''){
            iso %<>%
                st_as_sf() 
            
            times <- sort(unique(iso$time))
            n <- length(times)
            time_label <- paste0(c(0, times[1:(n-1)]), ' - ', times)
            iso %<>%
                arrange(time) %>%
                mutate(time = factor(time_label, levels = time_label)) 
            
            suppressMessages({
                iso_union <- iso %>%
                    group_by(time) %>%
                    summarise() %>%
                    st_cast("MULTIPOLYGON") %>%
                    st_difference() %>%
                    arrange(desc(time))
            })
            
            map_info$iso_union <- iso_union
            map_info$profile <- input$profile
        }
        
    })
    
    
    

    ################################################
    # Main map
    ################################################
    output$mainmap <- renderLeaflet({
        
        lng <- isolate(map_info$lng)
        lat <- isolate(map_info$lat)
        iso_union <- map_info$iso_union
        
        if(is.null(iso_union)){
            status('Error: choose other location!')
        }else{
            
            iso_union %>%
                st_bbox() %>%
                as.vector() -> bbox
            
            legend_title <- paste0("Travel time (min)</br> by ", isolate(map_info$profile))
            factpal <- colorFactor(palette  = isolate(input$color_scheme), 
                                   domain = iso_union$time,
                                   reverse = isolate(ifelse(input$reverse_color == 'Yes', TRUE, FALSE)))
            
            # Marker at the center
            icon <- case_when(isolate(map_info$profile) == 'driving' ~ 'car',
                              isolate(map_info$profile) == 'walking' ~ 'walking',
                              isolate(map_info$profile) == 'cycling' ~ 'biking')
            icon_center <- makeAwesomeIcon(text = fa(icon),
                                           markerColor = 'red', 
                                           iconColor = 'white')
            
            leaflet() %>%
                clearMarkers() %>%
                addProviderTiles(isolate(input$leaflet_design)) %>%
                fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
                addAwesomeMarkers(lng = lng, 
                                  lat = lat,
                                  icon = icon_center) %>%
                addPolygons(data = iso_union,
                            stroke=TRUE, 
                            color = NULL,
                            fillColor = ~factpal(time),
                            weight = 0, 
                            fillOpacity = isolate(input$opacity)) %>%
                addLegend(data = iso_union,
                          position = "bottomright", 
                          pal = factpal, 
                          values = ~time,
                          opacity = 0.8,
                          title = legend_title) %>%
                addSearchOSM(options = searchOptions(collapsed = TRUE))
        }
        
    })
    
    ################################################
    # Update map by leaflet background
    ################################################
    observeEvent(input$leaflet_design, {
        proxy <- leafletProxy('mainmap')
        proxy %>%
            clearTiles() %>%
            addProviderTiles(isolate(input$leaflet_design)) 
    })
    
    
    ################################################
    # Update map by palette input
    ################################################
    observeEvent(input$color_scheme,{
        
        iso_union <- map_info$iso_union
        legend_title <- paste0("Travel time (min)</br> by ", isolate(map_info$profile))
        factpal <- colorFactor(palette  = input$color_scheme, 
                               domain = iso_union$time,
                               reverse = isolate(ifelse(input$reverse_color == 'Yes', TRUE, FALSE)))
        proxy <- leafletProxy('mainmap')
        proxy %>%
            clearShapes() %>%
            clearControls() %>%
            addPolygons(data = iso_union,
                        stroke=TRUE, 
                        color = NULL,
                        fillColor = ~factpal(time),
                        weight = 0, 
                        fillOpacity = isolate(input$opacity)) %>%
            addLegend(data = iso_union,
                      position = "bottomright", 
                      pal = factpal, 
                      values = ~time,
                      opacity = 0.8,
                      title = legend_title) 
            
    })
    
    
    ################################################
    # Update map by reverse color input
    ################################################
    observeEvent(input$reverse_color,{
        
        iso_union <- map_info$iso_union
        legend_title <- paste0("Travel time (min)</br> by ", isolate(map_info$profile))
        factpal <- colorFactor(palette  = isolate(input$color_scheme), 
                               domain = iso_union$time,
                               reverse = ifelse(input$reverse_color == 'Yes', TRUE, FALSE))
        proxy <- leafletProxy('mainmap')
        proxy %>%
            clearShapes() %>%
            clearControls() %>%
            addPolygons(data = iso_union,
                        stroke=TRUE, 
                        color = NULL,
                        fillColor = ~factpal(time),
                        weight = 0, 
                        fillOpacity = isolate(input$opacity)) %>%
            addLegend(data = iso_union,
                      position = "bottomright", 
                      pal = factpal, 
                      values = ~time,
                      opacity = 0.8,
                      title = legend_title) 
        
    })
    
    ################################################
    # Update map by opacity input
    ################################################
    observeEvent(input$opacity,{
        
        iso_union <- map_info$iso_union
        factpal <- colorFactor(palette  = isolate(input$color_scheme), 
                               domain = iso_union$time,
                               reverse = isolate(ifelse(input$reverse_color == 'Yes', TRUE, FALSE)))
        proxy <- leafletProxy('mainmap')
        proxy %>%
            clearShapes() %>%
            addPolygons(data = iso_union,
                        stroke=TRUE, 
                        color = NULL,
                        fillColor = ~factpal(time),
                        weight = 0, 
                        fillOpacity = input$opacity)
        
    })

})


