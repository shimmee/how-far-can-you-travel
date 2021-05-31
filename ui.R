
# cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti

shinyUI(
    navbarPage(theme = shinytheme("lumen"), 
               collapsible = TRUE,
               title = a("How Far Can You Travel?", 
                         style = "font-weight: bold; font-size: 22x; color: white",
                         href = "https://keita-shimmei.shinyapps.io/travel_isochrone/"),
               
               windowTitle = "How Far Can You Travel?",

        
        ################################################################################
        # Body 
        ################################################################################
        tabPanel(span("Create Map", style = "font-weight: bold; font-size: 22x; color: white"),
                 tags$head(tags$style(HTML(".tooltip {width: 130px;}"))),
                 useShinyjs(),
                 chooseSliderSkin(skin = "Simple",
                                  color = "#5CB85C"), # https://www.rdocumentation.org/packages/shinyWidgets/versions/0.6.0/topics/chooseSliderSkin
                 setSliderColor("#5CB85C", 100),  # https://www.w3schools.com/colors/colors_names.asp
                 div(class = "outer",
                     tags$head(
                         includeCSS("styles.css")
                     ),
                     leafletOutput('mainmap', width = '100%', height = '100%')
                     ),
                     column(12,
                            absolutePanel(style="z-index:1003;",
                                          top = 0, left = '35',
                                          dropdown(status = "primary",
                                                   icon = icon("gear"), 
                                                   width = "270px",
                                                   style = "jelly", 
                                                   tooltip = tooltipOptions(title = "Change settings",
                                                                            placement = 'top'),
                                                   span(htmlOutput('setting_description'),
                                                        style = "font-size: 14px; font-weight: bold; color: #4A4A4A"),
                                                   hr(style = "border-color: #4A4A4A"),
                                                   radioGroupButtons(inputId = "profile",
                                                                     label = 'Means of transportation',
                                                                     choices = c('driving', 'walking', 'cycling'),
                                                                     selected = profile_default
                                                   ),
                                                   radioGroupButtons(inputId = "max_time",
                                                                     label = 'Maximum time (min)',
                                                                     choices = c(10, 20, 30, 40),
                                                                     selected = max_time_default
                                                   ),
                                                   radioGroupButtons(inputId = "by_time",
                                                                     label = "Increaments of time (min)",
                                                                     choices = c(5, 10),
                                                                     selected = by_time_default
                                                   )
                                          ),
                                          br(),
                                          uiOutput('status')
                                          
                            ),
                            absolutePanel(top = 0, left = 85, 
                                          style="z-index:1004;", # https://stackoverflow.com/questions/48908009/the-select-drop-down-menu-appear-behind-other-elements-in-shiny
                                          dropdown(status = "success",
                                                   icon = icon("palette"), 
                                                   style = "jelly", 
                                                   right = F,
                                                   width = "220px",
                                                   tooltip = tooltipOptions(title = "Change design of map",
                                                                            placement = 'top'),
                                                   pickerInput(inputId = 'leaflet_design',
                                                               label = 'Map background',
                                                               choices = leaflet_design_list,
                                                               selected = leaflet_desing_default
                                                   ),
                                                   palettePicker(inputId = "color_scheme", 
                                                                 label = "Color palette", 
                                                                 choices = color_scheme_list, 
                                                                 selected = color_scheme_default,
                                                                 textColor = c(rep("white", length(color_scheme_list$Viridis)),
                                                                               rep("black", length(color_scheme_list$Brewer)))
                                                   ),
                                                   sliderInput(inputId = 'opacity',
                                                               label = 'Opacity',
                                                               min = 0.1, 
                                                               max = 1,
                                                               value = opacity_default,
                                                               step = 0.1,
                                                               ticks = FALSE),
                                                   awesomeRadio(inputId = "reverse_color",
                                                                label = "Reverse color palette", 
                                                                choices = c("No", "Yes"),
                                                                selected = "No",
                                                                status = "success"
                                                   )
                                                   
                                          )),
                            absolutePanel(top = 0, left = 135,
                                          style="z-index:1005;",
                                          tipify(actionBttn(inputId = "update_iso", 
                                                            label = NULL,
                                                            icon = icon('sync'),
                                                            style = "pill", 
                                                            color = "danger"),
                                                 title = 'Update map',
                                                 placement = 'top')
                            ),
                            absolutePanel(top = 0, right = 0,
                                          style="z-index:1006;",
                                          dropdown(status = "warning",
                                                   icon = icon("globe-americas"), 
                                                   style = "jelly", 
                                                   right = T,
                                                   width = "280px",
                                                   tooltip = tooltipOptions(title = "Jump to major cities",
                                                                            placement = 'top'),
                                                   uiOutput('region_button'),
                                                   uiOutput('city_button'),
                                                   uiOutput('jump_button')
                                                   
                                          
                                          )
                                          )
                     
                 )
        ),
        tabPanel(span("Get Code", style = "font-weight: bold; font-size: 22x; color: white"),)
    
)
)