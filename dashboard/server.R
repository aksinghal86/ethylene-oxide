server <- function(input, output, session) { 
  ## INTERACTIVE MAP -----------------------------------------------------------
  
  
  #### Create data sets and update UI components--------------------------------
  rvs <- reactiveValues()
  
  observe({
    req(input$mapdata_year)

    rvs$emissions <- emissions %>% filter(year == input$mapdata_year)
    rvs$cancer <- cancer %>% 
      filter(year == input$mapdata_year) %>% 
      mutate(tooltip = paste0('Census tract #: ', geoid, '<br>', 
                              'Pt Cancer Risk: ', round(pt_cancer_, 1), ' per million'))
  })
  
  output$map <- renderMapdeck({ 
    token = "pk.eyJ1IjoiYWtzaW5naGFsODgiLCJhIjoiY2tuZHgyeWxyMWEycDJwbzB1dDBqMGR0NiJ9.XFjK_TTS-nKfFYkQY70wIQ"
    mapdeck(style = mapdeck_style('dark'), token = token) %>%
      mapdeck_view(location = c(-100, 40), zoom = 4, pitch = 10)
  })
  
  observe({ 
    req(input$mapdata_year)
    req(rvs$emissions)
    req(rvs$cancer)
    
    pal <- colorRamp(c("#A2A1A100", "#9E5353B3", "#9E0000E6"), alpha = T)((1:256)/256)
    # pal[, 4] <- pal[, 4]*0.8
    
    mapdeck_update(map_id = 'map') %>% 
      clear_polygon('cancer') %>% 
      add_polygon(
        rvs$cancer,
        fill_colour = "log_pt_cancer",
        palette = 'orrd',
        fill_opacity = 0.6,
        auto_highlight = T,
        highlight_colour = '#FFFFFF26',
        tooltip = 'tooltip',
        stroke_colour = F,
        stroke_width = 20,
        legend = list(fill_colour = T, stroke_colour = F),
        update_view = F,
        # colour_range = colourvalues::colour_values(1:6, palette = "plasma"),
        layer_id = 'cancer'
      )
  })

  observe({
    req(input$mapdata_year)
    req(rvs$emissions)
    req(rvs$cancer)

    pal <- colorRamp(c("#A2A1A14D", "#9E5353B3", "#9E0000E6"), alpha = T)((1:256)/256)
    # pal[, 4] <- pal[, 4]*0.8

    mapdeck_update(map_id = 'map') %>%
      clear_scatterplot('facilities') %>%
      add_scatterplot(
        rvs$emissions,
        lat = 'latitude', 
        lon = 'longitude', 
        radius = 250,
        fill_opacity = 0.6,
        # fill_colour = "total_emissions_epa",
        auto_highlight = T,
        highlight_colour = '#FFFFFF26',
        tooltip = 'site_name',
        stroke_colour = '#ECF307',
        stroke_width = 20,
        # legend = list(fill_colour = T, stroke_colour = F),
        update_view = F,
        layer_id = 'facilities'
      )
  })
}