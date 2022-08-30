server <- function(input, output, session) { 
  ## INTERACTIVE MAP -----------------------------------------------------------
  
  
  #### Create data sets and update UI components--------------------------------
  rvs <- reactiveValues()
  
  observe({
    req(input$mapdata_year)

    rvs$emissions_for_map <- emissions_for_map %>% 
      filter(year == input$mapdata_year) %>% 
      mutate(tooltip = paste0(site_name, '<br>', 
                              year, ' emissions: ', round(total_emissions_epa, 1), ' lbs'))
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
    # req(rvs$emissions)
    req(rvs$cancer)
    
    # pal <- colorRamp(c("#A2A1A100", "#9E5353B3", "#9E0000E6"), alpha = T)((1:256)/256)
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
        # stroke_colour = '#FFFFFF',
        stroke_width = 50,
        legend = F,
        # # legend = list(fill_colour = T, stroke_colour = F),
        update_view = F,
        # colour_range = colourvalues::colour_values(1:6, palette = "plasma"),
        layer_id = 'cancer'
      )
  })

  observe({
    req(input$mapdata_year)
    req(rvs$emissions_for_map)
    req(rvs$cancer)

    # pal <- colorRamp(c("#A2A1A14D", "#9E5353B3", "#9E0000E6"), alpha = T)((1:256)/256)
    # pal[, 4] <- pal[, 4]*0.8

    mapdeck_update(map_id = 'map') %>%
      clear_scatterplot('facilities') %>%
      add_scatterplot(
        rvs$emissions_for_map,
        lat = 'latitude',
        lon = 'longitude',
        radius = 750,
        radius_min_pixels = 3,
        fill_opacity = 0.8,
        # fill_colour = "total_emissions_epa",
        auto_highlight = T,
        highlight_colour = '#FFFFFF26',
        tooltip = 'tooltip',
        # stroke_colour = '#ECF307',
        stroke_width = 20,
        # # legend = list(fill_colour = T, stroke_colour = F),
        update_view = F,
        layer_id = 'facilities'
      )
  })
  
  output$emissions_plot <- renderGirafe({
    req(input$site_name) 
    
    plotdata <- emissions_for_plot %>% 
      filter(site_name %in% input$site_name) %>% 
      mutate(tooltip = paste(round(emissions), 'lbs'))
    
    plt <- ggplot(plotdata, aes(x = year, y = emissions, color = site_name)) + 
      geom_point_interactive(aes(tooltip = tooltip, data_id = site_name), size = 3) + 
      geom_textline(aes(label = site_name), size = 3, vjust = -0.5, text_smoothing = 30) +
      labs(x = 'Year', y = 'Emissions (lbs)') +
      theme_bw() +
      theme(legend.position = 'none')
    
    girafe(
      ggobj = plt, 
      options = list(opts_hover_inv(css = 'opacity: 0.3;'), 
                     opts_selection(type = 'single'))
    )
  })
  
  output$table <- renderReactable({
    site_cols <- c("site_name", "city", "state", "census_tract", "year")
    emissions_cols <- c("reported_emissions", "emissions_source") 
    cancer_cols <- c('est_pt_cancer', 'cancer_source') 
    
    make_color_pal <- function(colors, bias = 1) { 
      get_color <- colorRamp(colors, bias = bias) 
      function(x) rgb(get_color(x), maxColorValue = 255)
    }
    
    emissions_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 3)
    
    cancer_color <- function(value) { 
      case_when(
        value >= 100 ~ '#E74C3C', 
        between(value, 1, 100) ~ '#D4AC0D', 
        value < 1 ~ '#229954'
      )
    }
    
    reactable(
      emissions_for_table,
      striped = T,
      compact = T, 
      searchable = T, 
      showSortIcon = F,
      filterable = T,
      pagination = T, 
      defaultSorted = c("site_name", "year"), 
      defaultPageSize = 20,
      columnGroups = list(
        colGroup(name = 'Site', columns = site_cols), 
        colGroup(name = 'Emissions', columns = emissions_cols), 
        colGroup(name = 'Cancer', columns = cancer_cols)
      ), 
      defaultColDef = colDef(
        vAlign = 'center', 
        headerVAlign = 'bottom', 
        class = 'cell', 
        headerClass = 'table-header'
      ),
      columns = list(
        site_name = colDef(
          name = 'Name', 
          minWidth = 250,
          class = 'table-site-name'
        ), 
        emissions_source = colDef(
          name = 'Emissions Source'
        ), 
        cancer_source = colDef(
          name = 'Cancer source', 
        ),
        reported_emissions = colDef(
          name = "Emissions<br>(lbs)",
          cell = function(value) { 
            gemissions <- log(emissions_for_table$reported_emissions[!is.na(emissions_for_table$reported_emissions)])
            scaled <- 1 - (log(value) - min(gemissions)) / (max(gemissions) - min(gemissions))
            color <- emissions_color(scaled)
            value <- format(round(value))
            div(class = 'table-emissions', style = list(background = color), value)
          },
          html = T, 
          class = 'border-left'
        ), 
        est_pt_cancer = colDef(
          name = "Cancer Risk<br>(per million)", 
          cell = function(value) { 
            color <- cancer_color(value)
            value <- format(round(value))
            div(class = 'table-cancer', style = list(background = color), value)  
          },
          html = T, 
          class = 'border-left'
        )
      ), 
      borderless = T, 
      class = 'emissions-table'
    )  
  })
}