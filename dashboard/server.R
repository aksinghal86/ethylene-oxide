server <- function(input, output, session) { 
  
  ## INTERACTIVE MAP -----------------------------------------------------------
  #### Create data sets and update UI components--------------------------------
  output$searchUI <- renderUI({ 
    if (input$searchOptions == 'address') {
      textInput("search", NULL, width = '100%', placeholder = "Search by address, city, coordinates, etc.")
    } else if (input$searchOptions == 'facility') {
      xx <- emissions_for_map %>% 
        filter(!is.na(site_name)) %>% 
        mutate(site_name = toupper(paste0(site_name, ', ', state))) %>% 
        distinct(site_name, eisid) %>% 
        arrange(site_name)
      choices <- xx$eisid
      names(choices) <- xx$site_name
      
      selectizeInput('search', NULL, width = '100%', choices = c('Choose facility'='', choices))
    }
  })

  emissions_update <- reactive({
    req(input$mapdata_year)

    emissions_for_map %>%
      filter(year == input$mapdata_year) %>%
      mutate(tooltip = paste0(site_name, '<br>',
                              year, ' emissions: ', round(total_emissions_epa, 1), ' lbs'))
  }) %>%
    bindCache(input$mapdata_year)

  cancer_update <- reactive({
    req(input$mapdata_year)

    cancer %>%
      filter(year == input$mapdata_year) %>%
      mutate(tooltip = paste0('Census tract #: ', geoid, '<br>',
                              'Est Exposure Concentration: ', round(ec_ppb, 5), ' ppb', '<br>',  
                              'Pt Cancer Risk: ', round(pt_cancer_, 1), ' per million'))

  }) %>%
    bindCache(input$mapdata_year)
  
  output$map <- renderMapdeck({ 
    token <- read_lines('assets/mapdeck-token.txt')
    mapdeck(style = mapdeck_style('dark'), token = token) %>%
      mapdeck_view(location = c(-100, 40), zoom = 4, pitch = 10)
  })
  
  observe({ 
    req(input$mapdata_year)
    req(cancer_update())
    
    mapdeck_update(map_id = 'map') %>%
      clear_polygon('cancer') %>%
      add_polygon(
        cancer_update(),
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
    req(emissions_update())
    
    mapdeck_update(map_id = 'map') %>%
      clear_scatterplot('facilities') %>%
      add_scatterplot(
        emissions_update(),
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
  
  observeEvent(input$searchSubmit, { 

    if (input$searchOptions == 'address') {
      result <- geocode_OSM(input$search)  
      df <- data.frame(query = result$query, 
                       latitude = result$coords[['y']], 
                       longitude = result$coords[['x']])
    } else if (input$searchOptions == 'facility') {
      df <- emissions_for_map %>% 
        filter(eisid == input$search) %>% 
        distinct(eisid, .keep_all = T) %>% 
        mutate(query = paste0(site_name, ', ', state)) %>% 
        select(query, latitude, longitude)
    } 

    if (nrow(df) == 0) { 
      showModal(modalDialog( 
        title = 'No results found', 
        "The API returned zero results. Please try a different address.", 
        easyClose = T, 
        footer = NULL))  
    } else { 
      
      args <- list('map', c(df$longitude, df$latitude), 12)
      js_args <- jsonify::to_json(args, unbox = T)

      session$sendCustomMessage(
        'move_cam',
        js_args
      )

      mapdeck_update(map_id = 'map') %>%
        clear_scatterplot('search') %>%
        add_scatterplot(
          data = df,
          lat = 'latitude',
          lon = 'longitude',
          radius = 100,
          fill_opacity = 0.5,
          stroke_width = 70,
          radius_min_pixels = 10,
          # focus_layer = T, # This works but there is no transition compared to the JS
          tooltip = 'query',
          palette = 'oranges',
          update_view = F,
          layer_id = 'search'
        )
    }

  })
 
  ## SUMMARY CHARTS -----------------------------------------------------------
  output$emissions_plot <- renderGirafe({
    req(input$site_name) 
    
    plotdata <- emissions_for_plot %>% 
      filter(site_name %in% input$site_name) %>% 
      mutate(tooltip = paste(round(emissions), 'lbs'))
    
    plt <- ggplot(plotdata, aes(x = year, y = emissions, color = site_name)) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = site_name), size = 3) + 
      geom_textline(aes(label = site_name), size = 3, vjust = -0.5, text_smoothing = 30) +
      scale_y_continuous(labels = scales::comma) + 
      labs(x = NULL, y = NULL, 
           title = 'Ethylene Oxide Emissions (in lbs) by Year by Facility in the US', 
           subtitle = 'Emissions data from EPA') +
      theme_bw() +
      theme(legend.position = 'none', 
            panel.border = element_rect(color = 'grey'), 
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            text = element_text(size = 12), 
            plot.title.position = 'plot')
    
    girafe(
      ggobj = plt, 
      options = list(opts_hover_inv(css = 'opacity: 0.3;'), 
                     opts_selection(type = 'single'))
    )
  })
  
  ## DATA TABLE -----------------------------------------------------------
  output$table <- renderReactable({
    site_cols <- c("site_name", "city", "state", "census_tract", "year")
    emissions_cols <- c("reported_emissions", "emissions_source") 
    cancer_cols <- c('est_pt_cancer', 'cancer_source') 
    
    make_color_pal <- function(colors, bias = 1) { 
      get_color <- colorRamp(colors, bias = bias) 
      function(x) rgb(get_color(x), maxColorValue = 255)
    }
    
    emissions_color <- make_color_pal(c("#D35400", "#F39C12", "#F5B041", "#F9E79F", "#EAFAF1"), bias = 3)
    
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
        census_tract = colDef(
          name = 'Census Tract'
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
  }) %>% 
    bindCache(input$year) # bindCache requires an input. This is just a fake one.
}