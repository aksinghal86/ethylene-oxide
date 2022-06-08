ui <- navbarPage(
  'EH&E Ethylene Oxide Dashboard', 
  id = 'nav', 
  
  ## INTERACTIVE MAP ---------------------------------------------------------
  tabPanel(
    'Interactive Map', icon = icon('map'), 
    
    # useShinyjs(), 
    # includeScript('script.js'),
    div( tags$head(includeCSS('styles.css')) ),
    
    div(
      class = 'outer',
      mapdeckOutput('map', width = '100%', height = '100%')
    ),
    
    
    #### Map Navigation Panel ####
    absolutePanel(
      id = 'controls', draggable = T, 
      left = 'auto', right = 20, bottom = 'auto', width = 250, height = 'auto', 
      
      h3('Map Tools', icon('map')), 
      # selectizeInput('mapdata_pollutant', label = NULL, choices = c('Select pollutant' = '', 'Ethylene Oxide')), 
      selectizeInput('mapdata_year', label = NULL, choices = c('Select year' = '', 2014, 2017))
      # selectizeInput('mapdata_series', label = NULL, choices = c('Select data series' = '')), 
      # radioButtons('mapdata_smry', label = NULL, choices = c("Per capita", "Total quantity"), selected = "Per capita", inline = T)
    )
  ), 
  
  ## SUMMARY CHARTS ----------------------------------------------------------
  tabPanel(
    'Summary Charts', icon = icon('chart-bar'),
    tags$h4("Plots for each of the data categories are provided in the tabs below. ", 
            "Select the appropriate tab and select options therein for the plot to be generated. "), 
    tags$h4("Plots can be downloaded by clicking on the download button that appears on the far top right corner when hovering above the plot."), 
    tags$br()
    
    # tabsetPanel(
    #   id = 'smrytabs', 
    #   
    #   productionUI('production', unique(production$series), unique(production$state)),
    #   co2EmissionsUI('co2_emissions', unique(co2_emissions$state))
    # )
  ), 
  
  ## ABOUT ---------------------------------------------------------------------
  tabPanel(
    'About', icon = icon('info'), 
    tags$h3("TEMPORARY PLACEHOLDER..."),
    tags$p('Created by a team of air quality junkies and data science nerds at Environmental Health & Engineering, Inc.'), 
    tags$p('The code for this dashboard is available on our', tags$a('Github', href='https://github.com/ehe-analytics/energy-dashboard'), 'page.')
  )
)