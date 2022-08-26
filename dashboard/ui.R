ui <- navbarPage(
  'Ethylene Oxide Dashboard', 
  id = 'nav', 
  
  ## INTERACTIVE MAP ---------------------------------------------------------
  tabPanel(
    'Interactive Map', icon = icon('map'), 
    
    # useShinyjs(), 
    # includeScript('script.js'),
    div( tags$head(includeCSS('www/styles.css')) ),
    
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
      selectizeInput('mapdata_year', label = NULL, choices = c('Select year' = '', 2014, 2017)), 
      tags$div(
        class = 'panelnote', style = 'font-size: 10px;', 
        tags$em('Note:'), 
        "For 2014 and 2017, cancer risks are directly from EPA's point risk estimates from NATA (2014) or Air Toxics Screeing (2017).", 
        "For the other years, they have been interpolated using methodology described in the ", tags$em('About'), " section."
      )
      # selectizeInput('mapdata_series', label = NULL, choices = c('Select data series' = '')), 
      # radioButtons('mapdata_smry', label = NULL, choices = c("Per capita", "Total quantity"), selected = "Per capita", inline = T)
    )
  ), 
  
  ## SUMMARY CHARTS ----------------------------------------------------------
  tabPanel(
    'Summary Charts', icon = icon('chart-bar'),
    tags$p("This tools allows for comparison in emissions for a single or multiple facility over the years. ", 
           "Please select the facilities in the dropdown menu below. "), 
    tags$p("Emissions provided in the TRI are largely used here since it's the most consistent and complete data set.", 
           "In cases where multiple data sources were avialable for a given year, e.g., NEI, then the maximum emissions are used. "),
    tags$p("Plots are interactive. Try hovering over a data point. At a later date, a link may be added to direct to the facilities website.",
           "The generated plot can also be downloaded by clicking on the download button that appears on the far top right corner when hovering above the plot."), 
    tags$br(),

    selectizeInput('site_name', NULL, 
                   choices = c('Select facilities' = '', sort(unique(emissions_for_plot$site_name))), 
                   multiple = T, width = '50vh'), 
    ggiraphOutput('emissions_plot')
    
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
    tags$p("Created by", tags$a("Ankur Singhal", href = 'https://github.com/aksinghal86'), "(Empirical Solutions Consulting, LLC), ", 
           "as a side project to track ethylene oxide emissions", 
           "reported by emitting facilities and the corresponding analysis done by EPA to estimate cancer rates as part of ", 
           tags$a("Air Toxics Screen", href = "https://www.epa.gov/AirToxScreen"), "(formerly named ", 
           tags$a("NATA", href = "https://www.epa.gov/national-air-toxics-assessment"), ")."), 
    tags$h4("Motivation") , 
    tags$p("EPA has so far conducted a NATA/Air Toxics Screen only for emissions data provided in 2014 and 2017. ", 
           "It is often useful for clients to estimate what the cancer risk may be based on emissions reported in ", 
           "the intervening years or what they may be based on emissions to be reported for the following year. ", 
           "This can help clients update existing technologies or new technologies to ensure that the nearby communities ", 
           "are at no additional cancer risk due to emissions from their facilities."),
    tags$h4("Methodology"), 
    tags$p("Estimating cancer risk for a census tract in the intermittent years specifically by a facility is a somewhat arduous task. ", 
           "While EPA reports total cancer risk and ", 
           "point cancer risk, i.e., due to a specific chemical like ethylene oxide, by census tract, it does not provide ", 
           "further resolution on contributors. For example, it is not possible (at least directly) to estimate cancer risk ", 
           "from a specific emitting facility since the information on the following is not readily available/feasible to acquire: ", 
           "1) which census tract the facility belongs to; and", 
           "2) relative contribution downwind in a census tract where there may be contributions from multiple sources."), 
    tags$p("There are multiple ways to get to the answer. One could use AERMOD and CMAQ, but that is an expensive proposition ",
           "that is not realistic. Consider that it does take EPA three years to do these evaluations. "), 
    tags$p("Here, an alternative (programmatic) approach was used. ", 
           "Issue 1 above was resolved by finding the closest distance between a census centroid and the facility location", 
           "and then bounded by a census tract to identify which census tract a facility belongs to. ", 
           "For point 2, an inverse-distance weighted (IDW) approach was used to estimate emissions downwind at a census tract centroid, ", 
           "from all ethylene oxide emitting sources nearby (defined as within 50 km). ", 
           "Lastly, a weighted approach (total emissions to point cancer risk estimated by EPA) from the 2014 and 2017 EPA analysis was used ", 
           "to estimate emissions for years in which no analysis was conducted by EPA. "),
    tags$h4("Future directions"), 
    tags$p("Updates on cancer risk will be made based on 2022 ethylene oxide emissions. ", 
           "A faster processing time is also desirable but that is not expected any time soon due to limited time. ", 
           "A detailed explanation of the methodology as a Medium post is in the works."),
    tags$p('The code for this dashboard is available on', tags$a('Github', href='https://github.com/ehe-analytics/energy-dashboard'), '.')
  )
)