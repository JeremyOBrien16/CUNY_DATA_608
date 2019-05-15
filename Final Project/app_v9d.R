# knitr::opts_knit$set(root.dir =  "C:/Users/jlobr/OneDrive/Learning/_CUNY_SPS_MSDS/2019_1_Spring/DATA 608/Github Repo/CUNY_DATA_608/Final Project/Source Data&Info")

# REBUILD AS PROJECT FILE WITH REQUISITE DIRECTORIES

# [RECONFIGURE AS REQUIRE / INSTALL]

# BUILD PLACEHOLDER FOR FUTURE FUNCTIONALITY OF AUTOMATIC DATA UPDATES

library(shiny)
library(shinydashboard)
library(devtools)
install_github("nik01010/dashboardthemes") # [IN BETA, NEEDS TO BE DLOADED: https://github.com/nik01010/dashboardthemes]
library(dashboardthemes)
# library(semantic.dashboard)  # [WARNING: CAUSES CONFLICTS WITH COLUMN WIDTH]
library(shinyWidgets)
library(shinycssloaders)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(scales)
library(DT)
# library(stringr)
library(lubridate)
# library(magrittr)
library(ggplot2)
# library(purrr)
library(plotly)
library(dygraphs)
library(xts)
# library(datapasta)


# Github-hosted data calls
Homicides <- "https://raw.githubusercontent.com/JeremyOBrien16/CUNY_DATA_608/master/Final%20Project/data/Annual%20Firearm%20Homicides%20by%20State%2C%201999-2017.txt"
Suicides <- "https://raw.githubusercontent.com/JeremyOBrien16/CUNY_DATA_608/master/Final%20Project/data/Annual%20Firearm%20Suicides%20by%20State%2C%201999-2017.txt"
Checks <- "https://raw.githubusercontent.com/JeremyOBrien16/CUNY_DATA_608/master/Final%20Project/data/nics-firearm-background-checks.csv"

# Read in and organize homicide data table
Homi_data <- read_tsv(Homicides)
# Homi_data$Year <- parse_date_time(Homi_data$Year, 'Y')  # reassert for dygraph?
Homi_data <- Homi_data %>%
  dplyr::filter(!is.na(State)) %>%
  dplyr::filter(!is.na(Year)) %>%
  dplyr::select(State, Year, Deaths, Population) %>% 
  dplyr::rename(Homicides = Deaths, State_name = State)

# Read in and organize suicide data table
Suic_data <- read_tsv(Suicides)
# Suic_data$Year <- parse_date_time(Suic_data$Year, 'Y')  # reassert for dygraph?
Suic_data <- Suic_data %>%
  dplyr::filter(!is.na(State)) %>%
  dplyr::filter(!is.na(Year)) %>%
  dplyr::select(State, Year, Deaths, Population) %>% 
  dplyr::rename(Suicides = Deaths, State_name = State) 

# Join homicide and suicide data in single table
Death_data <- Homi_data %>% 
  left_join(Suic_data)
Death_data$State <- state.abb[match(Death_data$State_name, state.name)] %>% 
  replace_na('DC')
Death_data <- Death_data %>% 
  dplyr::select(State, Year, Population, Homicides, Suicides) %>% 
  mutate(Homicides = replace_na(as.numeric(Homicides), 0), 
         Suicides = replace_na(as.numeric(Suicides), 0))

# Read in background check data table
Check_data <- read_csv(Checks)
Check_data <- Check_data %>%
  dplyr::filter(!state %in% c('Guam', 'Mariana Islands', 'Virgin Islands')) %>% 
  dplyr::rename(State_name = state)
Check_data$State <- state.abb[match(Check_data$State_name, state.name)] %>% 
  replace_na('DC')
# assert as data with parse_date_time(, 'Y')
Check_data$Year <- as.numeric(str_extract_all(Check_data$month, '\\d{4}', simplify = TRUE))
Check_data_concise <- Check_data %>% 
  dplyr::filter(Year %in% 1999:2017) %>% 
  dplyr::select(Year, State, totals) %>% 
  dplyr::rename(Registrations = totals) %>% 
  group_by(State, Year) %>% 
  summarize_all(list(sum))

# [ADDRESS KENTUCKY REGISTRATIONS]

df <- Death_data %>% 
  left_join(y = Check_data_concise, by = c('State', 'Year')) %>% 
  dplyr::filter(!is.na(State)) %>% 
  dplyr::filter(!is.na(Year)) 

# [ADD IN MASS SHOOTING YEAR / MONTH / GEO / COUNT DATA


# Create front-end
ui <- dashboardPage(
  
  # Create header
  dashboardHeader(
    title = 'Gun Violence'# ,  # provisional title
    # titleWidth = 700  # provisional width
    # add html for tab title
  ),  # end dashboardHeader
  
  # Create sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem('tabBox',
               tabName = 'maps',
               icon = icon('map')
      )  # end menuItem
    )  # end sidebarMenu
    
    # [ADD ADDITIONAL ELEMENTS TO SIDEBAR]
    # [CONSIDER HOW TO USE SIDEBAR]
    
  ),  # end dashboardSiderbar
  
  
  # Set up boxes
  dashboardBody(
    
    shinyDashboardThemes(
      theme = 'grey_light'
    ),
    
    tags$head(
      tags$style(
        
        # Bold box titles
        HTML('
             h3  {
             font-weight: bold;
             }
             ')  # end HTML
        
        )  # end tags$style
    
      ),  # end tags$head
    
    # [ADD CSS STYLE SHEET]
    # https://community.rstudio.com/t/including-css-file-in-r-using-shiny-dashboard/2270/1
    # http://shiny.rstudio.com/articles/css.html
    # http://shiny.rstudio.com/articles/css.html
    
    tabItems(
      
      tabItem(tabName = 'maps',
              
              fluidRow(
                
                column(width = 8,
                       
                       
                       # Create dygraph (in place of conventional slider)                       
                       box(
                         title = '',
                         width = NULL,
                         
                         withSpinner(dygraphOutput('dygraph', height = '100px'))
                         
                       ),  # end box
                       
                       
                       # Create map box
                       tabBox(
                         title = 'Maps',
                         width = NULL,
                         
                         # [CONSIDER CREATING SIDE-BY-SIDE MAPS]
                         
                         
                         # Create homicides map tab    
                         tabPanel(
                           title = 'Homicides',
                           value = 'Homicides',  # value parameter is required for tabPanel
                           id = NULL, 
                           ## height = '250px',
                           width = NULL,
                           status = 'primary',
                           ## solidHeader = TRUE,
                           
                           withSpinner(plotlyOutput('homi_map'))
                           
                           ## [ADD IN DESCRIPTIVE TITLE / YEAR LABEL]
                         ), # end tabPanel 
                         
                         
                         # Create suicides map tab
                         tabPanel(
                           title = 'Suicides',
                           value = 'Suicides',  # value parameter is required for tabPanel
                           id = NULL,
                           width = NULL,
                           ##status = 'primary',
                           
                           withSpinner(plotlyOutput('suic_map'))
                           
                         ),  # end tabPanel
                         
                         
                         # Create registrations map tab
                         tabPanel(
                           title = 'Registrations',
                           value = 'Checks',  # value parameter is required for tabPanel
                           id = NULL,
                           width = NULL,
                           ##status = 'primary',
                           withSpinner(plotlyOutput('regi_map'))
                           
                         )  # end tabPanel
                         
                       ),  # end tabBox
                       
                       # [BRAINSTORM ON JOINT HOMI-SUIC-CHECK VISUALIZATION]
                       
                       
                       # Create time slide selector box
                       tabBox(
                         title = 'Time slider',
                         width = NULL,
                         #solidHeader = FALSE,
                         
                         # Create slider
                         sliderInput(inputId = 'year', 
                                     label = 'Year:', 
                                     min = 1999, 
                                     max = 2017,
                                     ticks = FALSE, # remove all ticks due to nonsensical mid-integer values
                                     value = c(1999,2017),  # swapped to c(1999,2017) when running totals across years implemented
                                     sep = '') #,
                         
                         # withSpinner(plotOutput('slider_barchart', height = 100)),
                         
                       )  # end box
                       
             ),  # end column
             
                column(width = 4,
                       
                       
                       # Create top-ten box
                       tabBox(
                         title = 'Top States by Gun Deaths',
                         width = NULL,
                         # solidHeader = TRUE,  # [LOOK INTO TABSETPANEL HEADER PARAMS]
                         # status = 'primary',  # [TABSETPANEL HAS NO STATUS PARAM?]
                         
                         # [CONSIDER TYING TAB SELECTION OF MAP AND LIST]
                         
                         
                         # Create homicides top-ten list tab
                         tabPanel(
                           title = 'Homicides',
                           value = 'Homicides',  # value parameter is required for tabPanel
                           id = NULL, 
                           ## height = '250px',
                           width = NULL,
                           status = 'primary',
                           ## solidHeader = TRUE,
                           
                           withSpinner(DT::dataTableOutput('homi_table'))
                           
                           ## [ADD IN DESCRIPTIVE TITLE / YEAR LABEL]
                         ), # end tabPanel 
                         
                         
                         # Create suicides top-ten list tab
                         tabPanel(
                           title = 'Suicides',
                           value = 'Suicides',  # value parameter is required for tabPanel
                           id = NULL,
                           width = NULL,
                           ##status = 'primary',
                           
                           withSpinner(DT::dataTableOutput('suic_table'))
                           
                         ),  # end tabPanel
                         
                         
                         # Create registrations top-ten list tab
                         tabPanel(
                           title = 'Registrations',
                           value = 'Checks',  # value parameter is required for tabPanel
                           id = NULL,
                           width = NULL,
                           ##status = 'primary',
                           
                           withSpinner(DT::dataTableOutput('regi_table'))
                           
                         )  # end tabPanel
                         
                       ),  # end tabBox
                       
                       
                       # Test-box for dygraph output
                       box(
                         title = 'Dygraph output test',
                         width = NULL,
                         # solidHeader = TRUE,  # [LOOK INTO TABSETPANEL HEADER PARAMS]
                         # status = 'primary',  # [TABSETPANEL HAS NO STATUS PARAM?]
                         
                         div(strong('From: ')), textOutput("from", inline = TRUE),
                         div(strong('To: ')), textOutput("to", inline = TRUE)
                         
                       ),  # end tabBox
                       
                       
                       # Create proportion box
                       box(title = 'Pie Chart',
                           width = NULL,
                           status = 'primary',
                           
                           withSpinner(plotOutput('piechart'))
                           
                       )  # end box
                       
                )  # end column
             
             )  # end fluidRow
             
      )  # end tabItem
      
    )  # end tabItems
    
  )  # end dashboardBody
  
)  # end dashboardPage


# Create back-end                   
server <- function(input, output) {
  
  # Subset dataset based on years selected
  year_selected <- reactive({
    
    req(input$year)
    df %>% 
      dplyr::filter(Year %in% input$year[1]:input$year[2]) %>%
      # dplyr::filter(Year %in% input$from:input$to) %>% 
      group_by(State) %>% 
      summarize_all(list(sum)) %>% 
      mutate(Homi_percap = Homicides / Population, 
             Suic_percap = Suicides / Population,
             Regi_percap = Registrations / Population) %>% 
      dplyr::select(State, 
                    Homicides, Homi_percap, 
                    Suicides, Suic_percap, 
                    Registrations, Regi_percap)
    
    # [ADD IN STEPS TO INCLUDE CHECK COUNTS]
    
  })
  
  # ignoreNULL = FALSE  # create starting plot based on default inputs (TBD)

  
  # Output lower and upper bounds of dygraph range
  # output$from <- renderText({
        # strftime(req(input$dygraph_date_window[[1]]), "%d %b %Y")      
  # })
  # output$to <- renderText({
    # strftime(req(input$dygraph_date_window[[2]]), "%d %b %Y")
  # })
  
  values <- reactiveValues()    
  
  limits <- debounce(reactive({
    if (!is.null(input$dygraph_date_window)) {
      rangewindow <- strftime(input$dygraph_date_window[[1]], '%Y-%m-%d')
      from <- rangewindow[1]
      to <- rangewindow[2]
    } else {
      from <- '1999-01-01'
      to <- '2017-01-01'
    }
    list(from = from,
         to = to)
  }), 1000)

  output$from <- renderText({ limits()[['from']] })
  output$to <- renderText({ limits()[['to']] })
  
  # toggle this out of the scope of the RenderPlotly so it feeds both homi and suic maps  
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),  # set map type
    lakecolor = toRGB('white')
    )
  
  l <- list(color = toRGB("white"), 
            width = 2)  # setting for white borders between states for cleaner look
  
  
  # Create state-level map of gun homicides based on selected time period
  output$homi_map <- renderPlotly({
    
    # Sys.sleep(2)  # included for testing purposes
    
    p <- plot_geo(data = year_selected(),
                  locationmode = 'USA-states') %>% 
      
      # [FIX UNIT OF MEASURE (I.E. NOT $MU$) AND ADJUST PROP DECIMAL ROUNDING]
      
      add_trace(
        z = ~Homi_percap,
        locations = ~State, # key df variable to two-letter state code in map
        color = ~Homi_percap,  # variable colored based on
        colors = 'Reds',  # color scheme
        zmin = 22, zmax = 6,  # fix colorscale
        
        # https://community.plot.ly/t/fixed-colorscale-chloropleth-map/1239
        
        # [DEAL W/ NA STATES THROUGH GREYING?]
        
        marker = list(line = l)) %>%  # create white borders
      colorbar(title = '') %>% 
      layout(geo = g)
      # layout(title = isolate({ paste(input$cause, 'in', input$year) }), # prevent title from updating without button being pushed
             # geo = g)
    
        # [LOCK COLOR RANGE SO CONSISTENT BETWEEN YEARS]    
    
      # [ADD MARKERS FOR LAT-LONG COORDS]
      # https://stackoverflow.com/questions/39729852/coordinates-based-segments-in-plotly-map-start-from-the-center
      # https://plot.ly/r/scatter-plots-on-maps/
      # Approx range of US lats is 19.5 to 64.9 US longs is -161.8 to -68.0
      # [ADD ICONS TO REPRESENT POIS]
    
  })

  
  # Create state-level map of gun suicides based on selected time period
  output$suic_map <- renderPlotly({

    p <- plot_geo(data = year_selected(),
                  locationmode = 'USA-states') %>% 
      add_trace(
        z = ~Suic_percap,
        locations = ~State, # key df variable to two-letter state code in map
        color = ~Suic_percap,  # variable colored based on
        colors = 'Blues',  # color scheme
        marker = list(line = l)) %>%  # create white borders
      colorbar(title = '') %>% 
      layout(geo = g)
    # layout(title = isolate({ paste(input$cause, 'in', input$year) }), # prevent title from updating without button being pushed
    # geo = g)
    
  })
  
  # [CHANGE TO PLOTLY BARCHART?]
  # [ALIGN SLIDER DATES W/ VERTICAL BARS]
  # [IN PLACE OF SLIDER, CONSIDER BRUSH WITH ONLY HORIZONTAL INTERACTION ON GGLOT BAR CHART]
  # [GREY CHART YEARS NOT SELECTED IN SLIDER]
  # https://shiny.rstudio.com/articles/plot-interaction.html
  
  
  # Create state-level map of gun checks based on selected time period
  output$regi_map <- renderPlotly({
    
    p <- plot_geo(data = year_selected(),
                  locationmode = 'USA-states') %>% 
      add_trace(
        z = ~Regi_percap,
        locations = ~State, # key df variable to two-letter state code in map
        color = ~Regi_percap,  # variable colored based on
        colors = 'Blues',  # color scheme
        marker = list(line = l)) %>%  # create white borders
      colorbar(title = '') %>% 
      layout(geo = g)
    # layout(title = isolate({ paste(input$cause, 'in', input$year) }), # prevent title from updating without button being pushed
    # geo = g)
    
  })
  
  
  ## [CHANGE TO PLOTLY BARCHART?]
  ## [ALIGN SLIDER DATES W/ VERTICAL BARS]
  ## [IN PLACE OF SLIDER, CONSIDER BRUSH WITH ONLY HORIZONTAL INTERACTION ON GGLOT BAR CHART]
  ## [GREY CHART YEARS NOT SELECTED IN SLIDER]
  ## https://shiny.rstudio.com/articles/plot-interaction.html

  # Create trend bar chart to parallel year slider selector
  output$slider_barchart <- renderPlot({

    # Sys.sleep(2)  # included for testing purposes
        
    df %>% 
      dplyr::select(Year, Homicides, Suicides) %>% 
      group_by(Year) %>% 
      # summarize(Homicides = sum(Homicides), 
                # Suicides = sum(Suicides)) %>% # calculate homicides / suicides across all states
      gather(key = "Type", value = "Counts", 2:3) %>% 
      
      ggplot(aes(x = Year, y = Counts, fill = Type)) +
        geom_bar(stat = 'identity') +
        labs(title = '', 
             x = '', 
             y = '') +
      theme_minimal() +
      theme(axis.text.y = element_blank(),  # Remove axis labels, annotating bar ends instead as eye is drawn there
      axis.ticks = element_blank(),  # Remove chrome / cruft
      legend.position = 'none')
    
    # [ADDRESS BORDERS GOING BLACK WHEN ZERO VALUE IN NEIGHBORING STATES]
    
  })

  
  # Create top-ten states by gun homicides table based on selected period
  output$homi_table <- DT::renderDataTable({
    
    # Sys.sleep(2)  # included for testing purposes
    
    table_length = 10
    
    DT::datatable(
      year_selected() %>% 
      mutate(`%Pop` = scales::percent(Homi_percap, accuracy = .001)) %>% 
      dplyr::rename(Deaths = Homicides) %>% 
      dplyr::select(State, `%Pop`, Deaths) %>% 
      mutate(Deaths = scales::comma(Deaths)) %>% 
      arrange(desc(`%Pop`)) %>% 
      head(n = table_length), 
    
    # [SORT OUT MISSING STATE LABELS]
    # [IMPLEMENT AS TAB IN BOX]
    # [FIT TO TAB WIDTH]
    # [ADJUST DT LOOK - NO LIGHT / DARK, FONT, COL WIDTH]
    # [RIGHT-JUSTIFY %POP]
    
    # for adjusting display options
    # https://rstudio.github.io/DT/options.html
    # https://datatables.net/reference/option/
    # https://shiny.rstudio.com/gallery/datatables-options.html
    # https://stackoverflow.com/questions/31486738/how-do-i-suppress-row-names-when-using-dtrenderdatatable-in-r-shiny
    options = list(pageLength = table_length,  # sets DT rows per page to same as rows returned by year_selected
                 lengthChange = FALSE, # removes user option to choose rows returned in DT dropdown
                 #pageLength = -1,
                 bPaginate = FALSE,  # removes pagination of DT i.e. 'next' button
                 bInfo = FALSE,  # removes DT entry count annotation
                 searching = FALSE),  # removes DT search bar
            
    rownames = FALSE  # removes row names i.e. index counts
    )
    })

    
  # Create top-ten states by gun suicides table based on selected time period
  output$suic_table <- DT::renderDataTable({
    
    # Sys.sleep(2)  # included for testing purposes
    
    table_length = 10
    
    DT::datatable(
      year_selected() %>% 
        mutate(`%Pop` = scales::percent(Suic_percap, accuracy = .001)) %>% 
        dplyr::rename(Deaths = Suicides) %>% 
        dplyr::select(State, `%Pop`, Deaths) %>% 
        mutate(Deaths = scales::comma(Deaths)) %>% 
        arrange(desc(`%Pop`)) %>% 
        head(n = table_length), 
      
      # [SORT OUT MISSING STATE LABELS]
      # [IMPLEMENT AS TAB IN BOX]
      # [FIT TO TAB WIDTH]
      # [ADJUST DT LOOK - NO LIGHT / DARK, FONT, COL WIDTH]
      # [RIGHT-JUSTIFY %POP]
      
      options = list(pageLength = table_length,  # sets DT rows per page to same as rows returned by year_selected
                     lengthChange = FALSE, # removes user option to choose rows returned in DT dropdown
                     #pageLength = -1,
                     bPaginate = FALSE,  # removes pagination of DT i.e. 'next' button
                     bInfo = FALSE,  # removes DT entry count annotation
                     searching = FALSE),  # removes DT search bar
      
      rownames = FALSE  # removes row names i.e. index counts
    )
  })

    
  # Create top-ten states by gun registrations table based on selected time period
  output$regi_table <- DT::renderDataTable({
    
    # Sys.sleep(2)  # included for testing purposes
    
    table_length = 10
    
    DT::datatable(
      year_selected() %>% 
        mutate(`%Pop` = scales::percent(Regi_percap, accuracy = .1)) %>% 
        dplyr::select(State, `%Pop`, Registrations) %>% 
        mutate(Registrations = scales::comma(Registrations)) %>% 
        arrange(desc(`%Pop`)) %>% 
        head(n = table_length), 
      
      # [SORT OUT MISSING STATE LABELS]
      # [IMPLEMENT AS TAB IN BOX]
      # [FIT TO TAB WIDTH]
      # [ADJUST DT LOOK - NO LIGHT / DARK, FONT, COL WIDTH]
      # [RIGHT-JUSTIFY %POP]
      
      options = list(pageLength = table_length,  # sets DT rows per page to same as rows returned by year_selected
                     lengthChange = FALSE, # removes user option to choose rows returned in DT dropdown
                     #pageLength = -1,
                     bPaginate = FALSE,  # removes pagination of DT i.e. 'next' button
                     bInfo = FALSE,  # removes DT entry count annotation
                     searching = FALSE  # removes DT search bar
                     # columnDefs = list(  # right justifies third column
                       # list(className = 'dt-right', targets = 3)) 
                     ), 
      
      rownames = FALSE  # removes row names i.e. index counts
      )
  })
  
  
  # Create pie chart of homicides vs. suicides vs. mass shooting deaths based on selected time period
  output$piechart <- renderPlot({
    
    # Sys.sleep(2)  # included for testing purposes
    
    # https://www.datanovia.com/en/blog/how-to-create-a-pie-chart-in-r-using-ggplot2/
    # [UPDATE TO REFLECT YEAR_SELECTED()]
    
    df %>% 
      dplyr::filter(Year == input$year) %>%
      dplyr::select(Homicides, Suicides) %>% # SUBTRACT MASS SHOOTING DEATHS FROM HOMI / SUIC AND MAKE SEPARATE PIE SLICE
      summarize(Homicides = sum(Homicides), Suicides = sum(Suicides)) %>% 
      gather(key = "Type", value = "Counts", 1:2) %>% 
      mutate(Prop = Counts / sum(Counts), Lab_Ypos = cumsum(Prop) - .5 * Prop) %>% 
      
      ggplot(aes(x = '', y = Counts, fill = Type)) +
      geom_bar(stat = 'identity', color = 'white') +
      coord_polar('y', start = 0) +
      geom_text(aes(y = Lab_Ypos, label = Prop), color = 'white')
      # scale_fill_manual(values = '')
    
  })
  
  
  # Create pie chart of homicides vs. suicides vs. mass shooting deaths based on selected time period
  output$dygraph <- renderDygraph({
    
    # Organize data for conversion to extensible time series format
    dygraph_df <- df %>%
      dplyr::select(Year, Homicides, Suicides) %>% 
      group_by(Year) %>%
      summarise_all(sum)
    
    # Convert Year to time series object
    dygraph_df$Year <- parse_date_time(dygraph_df$Year, 'Y')  # reassert for dygraph?
    dygraph_xts <- xts(x = dygraph_df[, -1], order.by = dygraph_df$Year)
    
    # [OUTPUT SELECTED RANGE AND CONVERT TO AS.NUMERIC(YEAR)]
    # [SHRINK HEIGHT OF CHART SO MAJORITY IS RANGE SELECTOR]
    # [STANDARDIZE Y-AXIS WITH COMMAS? REMOVE MONTHS FROM X-AXIS?]
    # [ADJUST COLORS AND FONTS FOR CONSISTENCY]
    # [REMOVE LEGEND AND BASE ON COLOR]
    # [ADJUST TAB TITLE AND REMOVE FROM CHART]
    # [ADJUST GRIDLINES]
    
    dyBarChart <- function(dygraph) {
      dyPlotter(dygraph = dygraph,
                name = "BarChart",
                path = system.file("examples/plotters/barchart.js", 
                                   package = "dygraphs"))
    }

      dygraph(dygraph_xts) %>% 
        # dyBarChart() %>% 
        dyRangeSelector(height = 30) %>% 
        dySeries('Homicides', fillGraph = TRUE, color = 'red') %>% 
        dySeries('Suicides', fillGraph = TRUE, color = 'blue') %>%
        dyAxis("x", drawGrid = FALSE) %>%
        dyAxis("y") %>%
        dyLegend(show = "follow") %>% 
        dyOptions(
          # stepPlot = TRUE,
          includeZero = TRUE, 
          gridLineColor = "gray",
          stackedGraph = TRUE,
          retainDateWindow = TRUE
          # drawGrid = input$showgrid,
          # colors = RColorBrewer::brewer.pal(3, "Set2")  # set color in line with design
          )
    
  })
  
}

# Data references for linkage
# Mass shootings:  https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/
# See links in proposal

# Other references
# Inputting slider range: https://stackoverflow.com/questions/38181744/r-shiny-input-slider-range-values
# Shiny dashboard themes: https://github.com/nik01010/dashboardthemes
# Shiny widgets: http://shinyapps.dreamrs.fr/shinyWidgets/
# DT:  https://rstudio.github.io/DT/
# dt: https://shiny.rstudio.com/articles/datatables.html
# dygraphs: https://rstudio.github.io/dygraphs/index.html
# dygraphs: https://www.r-graph-gallery.com/316-possible-inputs-for-the-dygraphs-library/
# dygraph plotter: https://rstudio.github.io/dygraphs/gallery-custom-plotters.html
# dygraph barchart: https://stackoverflow.com/questions/49440484/difficulty-reproducing-stacked-bar-graph-in-r-using-dygraphs
# dygraph options: http://dygraphs.com/options.html
# dygrapf ticker: https://stackoverflow.com/questions/8481437/how-to-set-specific-y-axis-label-points-in-dygraphs
# xts: https://s3.amazonaws.com/assets.datacamp.com/blog_assets/xts_Cheat_Sheet_R.pdf


# Deploy app
shinyApp(ui = ui, server = server)

