library(shiny)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(purrr)
library(plotly)

# read in data from Github
df <- read_csv('https://raw.githubusercontent.com/JeremyOBrien16/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv')

# remove ICD.Chapter categories: miscellaneous (mostly empty, no insight) and ear (mostly empty)
df <- df %>% 
  filter(!str_detect(ICD.Chapter, '^Codes.|.ear.'))

# rename ICD.Chapter categories for better fit within selector and chart title
df$ICD.Chapter <- as.factor(df$ICD.Chapter)
levels(df$ICD.Chapter) <- c(
  'Conditions from perinatal period',
  'Infectious / parasitic diseases',
  'Congenital malformations / deformations / chromosomal abnormalities',
  'Blood / blood-forming organ / immune diseases',
  'Circulatory system diseases',
  'Digestive system diseases',
  'Genitourinary system diseases',
  'Muscoluskeltal system / connective tissue diseases',
  'Nervous system diseases',
  'Respiratory system diseases',
  'Skin / subcutaneous tissue diseases',
  'Endocrine / nutritional / metabolic diseases',
  'External causes of morbidity / mortality',
  'Mental / behavioral disorders',
  'Neoplasms',
  'Pregnancy / childbirth / puerperium',
  'Other symptoms / signs / abnormal clinical-laboratory findings'
)

# in future iteration:
# consider fixing range for legend so color does fluctuate by numeric value between selections
# center footer text under plot
# add in ethnicity as selector

ui <- fluidPage(
  
  titlePanel('Annual Mortality Rates by State'),
  
  sidebarLayout(
    
    sidebarPanel(

      h5(tags$em('Compare mortality rates from'), 
         tags$a(tags$em('CDC Wonder'), href = 'https://wonder.cdc.gov/ucd-icd10.html'),
         tags$em('for specific causes between states in the year 2010.')),
      
      selectInput(inputId = 'cause', 
                  label = 'Cause:', 
                  choices = unique(df$ICD.Chapter), 
                  selected = 'Neoplasms'),
      
      sliderInput(inputId = 'year', 
                  label = 'Year:', 
                  min = 1999, 
                  max = 2010,
                  ticks = FALSE, # remove all ticks due to nonsensical mid-integer values
                  value = 2010,
                  sep = ''),
      
      actionButton('button', 'Chart'),
      
      width = 3),
    
    mainPanel(
      
      plotlyOutput('map'),
      
      br(),
      h6(tags$em('Crude rates are expressed as the number of deaths reported each calendar year by the selected cause, where crude rate = count / population * 100,000')),

      width = 9)
    )
)
                   
server <- function(input, output) {
  
  df_selected <- eventReactive(input$button, {
    
      df %>%
      select(State, ICD.Chapter, Year, Crude.Rate) %>% 
      filter(ICD.Chapter == input$cause, Year == input$year) %>% 
      group_by(State) %>% 
      arrange(Crude.Rate)
    },
    ignoreNULL = FALSE # create starting plot based on default inputs at start (Neoplasms, 2010)
    )  
  
  output$map <- renderPlotly({
      
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),  # set map type
        lakecolor = toRGB('white')
        )
      
      l <- list(color = toRGB("white"), width = 2)  # setting for white borders between states for cleaner look
      
      p <- plot_geo(data = df_selected(),
                    locationmode = 'USA-states') %>% 
        add_trace(
          z = ~Crude.Rate,
          locations = ~State, # key df variable to two-letter state code in map
          color = ~Crude.Rate,  # variable colored based on
          colors = 'Blues',  # color scheme
          marker = list(line = l)) %>%  # create white borders
        colorbar(title = '') %>% 
        layout(title = isolate({ paste(input$cause, 'in', input$year) }), # prevent title from updating without button being pushed
          geo = g)
  
    })
}

shinyApp(ui = ui, server = server)