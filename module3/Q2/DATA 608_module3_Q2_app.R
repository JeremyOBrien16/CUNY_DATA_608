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
# center footer text under plot

ui <- fluidPage(
  
  headerPanel('Mortality Rates: State vs. National'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h5(tags$em('Compare mortality rates from'), 
         tags$a(tags$em('CDC Wonder'), href = 'https://wonder.cdc.gov/ucd-icd10.html'),
        tags$em('for specific causes against the national average between 1999 and 2010.')),
      
      selectInput(inputId = 'cause', 
                  label = 'Cause', 
                  choices = unique(df$ICD.Chapter), 
                  selected='Neoplasms'),
      
      selectInput(inputId = 'state', 
                  label = 'State', 
                  choices = unique(df$State), 
                  selected='NY'),
      
      width = 3),
      
    mainPanel(
      
      htmlOutput(outputId = 'selection_summary'),
      
      plotOutput(outputId = 'barchart'),

      br(),
      h6(tags$em('Crude rates are expressed as the number of deaths reported each calendar year by the selected cause, where crude rate = count / population * 100,000')),
      
      width = 9
            
    )
  )
)

server <- function(input, output) {
  
  output$selection_summary <- renderText({
    
    paste('<b>Cause of death:</b>', input$cause, '<br>', '<b>State:</b>', input$state)
    
  })
  
  output$barchart <- renderPlot({
  
    # calculate national trend for selected cause of death
    national_trend <- df %>%
      filter(ICD.Chapter == input$cause) %>%
      group_by(Year) %>% 
      summarize(Natl.Avg = sum(Deaths) / sum(Population)) %>% 
      arrange(Year)
    
    # calculate state trend for selected state and cause of death
    state_trend <- df %>% 
      filter(ICD.Chapter == input$cause & State == input$state) %>% 
      select(Year, Deaths, Population) %>% 
      group_by(Year) %>% 
      mutate(State.Avg = Deaths / Population) %>% 
      select(Year, State.Avg)
      
    ggplot() +
      
      # plot state and national as separate geoms on same chart
      geom_line(data = national_trend,
                aes(x = Year, 
                    y = Natl.Avg, 
                    color = 'National Average'), # hack color label so reads jointly in legend with state df
                size = 2) + 
      geom_line(data = state_trend, 
                aes(x = Year, 
                    y = State.Avg, 
                    color = paste(input$state, 'Average')),  # hack color label so reads jointly in legend with national df
                size = 2) +
      
      scale_y_continuous(labels = scales::percent_format(accuracy = .001)) +  # adjust vertical axis number formatting for cleaner read
      scale_x_continuous(minor_breaks = seq(1999, 2010, by = 1),  # set horizontal axis based on date range
                         breaks = seq(1999, 2010, by = 1)) +
      labs(title = '',  # chart title replaced with selection_summary
           x = '',
           y = '') +
      theme_minimal() +
      
      # adjust size and angle of axis and legend text
      theme(axis.text.x = element_text(size = 12, angle = 45),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size = 12)) +

      # set colors
      scale_color_manual('', values = c('gray', 'dodgerblue3')) +
      scale_fill_manual('', values = c('white', 'dodgerblue3'))
    
  })
  
}

shinyApp(ui = ui, server = server)