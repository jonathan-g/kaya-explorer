
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)

shinyUI(fluidPage(

  # Application title
  titlePanel("Decarbonization Homework"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("analysis", "Analysis:",
                   c("Top-down" = 'top.down', "Bottom-up" = 'bottom.up'),
                   selected = 'bottom.up'),
      br(),
      selectInput('country', h3("Country"), choices = NULL),
      numericInput('ref_yr', 'Reference Year', step = 1, value = 1990),
      numericInput('target_yr', 'Target Year', step = 1, value = 2050),
      numericInput('target_reduc', 'Emissions reduction (%)', min=0, max=100,
                   value = 80),
      br(),
      htmlOutput('trend_title'),
      tableOutput('trend_table'),
      htmlOutput('target_emissions')
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Trends", "Historical Trends",
                           selectInput('trend_variable', 'Variable',
                                       choices = c('P', 'g', 'e', 'f', 'ef', 'G', 'E', 'F'),
                                       selected = 1),
                           fluidRow(
                             column(6,
                             ggvisOutput('trend_plot_ln')),
                             column(6,
                             ggvisOutput('trend_plot'))
                             )
                  ),
                  tabPanel("Historical", tableOutput("historical_table")),
                  tabPanel("Projected", tableOutput("projections")),
                  tabPanel("Implied Decarbonization",
                           fluidRow(
                             column(6,
                                    h3('foo')
                           )
                           ))
      )
    )
  )
))
