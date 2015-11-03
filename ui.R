
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
      selectInput('country', "Country", choices = NULL),
      numericInput('target_yr', 'Target Year', step = 1, value = 2050),
      numericInput('target_reduc', 'Emissions reduction (%)', min=0, max=100,
                   value = 80),
      numericInput('ref_yr', 'Reference Year', step = 1, value = 1990),
      htmlOutput('policy_goal'),
      br(),
      radioButtons("analysis", "Analysis:",
                   c("Bottom-up" = 'bottom.up', "Top-down" = 'top.down'),
                   inline = TRUE,
                   selected = 'bottom.up'),
      htmlOutput('trend_title'),
      tableOutput('trend_table'),
      htmlOutput('target_emissions')
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Trends", "Historical Trends",
                           inputPanel(
                            selectInput('trend_variable', 'Variable',
                                       choices = c('P', 'g', 'e', 'f', 'ef', 'G', 'E', 'F'),
                                       selected = 1),
                  textOutput('trend_display', inline = TRUE)
                           ),
                                           
                           fluidRow(
                             column(6,
                             ggvisOutput('trend_plot_ln')),
                             column(6,
                             ggvisOutput('trend_plot'))
                             )
                  ),
                  tabPanel("Calculations", "Calcuating Implied Decarbonization",
                           htmlOutput('step_1'),
                           tableOutput('step_1_table'),
                           htmlOutput('step_2'),
                           tableOutput('step_2_table'),
                           htmlOutput('step_3'),
                           tableOutput('step_3_table')
                           ),
                  tabPanel("Implied Decarbonization",
                           fluidRow(
                             column(6,
                                    h3('foo')
                             )
                           )),
                  tabPanel("Historical", tableOutput("historical_table"))
      )
    )
  )
))
