
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
  withMathJax(),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput('country', "Country", choices = NULL),
      numericInput('target_yr', 'Target year', step = 1, value = 2050),
      numericInput('target_reduc', 'Emissions reduction (%)', min=0, max=100,
                   value = 80),
      numericInput('ref_yr', 'Reference year for emissions reduction', step = 1, value = 1990),
      numericInput('trend_start_year', 'Calculate trends starting in',
                   step = 1, value = 1980),
      htmlOutput('policy_goal'),
      br(),
      radioButtons("analysis", "Analysis:",
#                   c("Bottom-up" = 'bottom.up', "Top-down" = 'top.down'),
                   c("Bottom-up" = 'bottom.up'),
                   inline = TRUE,
                   selected = 'bottom.up'),
      htmlOutput('trend_title'),
      tableOutput('trend_table'),
      htmlOutput('target_emissions')
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Trends", h3("Historical Trends"),
                           fluidRow(
                             column(2,
                             selectInput('trend_variable', 'Variable',
                                         choices = c('P', 'g', 'e', 'f', 'ef', 'G', 'E', 'F'),
                                         selected = 1)),
                             column(10,
                             br(),
                             htmlOutput('trend_display', inline = TRUE)
                             )
                           ),

                           fluidRow(
                             column(6,
                                    span(h4(strong(textOutput('trend_plot_ln_title'))),
                                         style="text-align:center;"),
                                    div(
                                    ggvisOutput('trend_plot_ln')
                                    )
                             ),
                             column(6,
                                    span(h4(strong(textOutput('trend_plot_title'))),
                                         style="text-align:center;"),
                                    div(
                                    ggvisOutput('trend_plot')
                                    )
                             )
                           )
                  ),
                  tabPanel("Calculations", h3("Calcuating Implied Decarbonization"),
                           br(),
                           checkboxInput('calc_show_answers', "Show answers", value = FALSE),
                           htmlOutput('step_1'),
                           tableOutput('step_1_table'),
                           htmlOutput('step_2'),
                           uiOutput('step_2_formula'),
                           tableOutput('step_2_table'),
                           htmlOutput('step_3'),
                           tableOutput('step_3_table'),
                           htmlOutput('step_4'),
                           tableOutput('step_4_table'),
                           htmlOutput('step_5'),
                           htmlOutput('step_6')
                  ),
                  tabPanel("Implied Decarbonization",
                           ggvisOutput('implied_decarb_plot')
                           ),
                  tabPanel("Historical", tableOutput("historical_table"))
      )
    )
  )
))
