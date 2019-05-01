
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
# library(V8)
library(shinyjs)
library(DT)
library(plotly)

enable_answers <- TRUE
enable_topdown <- FALSE


jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=\"' + name + '\"]');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=\"' + name + '\"]');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
  background-color: #fff !important;
  color: #aaa !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"

shinyUI(fluidPage(

  # Application title
  titlePanel("Decarbonization Explorer"),
  withMathJax(),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jscode),
  shinyjs::inlineCSS(css),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput('region', "Country/Region", choices = NULL),
      numericInput('target_yr', 'Target year', step = 1, value = 2050),
      numericInput('target_reduc', 'Emissions reduction (%)', max=100,
                   value = 80),
      numericInput('ref_yr', 'Reference year for emissions reduction', step = 1, value = 1990),
      numericInput('trend_start_year', 'Calculate trends starting in',
                   step = 1, value = 1980),
      htmlOutput('policy_goal'),
      br(),
      {
        if (enable_topdown) {
          radioButtons("analysis", "Analysis:",
                       #                   c("Bottom-up" = 'bottom.up', "Top-down" = 'top.down'),
                       c("Bottom-up" = 'bottom.up'),
                       inline = TRUE,
                       selected = 'bottom.up')
        } else {
          HTML("")
        }
      },
      htmlOutput('trend_title'),
      uiOutput('trend_table'),
      htmlOutput('target_emissions')
    ),
    mainPanel(
      tabsetPanel(id = "tabs", type="tabs",
                  tabPanel("Trends",
                           h3(textOutput("tab_title_trend", inline = TRUE)),
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
                                      plotlyOutput('trend_plot_ln')
                                    )
                             ),
                             column(6,
                                    span(h4(strong(textOutput('trend_plot_title'))),
                                         style="text-align:center;"),
                                    div(
                                      plotlyOutput('trend_plot')
                                    )
                             )
                           )
                  ),
                  tabPanel("Calculations",
                           h3(textOutput('tab_title_calc')),
                           br(),
                           {
                             if (enable_answers) {
                               checkboxInput('calc_show_answers', "Show answers", value = FALSE)
                             } else {
                               HTML("")
                             }
                           },
                           htmlOutput('step_1'),
                           tableOutput('step_1_table'),
                           htmlOutput('step_2'),
                           tableOutput('step_2_table'),
                           htmlOutput('step_3'),
                           uiOutput('step_3_formula'),
                           tableOutput('step_3_table'),
                           htmlOutput('step_4'),
                           tableOutput('step_4_table'),
                           htmlOutput('step_5'),
                           tableOutput('step_5_table'),
                           htmlOutput('step_6'),
                           htmlOutput('step_7')
                  ),
                  tabPanel("Implied Decarbonization",
                           h3(textOutput('tab_title_decarb')),
                           div(
                             plotlyOutput('implied_decarb_plot')
                           )
                  ),
                  # tabPanel("Top Down",
                  #          h3(textOutput('tab_title_top_down')),
                  #          div(
                  #            tableOutput('top_down_growth_table')
                  #            )
                  #          ),
                  tabPanel("Energy Mix",
                           h3(textOutput('tab_title_fuel_dist')),
                           div(
                             # htmlOutput('fuel_dist'),
                             tableOutput('fuel_dist_table'),
                             br(),
                             downloadButton('downloadFuelData', 'Download'),
                             br(),
                             plotOutput('fuel_dist_plot')
                           )
                           ),
                  tabPanel("Historical",
                           h3(textOutput('tab_title_historical')),
                           div(
                             DT::dataTableOutput("historical_table"),
                             br(),
                             downloadButton('downloadData', 'Download')
                           )
                  )
      )
    )
  )
))
