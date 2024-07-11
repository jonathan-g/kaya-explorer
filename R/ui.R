
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#' @include globals.R
NULL

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

ui <- function(request) {
  shiny::fluidPage(
    # Application title
    shiny::titlePanel("Decarbonization Explorer"),
    shiny::withMathJax(),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jscode, functions = c("disableTab", "enableTab")),
    shinyjs::inlineCSS(css),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput('region', "Country/Region", choices = NULL),
        shiny::numericInput('target_yr', 'Target year', step = 1, value = 2050),
        shiny::numericInput('target_reduc', 'Emissions reduction (%)', max=100,
                            value = 80),
        shiny::numericInput('ref_yr', 'Reference year for emissions reduction', step = 1, value = 1990),
        shiny::numericInput('trend_start_year', 'Calculate trends starting in',
                            step = 1, value = 1980),
        shiny::htmlOutput('policy_goal'),
        shiny::br(),
        {
          if (enable_topdown) {
            shiny::radioButtons("analysis", "Analysis:",
                                #                   c("Bottom-up" = 'bottom.up', "Top-down" = 'top.down'),
                                c("Bottom-up" = 'bottom.up'),
                                inline = TRUE,
                                selected = 'bottom.up')
          } else {
            shiny::HTML("")
          }
        },
        shiny::htmlOutput('trend_title'),
        shiny::uiOutput('trend_table'),
        shiny::htmlOutput('target_emissions')
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          id = "tabs", type="tabs",
          shiny::tabPanel("Trends",
                          shiny::h3(shiny::textOutput("tab_title_trend", inline = TRUE)),
                          shiny::fluidRow(
                            shiny::column(
                              2,
                              shiny::selectInput('trend_variable', 'Variable',
                                                 choices = c(P = 'P', g = 'g',
                                                             e = 'e', f = 'f',
                                                             ef = 'ef',
                                                             'F/P' = 'gef',
                                                             G = 'G', E = 'E',
                                                             F = 'F'),
                                                 selected = 1)
                            ),
                            shiny::column(
                              10,
                              shiny::br(),
                              shiny::htmlOutput('trend_display', inline = TRUE)
                            )
                          ),

                          shiny::fluidRow(
                            shiny::column(
                              6,
                              shiny::span(
                                shiny::h4(shiny::strong(shiny::textOutput('trend_plot_title'))),
                                style="text-align:center;"
                              ),
                              shiny::div(
                                plotly::plotlyOutput('trend_plot')
                              )
                            ),
                            shiny::column(
                              6,
                              shiny::span(
                                shiny::h4(shiny::strong(shiny::textOutput('trend_plot_ln_title'))),
                                style="text-align:center;"
                              ),
                              shiny::div(
                                plotly::plotlyOutput('trend_plot_ln')
                              )
                            ),
                          )
          ),
          shiny::tabPanel("Calculations",
                          shiny::h3(shiny::textOutput('tab_title_calc')),
                          shiny::br(),
                          {
                            if (enable_answers) {
                              shiny::checkboxInput('calc_show_answers',
                                                   "Show answers",
                                                   value = FALSE)
                            } else {
                              shiny::HTML("")
                            }
                          },
                          shiny::htmlOutput('step_1'),
                          shiny::tableOutput('step_1_table'),
                          shiny::htmlOutput('step_2'),
                          shiny::tableOutput('step_2_table'),
                          shiny::htmlOutput('step_3'),
                          shiny::uiOutput('step_3_formula'),
                          shiny::tableOutput('step_3_table'),
                          shiny::htmlOutput('step_4'),
                          shiny::tableOutput('step_4_table'),
                          shiny::htmlOutput('step_5'),
                          shiny::tableOutput('step_5_table'),
                          shiny::htmlOutput('step_6'),
                          shiny::htmlOutput('step_7')
          ),
          shiny::tabPanel("Implied Decarbonization",
                          shiny::h3(shiny::textOutput('tab_title_decarb')),
                          shiny::div(
                            plotly::plotlyOutput('implied_decarb_plot')
                          )
          ),
          # shiny::tabPanel("Top Down",
          #          shiny::h3(shiny::textOutput('tab_title_top_down')),
          #          shiny::div(
          #            shiny::tableOutput('top_down_growth_table')
          #            )
          #          ),
          shiny::tabPanel("Energy Mix",
                          shiny::h3(shiny::textOutput('tab_title_fuel_dist')),
                          shiny::div(
                            # shiny::htmlOutput('fuel_dist'),
                            shiny::tableOutput('fuel_dist_table'),
                            shiny::br(),
                            shiny::downloadButton('downloadFuelData', 'Download'),
                            shiny::br(),
                            shiny::plotOutput('fuel_dist_plot')
                          )
          ),
          shiny::tabPanel("Historical",
                          shiny::h3(shiny::textOutput('tab_title_historical')),
                          shiny::div(
                            DT::dataTableOutput("historical_table"),
                            shiny::br(),
                            shiny::downloadButton('downloadData', 'Download')
                          )
          )
        )
      )
    )
  )
}

assign("ui", ui, envir = .shinyenv)
