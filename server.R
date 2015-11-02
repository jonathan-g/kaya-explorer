
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(stringr)
library(dplyr)
library(tidyr)

source('load_kaya.R')

data <- load_kaya()
kaya <- data$kaya

kaya$id <- seq_len(nrow(kaya))

countries <- as.character(unique(kaya$country))

kaya_vars <- c(P = 'Population (billions)',
               G = 'GDP (trillion dollars)',
               E = 'Energy consumption (quads)',
               F = "CO2 emissions (MMT)",
               g = "Per-capita GDP (thousand dollars per person)",
               e = "Energy intensity of economy (quads per trillion dollars)",
               f = "CO2 intensity of energy supply (MMT per quad)",
               ef = "CO2 intensity of economy (Metric tons per billion dollars)"
)

kaya_labels <- data.frame(
  variable = c('P','G','E','F','g','e', 'f', 'ef'),
  unit = c('billion', 'trillion dollars', 'quad','MMT CO2',
           '$1000 per person', 'quad per $trillion', 'MMT per quad',
           'metric ton per $ billion'),
  long = c('Population', 'GDP', 'Energy consumption', 'Emissions',
           'Per-capita GDP', 'Energy intensity',
           'CO2 intensity of energy',
           'CO2 intensity of economy'),
  short = c('P', 'GDP', 'Energy', 'Emissions',
            'Per-capita GDP', 'Energy intensity',
            'Carbon intensity', 'Carbon intensity of economy')
)

shinyServer(function(input, output, session) {
  observe({
    updateSelectInput(session, 'country', choices = unique(countries))
    v <- input$ref_yr
    v <- min(max(input$ref_yr, min(kaya$year)), max(kaya$year))
    updateNumericInput(session, 'ref_yr', min = min(kaya$year),
                       max = max(kaya$year), step = 1, value = v)
    v <- max(input$target_yr, max(kaya$year))
    updateNumericInput(session, 'target_yr', min = max(kaya$year),
                       step = 1, value = v)
  })

  kaya_subset <- reactive({
    ctry <- input$country

    ks <- kaya  %>% filter(country == ctry) %>% arrange(year)
    ks
  })

  trends <- reactive({
    ks <- kaya_subset()
    vars <- c('P','g', 'e', 'f', 'ef', 'G', 'E', 'F')
    t <- data.frame(variable = vars, 'growth.rate' = NA)
    if (nrow(ks) > 0) {
      t$growth.rate <- unlist(lapply(vars,
                       function(v) {
                         f <- substitute(log(x) ~ year, list(x = as.symbol(v)))
                         mdl <- lm(f, data = ks, na.action = na.exclude)
                         growth <- coef(mdl)['year']
                       }))
    }
  t
})

  output$historical_table <- renderTable({
    ks <- kaya_subset() %>% select(year, P, g, e, f, ef, G, E, F)
    cn <- colnames(ks)
    labs <- kaya_labels %>% mutate(label = str_c(long, ' (', unit, ')')) %>% select(variable, label)
    colnames(ks) <- c('Year', labs$label[pmatch(cn[-1], labs$variable)])
    ks
  },
    digits = c(0, 0, 2, 2, 2, 2, 1, 1, 1, 0),
    include.rownames = FALSE
  )

  output$trend_title <- renderText({
    title <- c(top.down = "Top Down", bottom.up = "Bottom up")[input$analysis]
    as.character(h3(title))
    })

  output$trend_table <- renderTable({
    t <- trends()
    current_year <- max(kaya$year)
    t$variable <- ordered(t$variable, levels = t$variable)
    ks <- kaya_subset() %>% filter(year == current_year) %>%
                    select_(.dots = as.character(t$variable)) %>%
                    gather(key = variable, value = current)
    t <- merge(t, ks)
    t <- mutate(t, growth.pct = str_c(formatC(growth.rate * 100, digits=2, format='f'), '%'),
                projected = current * exp((input$target_yr - current_year) * growth.rate)) %>%
      mutate(current = formatC(current, digits = 3, big.mark = ',', format = 'fg'),
             projected = formatC(projected, digits = 3, big.mark = ',', format = 'fg')) %>%
      select(variable, growth.pct, current, projected) %>%
      arrange(variable) %>%
      mutate(variable = str_c(variable, ' (',
                              kaya_labels$unit[pmatch(variable, kaya_labels$variable)], ')'))
    names(t) <- c('', 'Growth Rate', paste0('Current (', current_year, ')'),
                  paste0('Projected (', input$target_yr, ')'))
    t
  }, align = 'ccrrr', include.rownames = FALSE)

  output$target_emissions <- renderText({
    ref_year <- input$ref_yr
    target_year <- input$target_yr
    target_reduc <- input$target_reduc
    ref_emissions <- kaya_subset() %>% filter(year == ref_year) %>%
      select_('F') %>% unlist()
    target_emissions <- ref_emissions * (1 - target_reduc / 100)
    target = formatC(target_emissions, digits = 3, big.mark = ',', format = 'fg')
    ref = formatC(ref_emissions, digits = 3, big.mark = ',', format = 'fg')
    paste0(
      as.character(h4(paste0(ref_year, ' emissions = ', ref, ' MMT CO2'))),
      as.character(h4(target_year, ' target:',
                      target_reduc, '% reduction = ', target, ' MMT')))
  })

  trend_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
    k <- isolate(kaya_subset())
    v <- input$trend_variable
    r = k[k$id == x$id,]
    year = r$year
    pt = r[v]
    paste0('<b>', year, ": ", v, " = ", formatC(as.numeric(pt), digits=2, format='f'), "</b>")
  }

  tp <- reactive({
    xvar_name <- 'Year'
    v <- input$trend_variable
    yvar_name <- with(kaya_labels[kaya_labels$variable == v,], paste0(variable, ' (', unit, ')'))
    yvar <- prop("y", as.symbol(input$trend_variable))
    kaya_subset %>%
      ggvis(x = ~year, y = yvar) %>%
      layer_points(size := 15, size.hover := 50,
                   fillOpacity := 1, fillOpacity.hover := 1,
                   fill := "darkred", stroke := "darkred",
                   key := ~id) %>%
      layer_lines(strokeWidth := 2, stroke := "darkred") %>%
      add_tooltip(trend_tooltip, "hover") %>%
      add_axis("x", title = xvar_name, format="4d",
               values = seq(10 * round(min(kaya$year / 10)), 10 * round(max(kaya$year / 10)), 10)) %>%
      add_axis("y", title = as.character(yvar_name)) %>%
      set_options(width="auto", height="auto", resizable = TRUE)
  })

  trend_model <- reactive({
    var <- input$trend_variable
    k <- kaya_subset()
    message("Var = ", var, " data has ", nrow(k), " rows")
    f <- substitute(log(x) ~ year, list(x = as.symbol(input$trend_variable)))
    if (nrow(k) > 0) {
      trend <- lm(as.formula(f), data = k)
      message(summary(trend))
    } else {
      trend <- NULL
    }
    trend
  })

  tp %>% bind_shiny("trend_plot", session = session)

  tpl <- reactive({
    xvar_name <- 'Year'
    v <- input$trend_variable
    yvar_name <- with(kaya_labels[kaya_labels$variable == v,], paste0('log(', variable, ')'))
    yvar <- prop("y", substitute(log(x), list(x = as.symbol(input$trend_variable))))
    plot <- kaya_subset %>%
      ggvis(x = ~year, y = yvar) %>%
      layer_points(size := 15, size.hover := 50,
                   fillOpacity := 1, fillOpacity.hover := 1,
                   fill := "darkred", stroke := "darkred",
                   key := ~id) %>%
      layer_lines(strokeWidth := 2, stroke := "darkred") %>%
      layer_model_predictions(model = "lm") %>%
      add_tooltip(trend_tooltip, "hover") %>%
      add_axis("x", title = xvar_name, format="4d",
               values = seq(10 * round(min(kaya$year / 10)), 10 * round(max(kaya$year / 10)), 10)
               ) %>%
      add_axis("y", title = yvar_name) %>%
      set_options(width="auto", height="auto", resizable = TRUE)
    plot
  })

  tpl %>% bind_shiny("trend_plot_ln", session = session)

})
