
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

mass.c <- 12
mass.co2 <- 44

c.to.co2 <- function(x) {x * mass.co2 / mass.c}
co2.to.c <- function(x) {x * mass.c / mass.co2}

kaya <- load_kaya()$kaya %>% mutate(F = c.to.co2(F), f = c.to.co2(f), ef = c.to.co2(ef))

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


add_units <- function(variables) {
  suppressWarnings(str_c(variables, ' (', kaya_labels$unit[pmatch(variables, kaya_labels$variable)], ')'))
}


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
    if (! ctry %in% kaya$country)
      ctry <- 'World'
    ks <- kaya  %>% filter(country == ctry) %>% arrange(year)
    ks
  })
  
  current_year <- reactive({
    max(kaya_subset()$year, na.rm = T)
  })
  
  history_start <- reactive({
    min(kaya_subset()$year, na.rm = T)
  })
  
  history_stop <- reactive({
    max(kaya_subset()$year)
  })
  
  ref_emissions <- reactive({
    ref_yr <- input$ref_yr
    ref_emissions <- kaya_subset() %>% filter(year == ref_yr) %>% select(F) %>% unlist()
  })
  
  target_emissions <- reactive({
    ref_emissions() * (1. - input$target_reduc / 100.)
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
  
  forecast <- reactive({
    t <- trends() %>% mutate(variable = ordered(variable, levels = variable))
    current_yr <- current_year()
    ks <- kaya_subset() %>% filter(year == current_yr) %>%
      select_(.dots = as.character(t$variable)) %>%
      gather(key = variable, value = current)
    t <- merge(t, ks)
    t <- mutate(t, projected = current * exp((input$target_yr - current_yr) * growth.rate)) %>%
      arrange(variable)
    t
  })
  
  output$trend_display <- renderText({
    v <- input$trend_variable
    t <- filter(trends(), variable == v)
    paste0('Growth rate of ', v, ' = ', formatC(t$growth.rate * 100, digits = 2, format = 'f'), '% per year')
  })
  
  output$historical_table <- renderTable({
    ks <- kaya_subset() %>% select(year, P, g, e, f, ef, G, E, F)
    cn <- colnames(ks)
    labs <- kaya_labels %>% mutate(label = suppressWarnings(str_c(long, ' (', unit, ')'))) %>% select(variable, label)
    colnames(ks) <- c('Year', labs$label[pmatch(cn[-1], labs$variable)])
    ks
  },
  digits = c(0, 0, 2, 2, 2, 2, 1, 1, 1, 0),
  include.rownames = FALSE
  )
  
  output$policy_goal <- renderText({
    as.character(span(strong('Policy goal: '), input$target_yr, ' emissions ', input$target_reduc, '% below ', input$ref_yr)) 
  })
  
  output$trend_title <- renderText({
    title <- c(top.down = "Top Down", bottom.up = "Bottom up")[input$analysis]
    as.character(h4(strong(title)))
  })
  
  output$trend_table <- renderTable({
    fcast <- forecast()
    current_yr <- current_year()
    fcast <- fcast %>% mutate(growth.pct = str_c(formatC(growth.rate * 100, digits=2, format='f'), '%'),
                              current = formatC(current, digits = 3, big.mark = ',', format = 'fg'),
                              projected = formatC(projected, digits = 3, big.mark = ',', format = 'fg')) %>%
      select(variable, growth.pct, current, projected) %>%
      mutate(variable = add_units(variable))
    rownames(fcast) <- fcast$variable
    fcast <- fcast %>% select(-variable)
    names(fcast) <- c('Growth Rate', paste0('Current (', current_yr, ')'), paste0('Projected (', input$target_yr, ')'))
    fcast
  }, 
  align = 'crrr')
  
  output$step_1 <- renderText({
    paste0(strong("Step 1: "), "write down current (",
           current_year(), ") and projected ", input$target_yr, " values for ",
           em('P'), ", ", em('g'), ", ", em('e'), ", and ", em('f'), ".")
  })
  
  output$step_1_table <- renderTable({
    target_yr <- input$target_yr
    current_yr <- current_year()
    fcast <- forecast() %>% filter(variable %in% c('P', 'g', 'e', 'f')) %>%
      mutate(variable = add_units(variable), current = formatC(current, digits = 3, big.mark = ',', format = 'fg'),
             projected = formatC(projected, digits = 3, big.mark = ',', format = 'fg'))
    rownames(fcast) <- fcast$variable
    fcast <- fcast %>% select(current, projected)
    names(fcast) <- suppressWarnings(str_c(c("Current", "Projected"), " (", c(current_yr, target_yr), ")"))
    message(paste(colnames(fcast), collapse = ', '))
    fcast
  }, align=c('c','r', 'r'))
  
  output$step_2 <- renderText({
    paste0(strong("Step 2: "), "Multiply the variables together to get ", em("F"), " for each year.")
  })
  
  output$step_2_table <- renderTable({
    target_yr <- input$target_yr
    current_yr <- current_year()
    fcast <- forecast() %>% filter(variable %in% c('F')) %>%
      mutate(variable = add_units(variable), current = formatC(current, digits = 3, big.mark = ',', format = 'fg'),
             projected = formatC(projected, digits = 3, big.mark = ',', format = 'fg'))
    rownames(fcast) <- fcast$variable
    fcast <- fcast %>% select(current, projected)
    names(fcast) <- suppressWarnings(str_c(c("Current", "Projected"), " (", c(current_yr, target_yr), ")"))
    message(paste(colnames(fcast), collapse = ', '))
    fcast
  }, align=c('c','r', 'r'))
  
  output$step_3 <- renderText({
    ref_yr <- input$ref_yr
    target_yr <- input$target_yr
    target_reduc <- input$target_reduc
    paste0(strong("Step 3: "), "Look up emissions for ", ref_yr, " and calculate the target emissions (", target_reduc, "% less) for ", target_yr, ".")
  })

  output$step_3_table <- renderTable({
    ref_yr <- input$ref_yr
    target_yr <- input$target_yr

    df <- data.frame(ref = ref_emissions(), target = target_emissions())
    names(df) <- str_c(c("Emissions in ", "Target emissions in "), c(ref_yr, target_yr))
    df
  }, include.rownames = FALSE)
  
  
  output$target_emissions <- renderText({
    target = formatC(target_emissions(), digits = 3, big.mark = ',', format = 'fg')
    ref = formatC(ref_emissions(), digits = 3, big.mark = ',', format = 'fg')
    paste0(
      as.character(h4(paste0(input$ref_yr, ' emissions = ', ref, ' MMT CO2'))),
      as.character(h4(input$target_yr, ' target:',
                      input$target_reduc, '% reduction = ', target, ' MMT')))
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
    if (is.na(history_start()))
      x_tics <- NULL
    else
      x_tics <- seq(10 * round(history_start() / 10), 10 * round(history_stop() / 10), 10)
    plot <- kaya_subset %>%
      ggvis(x = ~year, y = yvar) %>%
      layer_points(size := 15, size.hover := 50,
                   fillOpacity := 1, fillOpacity.hover := 1,
                   fill := "darkred", stroke := "darkred",
                   key := ~id) %>%
      layer_lines(strokeWidth := 2, stroke := "darkred") %>%
      add_tooltip(trend_tooltip, "hover") %>%
      add_axis("x", title = xvar_name, format="4d",
               values = x_tics) %>%
      add_axis("y", title = as.character(yvar_name)) %>%
      set_options(width="auto", height="auto", resizable = TRUE)
    plot
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
    if (is.na(history_start()))
      x_tics <- NULL
    else
      x_tics <- seq(10 * round(history_start() / 10), 10 * round(history_stop() / 10), 10)
    plot <- kaya_subset %>%
      ggvis(x = ~year, y = yvar) %>%
      layer_points(size := 15, size.hover := 50,
                   fillOpacity := 1, fillOpacity.hover := 1,
                   fill := "darkred", stroke := "darkred",
                   key := ~id) %>%
      layer_lines(strokeWidth := 2, stroke := "darkred") %>%
      layer_model_predictions(model = "lm") %>%
      add_tooltip(trend_tooltip, "hover") %>%
      add_axis("x", title = xvar_name, format="4d", values = x_tics) %>%
      add_axis("y", title = yvar_name) %>%
      set_options(width="auto", height="auto", resizable = TRUE)
    plot
  })
  
  tpl %>% bind_shiny("trend_plot_ln", session = session)
  
})
