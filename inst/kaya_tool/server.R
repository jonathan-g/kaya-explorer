# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(ggvis)
library(stringr)
library(rtable)
library(broom)
library(RColorBrewer)
library(DT)

source('load_kaya.R')
# source('energy_by_fuel.R')

goodness_of_fit <- function(r.squared) {
  gof <- NA
  if(r.squared >= 0.99)
    gof <- "excellent"
  else if (r.squared >= 0.98)
    gof <- "very good"
  else if (r.squared >= 0.95)
    gof <- "good"
  else if (r.squared >= 0.90)
    gof <- "all right, but with some discrepancies"
  else if (r.squared >= 0.80)
    gof <- "not so good"
  else
    gof <- "poor"
  gof
}

mass.c <- 12
mass.co2 <- 44

c.to.co2 <- function(x) {x * mass.co2 / mass.c}
co2.to.c <- function(x) {x * mass.c / mass.co2}

answer.bg <- '#FFFFA0'
answer.fg <- '#A000A0'

normal.body.props <- cellProperties(padding.left = 2, padding.right = 2)
normal.head.props <- cellProperties(padding.top = 2,
                                    padding.left = 2, padding.right = 2)
answer.body.props <- cellProperties(padding.left = 2, padding.right = 2,
                                    background.color = answer.bg)
answer.head.props <- cellProperties(padding.top = 2,
                                    padding.left = 2, padding.right = 2,
                                    background.color = answer.bg)

kaya <- load_kaya() %>% mutate(F = c.to.co2(F), f = c.to.co2(f), ef = c.to.co2(ef))

kaya$id <- seq_len(nrow(kaya))

top_down <- load_top_down()

energy_by_fuel <- load_energy_by_fuel()

kaya_countries <- as.character(unique(kaya$country))
td_countries <- as.character(unique(top_down$country)) %>% keep(~.x %in% kaya_countries)
ebf_countries <- as.character(unique(energy_by_fuel$country)) %>% keep(~.x %in% kaya_countries)

kaya_labels <- data.frame(
  variable = c('P','G','E','F','g','e', 'f', 'ef'),
  unit = c('billion', 'trillion dollars', 'quad','MMT CO2',
           '$1000 per person', 'quad per $trillion', 'MMT per quad',
           'metric ton per $ million'),
  long.unit = c('billion people', 'trillion dollars', 'quad','million metric tons CO2',
                '$1000 per person', 'quad of energy per $trillion  GDP', 'million metric tons CO2 per quad',
                'metric ton CO2 per million dollars GDP'),
  long.long = c('Population', 'GDP', 'Energy consumption', 'Emissions',
                'Per-capita GDP', 'Energy intensity of the economy',
                'CO2 intensity of the energy supply',
                'CO2 intensity of the economy'),
  long = c('Population', 'GDP', 'Energy consumption', 'Emissions',
           'Per-capita GDP', 'Energy intensity',
           'CO2 intensity of energy',
           'CO2 intensity of economy'),
  short = c('P', 'GDP', 'Energy', 'Emissions',
            'Per-capita GDP', 'Energy intensity',
            'Carbon intensity', 'Carbon intensity of economy'),
  stringsAsFactors = FALSE
)


prt <- function(x, digits = NULL, big.mark = ',', format = 'f') {
  formatC(x, digits = digits, big.mark = big.mark, format = format)
}

add_units <- function(variables) {
  suppressWarnings(str_c(variables, ' (', kaya_labels$unit[pmatch(variables, kaya_labels$variable)], ')'))
}

renderFlexTableIf <- function(expr, ..., env = parent.frame(), quoted = FALSE)
{
  func = NULL
  installExprFunction(expr, "func", env, quoted)
  markRenderFunction(tableOutput, function() {
    data = func()
    if (is.null(data))
      return(NULL)
    else
      return(as.html(data))
  })
}

shinyServer(function(input, output, session) {
  countries <- reactive({
  c_list <- kaya_countries
  if (input$tabs == 'Top Down') { c_list <- td_countries} else
    if (input$tabs == 'Energy Mix') { c_list <- ebf_countries }
  c_list
  })

  observeEvent(input$country, {
    ctry <- input$country
    if (is.null(ctry)) {
      message("NULL country")
      ctry <- 'World'
    } else if (ctry == '') {
      message('Empty country')
      ctry <- 'World'
    }
    updateSelectInput(session, 'country', choices = countries(), selected = ctry)
  })

  kaya_subset <- eventReactive(input$country, {
    ctry <- input$country
    if (! ctry %in% kaya_countries)
      ctry <- 'World'
    ks <- kaya  %>% filter(country == ctry) %>% arrange(year)
    ks
  })

  observeEvent(input$country, {
    if (input$country %in% td_countries) {
      message("enabling top down")
      js$enableTab("Top Down")
    } else {
      message("disabling top down")
      js$disableTab("Top Down")
    }
    if (input$country %in% ebf_countries) {
      message("enabling energy mix")
      js$enableTab("Energy Mix")
    } else {
      message("disabling energy mix")
      js$disableTab("Energy Mix")
    }
  })

  last_event_time <- eventReactive( any(input$target_yr, input$target_reduc,
                                        input$ref_yr, input$trend_start_year,
                                        input$country),
                 {
                   proc.time()['elapsed']
                 })

  observeEvent( kaya_subset(), {
    ks <- kaya_subset()
    ks.min.yr <- min(ks$year)
    ks.max.yr <- max(ks$year)
    v <- input$ref_yr
    v2 <- min(max(v, ks.min.yr), ks.max.yr)

    updateNumericInput(session, 'ref_yr',
                       min = ks.min.yr, max = ks.max.yr, step = 1, value = v2)
    v <- max(input$target_yr, ks.max.yr)
    updateNumericInput(session, 'target_yr',
                       min = ks.max.yr, step = 1, value = v)
    v <- min(max(input$trend_start_year, ks.min.yr), ks.max.yr)
    updateNumericInput(session, 'trend_start_year',
                       min = ks.min.yr, max = ks.max.yr, step = 1, value = v)
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

  calc_show_answers <- reactive({
    if ('calc_show_answers' %in% names(input))
      return(input$calc_show_answers)
    else
      return(FALSE)
  })

  trends <- reactive({
    trend.start <- input$trend_start_year
    ks <- kaya_subset() %>% filter(year >= trend.start)
    vars <- c('P','g', 'e', 'f', 'ef', 'G', 'E', 'F')
    if (nrow(ks) > 0) {
      t <- lapply(vars,
                  function(v) {
                    f <- substitute(log(x) ~ year, list(x = as.symbol(v)))
                    mdl <- lm(f, data = ks, na.action = na.exclude)
                    r.squared = mdl %>% glance() %>%
                      select(adj.r.squared) %>% unlist()
                    growth = mdl %>% tidy() %>% filter(term == 'year') %>%
                      select(estimate) %>% unlist()
                    tibble(variable = v, growth.rate = growth, r.squared = r.squared)
                  }) %>% bind_rows()
    } else {
      t <- data.frame(variable = vars, growth.rate = NA, r.squared = NA)
    }
  })

  top_down_trends <- reactive({
    td <- top_down
    td <- td %>% dplyr::filter(country == input$country) %>%
      mutate(g = G - P, e = E - G, f = F - E) %>%
      gather(key = variable, value = growth.rate, -country) %>%
      mutate(growth.rate = growth.rate * 0.01)
    if (nrow(td) == 0) {
      td  <- data.frame(variable = vars, growth.rate = NA)
    }
    td
  })

  fuel_dist <- reactive({
    fd <- energy_by_fuel %>% filter(country == input$country)
    if (nrow(fd) == 0) {
      fd <- NULL
    } else {
      if (current_year() %in% fd$year) {
        y <- current_year()
      } else if (current_year() > max(fd$year)) {
        y <- max(fd$year)
      } else if (current_year() < min(fd$year)) {
        y <- min(fd$year)
      } else {
        error("Cannot figure out year for current year = ", current_year(), " and fd range = (",
              paste0(range(fd$year), collapse = ', '), ")")
      }
      fd <- fd %>% filter(year == y)
    }
    message(print(fd))
    invisible(fd)
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

  forecast_top_down <- reactive({
    t <- top_down_trends() %>% mutate(variable = ordered(variable, levels = variable))
    current_yr <- current_year()
    ks <- kaya_subset() %>% filter(year == current_yr) %>%
      select_(.dots = as.character(t$variable)) %>%
      gather(key = variable, value = current)
    t <- merge(t, ks) %>%
      mutate(projected = current * exp((input$target_yr - current_yr) * growth.rate)) %>%
      arrange(variable)
    t
  })

  implied_decarb_rate <- reactive({
    ks <- kaya_subset() %>% mutate(cat = 'Historical')
    fcast <- forecast()
    target_em <- target_emissions()
    target_yr <- input$target_yr
    current_yr <- current_year()
    delta_yr <- target_yr - current_yr
    current_em <- fcast$current[fcast$variable == 'F']
    rate_G <- fcast$growth.rate[fcast$variable == 'G']

    rate = log(target_em / current_em) / delta_yr - rate_G

    if (FALSE) {
      message("target_em = ", prt(target_em,0), ", rate_G = ",
              prt(rate_G * 100, 2), "%, target ef rate  = ",
              prt(rate * 100,2), "%")
    }
    rate
  })

  implied_decarb <- reactive({
    ks <- kaya_subset() %>% select(year, ef) %>% mutate(cat = 'Historical')
    fcast <- forecast()
    target_yr <- input$target_yr
    current_yr <- current_year()
    current_ef <- fcast$current[fcast$variable == 'ef']
    trend_ef <- fcast$growth.rate[fcast$variable == 'ef']

    rate <- implied_decarb_rate()

    if (FALSE) {
      message("implied decarb rate = ", prt(100 * rate, 2), "%")
    }

    mdl <- lm(log(ef) ~ year, data = filter(ks, year >= input$trend_start_year))

    extrap <- data.frame(year = seq(history_start(), target_yr), cat = "Historical Trend")
    extrap$ef <- exp(predict(mdl, newdata = extrap))

    bottom_up <- data.frame(year = seq(current_yr, target_yr))
    bottom_up <- bottom_up %>% mutate( ef = current_ef * exp(rate * (year - current_yr)),
              cat = 'Bottom-up')

    ks <- bind_rows(ks, extrap, bottom_up) %>% mutate(id = row_number())
    ks
  })

  output$trend_display <- renderText({
    v <- input$trend_variable
    k.label <- kaya_labels %>% filter(variable == v)
    t <- filter(trends(), variable == v)
    HTML(paste0(k.label$long.long, " (", k.label$long.unit, "): ",
                'Rate of change of ', em(v), ' = ', prt(t$growth.rate * 100, digits = 2), '% per year',
                br(), "Calculated from the slope of ln(", em(v), ") starting in ", input$trend_start_year))
    # br(), "R", tags$sup("2"), " = ", prt(t$r.squared, digits = 3),
    # " (", goodness_of_fit(t$r.squared), ")"))
  })

  output$historical_table <- DT::renderDataTable({
    ks <- kaya_subset() %>% select(year, P, g, e, f, ef, G, E, F)
    ks <- ks %>% mutate(P = prt(P, 2),
                        g = prt(g,1),
                        e = prt(e,2), f = prt(f,2),
                        ef = prt(ef,0),
                        G = prt(G, 3), E = prt(E,2), F = prt(F,1))
    cn <- colnames(ks)
    labs <- kaya_labels %>% mutate(label = suppressWarnings(str_c(long, ' (', unit, ')'))) %>% select(variable, label)
    colnames(ks) <- c('Year', labs$label[pmatch(cn[-1], labs$variable)])
    # message("Kaya Subset:")
    # message(print(ks))
    dt <- DT::datatable(ks,
                        rownames =  FALSE, filter = 'none',
                        options = list(
                          # sDom  = '<"top">lrt<"bottom">ip',
                          columnDefs = list(list(targets = seq(0,ncol(ks)-1), className = 'dt-right'))
                        )
    )
    invisible(dt)
    # ks
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      input$country %>% str_replace_all('[^A-Za-z0-9]+', '_') %>%str_c('.csv')
      },
    content = function(file) {
      write_csv(
        kaya_subset() %>% select(-country, -geography, -id),
        path = file)
    })

  output$downloadFuelData <- downloadHandler(
    filename = function() {
      input$country %>% str_replace_all('[^A-Za-z0-9]+', '_') %>% str_c('_fuel.csv')
    },
    content = function(file) {
      df <- fuel_dist() %>% mutate(quads = round(quads, 2), pct = round(pct, 2)) %>%
        dplyr::select(Fuel = fuel, Quads = quads, Percent = pct) %>%
        arrange(Fuel)
      df_total = summarize(df, Fuel = 'Total', Quads = sum(Quads), Percent = sum(Percent))

      df <- df %>% bind_rows(df_total)
      message("df has ", nrow(df), " rows")
      message("file = ", file)
      write_csv(df, path = file)
    })

  output$policy_goal <- renderText({
    as.character(span(strong('Policy goal: '), paste0(input$target_yr, ' emissions ',
                      abs(input$target_reduc), '% ',
                      ifelse(input$target_reduc >= 0, 'below', 'above'), ' ', input$ref_yr)))
  })

  trend_title <- reactive({
    if ('analysis' %in% names(input)) {
      title <- c(top.down = "Top Down", bottom.up = "Bottom up")[input$analysis]
    } else {
      title <- 'Decarbonization'
    }
    str_c(title, " Analysis")
  })

  output$trend_title <- renderText({
    as.character(h4(strong(trend_title())))
  })

  output$tab_title_trend <- renderText({
    paste("Historical Trends for", input$country)
  })

  output$tab_title_calc <- renderText({
    paste("Calcuating Implied Decarbonization for", input$country)
  })

  output$tab_title_decarb <- renderText({
    paste("Implied Decarbonization for", input$country)
  })

  output$tab_title_fuel_dist <- renderText({
    x <- paste0("Energy Mix for ", input$country)
    if (! is.null(fuel_dist())) {
      x <- paste0(x, " in ", fuel_dist()$year[1])
    } else {
      x <- paste0(x, " is not available")
    }
    x
  })

  output$tab_title_historical <- renderText({
    paste("Historical Data for", input$country)
  })


  output$trend_table <- renderFlexTable({
    fcast <- forecast()
    current_yr <- current_year()
    fcast <- fcast %>% mutate(growth.pct = str_c(prt(growth.rate * 100, 2), '%'),
                              current = prt(current, 3, format = 'fg'),
                              projected = prt(projected, 3, format = 'fg')) %>%
      select(variable, growth.pct, current, projected) %>%
      mutate(variable = add_units(variable))
    names(fcast) <- c('', 'Rate of Change', paste0('Current (', current_yr, ')'),
                      paste0('Projected (', input$target_yr, ')'))
    ft <- FlexTable(fcast)
    ft[,, to='header'] <- parCenter()
    ft[,1] <- parCenter()
    ft[,2:ncol(fcast)] <- parRight()
    ft
  })

  output$step_1 <- renderText({
    paste0(strong("Step 1: "), "Write down current (",
           current_year(), ") values for ",
           em('P'), ", ", em('g'), ", ", em('e'), ", and ", em('f'),
           ", and their historical rates of change.")
  })

  output$step_1_table <- renderFlexTableIf({
    target_yr <- input$target_yr
    current_yr <- current_year()
    fcast <- forecast() %>% filter(variable %in% c('P', 'g', 'e', 'f')) %>%
      mutate(variable = add_units(variable),
             current = prt(current, 3, format = 'fg'),
             growth.rate = paste0(prt(growth.rate * 100, 2), '%')) %>%
      select(variable, current, growth.rate)
    names(fcast) <- c('', paste0("Current"," (",current_yr,")"), "Rate of Change")
    ft <- FlexTable(fcast, header.cell.props = answer.head.props,
                    body.cell.props = answer.body.props)
    ft[,,to="header"] <- parCenter()
    ft[,] <- parRight()
    ft[,1] <- parCenter()
    if (calc_show_answers())
      ft
    else
      NULL
  })

  output$step_2 <- renderText({
    paste0(strong("Step 2: "), "Which variables seem to follow a relatively constant rate of change, ",
           "and which do not?")
  })

  output$step_2_table <- renderFlexTableIf({
    t <- trends() %>%
      mutate(goodness = map(r.squared, goodness_of_fit) %>%
                              simplify()) %>%
      select(Variable = variable, `Goodness of Fit` = goodness)

    if (FALSE) {
      message(print(t))
    }

    ft <- FlexTable(t, header.cell.props = answer.head.props,
                    body.cell.props = answer.body.props)
    ft[,,to="header"] <- parCenter()
    ft[,] <- parRight()
    ft[,1] <- parCenter()
    if (calc_show_answers())
      ft
    else
      NULL
  })

  output$step_3 <- renderText({
    paste0(strong("Step 3: "), "Calculate the projected values for ",
           em('P'), ", ", em('g'), ", ", em('e'), ", and ", em('f'),
           ", in ", input$target_yr, '.',
           ' Show your work, and check your answers against the &ldquo;',
           trend_title(),
           '&rdquo; table on the left-hand panel.',
           ' (Remember to divide percentages by 100)')
  })

  output$step_3_formula <- renderUI({
    current_yr = current_year()
    target_yr = input$target_yr
    if (!calc_show_answers()) return()
    withMathJax(span(paste0("If variable \\(x\\) has value \\(x(",current_yr,")\\) in ",
                       current_yr, " and its rate of change is \\(r\\), then in ",
                       target_yr, " its value will be \\(x(",
                       target_yr,") = x(", current_yr, ") \\times \\exp(r \\times (",
                       target_yr, "-", current_yr,")) = x(", current_yr, ")  \\times \\exp(r \\times ",
                       (target_yr - current_yr), "))\\)"),
                     style=paste0("color:", answer.fg, ";font-weight:bold;")))
  })

  output$step_3_table <- renderFlexTableIf({
    if (! calc_show_answers()) return()
    target_yr <- input$target_yr
    current_yr <- current_year()
    fcast <- forecast() %>% filter(variable %in% c('P', 'g', 'e', 'f')) %>%
      mutate(variable = add_units(variable),
             projected = prt(projected, 3, format = 'fg')) %>%
      select(variable, projected)
    names(fcast) <- c('', paste0("Projected (", target_yr, ")"))
    ft <- FlexTable(fcast, header.cell.props = answer.head.props,
                    body.cell.props = answer.body.props)
    ft[,,to="header"] <- parCenter()
    ft[,] <- parRight()
    ft[,1] <- parCenter()
    ft
  })


  output$step_4 <- renderText({
    paste0(strong("Step 4: "), "Multiply the variables together to get ", em("F"), " for each year.",
           ' Show your work, and check your answers against the &ldquo;',
           trend_title(),
           '&rdquo; table on the left-hand panel.')
  })

  output$step_4_table <- renderFlexTableIf({
    target_yr <- input$target_yr
    current_yr <- current_year()
    fcast <- forecast() %>% filter(variable %in% c('F')) %>%
      mutate(variable = add_units(variable), current = prt(current, 3, format = 'fg'),
             projected = prt(projected, 3, format = 'fg'))
    fcast <- fcast %>% select(variable, current, projected)
    names(fcast) <- c('',suppressWarnings(str_c(c("Current", "Projected"), " (", c(current_yr, target_yr), ")")))
    ft <- FlexTable(fcast, header.cell.props = answer.head.props, body.cell.props = answer.body.props)
    ft[,,to='header'] <- parCenter()
    ft[,] <- parRight()
    ft[,1] <- parCenter()
    if (calc_show_answers())
      ft
    else {
      NULL
    }
  })

  output$step_5 <- renderText({
    ref_yr <- input$ref_yr
    target_yr <- input$target_yr
    target_reduc <- input$target_reduc
    paste0(strong("Step 5: "), "Look up emissions for ", ref_yr,
           " under the &ldquo;",
           trend_title(),
           "&rdquo; table on the left panel,",
           " or in the &ldquo;Historical&rdquo; tab,",
           " and calculate the target emissions for ", target_yr,
           " (", abs(target_reduc), "% ",
           ifelse(target_reduc >= 0, 'less', 'greater'),
           " than ", ref_yr, ") .")
  })

  output$step_5_table <- renderFlexTableIf({
    if (!calc_show_answers()) return()
    ref_yr <- input$ref_yr
    target_yr <- input$target_yr

    df <- data.frame(
      ref = prt(ref_emissions(), 0),
      target = prt(target_emissions(), 0))
    names(df) <- str_c(c("Emissions in ", "Target emissions in "), c(ref_yr, target_yr))
    ft <- FlexTable(df, header.cell.props = answer.head.props, body.cell.props = answer.body.props)
    ft[,] <- parRight()
    ft[,,to='header'] <- parCenter()
    ft
  })

  output$step_6 <- renderText({
    show_answers <- calc_show_answers()
    ref_yr <- input$ref_yr
    target_yr <- input$target_yr
    target_reduc <- input$target_reduc
    target_em <- target_emissions()
    current_yr <- current_year()
    delta_yr = target_yr - current_yr
    current_em <- forecast() %>% filter(variable == 'F') %>% select(current) %>% unlist()
    em_ratio <- target_em / current_em
    ln_em_ratio <- log(em_ratio)
    rate <- ln_em_ratio / delta_yr

    elements <- span(
      strong("Step 6: "),
      "Calculate the rate of emissions reduction necessary to meet this target:"
    )
    sub_elements <- tags$ol(style="list-style-type:lower-alpha;")
    li_element <- tags$li(paste0("Look up the current (", current_yr, ") emissions:"))
    if (show_answers) {
      li_element <- li_element %>%
        tagAppendChildren(br(),
                          span(paste0(prt(current_em, 0)),
                               style=paste0("color:",answer.fg,";font-weight:bold;"))
        )
    }
    sub_elements <- sub_elements %>%
      tagAppendChild(li_element)

    li_element <- tags$li(paste0("Calculate the ratio of the target (", target_yr,
                     ") emissions to current (", current_yr, ") emissions:"))
      if (show_answers) {
        li_element <- li_element %>%
          tagAppendChildren(br(),
                            span(paste0(prt(target_em, 0), ' / ', prt(current_em, 0),
                                        ' = ', prt(em_ratio, 3)),
                                 style=paste0("color:",answer.fg,";font-weight:bold;"))
          )
      }
    sub_elements <- sub_elements %>%
      tagAppendChild(li_element)

    li_element <- tags$li("Take the natural logarithm of the ratio: ")
    if (show_answers) {
      li_element <- li_element %>%
        tagAppendChildren(br(),
                          span(paste0("ln(", prt(em_ratio, 3), ') = ',
                                      prt(ln_em_ratio, 2)),
                               style=paste0("color:",answer.fg,";font-weight:bold;"))
        )
    }
    sub_elements <- sub_elements %>%
      tagAppendChild(li_element)

    li_element <- tags$li(
      paste0("Divide the logarithm by the number of years between ",
             current_yr, " and ", target_yr, ":"))
    if (show_answers) {
      li_element <- li_element %>%
        tagAppendChildren(br(),
                          span(paste0(target_yr, ' - ', current_yr, ' = ', delta_yr),
                               br(),
                               paste0(prt(ln_em_ratio,2), ' / ', delta_yr,
                                      ' = ',
                                      prt(rate, 3, format='fg'),
                                      ' = ',
                                      prt(rate * 100, 2), '%'),
                               br(),
                               paste0("Meeting the emissions goal of ", prt(target_em, 0),
                                      " in ", target_yr, " would require F to drop by ",
                                      prt(rate * 100, 2), "% per year"),
                               style=paste0("color:",answer.fg,";font-weight:bold;"))
        )
    }
    sub_elements <- sub_elements %>%
      tagAppendChild(li_element)

    elements <- elements %>% tagAppendChild(sub_elements)
    as.character(elements)
  })

  output$step_7 <- renderText({
    ref_yr <- input$ref_yr
    target_yr <- input$target_yr
    target_reduc <- input$target_reduc
    target_em <- target_emissions()
    current_yr <- current_year()
    delta_yr = target_yr - current_yr
    current_em <- forecast() %>% filter(variable == 'F') %>% select(current) %>% unlist()
    em_ratio <- target_em / current_em
    ln_em_ratio <- log(em_ratio)
    rate <- ln_em_ratio / delta_yr
    t <- trends()
    tP <- t$growth.rate[t$variable == 'P']
    tg <- t$growth.rate[t$variable == 'g']
    t.ef.proj <- rate - tP - tg
    t.ef.hist <- t$growth.rate[t$variable == 'ef']

    ref_yr <- input$ref_yr
    target_yr <- input$target_yr
    target_reduc <- input$target_reduc
    elements <- span(strong("Step 7: "),
                     HTML(paste0(
                     "The annual rate of change of ",
                     em("F"), ", which you calculated in the last step, is the sum of the rate of changes of ",
                     em("P"), ", ", em("g"), ", ", em("e"), ", and ", em("f"), ".",
                     " Look up the historical rate of changes of ", em("P"), " and ", em("g"),
                     ", and subtract them from the rate of change of ", em("F"), " that you calculated in the last step ",
                     "to get the decarbonization rate (the rate of change of ", em("ef"), ")."
    )))
    if (calc_show_answers()) {
      elements <- elements %>%
        tagAppendChildren(br(),
                          span(paste0(prt(rate * 100, 2), '% - (', prt(tP * 100,2), '% + ',
                                      prt(tg * 100,2), '%) = ', prt(t.ef.proj * 100,2), '% per year'),
                               br(),
                               HTML("For comparison, the historical trend for <i>ef</i> is ",
                                    prt(t.ef.hist * 100, 2), "% per year."),
                               style=paste0("color:",answer.fg,";font-weight:bold;"))
        )
    }
    as.character(elements)
  })

  output$tab_title_top_down <- renderText({
    title <- str_c("Top-down predictions of future growth rates for ",
                   input$country)
    td <- top_down_trends()
    if (is.null(td)) {
      title <- str_c(title, " are not available.")
    }
    title
  })

  output$top_down_growth_table <- renderFlexTableIf({
    td <- top_down_trends()
    if(is.null(td)) {
      ft <- NULL
    } else {
      fcast <- forecast_top_down()
      current_yr <- current_year()
      fcast <- fcast %>% mutate(growth.pct = str_c(prt(growth.rate * 100, 2), '%'),
                                current = prt(current, 3, format = 'fg'),
                                projected = prt(projected, 3, format = 'fg')) %>%
        select(variable, growth.pct, current, projected) %>%
        mutate(variable = add_units(variable))
      names(fcast) <- c('', 'Rate of Change', paste0('Current (', current_yr, ')'),
                        paste0('Projected (', input$target_yr, ')'))
      ft <- FlexTable(fcast)
      ft[,, to='header'] <- parCenter()
      ft[,1] <- parCenter()
      ft[,2:ncol(fcast)] <- parRight()
    }
    if (FALSE) {
      message("td: ", str_c(names(td), " = ", td, collapse = ", "))
      td <- td %>% rename(P = r.P, g = r.g, e = r.e) %>%
        mutate(P = 100 * P, g = 100 * g, e = 100 * e,
               G = P + g, E = G + e) %>%
        select(-country) %>%
        mutate_all(funs(prt(., 2) %>% str_c('%'))) %>%
        gather(key = Parameter, value = `Growth rate`) %>%
        mutate(Parameter = ordered(Parameter, levels = c('P', 'g', 'e', 'G', 'E')))

      ft <- FlexTable(td, header.cell.props = normal.head.props, body.cell.props = normal.body.props)
      ft[,1] <- parCenter()
      ft[,2] <- parRight()
      ft[,,to='header'] <- parCenter()
    }
    ft
  })

  output$fuel_dist <- renderText({
    if(is.null(fuel_dist())) {
       mix_title <- paste0("Energy mix for ", input$country, " is not available.")
    } else {
    mix_title <- paste0("Energy mix for ", input$country, " (quads), in ", fuel_dist()$year[[1]])
    }
    mix_title
  })

  output$fuel_dist_table <- renderFlexTableIf({
    df <- fuel_dist()
    if (is.null(df)) {
      NULL
    } else {
      df <- df %>% mutate(quads = round(quads, 2), pct = round(pct, 1)) %>%
        dplyr::select(Fuel = fuel, Quads = quads, '%' = pct) %>%
        arrange(Fuel)
      df_total = summarize(df, Fuel = 'Total', Quads = sum(Quads), `%` = sum(`%`))

      df <- df %>% bind_rows(df_total)
      ft <- FlexTable(df, header.cell.props = normal.head.props, body.cell.props = normal.body.props)
      ft[,] <- parRight()
      ft[,,to='header'] <- parCenter()
      ft
    }
  })

  output$fuel_dist_plot <- renderPlot({
    fd <- fuel_dist()
    if (is.null(fd)) {
      NULL
    } else {
    fd <- fd %>%
      arrange(fuel) %>%
      mutate(qmin = cumsum(lag(quads, default=0)), qmax = cumsum(quads))
    labels <- fd %>% mutate(label = str_c(fuel, ": ", prt(quads,2), " quads (", prt(pct,1), "%)")) %>%
      arrange(fuel) %>% select(fuel, label) %>%
      spread(key = fuel, value = label) %>% unlist()
    if (FALSE) {
      message(paste0(levels(fd$fuel), collapse=", "))
    }
    ggplot(fd, aes(ymin = qmin, ymax = qmax, fill = fuel)) +
      geom_rect(xmin = 2, xmax = 4) +
      coord_polar(theta = "y") +
      xlim(c(0,4)) +
      scale_fill_manual(values = c(Coal = '#e31a1c', 'Natural Gas' = '#fdbf6f', 'Oil' = '#ff7f00',
                                   Nuclear = '#b2df8a', Renewables = '#33a02c', Total = '#a6cee3'),
                        breaks = names(labels), labels = labels, name = "Fuel") +
      theme_bw(base_size = 20) +
      theme(panel.grid=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank())
    }
  })


  output$target_emissions <- renderText({
    target = prt(target_emissions(), 3, format = 'fg')
    ref = prt(ref_emissions(), 3, format = 'fg')
    paste0(
      as.character(h4(paste0(input$ref_yr, ' emissions = ', ref, ' MMT CO2'))),
      as.character(h4(paste0(input$target_yr, ' target: ',
                      abs(input$target_reduc), '% ',
                      ifelse(input$target_reduc >= 0, 'below', 'above'),
                      ' ', input$ref_yr, ' = ', target, ' MMT'))))
  })

  kaya_subset_plot <- reactive({
    ks <- kaya_subset()
    tsy <- input$trend_start_year
    before <- ks %>% filter(year <= tsy) %>% mutate(in.trend = FALSE)
    after <- ks %>% filter(year >= tsy) %>% mutate(in.trend = TRUE)
    bind_rows(before, after) %>% mutate(f.in.trend = factor(in.trend),
                                        id = row_number())
  })

  trend_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
    k <- isolate(kaya_subset_plot())
    v <- input$trend_variable
    r = k[k$id == x$id,]
    year = r$year
    pt = r[v]
    paste0('<b>', year, ": ", v, " = ", prt(as.numeric(pt), 2), "</b>")
  }

  decarb_tooltip <- function(x) {
    if (FALSE) {
      message("decarb_tooltip: x = ", paste(names(x), " = ", x, collapse = ', '))
    }
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
    df <- isolate(implied_decarb())
    r = df[df$id == x$id,]
    year = r$year
    pt = r$ef
    cat = r$cat
    paste0('<b>', year, ": ef(", cat, ")  = ", prt(as.numeric(pt), 2), "</b>")
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
    plot <- kaya_subset_plot %>%
      ggvis(x = ~year, y = yvar) %>%
      group_by(f.in.trend) %>%
      layer_lines(strokeWidth := 2, stroke = ~f.in.trend) %>%
      ungroup() %>%
      layer_points(size := 15, size.hover := 50,
                   fillOpacity := 1, fillOpacity.hover := 1,
                   fill = ~f.in.trend, stroke = ~f.in.trend,
                   key := ~id) %>%
      add_tooltip(trend_tooltip, "hover") %>%
      add_axis("x", title = xvar_name, format="4d",
               values = x_tics) %>%
      add_axis("y", title = as.character(yvar_name)) %>%
      scale_nominal("stroke", range = c('#FF8080', '#A00000')) %>%
      scale_nominal("fill", range = c('#FF8080', '#A00000')) %>%
      hide_legend(c('stroke', 'fill')) %>%
      set_options(width="auto", height="auto", resizable = FALSE)
    plot
  })

  trend_model <- reactive({
    var <- input$trend_variable
    ty <- input$trend_start_year
    k <- kaya_subset() %>% filter(year >= ty)
    # message("Var = ", var, " data has ", nrow(k), " rows")
    f <- substitute(log(x) ~ year, list(x = as.symbol(input$trend_variable)))
    if (nrow(k) > 0) {
      trend <- lm(as.formula(f), data = k)
      # message(summary(trend))
    } else {
      trend <- NULL
    }
    trend
  })

  tp %>% bind_shiny("trend_plot", session = session)

  tpl <- reactive({
    xvar_name <- 'Year'
    v <- input$trend_variable
    yvar_name <- with(kaya_labels[kaya_labels$variable == v,], paste0('ln(', variable, ')'))
    yvar <- prop("y", substitute(log(x), list(x = as.symbol(input$trend_variable))))
    if (is.na(history_start()))
      x_tics <- NULL
    else
      x_tics <- seq(10 * round(history_start() / 10), 10 * round(history_stop() / 10), 10)
    tsy <- input$trend_start_year
    plot <- kaya_subset_plot %>%
      #      mutate(in.trend = year >= tsy, f.in.trend = factor(in.trend)) %>%
      ggvis(x = ~year, y = yvar) %>%
      group_by(f.in.trend) %>%
      layer_lines(strokeWidth := 2, stroke = ~f.in.trend) %>%
      ungroup() %>%
      layer_points(size := 15, size.hover := 50,
                   fillOpacity := 1, fillOpacity.hover := 1,
                   fill = ~f.in.trend, stroke = ~f.in.trend,
                   key := ~id) %>%
      filter(year >= tsy) %>%
      layer_model_predictions(model = "lm") %>%
      add_tooltip(trend_tooltip, "hover") %>%
      add_axis("x", title = xvar_name, format="4d",
               values = x_tics) %>%
      add_axis("y", title = as.character(yvar_name)) %>%
      scale_nominal("stroke", range = c('#FF8080', '#A00000')) %>%
      scale_nominal("fill", range = c('#FF8080', '#A00000')) %>%
      hide_legend(c('stroke', 'fill')) %>%
      set_options(width="auto", height="auto", resizable = FALSE)
    plot
  })

  tpl %>% bind_shiny("trend_plot_ln", session = session)

  output$trend_plot_ln_title <- renderText({
    paste0('ln(', kaya_labels$long[kaya_labels$variable == input$trend_variable], ')')
  })

  output$trend_plot_title <- renderText({
    kaya_labels$long[kaya_labels$variable == input$trend_variable]
  })

  decarb_plot <- reactive({
    # message("Starting decarb_plot")
    xvar_name <- 'Year'
    yvar_name <- 'CO2 intensity (tonnes / $ million GDP)'

    if (is.na(history_start())) {
      x_tics <- NULL
    }
    else {
      x_tics <- seq(10 * round(history_start() / 10),
                    10 * round(input$target_yr / 10), 10)
    }
    message("Setting up implied decarbonization plot")
    plot <- implied_decarb() %>%
      ggvis(x = ~year, y = ~ef) %>%
      group_by(cat) %>%
      layer_lines(strokeWidth := 2, stroke = ~cat) %>%
      ungroup() %>%
      layer_points(size := 6, stroke = ~cat, fill = ~cat, key := ~id) %>%
      add_tooltip(decarb_tooltip, "hover") %>%
      add_axis("x", title = xvar_name, format="4d") %>%
      scale_numeric("x", nice=TRUE) %>%
      add_axis("y", title = yvar_name) %>%
      scale_numeric("y", nice=FALSE, zero=TRUE) %>%
      scale_nominal("stroke", range = brewer.pal(3, "Dark2")[c(2,3,1)], sort = FALSE,
                    domain = c("Historical", "Historical Trend", "Bottom-up"),
                               label = NA) %>%
      scale_nominal("fill", range = brewer.pal(3, "Dark2")[c(2,3,1)], sort = FALSE,
                    domain = c("Historical", "Historical Trend", "Bottom-up"),
                    label = NA) %>%
      set_options(width="auto", height="auto", resizable = FALSE)
    message("plot created")
    plot
  })

  decarb_plot %>% bind_shiny("implied_decarb_plot", session = session)

})
