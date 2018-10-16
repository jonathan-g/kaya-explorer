# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
# library(plotly)
library(ggvis)
library(stringr)
library(flextable)
library(officer)
library(broom)
library(RColorBrewer)
library(DT)

source('load_kaya.R')
# source('energy_by_fuel.R')

debugging = FALSE

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

# normal.body.props <- cellProperties(padding.left = 2, padding.right = 2)
# normal.head.props <- cellProperties(padding.top = 2,
#                                     padding.left = 2, padding.right = 2)
# answer.body.props <- cellProperties(padding.left = 2, padding.right = 2,
#                                     background.color = answer.bg)
# answer.head.props <- cellProperties(padding.top = 2,
#                                     padding.left = 2, padding.right = 2,
#                                     background.color = answer.bg)

normal.body.props <- list(pr_t = fp_text(font.size = 12),
                          pr_p = fp_par(padding.left = 2, padding.right = 2,
                                        padding.top = 2),
                          pr_c = fp_cell())
normal.head.props <- list(pr_t = fp_text(font.size = 12, bold = TRUE),
                          pr_p = fp_par(padding.left = 2, padding.right = 2,
                                        padding.top = 2),
                          pr_c = fp_cell())
answer.body.props <- list(pr_t = fp_text(font.size = 12),
                          pr_p = fp_par(padding.left = 2, padding.right = 2,
                                        padding.top = 2),
                          pr_c = fp_cell(background.color = answer.bg))
answer.head.props <- list(pr_t = fp_text(font.size = 12, bold = TRUE),
                          pr_p = fp_par(padding.left = 2, padding.right = 2,
                                        padding.top = 2),
                          pr_c = fp_cell(background.color = answer.bg))

theme_kaya <- function(x, answer = FALSE) {
  if (answer) {
    hprops <- answer.head.props
    bprops <- answer.body.props
  } else {
    hprops <- normal.head.props
    bprops <- normal.body.props
  }

  big_border <- fp_border(width = 2)
  std_border <- fp_border(width = 1)
  h_nrow <- flextable:::nrow_part(x, "header")
  f_nrow <- flextable:::nrow_part(x, "footer")
  b_nrow <- flextable:::nrow_part(x, "body")
  x <- border_remove(x)
  x <- style(x = x, pr_t = hprops$pr_t, pr_p = hprops$pr_p, pr_c = hprops$pr_c,
             part = "header") %>%
    style(pr_t = bprops$pr_t, pr_p = bprops$pr_p, pr_c = hprops$pr_c,
          part = "body")
  if (h_nrow > 0) {
    x <- x %>%
      hline_top(border = big_border, part = "header") %>%
      hline(border = std_border, part = "header") %>%
      hline_bottom(border = big_border, part = "header")
  }
  if (f_nrow > 0) {
    x <- x %>% hline(border = std_border, part = "footer") %>%
      hline_bottom(border = big_border, part = "footer")
  }
  if (b_nrow > 0) {
    x <- x %>% hline(border = std_border, part = "body") %>%
      hline_bottom(border = big_border, part = "body")
  }
  x
}

kaya <- load_kaya() %>% mutate(F = c.to.co2(F), f = c.to.co2(f), ef = c.to.co2(ef))
message("Kaya data loaded")

kaya$id <- seq_len(nrow(kaya))

top_down <- load_top_down()
message("Top-down data loaded")


energy_by_fuel <- load_energy_by_fuel()
message("Fuel mixes loaded")

kaya_regions <- as.character(unique(kaya$region))
td_regions <- as.character(unique(top_down$region)) %>% keep(~.x %in% kaya_regions)
ebf_regions <- as.character(unique(energy_by_fuel$region)) %>% keep(~.x %in% kaya_regions)

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

shinyServer(function(input, output, session) {
  regions <- reactive({
    if (debugging) message("regions")
    c_list <- kaya_regions
    if (input$tabs == 'Top Down') { c_list <- td_regions} else
      if (input$tabs == 'Energy Mix') { c_list <- ebf_regions }
    c_list
  })

  observeEvent(input$region, {
    if (debugging) message("Changing region #1")
    rgn <- input$region
    if (is.null(rgn)) {
      if (debugging) message("Setting NULL region")
      rgn <- 'World'
    } else if (rgn == '') {
      if (debugging) message('Setting empty region')
      rgn <- 'World'
    }
    if (debugging) message("Setting region to ", rgn)
    updateSelectInput(session, 'region', choices = regions(), selected = rgn)
  })

  kaya_subset <- eventReactive(input$region, {
    if (debugging) message("kaya_subset")
    rgn <- input$region
    if (! rgn %in% kaya_regions)
      rgn <- 'World'
    if (debugging) message("Calculating subset for ", rgn)
    ks <- kaya  %>% filter(region == rgn) %>% arrange(year)
    ks
  })

  observeEvent(input$region, {
    if (debugging) message("Changing region #2")
    if (input$region %in% td_regions) {
      if (debugging) message("enabling top down")
      js$enableTab("Top Down")
    } else {
      if (debugging) message("disabling top down")
      js$disableTab("Top Down")
    }
    if (input$region %in% ebf_regions) {
      if (debugging) message("enabling energy mix")
      js$enableTab("Energy Mix")
    } else {
      if (debugging) message("disabling energy mix")
      js$disableTab("Energy Mix")
    }
    if ("calc_show_answers" %in% names(input)) {
      if (input$region == "World") {
        if (debugging) message("enabling answers")
        shinyjs::enable("calc_show_answers")
        updateCheckboxInput(session, "calc_show_answers", value = FALSE)
      } else {
        if (debugging) message("disabling answers")
        updateCheckboxInput(session, "calc_show_answers", value = FALSE)
        shinyjs::disable("calc_show_answers")
      }
    }
  })

  last_event_time <- eventReactive( any(input$target_yr, input$target_reduc,
                                        input$ref_yr, input$trend_start_year,
                                        input$region),
                 {
                   proc.time()['elapsed']
                 })

  observeEvent( kaya_subset(), {
    if (debugging) message("Updating kaya subset")
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
    if (debugging) message("current_year")
    max(kaya_subset()$year, na.rm = T)
  })

  history_start <- reactive({
    if (debugging) message("history_start")
    min(kaya_subset()$year, na.rm = T)
  })

  history_stop <- reactive({
    if (debugging) message("history_stop")
    max(kaya_subset()$year)
  })

  ref_emissions <- reactive({
    if (debugging) message("ref_emissions")
    ref_yr <- input$ref_yr
    re <- kaya_subset()
    if (debugging) message("ref_emissions is a ", str_c(class(re), collapse = ", "))
    re <- re %>%
      filter(year == ref_yr) %>% select(F) %>% unlist()
    if (debugging) message("Filtered ref_emissions")
    re
  })

  target_emissions <- reactive({
    if (debugging) message("target_emissions")
    te <- ref_emissions() * (1. - input$target_reduc / 100.)
    if (debugging) message("finished target_emissions")
    te
  })

  calc_show_answers <- reactive({
    if (debugging) message("calc_show_answers")
    if ('calc_show_answers' %in% names(input))
      return(input$calc_show_answers)
    else
      return(FALSE)
  })

  trends <- reactive({
    if (debugging) message("trends")
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
    if (debugging) message("top_down_trends")
    td <- top_down
    td <- td %>% dplyr::filter(region == input$region) %>%
      mutate(g = G - P, e = E - G, f = F - E) %>%
      gather(key = variable, value = growth.rate, -region) %>%
      mutate(growth.rate = growth.rate * 0.01)
    if (nrow(td) == 0) {
      td  <- data.frame(variable = vars, growth.rate = NA)
    }
    td
  })

  fuel_dist <- reactive({
    if (debugging) message("fuel_dist")
    fd <- energy_by_fuel %>% filter(region == input$region)
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
      fd <- fd %>% filter(year == y) %>%
        mutate(fuel = fct_recode(fuel, Renewables = "Hydro")) %>%
        group_by(fuel) %>% summarize(quads = sum(quads), pct = sum(pct)) %>% ungroup()
    }
    if (debugging) message(print(fd))
    invisible(fd)
  })

  forecast <- reactive({
    if (debugging) message("forecast")
    t <- trends() %>% mutate(variable = ordered(variable, levels = variable))
    current_yr <- current_year()
    ks <- kaya_subset() %>% filter(year == current_yr) %>%
      select_(.dots = as.character(t$variable)) %>%
      gather(key = variable, value = current)
    t <- merge(t, ks)
    t <- mutate(t, projected = current * exp((input$target_yr - current_yr) * growth.rate)) %>%
      arrange(variable)
    if (debugging) message("finished forecast")
    t
  })

  forecast_top_down <- reactive({
    if (debugging) message("forecast_top_down")
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
    if (debugging) message("implied_decarb_rate")
    ks <- kaya_subset() %>% mutate(cat = 'Historical')
    fcast <- forecast()
    target_em <- target_emissions()
    target_yr <- input$target_yr
    current_yr <- current_year()
    delta_yr <- target_yr - current_yr
    current_em <- fcast$current[fcast$variable == 'F']
    rate_G <- fcast$growth.rate[fcast$variable == 'G']

    rate = log(target_em / current_em) / delta_yr - rate_G

    if (TRUE) {
      if (debugging) message("target_em = ", prt(target_em,0), ", rate_G = ",
              prt(rate_G * 100, 2), "%, target ef rate  = ",
              prt(rate * 100,2), "%")
    }
    rate
  })

  implied_decarb <- reactive({
    if (debugging) message("implied_decarb")
    ks <- kaya_subset() %>% select(year, ef) %>% mutate(cat = 'Historical')
    fcast <- forecast()
    target_yr <- input$target_yr
    current_yr <- current_year()
    current_ef <- fcast$current[fcast$variable == 'ef']
    trend_ef <- fcast$growth.rate[fcast$variable == 'ef']

    rate <- implied_decarb_rate()

    if (FALSE) {
      if (debugging) message("implied decarb rate = ", prt(100 * rate, 2), "%")
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
    if (debugging) message("output$trend_display")
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
    if (debugging) message("output$historical_table")
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
      input$region %>% str_replace_all('[^A-Za-z0-9]+', '_') %>%str_c('.csv')
      },
    content = function(file) {
      write_csv(
        kaya_subset() %>% select(-region, -geography, -id),
        path = file)
    })

  output$downloadFuelData <- downloadHandler(
    filename = function() {
      input$region %>% str_replace_all('[^A-Za-z0-9]+', '_') %>% str_c('_fuel.csv')
    },
    content = function(file) {
      df <- fuel_dist() %>% mutate(quads = round(quads, 2), pct = round(pct, 2)) %>%
        dplyr::select(Fuel = fuel, Quads = quads, Percent = pct) %>%
        arrange(Fuel)
      df_total = summarize(df, Fuel = 'Total', Quads = sum(Quads), Percent = sum(Percent))

      df <- df %>% bind_rows(df_total)
      if (debugging) message("df has ", nrow(df), " rows")
      if (debugging) message("file = ", file)
      write_csv(df, path = file)
    })

  output$policy_goal <- renderText({
    if (debugging) message("output$policy_goal")
    as.character(span(strong('Policy goal: '), paste0(input$target_yr, ' emissions ',
                      abs(input$target_reduc), '% ',
                      ifelse(input$target_reduc >= 0, 'below', 'above'), ' ', input$ref_yr)))
  })

  trend_title <- reactive({
    if (debugging) message("trend_title")
    if ('analysis' %in% names(input)) {
      title <- c(top.down = "Top Down", bottom.up = "Bottom up")[input$analysis]
    } else {
      title <- 'Decarbonization'
    }
    str_c(title, " Analysis")
  })

  output$trend_title <- renderText({
    if (debugging) message("output$trend_title")
    as.character(h4(strong(trend_title())))
  })

  output$tab_title_trend <- renderText({
    if (debugging) message("output$tab_title_trend")
    paste("Historical Trends for", input$region)
  })

  output$tab_title_calc <- renderText({
    if (debugging) message("output$tab_title_calc")
    paste("Calcuating Implied Decarbonization for", input$region)
  })

  output$tab_title_decarb <- renderText({
    if (debugging) message("output$tab_title_decarb")
    paste("Implied Decarbonization for", input$region)
  })

  output$tab_title_fuel_dist <- renderText({
    if (debugging) message("output$tab_title_fuel_dist")
    x <- paste0("Energy Mix for ", input$region)
    if (! is.null(fuel_dist())) {
      x <- paste0(x, " in ", fuel_dist()$year[1])
    } else {
      x <- paste0(x, " is not available")
    }
    x
  })

  output$tab_title_historical <- renderText({
    if (debugging) message("output$tab_title_historical")
    paste("Historical Data for", input$region)
  })


  output$trend_table <- renderUI({
    if (debugging) message("output$trend_table")
    fcast <- forecast()
    current_yr <- current_year()
    fcast <- fcast %>% mutate(growth.pct = str_c(prt(growth.rate * 100, 2), '%'),
                              current = prt(current, 3, format = 'fg'),
                              projected = prt(projected, 3, format = 'fg')) %>%
      select(variable, growth.pct, current, projected) %>%
      mutate(variable = add_units(variable))
    if (debugging) message("fcast: (", str_c(names(fcast), collapse = ", "), ")")
    ft <- flextable(fcast) %>%
      autofit(add_w = 0, add_h = 0) %>%
      set_header_labels(variable = '', growth.pct = 'Rate of Change',
                        current = paste0('Current (', current_yr, ')'),
                        projected = paste0('Projected (', input$target_yr, ')')) %>%
      # align(align = "center", part = "body", j = 1) %>%
      theme_kaya() %>%
      align(align = "center", part = "header") %>%
      align(align = "right", part = "body") %>%
      htmltools_value()
    if (debugging) message("finished output$trend_table")
    ft
  })

  output$step_1 <- renderText({
    if (debugging) message("output$step_1")
    paste0(strong("Step 1: "), "Write down current (",
           current_year(), ") values for ",
           em('P'), ", ", em('g'), ", ", em('e'), ", and ", em('f'),
           ", and their historical rates of change.")
  })

  output$step_1_table <- renderUI({
    if (debugging) message("output$step_1_table")
    if (calc_show_answers()) {
      target_yr <- input$target_yr
    current_yr <- current_year()
    fcast <- forecast() %>% filter(variable %in% c('P', 'g', 'e', 'f')) %>%
      mutate(variable = add_units(variable),
             current = prt(current, 3, format = 'fg'),
             growth.rate = paste0(prt(growth.rate * 100, 2), '%')) %>%
      select(variable, current, growth.rate)
    ft <- flextable(fcast) %>%
      autofit() %>%
      set_header_labels(variable = '',
                        current = paste0("Current"," (",current_yr,")"),
                        growth.rate = "Rate of Change") %>%
      theme_kaya(TRUE) %>%
      align(part = "body", align = "right") %>%
      align(part = "body", align = "center", j = 1) %>%
      htmltools_value()
    } else {
      NULL
    }
  })

  output$step_2 <- renderText({
    if (debugging) message("output$step_2")
    paste0(strong("Step 2: "), "Which variables seem to follow a relatively constant rate of change, ",
           "and which do not?")
  })

  output$step_2_table <- renderUI({
    if (debugging) message("output$step_2_table")
    if (calc_show_answers()) {
      t <- trends() %>%
        mutate(goodness = map(r.squared, goodness_of_fit) %>%
                 simplify()) %>%
        select(variable, goodness)

      if (FALSE) {
        if (debugging) message(print(t))
      }

      ft <- flextable(t) %>%
        autofit() %>%
        set_header_labels(variable = 'variable',
                          goodness = "Goodness of Fit") %>%
        theme_kaya(TRUE) %>%
        align(part = "body", align = "center") %>%
        align(part = "body", align = "center", j = 1) %>%
        htmltools_value()
    } else {
      NULL
    }
  })

  output$step_3 <- renderText({
    if (debugging) message("output$step_3")
    paste0(strong("Step 3: "), "Calculate the projected values for ",
           em('P'), ", ", em('g'), ", ", em('e'), ", and ", em('f'),
           ", in ", input$target_yr, '.',
           ' Show your work, and check your answers against the &ldquo;',
           trend_title(),
           '&rdquo; table on the left-hand panel.',
           ' (Remember to divide percentages by 100)')
  })

  output$step_3_formula <- renderUI({
    if (debugging) message("output$step_3_formula")
    if (calc_show_answers()) {
    current_yr = current_year()
    target_yr = input$target_yr
    withMathJax(span(paste0("If variable \\(x\\) has value \\(x(",current_yr,")\\) in ",
                       current_yr, " and its rate of change is \\(r\\), then in ",
                       target_yr, " its value will be \\(x(",
                       target_yr,") = x(", current_yr, ") \\times \\exp(r \\times (",
                       target_yr, "-", current_yr,")) = x(", current_yr, ")  \\times \\exp(r \\times ",
                       (target_yr - current_yr), "))\\)"),
                     style=paste0("color:", answer.fg, ";font-weight:bold;")))
    } else {
      NULL
    }
  })

  output$step_3_table <- renderUI({
    if (debugging) message("output$step_3_table")
    if (calc_show_answers()) {
      target_yr <- input$target_yr
      current_yr <- current_year()
      fcast <- forecast() %>% filter(variable %in% c('P', 'g', 'e', 'f')) %>%
        mutate(variable = add_units(variable),
               projected = prt(projected, 3, format = 'fg')) %>%
        select(variable, projected)
      ft <- flextable(fcast) %>%
        autofit() %>%
        set_header_labels(variable = "",
                          projected = paste0("Projected (", target_yr, ")")) %>%
        theme_kaya(TRUE) %>%
        align(part = "body", align = "right") %>%
        align(part = "body", align = "center", j = 1) %>%
        htmltools_value()
    } else {
      NULL
    }
  })


  output$step_4 <- renderText({
    if (debugging) message("output$step_4")
    paste0(strong("Step 4: "), "Multiply the variables together to get ", em("F"), " for each year.",
           ' Show your work, and check your answers against the &ldquo;',
           trend_title(),
           '&rdquo; table on the left-hand panel.')
  })

  output$step_4_table <- renderUI({
    if (debugging) message("output$step_4_table")
    if(calc_show_answers()) {
      target_yr <- input$target_yr
      current_yr <- current_year()
      fcast <- forecast() %>% filter(variable %in% c('F')) %>%
        mutate(variable = add_units(variable), current = prt(current, 3, format = 'fg'),
               projected = prt(projected, 3, format = 'fg'))
      fcast <- fcast %>% select(variable, current, projected)
      ft <- flextable(fcast) %>%
        autofit() %>%
        set_header_labels(variable = "",
                          current = str_c("Current (", current_yr, ")"),
                          projected = str_c("Projected (", target_yr, ")")) %>%
        theme_kaya(TRUE) %>%
        align(part = "body", align = "right") %>%
        align(part = "body", align = "center", j = 1) %>%
        htmltools_value()
    } else {
      NULL
    }
  })

  output$step_5 <- renderText({
    if (debugging) message("output$step_5")
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

  output$step_5_table <- renderUI({
    if (debugging) message("output$step_5_table")
    if (calc_show_answers()) {
      ref_yr <- input$ref_yr
      target_yr <- input$target_yr

      df <- data.frame(
        ref = prt(ref_emissions(), 0),
        target = prt(target_emissions(), 0))
      ft <- flextable(df) %>%
        autofit() %>%
        set_header_labels(ref = str_c("Emissions in ", ref_yr),
                          target = str_c("Target emissions in ", target_yr)) %>%
        theme_kaya(TRUE) %>%
        align(part = "body", align = "right") %>%
        align(part = "body", align = "center", j = 1) %>%
        htmltools_value()
    } else {
      NULL
    }
  })

  output$step_6 <- renderText({
    if (debugging) message("output$step_6")
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
    if (debugging) message("output$step_7")
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
    if (debugging) message("output$tab_title_top_down")
    title <- str_c("Top-down predictions of future growth rates for ",
                   input$region)
    td <- top_down_trends()
    if (is.null(td)) {
      title <- str_c(title, " are not available.")
    }
    title
  })

  output$fuel_dist <- renderText({
    if (debugging) message("output$fuel_dist")
    if(is.null(fuel_dist())) {
       mix_title <- paste0("Energy mix for ", input$region, " is not available.")
    } else {
    mix_title <- paste0("Energy mix for ", input$region, " (quads), in ", fuel_dist()$year[[1]])
    }
    mix_title
  })

  output$fuel_dist_table <- renderUI({
    if (debugging) message("output$fuel_dist_table")
    df <- fuel_dist()
    if (is.null(df)) {
      NULL
    } else {
      df <- df %>% mutate(quads = round(quads, 2), pct = round(pct, 1)) %>%
        dplyr::select(fuel, quads, pct) %>%
        arrange(fuel)
      df_total = summarize(df, fuel = 'Total', quads = sum(quads), pct = sum(pct))

      ft <- flextable(df) %>%
        autofit() %>%
        set_header_labels(fuel = "Fuel", quads = "Quads", pct = "%") %>%
        theme_kaya() %>%
        align(part = "body", align = "right") %>%
        align(part = "body", align = "center", j = 1) %>%
      htmltools_value()
    }
  })

  output$fuel_dist_plot <- renderPlot({
    if (debugging) message("output$fuel_dist_plot")
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
      if (debugging) message(paste0(levels(fd$fuel), collapse=", "))
    }
    if (debugging) message("In output$fuel_dist_plot, fd is a ", str_c(class(fd), collapse = ", "))
    ggplot(fd, aes(ymin = qmin, ymax = qmax, fill = fuel)) +
      geom_rect(xmin = 2, xmax = 4) +
      coord_polar(theta = "y") +
      xlim(c(0,4)) +
      scale_fill_manual(values = c(Coal = '#e31a1c', 'Natural Gas' = '#fdbf6f', 'Oil' = '#ff7f00',
                                   Nuclear = '#33a02c', Renewables = '#b2df8a', Total = '#a6cee3'),
                        breaks = names(labels), labels = labels, name = "Fuel") +
      theme_bw(base_size = 20) +
      theme(panel.grid=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank())
    }
  })


  output$target_emissions <- renderText({
    if (debugging) message("output$target_emissions")
    target = prt(target_emissions(), 3, format = 'fg')
    ref = prt(ref_emissions(), 3, format = 'fg')
    x <- paste0(
      as.character(h4(paste0(input$ref_yr, ' emissions = ', ref, ' MMT CO2'))),
      as.character(h4(paste0(input$target_yr, ' target: ',
                      abs(input$target_reduc), '% ',
                      ifelse(input$target_reduc >= 0, 'below', 'above'),
                      ' ', input$ref_yr, ' = ', target, ' MMT'))))
    if (debugging) message("finished output$target_emissions")
    x
  })

  kaya_subset_plot <- reactive({
    if (debugging) message("kaya_subset_plot")
    tv <- sym(input$trend_variable)
    ks <- kaya_subset() %>% filter(! is.na(!!tv))
    tsy <- input$trend_start_year
    before <- ks %>% filter(year <= tsy) %>% mutate(in.trend = FALSE)
    after <- ks %>% filter(year >= tsy) %>% mutate(in.trend = TRUE)
    bind_rows(before, after) %>% mutate(f.in.trend = factor(in.trend),
                                        id = row_number())
  })

  trend_tooltip <- function(x) {
    if (debugging) message("trend_tooltip")
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
    if (debugging) message("decarb_tooltip")
    if (FALSE) {
      if (debugging) message("decarb_tooltip: x = ", paste(names(x), " = ", x, collapse = ', '))
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
    if (debugging) message("tp")
    xvar_name <- 'Year'
    v <- input$trend_variable
    yvar_name <- with(kaya_labels[kaya_labels$variable == v,], paste0(variable, ' (', unit, ')'))
    yvar <- prop("y", as.symbol(input$trend_variable))
    if (is.na(history_start()))
      x_tics <- NULL
    else
      x_tics <- seq(10 * round(history_start() / 10), 10 * round(history_stop() / 10), 10)
    ksp <- kaya_subset_plot()
    if (debugging) message("In tp, ksp is a ", str_c(class(ksp), collapse = ", "))
    plot <- ksp %>%
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
      hide_legend(scales = c("stroke", "fill")) %>%
      set_options(width="auto", height="auto", resizable = FALSE)
    if (debugging) message("finished tp")
    plot
  })

  trend_model <- reactive({
    if (debugging) message("trend_model")
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
    if (debugging) message("tpl")
    xvar_name <- 'Year'
    v <- input$trend_variable
    yvar_name <- with(kaya_labels[kaya_labels$variable == v,], paste0('ln(', variable, ')'))
    # tv <- sym(input$trend_variable)
    # yvar <- expr(log(!!tv))
    # if (is.na(history_start()))
    #   x_tics <- NULL
    # else
    #   x_tics <- seq(10 * round(history_start() / 10), 10 * round(history_stop() / 10), 10)
    # tsy <- input$trend_start_year
    # ksp <- kaya_subset_plot()
    # if (debugging) message("In tpl, ksp is a ", str_c(class(ksp), collapse = ", "),
    #                        " with columns ", str_c(names(ksp), collapse = ", "),
    #                        " and yvar = ", yvar,
    #                        " yvar is a ", str_c(class(yvar), collapse = ", "))
    #
    # # plot <- ksp %>%
    # #   mutate(in.trend = year >= tsy, f.in.trend = factor(in.trend)) %>%
    # # plot <- ggplot(ksp, aes(x = year, y = !!yvar, color = f.in.trend)) +
    # plot <- ggplot(ksp, aes(x = year, y = P)) +
    # geom_point(size = 15, na.rm = TRUE) + geom_line(size = 1, na.rm = TRUE)
    # message("started plot")
    # plot <- plot +
    #   geom_smooth(method = "lm", data = filter(kaya_subset_plot, year >= tsy),
    #               na.rm = TRUE, fill = NA) +
    #   scale_color_manual(values = c("#FF8080", "A00000"), guide = "none") +
    #   theme_bw()
    # message("finished plot")
    #
    #   # geom_plotly(plot, tooltip = c("x", "y"))

    tsy <- input$trend_start_year
    yvar <- prop("y", as.symbol(input$trend_variable))
    if (is.na(history_start()))
      x_tics <- NULL
    else
      x_tics <- seq(10 * round(history_start() / 10), 10 * round(history_stop() / 10), 10)
    ksp <- kaya_subset_plot()
    if (debugging) message("In tpl, ksp is a ", str_c(class(ksp), collapse = ", "),
                           " with columns ", str_c(names(ksp), collapse = ", "),
                           " and yvar = ", yvar,
                           " yvar is a ", str_c(class(yvar), collapse = ", "))

    plot <- ksp %>%
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
      hide_legend(scales = c("stroke", "fill")) %>%
      set_options(width="auto", height="auto", resizable = FALSE)

    if (debugging) message("finished tpl")
    plot
  })

  tpl %>% bind_shiny("trend_plot_ln", session = session)

  output$trend_plot_ln_title <- renderText({
    if (debugging) message("output$trend_plot_ln_title")
    paste0('ln(', kaya_labels$long[kaya_labels$variable == input$trend_variable], ')')
  })

  output$trend_plot_title <- renderText({
    if (debugging) message("output$trend_plot_title")
    kaya_labels$long[kaya_labels$variable == input$trend_variable]
  })

  decarb_plot <- reactive({
    if (debugging) message("decarb_plot")
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
    if (debugging) message("Setting up implied decarbonization plot")
    idc <- implied_decarb()
    if (debugging) message("idc is a ", str_c(class(idc), collapse = ", "))
    plot <- idc %>% filter(! is.na(ef), year >= 1980) %>%
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
    if (debugging) message("plot created")
    plot
  })

  decarb_plot %>% bind_shiny("implied_decarb_plot", session = session)

})
