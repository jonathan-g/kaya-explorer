# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#' @include globals.R load_kaya.R
NULL


goodness_of_fit <- function(r_squared) {
  gof <- NA
  if(r_squared >= 0.99)
    gof <- "excellent"
  else if (r_squared >= 0.98)
    gof <- "very good"
  else if (r_squared >= 0.95)
    gof <- "good"
  else if (r_squared >= 0.90)
    gof <- "all right, but with some discrepancies"
  else if (r_squared >= 0.80)
    gof <- "not so good"
  else
    gof <- "poor"
  gof
}


#' Convert mass of carbon to mass of CO2
#'
#' Converts a mass of carbon atoms in CO2 molecules into a mass of CO2
#'
#' @param x Mass of carbon atoms.
#'
#' @return Mass of CO2
#'
#' @examples
#' c_to_co2(100)
#'
#' @export
#'
c_to_co2 <- function(x) {
  x * get("mass.co2", envir = .globalvars) / get("mass.c", envir = .globalvars)
}

#' Convert mass of CO2 to the equivalent mass of carbon atoms
#'
#' Converts a mass of CO2 molecules into the equivalent mass of carbon atoms.
#'
#' @param x Mass of CO2.
#'
#' @return Mass of carbon atoms
#'
#' @examples
#' co2_to_c(100)
#'
#' @export
#'
co2_to_c <- function(x) {
  x * get("mass.c", envir = .globalvars) / get("mass.co2", envir = .globalvars)
}

theme_kaya <- function(x, answer = FALSE) {
  if (answer) {
    hprops <- get("answer.head.props", envir = .globalvars)
    bprops <- get("answer.body.props", envir = .globalvars)
  } else {
    hprops <- get("normal.head.props", envir = .globalvars)
    bprops <- get("normal.body.props", envir = .globalvars)
  }

  big_border <- officer::fp_border(width = 2)
  std_border <- officer::fp_border(width = 1)
  h_nrow <- flextable::nrow_part(x, "header")
  f_nrow <- flextable::nrow_part(x, "footer")
  b_nrow <- flextable::nrow_part(x, "body")
  x <- flextable::border_remove(x)
  x <- flextable::style(x = x, pr_t = hprops$pr_t, pr_p = hprops$pr_p, pr_c = hprops$pr_c,
                        part = "header") %>%
    flextable::style(pr_t = bprops$pr_t, pr_p = bprops$pr_p, pr_c = hprops$pr_c,
                     part = "body")
  if (h_nrow > 0) {
    x <- x %>%
      flextable::hline_top(border = big_border, part = "header") %>%
      flextable::hline(border = std_border, part = "header") %>%
      flextable::hline_bottom(border = big_border, part = "header")
  }
  if (f_nrow > 0) {
    x <- x %>% flextable::hline(border = std_border, part = "footer") %>%
      flextable::hline_bottom(border = big_border, part = "footer")
  }
  if (b_nrow > 0) {
    x <- x %>% flextable::hline(border = std_border, part = "body") %>%
      flextable::hline_bottom(border = big_border, part = "body")
  }
  x
}

prt <- function(x, digits = NULL, big.mark = ',', format = 'f') {
  formatC(x, digits = digits, big.mark = big.mark, format = format)
}

add_units <- function(variables) {
  kaya_labels <- get("kaya_labels", envir = .globalvars)
  indices = pmatch(variables, kaya_labels$variable)
  suppressWarnings(stringr::str_c(kaya_labels$varname[indices], ' (',
                                  kaya_labels$unit[indices], ')'))
}

server <- function(input, output, session) {
  # Static variables
  debugging <- get("debugging", envir = .globalvars)
  max_year <- max(kayadata::kaya_data$year)
  min_year <- min(kayadata::kaya_data$year)
  kaya_labels <- get("kaya_labels", envir = .globalvars)
  kaya_regions <- get("kaya_regions", envir = .globalvars)
  td_regions <- get("td_regions", envir = .globalvars)
  ebf_regions <- get("ebf_regions", envir = .globalvars)
  answer.fg <- get("answer.fg", envir = .globalvars)
  answer.bg <- get("answer.bg", envir = .globalvars)
  answers_available <- shiny::reactive(
    enable_answers && input$region == "World"
  )

  regions <- shiny::reactive({
    if (debugging) message("regions")
    c_list <- kaya_regions
    if (input$tabs == 'Top Down') { c_list <- td_regions} else
      if (input$tabs == 'Energy Mix') { c_list <- ebf_regions }
    c_list
  })

  shiny::observeEvent(input$region, {
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
    shiny::updateSelectInput(session, 'region', choices = regions(), selected = rgn)
  })

  kaya_subset <- shiny::eventReactive(input$region, {
    if (debugging) message("kaya_subset")
    rgn <- input$region
    if (! rgn %in% kaya_regions)
      rgn <- 'World'
    if (debugging) message("Calculating subset for ", rgn)
    ks <- extract_kaya(rgn) %>%
      dplyr::arrange(.data$year)
    ks
  })

  shiny::observeEvent(input$region, {
    if (debugging) message("Changing region #2")
    if (debugging) {
      fn_lst <- ls(shinyjs::js) %>%
        purrr::set_names() %>%
        purrr::map_chr(~class(get(.x, envir = shinyjs::js)) %>%
                         stringr::str_c(collapse = ", "))
      message("JS functions = ")
      count =
        for (n in names(fn_lst)) {
          message(n, ": ", fn_lst[n])
        }
      message("\n")
    }
    if (input$region %in% td_regions) {
      if (debugging) message("enabling top down")
      shinyjs::js$enableTab("Top Down")
    } else {
      if (debugging) message("disabling top down")
      shinyjs::js$disableTab("Top Down")
    }
    if (input$region %in% ebf_regions) {
      if (debugging) message("enabling energy mix")
      shinyjs::js$enableTab("Energy Mix")
    } else {
      if (debugging) message("disabling energy mix")
      shinyjs::js$disableTab("Energy Mix")
    }
    if ("calc_show_answers" %in% names(input)) {
      if (answers_available()) {
        if (debugging) message("enabling answers")
        shinyjs::show("calc_show_answers")
        shinyjs::enable("calc_show_answers")
        shiny::updateCheckboxInput(session, "calc_show_answers", value = FALSE)
      } else {
        if (debugging) message("disabling answers")
        shiny::updateCheckboxInput(session, "calc_show_answers", value = FALSE)
        shinyjs::disable("calc_show_answers")
        shinyjs::hide("calc_show_answers")
      }
    }
  })

  last_event_time <- shiny::eventReactive(
    any(input$target_yr, input$target_reduc, input$ref_yr,
        input$trend_start_year, input$region),
    { proc.time()['elapsed'] }
  )

  shiny::observeEvent( kaya_subset(), {
    if (debugging) message("Updating kaya subset")
    ks <- kaya_subset()
    ks.min.yr <- min(ks$year)
    ks.max.yr <- max(ks$year)
    v <- input$ref_yr
    v2 <- min(max(v, ks.min.yr), ks.max.yr)

    shiny::updateNumericInput(session, 'ref_yr',
                              min = ks.min.yr, max = ks.max.yr, step = 1, value = v2)
    v <- max(input$target_yr, ks.max.yr)
    shiny::updateNumericInput(session, 'target_yr',
                              min = ks.max.yr, step = 1, value = v)
    v <- min(max(input$trend_start_year, ks.min.yr), ks.max.yr)
    shiny::updateNumericInput(session, 'trend_start_year',
                              min = ks.min.yr, max = ks.max.yr, step = 1, value = v)
  })

  current_year <- shiny::reactive({
    if (debugging) message("current_year")
    max(kaya_subset()$year, na.rm = T)
  })

  history_start <- shiny::reactive({
    if (debugging) message("history_start")
    min(kaya_subset()$year, na.rm = T)
  })

  history_stop <- shiny::reactive({
    if (debugging) message("history_stop")
    max(kaya_subset()$year)
  })

  ref_emissions <- shiny::reactive({
    if (debugging) message("ref_emissions")
    ref_yr <- input$ref_yr
    re <- kaya_subset()
    if (debugging) message("ref_emissions is a ",
                           stringr::str_c(class(re), collapse = ", "))
    re <- re %>%
      dplyr::filter(.data$year == ref_yr) %>% dplyr::pull("F")
    if (debugging) message("Filtered ref_emissions")
    re
  })

  target_emissions <- shiny::reactive({
    if (debugging) message("target_emissions")
    te <- ref_emissions() * (1. - input$target_reduc / 100.)
    if (debugging) message("finished target_emissions")
    te
  })

  calc_show_answers <- shiny::reactive({
    if (debugging) message("calc_show_answers")
    if ('calc_show_answers' %in% names(input))
      return(input$calc_show_answers)
    else
      return(FALSE)
  })

  trends <- shiny::reactive({
    if (debugging) message("trends")
    trend.start <- input$trend_start_year
    ks <- kaya_subset() %>% dplyr::filter(.data$year >= trend.start)
    vars <- c('P','g', 'e', 'f', 'ef', 'gef', 'G', 'E', 'F')
    ks <- ks %>% dplyr::mutate(dplyr::across(dplyr::any_of(vars),
                               ~ifelse(!is.na(.x) & .x <= 0, NA, .x)))
    if (nrow(ks) > 0) {
      t <- purrr::map_df(
        vars,
        function(.x) {
          if (debugging) {
            message("Fitting trend to ", .x, ": vars = ",
                    stringr::str_c(names(ks), collapse = ", "))
          }
          tidy <- broom::tidy
          fmla <- rlang::new_quosure(expr(log(!!(sym(.x))) ~ year))
          if (debugging) message("Formula = ", as_label(fmla))
          mdl <- stats::lm(fmla, data = ks, na.action = stats::na.exclude)
          if (debugging) message("Done fitting trend.")
          r_sq <- mdl %>% broom::glance() %>%
            dplyr::pull("adj.r.squared")
          growth <- mdl %>% tidy() %>%
            dplyr::filter(.data$term == "year") %>%
            dplyr::pull("estimate")
          if (debugging) message("Estracted r.squared and rate.")
          dplyr::tibble(variable = .x, growth.rate = growth, r_squared = r_sq)
        }
      )
    } else {
      t <- dplyr::tibble(variable = vars, growth.rate = NA, r_squared = NA)
    }
  })

  top_down_trends <- shiny::reactive({
    if (debugging) message("top_down_trends")
    td <- extract_td_trends(input$region) %>%
      tidyr::gather(key = "variable", value = "growth.rate", -"region") %>%
      if (nrow(td) == 0) {
        td  <- dplyr::tibble(variable = vars, growth.rate = NA)
      }
    td
  })

  fuel_dist <- shiny::reactive({
    if (debugging) message("fuel_dist")
    df <- extract_fuel_mix(input$region)
    if (nrow(df) == 0) {
      fd <- NULL
    } else {
      if (current_year() %in% df$year) {
        y <- current_year()
      } else if (current_year() > max(df$year)) {
        y <- max(df$year)
      } else if (current_year() < min(df$year)) {
        y <- min(df$year)
      } else {
        stop("Cannot figure out year for current year = ", current_year(),
             " and df range = (", stringr::str_c(range(df$year), collapse = ', '), ")")
      }
      df <- df %>% dplyr::filter(.data$year == y) %>%
        dplyr::mutate(fuel = forcats::fct_recode(.data$fuel, Renewables = "Hydro")) %>%
        dplyr::summarize(quads = sum(.data$quads), frac = sum(.data$frac),
                         .by = "fuel")
      fd <- list(year = y, df = df)
    }
    if (debugging) message(print(df))
    invisible(fd)
  })

  forecast <- shiny::reactive({
    if (debugging) message("forecast")
    t <- trends() %>%
      dplyr::mutate(variable = ordered(.data$variable, levels = .data$variable))
    current_yr <- current_year()
    if (debugging)
      message("t$variable = ", stringr::str_c('"', t$variable, '"', collapse = ", "))
    ks <- kaya_subset() %>%
      dplyr::select("year", dplyr::all_of(t$variable)) %>%
      tidyr::gather(-"year", key = "variable", value = "current") %>%
      dplyr::filter(! is.na(.data$current)) %>%
      dplyr::group_by("variable") %>% dplyr::top_n(1, .data$year) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(variable = ordered(.data$variable,
                                       levels = levels(t$variable)))
    t <- dplyr::left_join(t, ks)
    t <- dplyr::mutate(
      t,
      projected = .data$current * exp((input$target_yr - .data$year) *
                                        .data$growth.rate),
      current = ifelse(.data$year == current_yr, .data$current,
                       .data$current * exp((current_yr - .data$year) *
                                             .data$growth.rate)
      )
    ) %>%
      dplyr::arrange(.data$variable) %>% dplyr::select(-"year")
    if (debugging) message("finished forecast")
    t
  })

  forecast_top_down <- shiny::reactive({
    if (debugging) message("forecast_top_down")
    t <- top_down_trends() %>%
      dplyr::mutate(variable = ordered(.data$variable, levels = .data$variable))
    current_yr <- current_year()
    ks <- kaya_subset() %>% dplyr::filter(.data$year == current_yr) %>%
      dplyr::select(dplyr::all_of(t$variable)) %>%
      tidyr::gather(key = "variable", value = "current")
    t <- merge(t, ks) %>%
      dplyr::mutate(
        projected = .data$current * exp((input$target_yr - current_yr) *
                                          .data$growth.rate)
      ) %>% dplyr::arrange(.data$variable)
    t
  })

  implied_decarb_rate <- shiny::reactive({
    if (debugging) message("implied_decarb_rate")
    ks <- kaya_subset() %>% dplyr::mutate(cat = 'Historical')
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

  implied_decarb <- shiny::reactive({
    if (debugging) message("implied_decarb")
    ks <- kaya_subset() %>% dplyr::select("year", "ef") %>%
      dplyr::mutate(cat = 'Historical')
    fcast <- forecast()
    target_yr <- input$target_yr
    current_yr <- current_year()
    current_ef <- fcast$current[fcast$variable == 'ef']
    trend_ef <- fcast$growth.rate[fcast$variable == 'ef']

    rate <- implied_decarb_rate()

    if (FALSE) {
      if (debugging) message("implied decarb rate = ", prt(100 * rate, 2), "%")
    }

    mdl <- stats::lm(log(ef) ~ year,
                     data = dplyr::filter(ks, .data$year >= input$trend_start_year),
                     na.action = stats::na.exclude)

    years <- seq(history_start(), target_yr)
    extrap <- dplyr::tibble(year = years,
                            ef = exp(stats::predict(
                              mdl, newdata = dplyr::tibble(year = years))
                            ),
                            cat = "Historical Trend")

    bottom_up <- dplyr::tibble(year = seq(current_yr, target_yr)) %>%
      dplyr::mutate(ef = current_ef * exp(rate * (.data$year - current_yr)),
                    cat = 'Bottom-up')

    if (debugging) {
      message("ks has: ", stringr::str_c(names(ks), collapse = ", "),
              ", with types ", stringr::str_c(purrr::map_chr(ks, typeof), collapse = ", "))
      message("extrap has: ", stringr::str_c(names(extrap), collapse = ", "),
              ", with types ", stringr::str_c(purrr::map_chr(extrap, typeof), collapse = ", "))
      message("bottom_up has: ", stringr::str_c(names(bottom_up), collapse = ", "),
              ", with types ", stringr::str_c(purrr::map_chr(bottom_up, typeof), collapse = ", "))
    }
    ks <- ks %>%
      dplyr::bind_rows(extrap, bottom_up) %>% dplyr::mutate(id = dplyr::row_number())
    ks
  })

  output$trend_display <- shiny::renderText({
    if (debugging) message("output$trend_display")
    v <- input$trend_variable
    k.label <- kaya_labels %>% dplyr::filter(.data$variable == v)
    t <- dplyr::filter(trends(), .data$variable == v)
    shiny::HTML(paste0(
      k.label$long.long, " (", k.label$long.unit, "): ",
      'Rate of change of ', shiny::em(v), ' = ',
      prt(t$growth.rate * 100, digits = 2), '% per year',
      shiny::br(), "Calculated from the slope of ln(", shiny::em(v),
      ") starting in ",
      input$trend_start_year
    ))
    # shiny::br(), "R", shiny::tags$sup("2"), " = ", prt(t$r_squared, digits = 3),
    # " (", goodness_of_fit(t$r_squared), ")"))
  })

  output$historical_table <- DT::renderDataTable({
    if (debugging) message("output$historical_table")
    ks <- kaya_subset() %>%
      dplyr::select("year", "P", "g", "e", "f", "ef", "gef", "G", "E", "F")
    ks <- ks %>% dplyr::mutate(P = prt(.data$P, 2), g = prt(.data$g,1),
                               e = prt(.data$e,2), f = prt(.data$f,2),
                               ef = prt(.data$ef,0), gef = prt(.data$gef, 2),
                               G = prt(.data$G, 3), E = prt(.data$E,2),
                               F = prt(.data$F,1)
    )
    cn <- colnames(ks)
    labs <- kaya_labels %>%
      dplyr::mutate(label =
                      suppressWarnings(stringr::str_c(.data$long,
                                                      ' (', unit, ')'))) %>%
      dplyr::select("variable", "label")
    colnames(ks) <- c('Year', labs$label[pmatch(cn[-1], labs$variable)])
    # message("Kaya Subset:")
    # message(print(ks))
    dt <- DT::datatable(ks,
                        rownames =  FALSE, filter = 'none',
                        options = list(
                          # sDom  = '<"top">lrt<"bottom">ip',
                          columnDefs = list(list(targets = seq(0,ncol(ks)-1),
                                                 className = 'dt-right'))
                        )
    )
    invisible(dt)
    # ks
  })

  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      f <- input$region %>% stringr::str_replace_all('[^A-Za-z0-9]+', '_') %>%
        stringr::str_c('.csv')
      if (debugging) message('downloadData: filename = "', f, '"')
      f
    },
    content = function(file) {
      if (debugging) message('downloadData: Writing file "', file, '"')
      readr::write_csv(
        kaya_subset() %>% dplyr::select(-c("region", "geography", "id")),
        path = file)
    })

  output$downloadFuelData <- shiny::downloadHandler(
    filename = function() {
      f <- input$region %>% stringr::str_replace_all('[^A-Za-z0-9]+', '_') %>%
        stringr::str_c('_fuel.csv')
      if (debugging) message('downloadFuelData: filename = "', f, '"')
      f
    },
    content = function(file) {
      if (debugging) message('downloadFuelData: Writing file "', file, '"')
      fd <- fuel_dist()
      df <- fd$df %>% dplyr::mutate(quads = round(.data$quads, 2),
                                    pct = round(.data$frac * 100, 2)) %>%
        dplyr::select(Fuel = "fuel", Quads = "quads", Percent = "pct") %>%
        dplyr::arrange(.data$Fuel)
      df_total = dplyr::summarize(df, Fuel = 'Total', Quads = sum(.data$Quads),
                                  Percent = sum(.data$Percent))

      df <- df %>% dplyr::mutate(Fuel = as.character(.data$Fuel)) %>%
        dplyr::bind_rows(df_total)
      if (debugging) message("df has ", nrow(df), " rows")
      if (debugging) message("file = ", file)
      readr::write_csv(df, path = file)
    })

  output$policy_goal <- shiny::renderText({
    if (debugging) message("output$policy_goal")
    as.character(shiny::span(
      shiny::strong('Policy goal: '),
      stringr::str_c(input$target_yr, ' emissions ',
             abs(input$target_reduc), '% ',
             ifelse(input$target_reduc >= 0, 'below', 'above'), ' ',
             input$ref_yr)))
  })

  trend_title <- shiny::reactive({
    if (debugging) message("trend_title")
    if ('analysis' %in% names(input)) {
      title <- c(top.down = "Top Down", bottom.up = "Bottom up")[input$analysis]
    } else {
      title <- 'Decarbonization'
    }
    stringr::str_c(title, " Analysis")
  })

  output$trend_title <- shiny::renderText({
    if (debugging) message("output$trend_title")
    as.character(shiny::h4(shiny::strong(trend_title())))
  })

  output$tab_title_trend <- shiny::renderText({
    if (debugging) message("output$tab_title_trend")
    stringr::str_c("Historical Trends for", input$region, sep = " ")
  })

  output$tab_title_calc <- shiny::renderText({
    if (debugging) message("output$tab_title_calc")
    stringr::str_c("Calcuating Implied Decarbonization for", input$region, sep = " ")
  })

  output$tab_title_decarb <- shiny::renderText({
    if (debugging) message("output$tab_title_decarb")
    stringr::str_c("Implied Decarbonization for", input$region, sep = " ")
  })

  output$tab_title_fuel_dist <- shiny::renderText({
    if (debugging) message("output$tab_title_fuel_dist")
    x <- stringr::str_c("Energy Mix for ", input$region)
    fd <- fuel_dist()
    if (! is.null(fd)) {
      x <- stringr::str_c(x, " in ", fd$year[1])
    } else {
      x <- stringr::str_c(x, " is not available")
    }
    x
  })

  output$tab_title_historical <- shiny::renderText({
    if (debugging) message("output$tab_title_historical")
    stringr::str_c("Historical Data for", input$region, sep = " ")
  })


  output$trend_table <- shiny::renderUI({
    if (debugging) message("output$trend_table")
    fcast <- forecast()
    current_yr <- current_year()
    fcast <- fcast %>%
      dplyr::mutate(
        growth.pct = stringr::str_c(prt(.data$growth.rate * 100, 2), '%'),
        current = prt(.data$current, 3, format = 'fg'),
        projected = prt(.data$projected, 3, format = 'fg')
      ) %>%
      dplyr::select("variable", "growth.pct", "current", "projected") %>%
      dplyr::mutate(variable = add_units(.data$variable))
    if (debugging) message("fcast: (", stringr::str_c(names(fcast), collapse = ", "), ")")
    ft <- flextable::flextable(fcast) %>%
      flextable::autofit(add_w = 0, add_h = 0) %>%
      flextable::set_header_labels(
        variable = '', growth.pct = 'Rate of Change',
        current = stringr::str_c('Current (', current_yr, ')'),
        projected = stringr::str_c('Projected (', input$target_yr, ')')
      ) %>%
      # align(align = "center", part = "body", j = 1) %>%
      theme_kaya() %>%
      flextable::align(align = "center", part = "header") %>%
      flextable::align(align = "right", part = "body") %>%
      flextable::htmltools_value()
    if (debugging) message("finished output$trend_table")
    ft
  })

  output$step_1 <- shiny::renderText({
    if (debugging) message("output$step_1")
    paste0(shiny::strong("Step 1: "), "Write down current (",
           current_year(), ") values for ",
           shiny::em('P'), ", ", shiny::em('g'), ", ", shiny::em('e'), ", and ", shiny::em('f'),
           ", and their historical rates of change.")
  })

  output$step_1_table <- shiny::renderUI({
    if (debugging) message("output$step_1_table")
    if (calc_show_answers()) {
      target_yr <- input$target_yr
      current_yr <- current_year()
      fcast <- forecast() %>%
        dplyr::filter(.data$variable %in% c('P', 'g', 'e', 'f')) %>%
        dplyr::mutate(variable = add_units(.data$variable),
                      current = prt(.data$current, 3, format = 'fg'),
                      growth.rate = stringr::str_c(prt(.data$growth.rate * 100, 2), '%')) %>%
        dplyr::select("variable", "current", "growth.rate")
      ft <- flextable::flextable(fcast) %>%
        flextable::autofit() %>%
        flextable::set_header_labels(
          variable = '',
          current = stringr::str_c("Current"," (",current_yr,")"),
          growth.rate = "Rate of Change") %>%
        theme_kaya(TRUE) %>%
        flextable::align(part = "body", align = "right") %>%
        flextable::align(part = "body", align = "center", j = 1) %>%
        flextable::htmltools_value()
    } else {
      NULL
    }
  })

  output$step_2 <- shiny::renderText({
    if (debugging) message("output$step_2")
    paste0(shiny::strong("Step 2: "),
           "Which variables seem to follow a relatively constant ",
           "rate of change, and which do not?")
  })

  output$step_2_table <- shiny::renderUI({
    if (debugging) message("output$step_2_table")
    if (calc_show_answers()) {
      t <- trends() %>%
        dplyr::mutate(goodness = purrr::map_chr(.data$r_squared,
                                                goodness_of_fit)) %>%
        dplyr::select("variable", "goodness")

      if (FALSE) {
        if (debugging) message(print(t))
      }

      ft <- flextable::flextable(t) %>%
        flextable::autofit() %>%
        flextable::set_header_labels(variable = 'variable',
                                     goodness = "Goodness of Fit") %>%
        theme_kaya(TRUE) %>%
        flextable::align(part = "body", align = "center") %>%
        flextable::align(part = "body", align = "center", j = 1) %>%
        flextable::htmltools_value()
    } else {
      NULL
    }
  })

  output$step_3 <- shiny::renderText({
    if (debugging) message("output$step_3")
    paste0(shiny::strong("Step 3: "), "Calculate the projected values for ",
           shiny::em('P'), ", ", shiny::em('g'), ", ", shiny::em('e'), ", and ", shiny::em('f'),
           ", in ", input$target_yr, '.',
           ' Show your work, and check your answers against the &ldquo;',
           trend_title(),
           '&rdquo; table on the left-hand panel.',
           ' (Remember to divide percentages by 100)')
  })

  output$step_3_formula <- shiny::renderUI({
    if (debugging) message("output$step_3_formula")
    if (calc_show_answers()) {
      current_yr = current_year()
      target_yr = input$target_yr
      shiny::withMathJax(shiny::span(stringr::str_c(
        "If variable \\(x\\) has value \\(x(",current_yr,")\\) in ",
        current_yr, " and its rate of change is \\(r\\), then in ",
        target_yr, " its value will be \\(x(",
        target_yr,") = x(", current_yr, ") \\times \\exp(r \\times (",
        target_yr, "-", current_yr,")) = x(",
        current_yr, ")  \\times \\exp(r \\times ",
        (target_yr - current_yr), "))\\)"
      ),
      style=stringr::str_c("color:", answer.fg, ";font-weight:bold;")
      )
      )
    } else {
      NULL
    }
  })

  output$step_3_table <- shiny::renderUI({
    if (debugging) message("output$step_3_table")
    if (calc_show_answers()) {
      target_yr <- input$target_yr
      current_yr <- current_year()
      fcast <- forecast() %>%
        dplyr::filter(.data$variable %in% c('P', 'g', 'e', 'f')) %>%
        dplyr::mutate(variable = add_units(.data$variable),
                      projected = prt(.data$projected, 3, format = 'fg')) %>%
        dplyr::select("variable", "projected")
      ft <- flextable::flextable(fcast) %>%
        flextable::autofit() %>%
        flextable::set_header_labels(variable = "",
                                     projected = stringr::str_c("Projected (", target_yr, ")")) %>%
        theme_kaya(TRUE) %>%
        flextable::align(part = "body", align = "right") %>%
        flextable::align(part = "body", align = "center", j = 1) %>%
        flextable::htmltools_value()
    } else {
      NULL
    }
  })


  output$step_4 <- shiny::renderText({
    if (debugging) message("output$step_4")
    paste0(shiny::strong("Step 4: "), "Multiply the variables together to get ",
           shiny::em("F"), " for each year.",
           ' Show your work, and check your answers against the &ldquo;',
           trend_title(),
           '&rdquo; table on the left-hand panel.')
  })

  output$step_4_table <- shiny::renderUI({
    if (debugging) message("output$step_4_table")
    if(calc_show_answers()) {
      target_yr <- input$target_yr
      current_yr <- current_year()
      fcast <- forecast() %>% dplyr::filter(.data$variable %in% c('F')) %>%
        dplyr::mutate(variable = add_units(.data$variable),
                      current = prt(.data$current, 3, format = 'fg'),
                      projected = prt(.data$projected, 3, format = 'fg'))
      fcast <- fcast %>% dplyr::select("variable", "current", "projected")
      ft <- flextable::flextable(fcast) %>%
        flextable::autofit() %>%
        flextable::set_header_labels(variable = "",
                                     current = stringr::str_c("Current (", current_yr, ")"),
                                     projected = stringr::str_c("Projected (", target_yr, ")")) %>%
        theme_kaya(TRUE) %>%
        flextable::align(part = "body", align = "right") %>%
        flextable::align(part = "body", align = "center", j = 1) %>%
        flextable::htmltools_value()
    } else {
      NULL
    }
  })

  output$step_5 <- shiny::renderText({
    if (debugging) message("output$step_5")
    ref_yr <- input$ref_yr
    target_yr <- input$target_yr
    target_reduc <- input$target_reduc
    paste0(shiny::strong("Step 5: "), "Look up emissions for ", ref_yr,
           " under the &ldquo;",
           trend_title(),
           "&rdquo; table on the left panel,",
           " or in the &ldquo;Historical&rdquo; tab,",
           " and calculate the target emissions for ", target_yr,
           " (", abs(target_reduc), "% ",
           ifelse(target_reduc >= 0, 'less', 'greater'),
           " than ", ref_yr, ") .")
  })

  output$step_5_table <- shiny::renderUI({
    if (debugging) message("output$step_5_table")
    if (calc_show_answers()) {
      ref_yr <- input$ref_yr
      target_yr <- input$target_yr

      df <- dplyr::tibble(
        ref = prt(ref_emissions(), 0),
        target = prt(target_emissions(), 0))
      ft <- flextable::flextable(df) %>%
        flextable::autofit() %>%
        flextable::set_header_labels(ref = stringr::str_c("Emissions in ", ref_yr),
                                     target = stringr::str_c("Target emissions in ", target_yr)) %>%
        theme_kaya(TRUE) %>%
        flextable::align(part = "body", align = "right") %>%
        flextable::align(part = "body", align = "center", j = 1) %>%
        flextable::htmltools_value()
    } else {
      NULL
    }
  })

  output$step_6 <- shiny::renderText({
    if (debugging) message("output$step_6")
    show_answers <- calc_show_answers()
    ref_yr <- input$ref_yr
    target_yr <- input$target_yr
    target_reduc <- input$target_reduc
    target_em <- target_emissions()
    current_yr <- current_year()
    delta_yr = target_yr - current_yr
    current_em <- forecast() %>% dplyr::filter(.data$variable == 'F') %>%
      dplyr::select("current") %>% unlist()
    em_ratio <- target_em / current_em
    ln_em_ratio <- log(em_ratio)
    rate <- ln_em_ratio / delta_yr

    elements <- shiny::span(
      shiny::strong("Step 6: "),
      "Calculate the rate of emissions reduction necessary to meet this target:"
    )
    sub_elements <- shiny::tags$ol(style="list-style-type:lower-alpha;")
    li_element <- shiny::tags$li(stringr::str_c("Look up the current (",
                                        current_yr, ") emissions:"))
    if (show_answers) {
      li_element <- li_element %>%
        shiny::tagAppendChildren(shiny::br(),
                                 shiny::span(stringr::str_c(prt(current_em, 0)),
                                             style=stringr::str_c("color:", answer.fg,
                                                          ";font-weight:bold;"))
        )
    }
    sub_elements <- sub_elements %>%
      shiny::tagAppendChild(li_element)

    li_element <- shiny::tags$li(stringr::str_c("Calculate the ratio of the target (",
                                        target_yr, ") emissions to current (",
                                        current_yr, ") emissions:"))
    if (show_answers) {
      li_element <- li_element %>%
        shiny::tagAppendChildren(
          shiny::br(),
          shiny::span(stringr::str_c(prt(target_em, 0), ' / ', prt(current_em, 0), ' = ',
                             prt(em_ratio, 3)),
                      style=stringr::str_c("color:",answer.fg,";font-weight:bold;"))
        )
    }
    sub_elements <- sub_elements %>%
      shiny::tagAppendChild(li_element)

    li_element <- shiny::tags$li("Take the natural logarithm of the ratio: ")
    if (show_answers) {
      li_element <- li_element %>%
        shiny::tagAppendChildren(
          shiny::br(),
          shiny::span(stringr::str_c("ln(", prt(em_ratio, 3), ') = ',
                             prt(ln_em_ratio, 2)),
                      style=stringr::str_c("color:",answer.fg,";font-weight:bold;"))
        )
    }
    sub_elements <- sub_elements %>%
      shiny::tagAppendChild(li_element)

    li_element <- shiny::tags$li(
      stringr::str_c("Divide the logarithm by the number of years between ",
             current_yr, " and ", target_yr, ":"))
    if (show_answers) {
      li_element <- li_element %>%
        shiny::tagAppendChildren(
          shiny::br(),
          shiny::span(stringr::str_c(target_yr, ' - ', current_yr, ' = ', delta_yr),
                      shiny::br(),
                      stringr::str_c(prt(ln_em_ratio,2), ' / ', delta_yr,
                             ' = ',
                             prt(rate, 3, format='fg'),
                             ' = ',
                             prt(rate * 100, 2), '%'),
                      shiny::br(),
                      stringr::str_c("Meeting the emissions goal of ", prt(target_em, 0),
                             " in ", target_yr, " would require F to drop by ",
                             prt(rate * 100, 2), "% per year"),
                      style=stringr::str_c("color:",answer.fg,";font-weight:bold;"))
        )
    }
    sub_elements <- sub_elements %>%
      shiny::tagAppendChild(li_element)

    elements <- elements %>% shiny::tagAppendChild(sub_elements)
    as.character(elements)
  })

  output$step_7 <- shiny::renderText({
    if (debugging) message("output$step_7")
    ref_yr <- input$ref_yr
    target_yr <- input$target_yr
    target_reduc <- input$target_reduc
    target_em <- target_emissions()
    current_yr <- current_year()
    delta_yr = target_yr - current_yr
    current_em <- forecast() %>% dplyr::filter(.data$variable == 'F') %>%
      dplyr::select("current") %>% unlist()
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
    elements <- shiny::span(
      shiny::strong("Step 7: "),
      shiny::HTML(paste0(
        "The annual rate of change of ",
        shiny::em("F"),
        ", which you calculated in the last step, is the sum of the rate of changes of ",
        shiny::em("P"), ", ", shiny::em("g"), ", ", shiny::em("e"),
        ", and ", shiny::em("f"), ".",
        " Look up the historical rate of changes of ",
        shiny::em("P"), " and ", shiny::em("g"),
        ", and subtract them from the rate of change of ", shiny::em("F"),
        " that you calculated in the last step ",
        "to get the decarbonization rate (the rate of change of ",
        shiny::em("ef"), ")."
      ))
    )
    if (calc_show_answers()) {
      elements <- elements %>%
        shiny::tagAppendChildren(
          shiny::br(),
          shiny::span(
            stringr::str_c(prt(rate * 100, 2), '% - (', prt(tP * 100,2), '% + ',
                           prt(tg * 100,2), '%) = ', prt(t.ef.proj * 100,2),
                           '% per year'),
            shiny::br(),
            shiny::HTML("For comparison, the historical trend for <i>ef</i> is ",
                        prt(t.ef.hist * 100, 2), "% per year."),
            style=stringr::str_c("color:",answer.fg,";font-weight:bold;")
          )
        )
    }
    as.character(elements)
  })

  output$tab_title_top_down <- shiny::renderText({
    if (debugging) message("output$tab_title_top_down")
    title <- stringr::str_c("Top-down predictions of future growth rates for ",
                            input$region)
    td <- top_down_trends()
    if (is.null(td)) {
      title <- stringr::str_c(title, " are not available.")
    }
    title
  })

  output$fuel_dist <- shiny::renderText({
    if (debugging) message("output$fuel_dist")
    fd <- fuel_dist()
    if(is.null(fd)) {
      mix_title <- stringr::str_c("Energy mix for ", input$region, " is not available.")
    } else {
      mix_title <- stringr::str_c("Energy mix for ", input$region, " (quads), in ",
                          fd$year)
    }
    mix_title
  })

  output$fuel_dist_table <- shiny::renderUI({
    if (debugging) message("output$fuel_dist_table")
    fd <- fuel_dist()
    if (is.null(fd)) {
      NULL
    } else {
      df <- fd$df %>% dplyr::mutate(quads = round(.data$quads, 2),
                                    pct = round(100 * .data$frac, 2)) %>%
        dplyr::select("fuel", "quads", "pct") %>%
        dplyr::arrange(.data$fuel)

      df_total <- dplyr::summarize(df, fuel = 'Total', quads = sum(.data$quads),
                                   pct = sum(.data$pct))

      df <- df %>% dplyr::mutate(fuel = as.character(.data$fuel)) %>%
        dplyr::bind_rows(df_total)

      if (debugging) message("Preparing flextable")

      ft <- flextable::flextable(df) %>%
        flextable::autofit() %>%
        flextable::set_header_labels(fuel = "Fuel", quads = "Quads", pct = "Percent") %>%
        theme_kaya() %>%
        flextable::align(part = "header", align = "center") %>%
        flextable::align(part = "body", align = "right") %>%
        flextable::align(part = "body", align = "left", j = 1) %>%
        flextable::colformat_num(j = "quads", big.mark = ",", digits = 2) %>%
        flextable::colformat_num(j = "pct", digits = 2, suffix = "%") %>%
        flextable::htmltools_value()

      if (debugging) message("Finished preparing flextable")

      ft
    }
  })

  output$fuel_dist_plot <- shiny::renderPlot({
    if (debugging) message("output$fuel_dist_plot")
    fd <- fuel_dist()
    if (is.null(fd)) {
      NULL
    } else {
      df <- fd$df %>%
        dplyr::arrange(.data$fuel) %>%
        dplyr::mutate(qmin = cumsum(dplyr::lag(.data$quads, default=0)),
                      qmax = cumsum(.data$quads))
      labels <- df %>%
        dplyr::mutate(label = stringr::str_c(.data$fuel, ": ",
                                             prt(.data$quads,2), " quads (",
                                             prt(.data$frac * 100,1), "%)")) %>%
        dplyr::arrange(.data$fuel) %>% dplyr::select("fuel", "label") %>%
        tidyr::spread(key = "fuel", value = "label") %>% unlist()
      if (FALSE) {
        if (debugging) message(stringr::str_c(levels(df$fuel), collapse=", "))
      }
      if (debugging) message("In output$fuel_dist_plot, df is a ",
                             stringr::str_c(class(df), collapse = ", "))
      ggplot(df, aes(ymin = .data$qmin, ymax = .data$qmax, fill = .data$fuel)) +
        geom_rect(xmin = 2, xmax = 4) +
        coord_polar(theta = "y") +
        xlim(c(0,4)) +
        scale_fill_manual(values = c(
          Coal = '#e31a1c', 'Natural Gas' = '#fdbf6f', 'Oil' = '#ff7f00',
          Nuclear = '#33a02c', Renewables = '#b2df8a', Total = '#a6cee3'),
          breaks = names(labels), labels = labels, name = "Fuel"
        ) +
        theme_bw(base_size = 20) +
        theme(panel.grid=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank())
    }
  })


  output$target_emissions <- shiny::renderText({
    if (debugging) message("output$target_emissions")
    target = prt(target_emissions(), 3, format = 'fg')
    ref = prt(ref_emissions(), 3, format = 'fg')
    x <- stringr::str_c(
      as.character(shiny::h4(stringr::str_c(input$ref_yr, ' emissions = ', ref, ' MMT CO2'))),
      as.character(shiny::h4(stringr::str_c(input$target_yr, ' target: ',
                                    abs(input$target_reduc), '% ',
                                    ifelse(input$target_reduc >= 0, 'below', 'above'),
                                    ' ', input$ref_yr, ' = ', target, ' MMT'))))
    if (debugging) message("finished output$target_emissions")
    x
  })

  kaya_subset_plot <- shiny::reactive({
    if (debugging) message("kaya_subset_plot")
    tv <- sym(input$trend_variable)
    ks <- kaya_subset() %>% dplyr::filter(! is.na(!!tv))
    tsy <- input$trend_start_year
    before <- ks %>% dplyr::filter(.data$year <= tsy) %>%
      dplyr::mutate(in.trend = FALSE)
    after <- ks %>% dplyr::filter(.data$year >= tsy) %>%
      dplyr::mutate(in.trend = TRUE)
    dplyr::bind_rows(before, after) %>%
      dplyr::mutate(fitted = ordered(.data$in.trend,
                                     levels = c("FALSE", "TRUE", "Best line")),
                    id = dplyr::row_number())
  })

  trend_tooltip <- function(x) {
    if (debugging) message("trend_tooltip")
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
    k <- shiny::isolate(kaya_subset_plot())
    v <- input$trend_variable
    r = k[k$id == x$id,]
    year = r$year
    pt = r[v]
    stringr::str_c('<b>', year, ": ", v, " = ", prt(as.numeric(pt), 2), "</b>")
  }

  decarb_tooltip <- function(x) {
    if (debugging) message("decarb_tooltip")
    if (FALSE) {
      if (debugging) message("decarb_tooltip: x = ",
                             stringr::str_c(names(x), " = ", x, sep = " ",
                                   collapse = ', '))
    }
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
    df <- shiny::isolate(implied_decarb())
    r = df[df$id == x$id,]
    year = r$year
    pt = r$ef
    cat = r$cat
    stringr::str_c('<b>', year, ": ef(", cat, ")  = ", prt(as.numeric(pt), 2), "</b>")
  }


  output$trend_plot <- plotly::renderPlotly({
    if (debugging) message("tp")
    xvar_name <- 'Year'
    v <- input$trend_variable
    yvar_name <- with(kaya_labels[kaya_labels$variable == v,],
                      stringr::str_c(varname, ' (', unit, ')'))
    yvar = sym(v)
    if (is.na(history_start()))
      x_tics <- waiver()
    else
      x_tics <- seq(10 * round(history_start() / 10),
                    10 * round(history_stop() / 10), 10)
    ksp <- kaya_subset_plot()
    # if (debugging) message("In tp, ksp is a ", stringr::str_c(class(ksp), collapse = ", "))

    plot <- ksp %>%
      ggplot(aes(x = .data$year, y = .data[[yvar]], color = .data$fitted)) +
      geom_line(linewidth = 0.25) +
      geom_point(size = 1) +
      scale_x_continuous(name = xvar_name,
                         breaks = x_tics, limits = c(min_year, max_year)) +
      scale_y_continuous(name = yvar_name) +
      scale_color_manual(values = c("TRUE" = "#A00000", "FALSE" = "#FF8080",
                                    "Best line" = "dark blue"),
                         guide = "none") +
      scale_fill_manual(values = c("TRUE" = "#A00000", "FALSE" = "#FF8080",
                                   "Best line" = "dark blue"),
                        guide = "none") +
      theme_bw() +
      theme(legend.position = "none")

    if (debugging) message("finished tp")
    plotly::ggplotly(plot) %>% plotly::layout(autosize=TRUE)
  })

  trend_model <- shiny::reactive({
    if (debugging) message("trend_model")
    var <- input$trend_variable
    ty <- input$trend_start_year
    k <- kaya_subset() %>% dplyr::filter(.data$year >= ty)
    # message("Var = ", var, " data has ", nrow(k), " rows")
    f <- substitute(log(x) ~ year, list(x = as.symbol(input$trend_variable)))
    if (nrow(k) > 0) {
      trend <- stats::lm(stats::as.formula(f), data = k,
                         na.action = stats::na.exclude())
      # message(summary(trend))
    } else {
      trend <- NULL
    }
    trend
  })

  output$trend_plot_ln <- plotly::renderPlotly({
    if (debugging) message("tpl")
    xvar_name <- 'Year'
    v <- input$trend_variable
    yvar_name <- with(kaya_labels[kaya_labels$variable == v,],
                      stringr::str_c(varname, ' (', unit, ')'))
    yvar = sym(input$trend_variable)

    if (is.na(history_start()))
      x_tics <- waiver()
    else
      x_tics <- seq(10 * round(history_start() / 10),
                    10 * round(history_stop() / 10), 10)
    tsy <- input$trend_start_year

    ksp <- kaya_subset_plot()
    if (debugging) message("In tpl, ksp is a ", stringr::str_c(class(ksp),
                                                               collapse = ", "))

    plot <- ksp %>%
      ggplot(aes(x = .data$year, y = .data[[yvar]], color = .data$fitted)) +
      geom_line(linewidth = 0.25) +
      geom_smooth(aes(color = "Best line"),
                  data = dplyr::filter(ksp, .data$year >= tsy),
                  method = "lm", se = FALSE, size = 0.25) +
      geom_point(size = 1) +
      scale_x_continuous(name = xvar_name,
                         breaks = x_tics, limits = c(min_year, max_year)) +
      scale_y_log10(name = yvar_name) +
      scale_color_manual(values = c("TRUE" = "#A00000", "FALSE" = "#FF8080",
                                    "Best line" = "dark blue"),
                         guide = "none") +
      scale_fill_manual(values = c("TRUE" = "#A00000", "FALSE" = "#FF8080",
                                   "Best line" = "dark blue"),
                        guide = "none") +
      theme_bw() +
      theme(legend.position = "none")

    if (debugging) message("finished tpl")
    plotly::ggplotly(plot) %>% plotly::layout(autosize=TRUE)
  })

  output$trend_plot_title <- shiny::renderText({
    if (debugging) message("output$trend_plot_title")
    kaya_labels$long[kaya_labels$variable == input$trend_variable]
  })

  output$trend_plot_ln_title <- shiny::renderText({
    if (debugging) message("output$trend_plot_ln_title")
    stringr::str_c(kaya_labels$long[kaya_labels$variable == input$trend_variable],
           ' (log scale)')
  })

  output$implied_decarb_plot <- plotly::renderPlotly({
    if (debugging) message("implied_decarb_plot")
    xvar_name <- 'Year'
    yvar_name <- 'CO2 intensity (tonnes / $ million GDP)'

    if (is.na(history_start())) {
      x_tics <- waiver()
    }
    else {
      x_tics <- seq(10 * round(history_start() / 10),
                    10 * round(input$target_yr / 10), 10)
    }
    if (debugging) message("Setting up implied decarbonization plot")
    idc <- implied_decarb()
    if (debugging) message("idc is a ", stringr::str_c(class(idc), collapse = ", "))

    plot <- idc %>%
      dplyr::mutate(cat = ordered(cat, levels = c("Historical", "Historical Trend",
                                                  "Bottom-up"))) %>%
      dplyr::filter(! is.na(.data$ef), .data$year >= 1980) %>%
      ggplot(aes(x = .data$year, y = .data$ef, color = .data$cat)) +
      geom_line(linewidth = 0.5) +
      geom_point(size = 1) +
      scale_x_continuous(name = xvar_name, breaks = x_tics) +
      scale_y_continuous(name = yvar_name) +
      scale_color_brewer(palette = "Dark2") +
      theme_bw()
    if (debugging) message("implied_decarb_plot created")
    plotly::ggplotly(plot) %>% plotly::layout(autosize=TRUE)
  })

}

assign("server", server, envir = .shinyenv)
