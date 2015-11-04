
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
library(rtable)

source('load_kaya.R')

mass.c <- 12
mass.co2 <- 44

c.to.co2 <- function(x) {x * mass.co2 / mass.c}
co2.to.c <- function(x) {x * mass.c / mass.co2}

answer.bg <- '#FFFFA0'
answer.fg <- '#0000A0'

normal.body.props <- cellProperties(padding.left = 2, padding.right = 2)
normal.head.props <- cellProperties(padding.top = 2,
                                    padding.left = 2, padding.right = 2)
answer.body.props <- cellProperties(padding.left = 2, padding.right = 2,
                                    background.color = answer.bg)
answer.head.props <- cellProperties(padding.top = 2,
                                    padding.left = 2, padding.right = 2,
                                    background.color = answer.bg)

kaya <- load_kaya()$kaya %>% mutate(F = c.to.co2(F), f = c.to.co2(f), ef = c.to.co2(ef))

kaya$id <- seq_len(nrow(kaya))

countries <- as.character(unique(kaya$country))

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
  observe({
    updateSelectInput(session, 'country', choices = unique(countries))
  })

  kaya_subset <- reactive({
    ctry <- input$country
    if (! ctry %in% kaya$country)
      ctry <- 'World'
    ks <- kaya  %>% filter(country == ctry) %>% arrange(year)
    ks
  })

  observe({
    ks <- kaya_subset()
    ks.min.yr <- min(ks$year)
    ks.max.yr <- max(ks$year)
    v <- input$ref_yr
    v <- min(max(input$ref_yr, ks.min.yr), ks.max.yr)
    updateNumericInput(session, 'ref_yr',
                       min = ks.min.yr, max = ks.max.yr, step = 1, value = v)
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

  trends <- reactive({
    trend.start <- input$trend_start_year
    ks <- kaya_subset() %>% filter(year >= trend.start)
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
    paste0('Growth rate of ', v, ' = ', prt(t$growth.rate * 100, digits = 2), '% per year')
  })

  output$historical_table <- renderFlexTable({
    ks <- kaya_subset() %>% select(year, P, g, e, f, ef, G, E, F)
    ks <- ks %>% mutate(P = prt(P, 2),
                        g = prt(g,1),
                        e = prt(e,2), f = prt(f,2),
                        ef = prt(ef,0),
                        G = prt(G, 3), E = prt(E,2), F = prt(F,1))
    cn <- colnames(ks)
    labs <- kaya_labels %>% mutate(label = suppressWarnings(str_c(long, ' (', unit, ')'))) %>% select(variable, label)
    colnames(ks) <- c('Year', labs$label[pmatch(cn[-1], labs$variable)])
    ks
    ft <- FlexTable(ks)
    ft[,,to="header"] <- parCenter()
    ft[,] <- parRight()
    ft[,1] <- parCenter()
    ft

  })

  output$policy_goal <- renderText({
    as.character(span(strong('Policy goal: '), input$target_yr, ' emissions ', input$target_reduc, '% below ', input$ref_yr))
  })

  output$trend_title <- renderText({
    title <- c(top.down = "Top Down", bottom.up = "Bottom up")[input$analysis]
    as.character(h4(strong(title)))
  })

  output$trend_table <- renderFlexTable({
    fcast <- forecast()
    current_yr <- current_year()
    fcast <- fcast %>% mutate(growth.pct = str_c(prt(growth.rate * 100, 2), '%'),
                              current = prt(current, 3, format = 'fg'),
                              projected = prt(projected, 3, format = 'fg')) %>%
      select(variable, growth.pct, current, projected) %>%
      mutate(variable = add_units(variable))
    names(fcast) <- c('', 'Growth Rate', paste0('Current (', current_yr, ')'),
                      paste0('Projected (', input$target_yr, ')'))
    ft <- FlexTable(fcast)
    ft[,, to='header'] <- parCenter()
    ft[,1] <- parCenter()
    ft[,2:ncol(fcast)] <- parRight()
    ft
  })

  output$step_1 <- renderText({
    paste0(strong("Step 1: "), "write down current (",
           current_year(), ") and projected ", input$target_yr, " values for ",
           em('P'), ", ", em('g'), ", ", em('e'), ", and ", em('f'), ".")
  })

  output$step_1_table <- renderFlexTable({
    target_yr <- input$target_yr
    current_yr <- current_year()
    fcast <- forecast() %>% filter(variable %in% c('P', 'g', 'e', 'f')) %>%
      mutate(variable = add_units(variable),
             current = prt(current, 3, format = 'fg'),
             projected = prt(projected, 3, format = 'fg')) %>%
      select(variable, current, projected)
    names(fcast) <- c('', suppressWarnings(str_c(c("Current", "Projected"), " (", c(current_yr, target_yr), ")")))
    message(paste(colnames(fcast), collapse = ', '))
    ft <- FlexTable(fcast, header.cell.props = normal.head.props, body.cell.props = normal.body.props)
    ft[,,to="header"] <- parCenter()
    ft[,] <- parRight()
    ft[,1] <- parCenter()
    ft
  })

  output$step_2 <- renderText({
    paste0(strong("Step 2: "), "Multiply the variables together to get ", em("F"), " for each year.")
  })

  output$step_2_table <- renderFlexTableIf({
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
    if (input$calc_show_answers)
      ft
    else {
      NULL
    }
  })

  output$step_3 <- renderText({
    ref_yr <- input$ref_yr
    target_yr <- input$target_yr
    target_reduc <- input$target_reduc
    paste0(strong("Step 3: "), "Look up emissions for ", ref_yr, " and calculate the target emissions (", target_reduc, "% less) for ", target_yr, ".")
  })

  output$step_3_table <- renderFlexTableIf({
    ref_yr <- input$ref_yr
    target_yr <- input$target_yr

    df <- data.frame(
      ref = prt(ref_emissions(), 0),
      target = prt(target_emissions(), 0))
    names(df) <- str_c(c("Emissions in ", "Target emissions in "), c(ref_yr, target_yr))
    ft <- FlexTable(df, header.cell.props = answer.head.props, body.cell.props = answer.body.props)
    ft[,] <- parRight()
    ft[,,to='header'] <- parCenter()
    if (input$calc_show_answers)
      ft
    else
      NULL
  },
  align = 'rrr', include.rownames = FALSE,
  html.table.attributes = list(style="color:darkblue;")
  )

  output$step_4 <- renderText({
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
      strong("Step 4: "),
      "Calculate the rate of emissions reduction necessary to meet this target:"
      )
    sub_elements <- tags$ol(style="list-style-type:lower-alpha;")
    li_element <- tags$li(paste0("Calculate the ratio of the target (", target_yr,
                                 ") emissions to current (", current_yr, ") emissions:"))
    if (input$calc_show_answers) {
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
    if (input$calc_show_answers) {
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
    if (input$calc_show_answers) {
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

  output$step_5 <- renderText({
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
    elements <- span(strong("Step 5: "),
                     "The annual growth rate of F, which you calculated in the last step, is the sum of the growth rates of P, g, e, and f. Look up the historical growth rates of P and g, and subtract them from the growth rate of F that you calculated in the last step."
                     )
    if (input$calc_show_answers) {
      elements <- elements %>%
        tagAppendChildren(br(),
                          span(paste0(prt(rate * 100, 2), '% - (', prt(tP * 100,2), '% + ',
                                      prt(tg * 100,2), '%) = ', prt(t.ef.proj * 100,2), '%'),
                               style=paste0("color:",answer.fg,";font-weight:bold;"))
                          )
    }
    as.character(elements)
  })


  output$target_emissions <- renderText({
    target = prt(target_emissions(), 3, format = 'fg')
    ref = prt(ref_emissions(), 3, format = 'fg')
    paste0(
      as.character(h4(paste0(input$ref_yr, ' emissions = ', ref, ' MMT CO2'))),
      as.character(h4(input$target_yr, ' target:',
                      input$target_reduc, '% reduction = ', target, ' MMT')))
  })

  kaya_subset_plot <- reactive({
    ks <- kaya_subset()
    tsy <- input$trend_start_year
    before <- ks %>% filter(year <= tsy) %>% mutate(in.trend = FALSE)
    after <- ks %>% filter(year >= tsy) %>% mutate(in.trend = TRUE)
    bind_rows(before, after) %>% mutate(f.in.trend = factor(in.trend))
  })

  trend_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
    k <- isolate(kaya_subset())
    v <- input$trend_variable
    r = k[k$id == x$id,]
    year = r$year
    pt = r[v]
    paste0('<b>', year, ": ", v, " = ", prt(as.numeric(pt), 2), "</b>")
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

})
