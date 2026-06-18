source("R/utils.R")
source("R/parameters.R")
source("R/simulate_workers.R")
source("R/simulate_production.R")
source("R/price_rules.R")
source("R/simulate_trade.R")
source("R/simulate_markets.R")
source("R/money_supply.R")
source("R/run_world_simulation.R")
source("R/summarize_outputs.R")
source("R/plotting_functions.R")

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Package 'shiny' is required. Install it with install.packages('shiny').", call. = FALSE)
}

scenario_files <- list.files("scenarios", pattern = "\\.yaml$", full.names = TRUE)
baseline_params <- load_parameters("scenarios/baseline.yaml")
sector_names <- baseline_params$sectors

graph_sources <- c("Ore output", "Money supply by coin", "Money supply stages")

graph_source_config <- function(result, source) {
  switch(
    source,
    "Ore output" = list(
      data = result$ore_output,
      variables = c("output", "raw_output", "successful_runs", "total_runs", "workers"),
      unit_column = "good"
    ),
    "Money supply by coin" = list(
      data = result$money_supply,
      variables = c(
        "minted_coins",
        "enchanted_coins",
        "country_y_reserve",
        "returned_supply",
        "supply_value",
        "net_money_supply_value",
        "spill",
        "spill_loss"
      ),
      unit_column = "coin_type"
    ),
    "Money supply stages" = list(
      data = money_supply_flow_long(result$money_supply),
      variables = c("coins"),
      unit_column = "stage"
    )
  )
}

build_series_catalog <- function(result) {
  rows <- list()
  index <- 1

  for (source in graph_sources) {
    config <- graph_source_config(result, source)
    units <- unique(config$data[[config$unit_column]])

    for (variable in config$variables) {
      for (unit in units) {
        rows[[index]] <- data.frame(
          series_id = paste(source, variable, unit, sep = "::"),
          source = source,
          variable = variable,
          unit = unit,
          unit_column = config$unit_column,
          label = paste(source, variable, unit, sep = " | "),
          stringsAsFactors = FALSE
        )
        index <- index + 1
      }
    }
  }

  do.call(rbind, rows)
}

build_graph_data <- function(result, catalog, selected_ids, transform, time_range) {
  empty_graph_data <- data.frame(
    time = integer(),
    series = character(),
    graph_value = numeric()
  )

  if (length(selected_ids) == 0) {
    return(empty_graph_data)
  }

  rows <- list()
  index <- 1
  selected_catalog <- catalog[catalog$series_id %in% selected_ids, ]
  if (nrow(selected_catalog) == 0) {
    return(empty_graph_data)
  }

  for (i in seq_len(nrow(selected_catalog))) {
    entry <- selected_catalog[i, ]
    config <- graph_source_config(result, entry$source)
    data <- config$data
    data <- data[data[[entry$unit_column]] == entry$unit, ]
    data <- data[
      data$time >= time_range[1] &
        data$time <= time_range[2],
    ]
    if (nrow(data) == 0) {
      next
    }
    data <- data[order(data$time), ]
    values <- data[[entry$variable]]

    if (identical(transform, "Cumulative")) {
      values <- cumsum(values)
    }

    rows[[index]] <- data.frame(
      time = data$time,
      series = entry$label,
      graph_value = values,
      stringsAsFactors = FALSE
    )
    index <- index + 1
  }

  if (length(rows) == 0) {
    return(empty_graph_data)
  }

  graph_data <- do.call(rbind, rows)
  graph_data[order(graph_data$time, graph_data$series), ]
}

ui <- shiny::fluidPage(
  shiny::includeCSS("www/custom_style.css"),
  shiny::titlePanel("Ore Output Dashboard"),
  shiny::tabsetPanel(
    id = "main_tabs",
    shiny::tabPanel(
      "World Overview",
      shiny::br(),
      shiny::fluidRow(
        shiny::column(3, shiny::wellPanel(shiny::h4("Actual Money Supply"), shiny::textOutput("actual_money_supply"))),
        shiny::column(3, shiny::wellPanel(shiny::h4("Total Ore Output"), shiny::textOutput("total_output"))),
        shiny::column(3, shiny::wellPanel(shiny::h4("Returned Coins"), shiny::textOutput("returned_coin_supply"))),
        shiny::column(3, shiny::wellPanel(shiny::h4("Country Y Reserve"), shiny::textOutput("country_y_reserve")))
      ),
      shiny::fluidRow(
        shiny::column(12, shiny::plotOutput("current_money_supply_bar", height = 340))
      )
    ),
    shiny::tabPanel(
      "Graphs",
      shiny::br(),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput(
            "graph_source",
            "Dataset",
            choices = c("Ore output", "Money supply by coin", "Money supply stages")
          ),
          shiny::selectInput("series_variable", "Measure", choices = character()),
          shiny::checkboxGroupInput("available_series", "Available series", choices = character()),
          shiny::actionButton("add_series", "Add series", class = "btn-primary"),
          shiny::actionButton("clear_series", "Clear graph"),
          shiny::checkboxGroupInput("selected_series", "Displayed series", choices = character()),
          shiny::selectInput("graph_transform", "View", choices = c("Level", "Cumulative")),
          shiny::sliderInput("graph_time", "Time range", min = 1, max = baseline_params$T, value = c(1, baseline_params$T), step = 1)
        ),
        shiny::mainPanel(
          shiny::plotOutput("graph_plot", height = 460),
          shiny::tableOutput("graph_table")
        )
      )
    ),
    shiny::tabPanel(
      "Ore Inspector",
      shiny::br(),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput("inspect_good", "Ore", choices = sector_names),
          shiny::sliderInput("inspect_time", "Time range", min = 1, max = baseline_params$T, value = c(1, baseline_params$T), step = 1),
          shiny::selectInput(
            "inspect_variable",
            "Variable",
            choices = c(
              "total_runs",
              "successful_runs",
              "raw_output",
              "output"
            ),
            selected = "raw_output"
          )
        ),
        shiny::mainPanel(
          shiny::tableOutput("ore_table")
        )
      )
    ),
    shiny::tabPanel(
      "Money Supply",
      shiny::br(),
      shiny::tableOutput("money_supply_table")
    ),
    shiny::tabPanel(
      "Scenario Controls",
      shiny::br(),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput(
            "scenario_file",
            "Scenario file",
            choices = stats::setNames(scenario_files, basename(scenario_files)),
            selected = "scenarios/baseline.yaml"
          ),
          shiny::textInput("scenario_name", "Run label", value = "custom_run"),
          shiny::numericInput("total_labor", "Total labor L", value = baseline_params$L, min = 0, step = 1),
          shiny::numericInput("time_horizon", "Time horizon T", value = baseline_params$T, min = 1, step = 1),
          shiny::textInput("random_seed", "Random seed", value = ""),
          shiny::numericInput("mint_loss_min", "Mint loss min", value = baseline_params$money_supply$mint_loss_min, min = 0, step = 0.00001),
          shiny::numericInput("mint_loss_max", "Mint loss max", value = baseline_params$money_supply$mint_loss_max, min = 0, step = 0.00001),
          shiny::checkboxInput("save_run", "Save run to data CSV files", value = FALSE),
          shiny::actionButton("run_sim", "Run simulation", class = "btn-primary")
        ),
        shiny::mainPanel(
          shiny::h4("Mining production process"),
          shiny::fluidRow(
            shiny::column(4, shiny::numericInput("worker_T", "Worker time budget T", value = baseline_params$mining_worker$T, min = 1, step = 1)),
            shiny::column(4, shiny::numericInput("worker_t", "Task time t", value = baseline_params$mining_worker$t, min = 1, step = 1)),
            shiny::column(4, shiny::numericInput("start_x", "Start threshold x", value = baseline_params$mining_worker$start_x, min = 0, step = 1))
          ),
          shiny::fluidRow(
            shiny::column(3, shiny::numericInput("task_min", "Task draw min", value = baseline_params$mining_worker$task_min, step = 1)),
            shiny::column(3, shiny::numericInput("task_max", "Task draw max", value = baseline_params$mining_worker$task_max, step = 1)),
            shiny::column(3, shiny::numericInput("output_min", "Output draw min", value = baseline_params$mining_worker$output_min, min = 0, step = 0.1)),
            shiny::column(3, shiny::numericInput("output_max", "Output draw max", value = baseline_params$mining_worker$output_max, min = 0, step = 0.1))
          ),
          shiny::p(class = "small-note", "Leave the random seed blank to generate a fresh stochastic production path on each run."),
          shiny::verbatimTextOutput("run_details")
        )
      )
    ),
    shiny::tabPanel("Runs", shiny::br(), shiny::tableOutput("runs_table"))
  )
)

server <- function(input, output, session) {
  controls_from_input <- function() {
    list(
      scenario_name = input$scenario_name,
      T = input$time_horizon,
      L = input$total_labor,
      random_seed = input$random_seed,
      money_supply = list(
        mint_loss_min = input$mint_loss_min,
        mint_loss_max = input$mint_loss_max
      ),
      mining_worker = list(
        T = input$worker_T,
        t = input$worker_t,
        start_x = input$start_x,
        task_min = input$task_min,
        task_max = input$task_max,
        output_min = input$output_min,
        output_max = input$output_max
      )
    )
  }

  initial_result <- run_world_simulation(baseline_params)
  current_result <- shiny::reactiveVal(initial_result)
  comparison_runs <- shiny::reactiveVal(list(initial_result))
  selected_series_ids <- shiny::reactiveVal(character())

  shiny::observeEvent(input$scenario_file, {
    params <- load_parameters(input$scenario_file)
    shiny::updateTextInput(session, "scenario_name", value = paste0(params$scenario_name, "_run"))
    shiny::updateNumericInput(session, "total_labor", value = params$L)
    shiny::updateNumericInput(session, "time_horizon", value = params$T)
    shiny::updateTextInput(session, "random_seed", value = as.character(params$random_seed %||% ""))
    shiny::updateNumericInput(session, "mint_loss_min", value = params$money_supply$mint_loss_min)
    shiny::updateNumericInput(session, "mint_loss_max", value = params$money_supply$mint_loss_max)
    shiny::updateNumericInput(session, "worker_T", value = params$mining_worker$T)
    shiny::updateNumericInput(session, "worker_t", value = params$mining_worker$t)
    shiny::updateNumericInput(session, "start_x", value = params$mining_worker$start_x)
    shiny::updateNumericInput(session, "task_min", value = params$mining_worker$task_min)
    shiny::updateNumericInput(session, "task_max", value = params$mining_worker$task_max)
    shiny::updateNumericInput(session, "output_min", value = params$mining_worker$output_min)
    shiny::updateNumericInput(session, "output_max", value = params$mining_worker$output_max)
    shiny::updateSliderInput(session, "inspect_time", max = params$T, value = c(1, params$T))
    shiny::updateSliderInput(session, "graph_time", max = params$T, value = c(1, params$T))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$run_sim, {
    params <- apply_control_overrides(load_parameters(input$scenario_file), controls_from_input())
    result <- run_world_simulation(params, save_results = isTRUE(input$save_run))
    current_result(result)
    comparison_runs(c(comparison_runs(), list(result)))
    shiny::updateSliderInput(session, "inspect_time", max = params$T, value = c(1, params$T))
    shiny::updateSliderInput(session, "graph_time", max = params$T, value = c(1, params$T))
    catalog <- build_series_catalog(result)
    selected_series_ids(intersect(selected_series_ids(), catalog$series_id))
  })

  output$total_output <- shiny::renderText({
    format(round(sum(current_result()$ore_output$output), 1), big.mark = ",")
  })

  output$actual_money_supply <- shiny::renderText({
    format(round(sum(current_result()$money_supply$net_money_supply_value), 1), big.mark = ",")
  })

  output$returned_coin_supply <- shiny::renderText({
    format(round(sum(current_result()$money_supply$returned_supply), 1), big.mark = ",")
  })

  output$country_y_reserve <- shiny::renderText({
    format(round(sum(current_result()$money_supply$country_y_reserve), 1), big.mark = ",")
  })

  output$current_money_supply_bar <- shiny::renderPlot({
    money <- current_result()$money_supply
    current_time <- max(money$time)
    data <- money[money$time == current_time, ]
    data$money_supply <- data$net_money_supply_value
    plot_bar(data, "coin_type", "money_supply", paste("Actual money supply at period", current_time))
  })

  output$money_supply_table <- shiny::renderTable({
    money_supply_totals(current_result()$money_supply)
  })

  series_catalog <- shiny::reactive({
    build_series_catalog(current_result())
  })

  shiny::observeEvent(input$graph_source, {
    catalog <- series_catalog()
    source_catalog <- catalog[catalog$source == input$graph_source, ]
    shiny::updateSelectInput(
      session,
      "series_variable",
      choices = unique(source_catalog$variable),
      selected = unique(source_catalog$variable)[1]
    )
  }, ignoreInit = FALSE)

  shiny::observeEvent(list(input$graph_source, input$series_variable, current_result()), {
    catalog <- series_catalog()
    source_catalog <- catalog[
      catalog$source == input$graph_source &
        catalog$variable == input$series_variable,
    ]

    choices <- stats::setNames(source_catalog$series_id, source_catalog$unit)
    shiny::updateCheckboxGroupInput(
      session,
      "available_series",
      choices = choices,
      selected = character()
    )
  }, ignoreInit = FALSE)

  update_selected_series_control <- function() {
    catalog <- series_catalog()
    selected <- selected_series_ids()
    selected_catalog <- catalog[catalog$series_id %in% selected, ]
    choices <- stats::setNames(selected_catalog$series_id, selected_catalog$label)
    shiny::updateCheckboxGroupInput(
      session,
      "selected_series",
      choices = choices,
      selected = selected
    )
  }

  shiny::observeEvent(input$add_series, {
    series_ids <- input$available_series
    if (length(series_ids) > 0) {
      selected_series_ids(unique(c(selected_series_ids(), series_ids)))
      update_selected_series_control()
    }
  })

  shiny::observeEvent(input$clear_series, {
    selected_series_ids(character())
    update_selected_series_control()
  })

  shiny::observeEvent(input$selected_series, {
    selected_series_ids(input$selected_series %||% character())
  }, ignoreInit = TRUE)

  graph_data <- shiny::reactive({
    time_range <- input$graph_time
    if (is.null(time_range)) {
      time_range <- c(1, current_result()$params$T)
    }

    build_graph_data(
      result = current_result(),
      catalog = series_catalog(),
      selected_ids = selected_series_ids(),
      transform = input$graph_transform,
      time_range = time_range
    )
  })

  output$graph_plot <- shiny::renderPlot({
    data <- graph_data()
    if (nrow(data) == 0) {
      plot_empty_graph()
    } else {
      plot_time_series(data, "graph_value", "series", "Selected time series")
    }
  })

  output$graph_table <- shiny::renderTable({
    data <- graph_data()
    if (nrow(data) == 0) {
      return(data.frame(time = integer(), series = character(), graph_value = numeric()))
    }
    utils::head(data[, c("time", "series", "graph_value")], 12)
  })

  inspected_ore_data <- shiny::reactive({
    data <- current_result()$ore_output
    data[
      data$good == input$inspect_good &
        data$time >= input$inspect_time[1] &
        data$time <= input$inspect_time[2],
    ]
  })

  output$ore_table <- shiny::renderTable({
    utils::head(inspected_ore_data(), 12)
  })

  output$run_details <- shiny::renderPrint({
    current_result()$run_metadata
  })

  output$runs_table <- shiny::renderTable({
    rows <- lapply(comparison_runs(), function(result) result$run_metadata)
    do.call(rbind, rows)
  })
}

shiny::shinyApp(ui = ui, server = server)
