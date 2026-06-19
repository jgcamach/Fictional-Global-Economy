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
world_continents <- c("Illvaria", "Harlund", "Halcyx")

world_id <- function(value) {
  gsub("[^a-z0-9]+", "_", tolower(value))
}

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
  shiny::tags$script(shiny::HTML(
    "document.addEventListener('click', function(event) {
       if (event.target && event.target.id === 'sidebarToggle') {
         document.querySelector('.app-shell').classList.toggle('is-collapsed');
       }
     });
     window.worldAction = function(payload) {
       payload.nonce = Math.random();
       Shiny.setInputValue('world_action', payload, {priority: 'event'});
     };
     window.worldAddFromInput = function(payload, inputId) {
       var input = document.getElementById(inputId);
       payload.value = input ? input.value : '';
       payload.nonce = Math.random();
       Shiny.setInputValue('world_action', payload, {priority: 'event'});
       if (input) { input.value = ''; input.dispatchEvent(new Event('change', {bubbles: true})); }
     };
     window.worldAddSettlement = function(payload, inputId, typeId) {
       var input = document.getElementById(inputId);
       var type = document.getElementById(typeId);
       payload.value = input ? input.value : '';
       payload.settlement_type = type ? type.value : 'City';
       payload.nonce = Math.random();
       Shiny.setInputValue('world_action', payload, {priority: 'event'});
       if (input) { input.value = ''; input.dispatchEvent(new Event('change', {bubbles: true})); }
     };"
  )),
  shiny::div(
    class = "app-shell",
    shiny::div(
      class = "app-header",
      shiny::tags$button(id = "sidebarToggle", class = "sidebar-toggle", type = "button", "\u2630"),
      shiny::div(
        class = "brand-block",
        shiny::h2("Ore Output Dashboard"),
        shiny::span("Mining and enchanted coin supply")
      )
    ),
    shiny::div(
      class = "app-main",
      shiny::tabsetPanel(
        id = "main_tabs",
        shiny::tabPanel(
      "World Overview",
      shiny::br(),
      shiny::uiOutput("worldview_ui")
    ),
    shiny::tabPanel(
      "Graphs",
      shiny::br(),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput(
            "graph_source",
            "Dataset",
            choices = c("Ore output", "Money supply by coin", "Money supply stages"),
            selectize = FALSE
          ),
          shiny::selectInput("series_variable", "Measure", choices = character(), selectize = FALSE),
          shiny::checkboxGroupInput("available_series", "Available series", choices = character()),
          shiny::actionButton("clear_series", "Clear graph"),
          shiny::selectInput("graph_transform", "View", choices = c("Level", "Cumulative"), selectize = FALSE),
          shiny::sliderInput("graph_time", "Time range", min = 1, max = baseline_params$T, value = c(1, baseline_params$T), step = 1)
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            id = "graph_view_tabs",
            shiny::tabPanel("Graph", shiny::br(), shiny::plotOutput("graph_plot", height = 460)),
            shiny::tabPanel("Table", shiny::br(), shiny::tableOutput("graph_table"))
          )
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
            selected = "scenarios/baseline.yaml",
            selectize = FALSE
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
  )
)

server <- function(input, output, session) {
  hide_frontend_ids <- function(data) {
    if (is.data.frame(data)) {
      return(data[, setdiff(names(data), "run_id"), drop = FALSE])
    }
    if (is.list(data)) {
      data$run_id <- NULL
    }
    data
  }

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
  active_world_continent <- shiny::reactiveVal(NULL)
  active_world_country <- shiny::reactiveVal(NULL)
  active_world_settlement <- shiny::reactiveVal(NULL)
  world_map <- shiny::reactiveVal(stats::setNames(
    lapply(world_continents, function(continent) {
      list(sectors = character(), countries = list())
    }),
    world_continents
  ))

  clean_entry <- function(value) {
    value <- trimws(value %||% "")
    if (!nzchar(value)) {
      return(NULL)
    }
    value
  }

  add_unique <- function(values, value) {
    unique(c(values, value))
  }

  payload_json <- function(payload) {
    jsonlite::toJSON(payload, auto_unbox = TRUE)
  }

  world_button <- function(label, payload, class = "btn btn-default") {
    shiny::tags$button(
      type = "button",
      class = class,
      onclick = paste0("worldAction(", payload_json(payload), ")"),
      label
    )
  }

  world_add_button <- function(label, payload, input_id, class = "btn btn-default") {
    shiny::tags$button(
      type = "button",
      class = class,
      onclick = paste0("worldAddFromInput(", payload_json(payload), ", '", input_id, "')"),
      label
    )
  }

  settlement_add_button <- function(label, payload, input_id, type_id, class = "btn btn-default") {
    shiny::tags$button(
      type = "button",
      class = class,
      onclick = paste0("worldAddSettlement(", payload_json(payload), ", '", input_id, "', '", type_id, "')"),
      label
    )
  }

  placeholder_dashboard <- function(title, scope) {
    shiny::div(
      class = "entity-dashboard",
      shiny::h4(title),
      shiny::div(
        class = "dashboard-placeholder-grid",
        shiny::div(class = "dashboard-placeholder-card", shiny::h5("Production"), shiny::span("Backend simulation placeholder")),
        shiny::div(class = "dashboard-placeholder-card", shiny::h5("Money Supply"), shiny::span("Backend simulation placeholder")),
        shiny::div(class = "dashboard-placeholder-card", shiny::h5("Trade"), shiny::span("Backend simulation placeholder")),
        shiny::div(class = "dashboard-placeholder-card", shiny::h5("Population"), shiny::span("Backend simulation placeholder"))
      ),
      shiny::p(class = "small-note", paste("This", scope, "dashboard is a navigation placeholder for future backend data."))
    )
  }

  sector_list <- function(sectors, continent, country = NULL) {
    if (length(sectors) == 0) {
      return(shiny::p(class = "small-note", "No sectors added yet."))
    }
    shiny::tags$ul(class = "editable-list", lapply(sectors, function(sector) {
      action <- if (is.null(country)) "remove_continent_sector" else "remove_country_sector"
      payload <- list(action = action, continent = continent, sector = sector)
      if (!is.null(country)) {
        payload$country <- country
      }
      shiny::tags$li(
        shiny::span(sector),
        world_button("Remove", payload, class = "btn btn-link remove-link")
      )
    }))
  }

  settlement_panel <- function(continent, country, settlement_type, settlement_name) {
    shiny::tabPanel(
      settlement_name,
      shiny::br(),
      shiny::div(
        class = "world-dashboard-shell",
        placeholder_dashboard(paste(settlement_type, settlement_name, sep = ": "), tolower(settlement_type)),
        world_button(
          paste("Remove", tolower(settlement_type)),
          list(action = "remove_settlement", continent = continent, country = country, settlement = settlement_name),
          class = "btn btn-default danger-action"
        )
      )
    )
  }

  country_panel <- function(continent, country, country_data) {
    id <- world_id(paste(continent, country, sep = "_"))
    shiny::tabPanel(
      country,
      shiny::br(),
      do.call(
        shiny::tabsetPanel,
        c(
          list(
            shiny::tabPanel(
              "Country Dashboard",
              shiny::br(),
              shiny::div(
                class = "world-dashboard-shell",
                placeholder_dashboard(country, "country"),
                shiny::div(
                  class = "world-admin-panel",
                  shiny::h4("Country Structure"),
                  shiny::h5("Sectors"),
                  sector_list(country_data$sectors, continent, country),
                  shiny::div(
                    class = "inline-add",
                    shiny::textInput(paste0(id, "_sector"), NULL, placeholder = "Add country sector"),
                    world_add_button(
                      "+",
                      list(action = "add_country_sector", continent = continent, country = country),
                      paste0(id, "_sector"),
                      class = "btn btn-primary icon-action"
                    )
                  ),
                  shiny::h5("Cities/Villages"),
                  if (nrow(country_data$settlements) == 0) {
                    shiny::p(class = "small-note", "No cities or villages added yet.")
                  } else {
                    shiny::tags$ul(class = "editable-list", lapply(seq_len(nrow(country_data$settlements)), function(i) {
                      settlement <- country_data$settlements$name[i]
                      shiny::tags$li(
                        shiny::span(paste(country_data$settlements$type[i], settlement, sep = ": ")),
                        world_button(
                          "Remove",
                          list(action = "remove_settlement", continent = continent, country = country, settlement = settlement),
                          class = "btn btn-link remove-link"
                        )
                      )
                    }))
                  },
                  shiny::div(
                    class = "inline-add settlement-add",
                    shiny::textInput(paste0(id, "_settlement"), NULL, placeholder = "Add city/village"),
                    shiny::selectInput(paste0(id, "_settlement_type"), NULL, choices = c("City", "Village"), selectize = FALSE),
                    settlement_add_button(
                      "+",
                      list(action = "add_settlement", continent = continent, country = country),
                      paste0(id, "_settlement"),
                      paste0(id, "_settlement_type"),
                      class = "btn btn-primary icon-action"
                    )
                  ),
                  world_button(
                    "Remove country",
                    list(action = "remove_country", continent = continent, country = country),
                    class = "btn btn-default danger-action"
                  )
                )
              )
            )
          ),
          lapply(seq_len(nrow(country_data$settlements)), function(i) {
          settlement_panel(
            continent,
            country,
            country_data$settlements$type[i],
            country_data$settlements$name[i]
          )
          }),
          list(
            id = paste0(id, "_tabs"),
            selected = if (!is.null(active_world_country()) && identical(country, active_world_country())) {
              active_world_settlement() %||% "Country Dashboard"
            } else {
              "Country Dashboard"
            }
          )
        )
      )
    )
  }

  continent_panel <- function(continent, continent_data) {
    id <- world_id(continent)
    country_names <- names(continent_data$countries)
    shiny::tabPanel(
      continent,
      shiny::br(),
      do.call(
        shiny::tabsetPanel,
        c(
          list(
            shiny::tabPanel(
              "Continent Dashboard",
              shiny::br(),
              shiny::div(
                class = "world-dashboard-shell",
                placeholder_dashboard(continent, "continent"),
                shiny::div(
                  class = "world-admin-panel",
                  shiny::h4("Continent Structure"),
                  shiny::h5("Sectors"),
                  sector_list(continent_data$sectors, continent),
                  shiny::div(
                    class = "inline-add",
                    shiny::textInput(paste0(id, "_sector"), NULL, placeholder = "Add continent sector"),
                    world_add_button(
                      "+",
                      list(action = "add_continent_sector", continent = continent),
                      paste0(id, "_sector"),
                      class = "btn btn-primary icon-action"
                    )
                  ),
                  shiny::h5("Countries"),
                  if (length(country_names) == 0) {
                    shiny::p(class = "small-note", "No countries added yet.")
                  } else {
                    shiny::tags$ul(class = "editable-list", lapply(country_names, function(country) {
                      shiny::tags$li(
                        shiny::span(country),
                        world_button(
                          "Remove",
                          list(action = "remove_country", continent = continent, country = country),
                          class = "btn btn-link remove-link"
                        )
                      )
                    }))
                  },
                  shiny::div(
                    class = "inline-add",
                    shiny::textInput(paste0(id, "_country"), NULL, placeholder = "Add country"),
                    world_add_button(
                      "+",
                      list(action = "add_country", continent = continent),
                      paste0(id, "_country"),
                      class = "btn btn-primary icon-action"
                    )
                  ),
                  world_button(
                    "Remove continent",
                    list(action = "remove_continent", continent = continent),
                    class = "btn btn-default danger-action"
                  )
                )
              )
            )
          ),
          lapply(country_names, function(country) {
          country_panel(continent, country, continent_data$countries[[country]])
          }),
          list(
            id = paste0(id, "_tabs"),
            selected = if (identical(continent, active_world_continent())) {
              active_world_country() %||% "Continent Dashboard"
            } else {
              "Continent Dashboard"
            }
          )
        )
      )
    )
  }

  output$worldview_ui <- shiny::renderUI({
    continents <- names(world_map())
    continent_tabs <- lapply(continents, function(continent) {
      continent_panel(continent, world_map()[[continent]])
    })
    add_id <- "world_new_continent"
    shiny::tagList(
      shiny::div(
        class = "world-tab-toolbar",
        shiny::textInput(add_id, NULL, placeholder = "Add continent"),
        world_add_button(
          "+",
          list(action = "add_continent"),
          add_id,
          class = "btn btn-primary icon-action"
        )
      ),
      do.call(
        shiny::tabsetPanel,
        c(
          list(
            shiny::tabPanel(
              "Overview",
              shiny::br(),
              shiny::div(
                class = "kpi-grid",
                shiny::wellPanel(shiny::h4("Actual Money Supply"), shiny::textOutput("actual_money_supply")),
                shiny::wellPanel(shiny::h4("Total Ore Output"), shiny::textOutput("total_output")),
                shiny::wellPanel(shiny::h4("Returned Coins"), shiny::textOutput("returned_coin_supply")),
                shiny::wellPanel(shiny::h4("Country Y Reserve"), shiny::textOutput("country_y_reserve"))
              ),
              shiny::div(
                class = "overview-grid",
                shiny::div(
                  class = "overview-card",
                  shiny::h4("Current Money Supply"),
                  shiny::uiOutput("current_money_supply_slot")
                )
              )
            )
          ),
          continent_tabs,
          list(
            id = "worldview_tabs",
            selected = active_world_continent() %||% "Overview"
          )
        )
      )
    )
  })

  shiny::observeEvent(input$world_action, {
    event <- input$world_action
    action <- event$action %||% ""
    data <- world_map()
    value <- clean_entry(event$value)
    continent <- event$continent
    country <- event$country
    sector <- event$sector
    settlement <- event$settlement

    if (identical(action, "add_continent") && !is.null(value) && is.null(data[[value]])) {
      data[[value]] <- list(sectors = character(), countries = list())
      active_world_continent(value)
      active_world_country(NULL)
      active_world_settlement(NULL)
    } else if (identical(action, "remove_continent") && !is.null(continent)) {
      data[[continent]] <- NULL
      active_world_continent("Overview")
      active_world_country(NULL)
      active_world_settlement(NULL)
    } else if (identical(action, "add_continent_sector") && !is.null(value) && !is.null(data[[continent]])) {
      data[[continent]]$sectors <- add_unique(data[[continent]]$sectors, value)
      active_world_continent(continent)
      active_world_country(NULL)
      active_world_settlement(NULL)
    } else if (identical(action, "remove_continent_sector") && !is.null(data[[continent]])) {
      data[[continent]]$sectors <- setdiff(data[[continent]]$sectors, sector)
      active_world_continent(continent)
      active_world_country(NULL)
      active_world_settlement(NULL)
    } else if (identical(action, "add_country") && !is.null(value) && !is.null(data[[continent]])) {
      if (is.null(data[[continent]]$countries[[value]])) {
        data[[continent]]$countries[[value]] <- list(
          sectors = character(),
          settlements = data.frame(type = character(), name = character(), stringsAsFactors = FALSE)
        )
      }
      active_world_continent(continent)
      active_world_country(value)
      active_world_settlement(NULL)
    } else if (identical(action, "remove_country") && !is.null(data[[continent]])) {
      data[[continent]]$countries[[country]] <- NULL
      active_world_continent(continent)
      active_world_country(NULL)
      active_world_settlement(NULL)
    } else if (identical(action, "add_country_sector") && !is.null(value) &&
      !is.null(data[[continent]]) && !is.null(data[[continent]]$countries[[country]])) {
      data[[continent]]$countries[[country]]$sectors <- add_unique(
        data[[continent]]$countries[[country]]$sectors,
        value
      )
      active_world_continent(continent)
      active_world_country(country)
      active_world_settlement(NULL)
    } else if (identical(action, "remove_country_sector") &&
      !is.null(data[[continent]]) && !is.null(data[[continent]]$countries[[country]])) {
      data[[continent]]$countries[[country]]$sectors <- setdiff(
        data[[continent]]$countries[[country]]$sectors,
        sector
      )
      active_world_continent(continent)
      active_world_country(country)
      active_world_settlement(NULL)
    } else if (identical(action, "add_settlement") && !is.null(value) &&
      !is.null(data[[continent]]) && !is.null(data[[continent]]$countries[[country]])) {
      new_settlement <- data.frame(
        type = event$settlement_type %||% "City",
        name = value,
        stringsAsFactors = FALSE
      )
      data[[continent]]$countries[[country]]$settlements <- unique(rbind(
        data[[continent]]$countries[[country]]$settlements,
        new_settlement
      ))
      active_world_continent(continent)
      active_world_country(country)
      active_world_settlement(value)
    } else if (identical(action, "remove_settlement") &&
      !is.null(data[[continent]]) && !is.null(data[[continent]]$countries[[country]])) {
      settlements <- data[[continent]]$countries[[country]]$settlements
      data[[continent]]$countries[[country]]$settlements <- settlements[settlements$name != settlement, , drop = FALSE]
      active_world_continent(continent)
      active_world_country(country)
      active_world_settlement(NULL)
    }

    world_map(data)
  }, ignoreInit = TRUE)

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
    shiny::updateSliderInput(session, "graph_time", max = params$T, value = c(1, params$T))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$run_sim, {
    params <- apply_control_overrides(load_parameters(input$scenario_file), controls_from_input())
    result <- run_world_simulation(params, save_results = isTRUE(input$save_run))
    current_result(result)
    comparison_runs(c(comparison_runs(), list(result)))
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

  output$current_money_supply_slot <- shiny::renderUI({
    shiny::plotOutput(
      "current_money_supply_bar",
      width = "260px",
      height = "300px"
    )
  })

  output$current_money_supply_bar <- shiny::renderPlot({
    money <- current_result()$money_supply
    current_time <- max(money$time)
    data <- data.frame(
      label = paste("Period", current_time),
      money_supply = sum(money$net_money_supply_value[money$time == current_time])
    )
    plot_bar(
      data,
      "label",
      "money_supply",
      paste("Actual money supply at period", current_time),
      transparent = TRUE
    )
  }, bg = "transparent")

  output$money_supply_table <- shiny::renderTable({
    hide_frontend_ids(money_supply_totals(current_result()$money_supply))
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
      selected = intersect(selected_series_ids(), source_catalog$series_id)
    )
  }, ignoreInit = FALSE)

  shiny::observeEvent(input$clear_series, {
    selected_series_ids(character())
    shiny::updateCheckboxGroupInput(
      session,
      "available_series",
      selected = character()
    )
  })

  shiny::observeEvent(input$available_series, {
    catalog <- series_catalog()
    source_catalog <- catalog[
      catalog$source == input$graph_source &
        catalog$variable == input$series_variable,
    ]
    selected_outside_view <- setdiff(selected_series_ids(), source_catalog$series_id)
    selected_series_ids(unique(c(selected_outside_view, input$available_series %||% character())))
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

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
    data[, c("time", "series", "graph_value")]
  })

  output$run_details <- shiny::renderPrint({
    hide_frontend_ids(current_result()$run_metadata)
  })

  output$runs_table <- shiny::renderTable({
    rows <- lapply(comparison_runs(), function(result) hide_frontend_ids(result$run_metadata))
    hide_frontend_ids(do.call(rbind, rows))
  })
}

shiny::shinyApp(ui = ui, server = server)
