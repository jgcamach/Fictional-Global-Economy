load_parameters <- function(path = "scenarios/baseline.yaml") {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to load scenario files.", call. = FALSE)
  }

  params <- yaml::read_yaml(path)
  normalize_parameters(params)
}

normalize_parameters <- function(params) {
  sectors <- names(params$ores %||% params$production %||% params$labor_allocation$sectors)
  params$sectors <- sectors
  params$scenario_name <- params$scenario_name %||% "custom"
  params$T <- as.integer(params$T %||% 100)
  params$L <- as.integer(params$L %||% 100)
  params$mining_worker$T <- as_numeric(
    params$mining_worker$T %||% params$mining_worker$period_length,
    100
  )
  params$mining_worker$t <- as_numeric(
    params$mining_worker$t %||% params$mining_worker$attempt_time,
    5
  )
  params$mining_worker$start_x <- as_numeric(params$mining_worker$start_x, 20)
  params$mining_worker$task_min <- as_numeric(
    params$mining_worker$task_min %||% params$mining_worker$min_draw,
    1
  )
  params$mining_worker$task_max <- as_numeric(
    params$mining_worker$task_max %||% params$mining_worker$max_draw,
    20
  )
  params$mining_worker$output_min <- as_numeric(params$mining_worker$output_min, 1)
  params$mining_worker$output_max <- as_numeric(params$mining_worker$output_max, 4)
  params$money_supply$mint_loss_min <- as_numeric(params$money_supply$mint_loss_min, 0)
  params$money_supply$mint_loss_max <- as_numeric(params$money_supply$mint_loss_max, 0.0002)
  if (is.null(params$random_seed) || identical(params$random_seed, "")) {
    params$random_seed <- NULL
  } else {
    params$random_seed <- as.integer(params$random_seed)
    if (is.na(params$random_seed)) {
      params$random_seed <- NULL
    }
  }

  if (is.null(params$labor_allocation$sectors)) {
    params$labor_allocation$method <- "equal_split"
    params$labor_allocation$sectors <- stats::setNames(
      rep(1 / length(sectors), length(sectors)),
      sectors
    )
  }

  for (sector in sectors) {
    params$money_supply$coins[[sector]]$coin_type <- params$money_supply$coins[[sector]]$coin_type %||% sector
    params$money_supply$coins[[sector]]$coins_per_ore <- as_numeric(
      params$money_supply$coins[[sector]]$coins_per_ore,
      10
    )
    params$money_supply$coins[[sector]]$weekly_wage <- as_numeric(
      params$money_supply$coins[[sector]]$weekly_wage,
      1
    )
    params$money_supply$coins[[sector]]$mint_cost <- as_numeric(
      params$money_supply$coins[[sector]]$mint_cost,
      10
    )
    params$money_supply$coins[[sector]]$enchant_success_rate <- as_numeric(
      params$money_supply$coins[[sector]]$enchant_success_rate,
      1
    )
    params$money_supply$coins[[sector]]$reserve_share <- as_numeric(
      params$money_supply$coins[[sector]]$reserve_share,
      0.1
    )
    params$money_supply$coins[[sector]]$value_weight <- as_numeric(
      params$money_supply$coins[[sector]]$value_weight,
      1
    )
  }

  params
}

apply_control_overrides <- function(params, controls) {
  params$scenario_name <- controls$scenario_name %||% params$scenario_name
  params$T <- as.integer(controls$T %||% params$T)
  params$L <- as.integer(controls$L %||% params$L)
  if (!is.null(controls$random_seed)) {
    params$random_seed <- controls$random_seed
  }
  if (!is.null(controls$mining_worker)) {
    params$mining_worker$T <- controls$mining_worker$T %||% params$mining_worker$T
    params$mining_worker$t <- controls$mining_worker$t %||% params$mining_worker$t
    params$mining_worker$start_x <- controls$mining_worker$start_x %||% params$mining_worker$start_x
    params$mining_worker$task_min <- controls$mining_worker$task_min %||% params$mining_worker$task_min
    params$mining_worker$task_max <- controls$mining_worker$task_max %||% params$mining_worker$task_max
    params$mining_worker$output_min <- controls$mining_worker$output_min %||% params$mining_worker$output_min
    params$mining_worker$output_max <- controls$mining_worker$output_max %||% params$mining_worker$output_max
  }
  if (!is.null(controls$money_supply)) {
    params$money_supply$mint_loss_min <- controls$money_supply$mint_loss_min %||% params$money_supply$mint_loss_min
    params$money_supply$mint_loss_max <- controls$money_supply$mint_loss_max %||% params$money_supply$mint_loss_max
  }

  normalize_parameters(params)
}
