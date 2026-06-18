assemble_ore_output <- function(production_state, params, run_id) {
  production_state$run_id <- run_id
  production_state$scenario_name <- params$scenario_name
  production_state[, c(
    "run_id",
    "scenario_name",
    "time",
    "sector",
    "good",
    "workers",
    "total_runs",
    "successful_runs",
    "raw_output",
    "output"
  )]
}

run_world_simulation <- function(params, save_results = FALSE) {
  run_id <- digest::digest(list(params, Sys.time()))
  worker_state <- simulate_worker_state(params)
  production_state <- simulate_production(worker_state, params)
  ore_output <- assemble_ore_output(production_state, params, run_id)
  money_supply <- simulate_money_supply(ore_output, params)
  summary_outputs <- summarize_outputs(ore_output, params)

  run_metadata <- data.frame(
    run_id = run_id,
    scenario_name = params$scenario_name,
    T = params$T,
    L = params$L,
    random_seed = params$random_seed %||% NA_integer_,
    worker_T = params$mining_worker$T,
    worker_t = params$mining_worker$t,
    task_min = params$mining_worker$task_min,
    task_max = params$mining_worker$task_max,
    output_min = params$mining_worker$output_min,
    output_max = params$mining_worker$output_max,
    mint_loss_min = params$money_supply$mint_loss_min,
    mint_loss_max = params$money_supply$mint_loss_max,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )

  if (save_results) {
    write_or_append_csv(run_metadata, "data/simulation_runs.csv")
    write_or_append_csv(ore_output, "data/ore_outputs.csv")
    write_or_append_csv(money_supply, "data/money_supply_outputs.csv")
  }

  list(
    run_id = run_id,
    params = params,
    workers = worker_state,
    production = production_state,
    ore_output = ore_output,
    money_supply = money_supply,
    summaries = summary_outputs,
    run_metadata = run_metadata
  )
}
