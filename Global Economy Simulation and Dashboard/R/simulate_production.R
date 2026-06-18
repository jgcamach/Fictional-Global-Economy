simulate_production <- function(worker_state, params) {
  worker_state$output <- worker_state$raw_output
  worker_state[, c(
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
