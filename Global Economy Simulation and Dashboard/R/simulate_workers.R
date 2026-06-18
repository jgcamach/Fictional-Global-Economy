allocate_labor <- function(L, allocation_rules) {
  sectors <- names(allocation_rules$sectors)
  shares <- unlist(allocation_rules$sectors)
  shares <- shares / sum(shares)

  workers <- floor(L * shares)
  leftover <- L - sum(workers)

  if (leftover > 0 && length(workers) > 0) {
    workers[1] <- workers[1] + leftover
  }

  data.frame(
    sector = sectors,
    good = sectors,
    workers = as.integer(workers),
    stringsAsFactors = FALSE
  )
}

run_worker <- function(params) {
  x <- params$start_x
  runs <- 0
  successes <- 0

  max_runs <- floor(params$T / params$t)

  while (runs < max_runs) {
    draw <- stats::runif(
      1,
      min = params$task_min,
      max = params$task_max
    )

    runs <- runs + 1

    if (draw < x) {
      x <- x - 1
    } else {
      successes <- successes + 1
      x <- params$start_x
    }
  }

  successes
}

produce_output <- function(successes, params) {
  if (successes == 0) {
    return(0)
  }

  sum(
    stats::runif(
      successes,
      min = params$output_min,
      max = params$output_max
    )
  )
}

produce_ore <- function(L, params) {
  worker_outputs <- replicate(
    L,
    {
      successes <- run_worker(params)
      produce_output(successes, params)
    }
  )

  sum(worker_outputs)
}

simulate_workers <- function(n_workers, params) {
  if (n_workers <= 0) {
    return(data.frame(successes = integer(), runs = integer(), output = numeric()))
  }

  max_runs <- floor(params$T / params$t)
  worker_results <- replicate(
    n_workers,
    {
      successes <- run_worker(params)
      data.frame(
        successes = successes,
        runs = max_runs,
        output = produce_output(successes, params)
      )
    },
    simplify = FALSE
  )

  worker_df <- do.call(
    rbind,
    worker_results
  )

  rownames(worker_df) <- NULL

  worker_df
}

simulate_worker_state <- function(params) {
  if (!is.null(params$random_seed)) {
    set.seed(params$random_seed)
  }

  labor <- allocate_labor(params$L, params$labor_allocation)
  mining <- params$mining_worker
  rows <- list()
  index <- 1

  for (time in seq_len(params$T)) {
    for (i in seq_len(nrow(labor))) {
      worker_df <- simulate_workers(
        n_workers = labor$workers[i],
        params = mining
      )

      rows[[index]] <- data.frame(
        time = time,
        sector = labor$sector[i],
        good = labor$good[i],
        workers = labor$workers[i],
        total_runs = sum(worker_df$runs),
        successful_runs = sum(worker_df$successes),
        raw_output = sum(worker_df$output),
        stringsAsFactors = FALSE
      )
      index <- index + 1
    }
  }

  do.call(rbind, rows)
}
