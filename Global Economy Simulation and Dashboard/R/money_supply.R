simulate_money_supply <- function(ore_output, params) {
  money <- params$money_supply
  rows <- list()
  index <- 1

  for (time in sort(unique(ore_output$time))) {
    loss_rate <- stats::runif(
      1,
      min = money$mint_loss_min,
      max = money$mint_loss_max
    )
    time_output <- ore_output[ore_output$time == time, ]

    for (i in seq_len(nrow(time_output))) {
      good <- time_output$good[i]
      coin <- money$coins[[good]]
      ore_units <- time_output$output[i]
      minted_exact <- coin$coins_per_ore * ore_units * (1 - loss_rate)
      minted <- floor(minted_exact)
      spill <- coin$coins_per_ore * ore_units - minted
      spill_loss <- spill * coin$value_weight
      labor_cost <- coin$weekly_wage * time_output$workers[i]
      mint_cost <- coin$mint_cost
      enchanted <- coin$enchant_success_rate * minted
      reserve <- coin$reserve_share * enchanted
      returned_supply <- enchanted - reserve
      creation_cost <- labor_cost + mint_cost
      supply_value <- returned_supply * coin$value_weight
      net_money_supply_value <- supply_value - creation_cost

      rows[[index]] <- data.frame(
        time = time,
        good = good,
        coin_type = coin$coin_type,
        ore_output = ore_units,
        mint_loss_rate = loss_rate,
        minted_coins = minted,
        spill = spill,
        spill_loss = spill_loss,
        enchanted_coins = enchanted,
        country_y_reserve = reserve,
        returned_supply = returned_supply,
        labor_cost = labor_cost,
        mint_cost = mint_cost,
        creation_cost = creation_cost,
        supply_value = supply_value,
        net_money_supply_value = net_money_supply_value,
        stringsAsFactors = FALSE
      )
      index <- index + 1
    }
  }

  do.call(rbind, rows)
}

money_supply_flow_long <- function(money_supply) {
  measures <- c(
    "minted_coins",
    "enchanted_coins",
    "country_y_reserve",
    "returned_supply"
  )
  labels <- c(
    minted_coins = "Country X minted",
    enchanted_coins = "Country Y enchanted",
    country_y_reserve = "Country Y reserve",
    returned_supply = "Returned supply"
  )

  rows <- list()
  index <- 1
  for (measure in measures) {
    rows[[index]] <- data.frame(
      time = money_supply$time,
      stage = labels[[measure]],
      coins = money_supply[[measure]],
      stringsAsFactors = FALSE
    )
    index <- index + 1
  }

  flow <- do.call(rbind, rows)
  aggregate(
    coins ~ time + stage,
    data = flow,
    FUN = sum
  )
}

money_supply_totals <- function(money_supply) {
  aggregate(
    cbind(minted_coins, enchanted_coins, country_y_reserve, returned_supply, net_money_supply_value) ~ good + coin_type,
    data = money_supply,
    FUN = sum
  )
}
