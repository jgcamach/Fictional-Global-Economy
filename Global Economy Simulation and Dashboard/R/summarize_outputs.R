summarize_outputs <- function(ore_output, params) {
  total_output_by_good <- aggregate(
    output ~ good,
    data = ore_output,
    FUN = sum
  )

  list(
    total_output_by_good = total_output_by_good
  )
}
