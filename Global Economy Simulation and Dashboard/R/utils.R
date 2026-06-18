`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

as_numeric <- function(x, default = 0) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) == 0 || is.na(value)) default else value
}

coerce_sector_name <- function(x) {
  gsub("_", " ", x, fixed = TRUE)
}

ensure_data_dir <- function(path = "data") {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  invisible(path)
}

write_or_append_csv <- function(data, path) {
  ensure_data_dir(dirname(path))
  write_header <- !file.exists(path)
  utils::write.table(
    data,
    file = path,
    sep = ",",
    row.names = FALSE,
    col.names = write_header,
    append = !write_header,
    qmethod = "double"
  )
}

