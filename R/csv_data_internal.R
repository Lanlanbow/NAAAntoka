# The data-creation API accepts already-converted UTF-8 CSVs only.
.naaa_fread <- function(path, ...) {
  .naaa_parse_csv(
    data.table::fread(
      file = path, sep = ",", header = TRUE, encoding = "UTF-8",
      colClasses = "character", na.strings = NULL, strip.white = FALSE,
      check.names = FALSE, fill = FALSE, blank.lines.skip = FALSE,
      showProgress = FALSE, ...
    ), path
  )
}

.naaa_csv_header <- function(path) {
  # Read one data record to limit sampling while retaining CSV header parsing.
  preview <- .naaa_fread(path, nrows = 1L)
  if (any(grepl('"', names(preview), fixed = TRUE, useBytes = TRUE))) {
    preview <- .naaa_readr_csv(
      path, col_types = readr::cols(.default = readr::col_character()), n_max = 0L
    )
  }
  header <- names(preview)
  if (length(header) == 0L || anyNA(header) || any(!nzchar(header))) {
    stop("The CSV must have a non-empty header: ", path, call. = FALSE)
  }
  if (anyDuplicated(header)) {
    stop("The CSV has duplicate column names: ", path, call. = FALSE)
  }
  if (any(!validUTF8(header))) {
    stop("The CSV header is not valid UTF-8; convert the file first: ", path,
         call. = FALSE)
  }
  header
}

.naaa_read_csv_columns <- function(path, columns) {
  data <- .naaa_fread(path, select = columns)
  quoted <- any(vapply(data, function(column) {
    any(grepl('"', column, fixed = TRUE, useBytes = TRUE), na.rm = TRUE)
  }, logical(1L)))
  if (quoted) {
    # fread currently retains doubled quotes in quoted fields. Reparse these
    # cases instead of globally replacing quote pairs (which corrupts literals).
    header <- .naaa_csv_header(path)
    types <- paste0(ifelse(header %in% columns, "c", "_"), collapse = "")
    parsed <- .naaa_readr_csv(path, col_types = types)
    if (!setequal(names(parsed), columns)) {
      stop("Selected CSV columns could not be read completely: ", path,
           call. = FALSE)
    }
    if (nrow(parsed) != nrow(data)) {
      stop("CSV readers disagree on the number of records: ", path,
           call. = FALSE)
    }
    data <- data.table::as.data.table(parsed)
    data.table::setcolorder(data, columns)
  }
  if (!identical(names(data), columns)) {
    stop("The CSV columns changed or could not be read completely: ", path,
         call. = FALSE)
  }
  invalid <- vapply(data, function(column) {
    any(!validUTF8(column[!is.na(column)]))
  }, logical(1L))
  if (any(invalid)) {
    stop("Selected CSV values are not valid UTF-8; convert the file first: ", path,
         call. = FALSE)
  }
  data
}

.naaa_readr_csv <- function(path, col_types, n_max = Inf) {
  .naaa_assert_utf8_file(path)
  result <- .naaa_parse_csv(
    readr::read_csv(
      path, col_types = col_types, locale = readr::locale(encoding = "UTF-8"),
      na = character(), trim_ws = FALSE, name_repair = "minimal",
      progress = FALSE, show_col_types = FALSE, skip_empty_rows = FALSE,
      lazy = FALSE, num_threads = 1L, n_max = n_max
    ), path
  )
  if (nrow(readr::problems(result))) {
    stop("The CSV contains parsing problems: ", path, call. = FALSE)
  }
  result
}

.naaa_parse_csv <- function(expression, path) {
  messages <- character()
  result <- tryCatch(withCallingHandlers(
    force(expression),
    warning = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  ), error = function(condition) condition)
  # Let native parsers complete their cleanup before rejecting partial results.
  if (inherits(result, "error")) {
    stop("Could not read UTF-8 CSV '", path, "': ", conditionMessage(result),
         call. = FALSE)
  }
  if (length(messages)) {
    stop("Could not read UTF-8 CSV '", path, "': ",
         paste(unique(messages), collapse = "; "), call. = FALSE)
  }
  result
}

.naaa_assert_utf8_file <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  repeat {
    lines <- withCallingHandlers(
      readLines(connection, n = 10000L, warn = FALSE, encoding = "UTF-8"),
      warning = function(condition) {
        stop("The CSV contains invalid text: ", path, call. = FALSE)
      }
    )
    if (!length(lines)) break
    if (any(!validUTF8(lines))) {
      stop("The CSV is not valid UTF-8; convert it first: ", path, call. = FALSE)
    }
  }
  invisible(NULL)
}
