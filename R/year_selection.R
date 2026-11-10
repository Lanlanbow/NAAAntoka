# Convert explicit year labels to Gregorian integer years. These helpers never
# decide which survey years are available; that comes from the selected metadata.
.naaa_parse_years <- function(x, argument = "years") {
  invalid <- function() {
    stop("'", argument, "' must contain positive whole Gregorian years or ",
         "H/R era years, without missing values.", call. = FALSE)
  }
  if (length(x) == 0L || !is.null(dim(x))) invalid()

  maximum <- .Machine$integer.max
  if ((is.integer(x) || is.double(x)) && !is.object(x)) {
    if (anyNA(x) || any(!is.finite(x)) || any(x < 1) ||
        any(x != floor(x)) || any(x > maximum)) {
      invalid()
    }
    return(as.integer(x))
  }
  if (!is.character(x) || anyNA(x)) invalid()

  labels <- trimws(x, whitespace = "[\\h\\v]")
  gregorian <- grepl("^[0-9]+$", labels)
  heisei <- grepl("^[Hh][0-9]+$", labels)
  reiwa <- grepl("^[Rr][0-9]+$", labels)
  if (any(!(gregorian | heisei | reiwa))) invalid()

  numbers <- rep(NA_real_, length(labels))
  numbers[gregorian] <- suppressWarnings(as.numeric(labels[gregorian]))
  era <- heisei | reiwa
  numbers[era] <- suppressWarnings(as.numeric(substring(labels[era], 2L)))
  if (anyNA(numbers) || any(!is.finite(numbers)) || any(numbers < 1) ||
      any(numbers != floor(numbers))) {
    invalid()
  }
  if (any(numbers[heisei] > 31)) {
    stop("'", argument, "' uses a Heisei year outside H1 to H31.",
         call. = FALSE)
  }

  # Offsets are historical era definitions, not limits on available data.
  numbers[heisei] <- numbers[heisei] + 1988
  numbers[reiwa] <- numbers[reiwa] + 2018
  if (any(numbers > maximum)) {
    stop("'", argument, "' exceeds the supported integer year range.",
         call. = FALSE)
  }
  as.integer(numbers)
}

.naaa_requested_years <- function(years = NULL, year_range = NULL) {
  if (!is.null(years) && !is.null(year_range)) {
    stop("Specify only one of 'years' and 'year_range'.", call. = FALSE)
  }
  if (is.null(years) && is.null(year_range)) return(NULL)
  if (!is.null(years)) {
    return(sort(unique(.naaa_parse_years(years, argument = "years"))))
  }
  if (length(year_range) != 2L) {
    stop("'year_range' must contain exactly two endpoints.", call. = FALSE)
  }
  endpoints <- .naaa_parse_years(year_range, argument = "year_range")
  if (endpoints[[1L]] > endpoints[[2L]]) {
    stop("'year_range' endpoints must be in ascending order.", call. = FALSE)
  }
  width <- as.double(endpoints[[2L]]) - as.double(endpoints[[1L]]) + 1
  if (width > 10000) {
    stop("'year_range' must span no more than 10000 years; use explicit ",
         "'years' for widely separated individual years.", call. = FALSE)
  }
  seq.int(endpoints[[1L]], endpoints[[2L]])
}
