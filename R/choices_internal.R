# Choice dictionaries come from literal value-code columns of long metadata.
# Legacy catalogs do not have these columns and therefore have no dictionary.
.naaa_catalog_choices <- function(catalog, index = seq_len(nrow(catalog))) {
  columns <- attr(catalog, "naaa_choice_columns", exact = TRUE)
  if (is.null(columns)) columns <- character()
  if (!is.character(columns) || anyNA(columns) || anyDuplicated(columns) ||
      any(!nzchar(columns)) || !all(columns %in% names(catalog))) {
    stop("The catalog contains invalid choice-column metadata.", call. = FALSE)
  }
  empty <- data.frame(
    code = character(), label = character(), stringsAsFactors = FALSE
  )
  if (!length(columns)) return(rep(list(empty), length(index)))

  lapply(index, function(row) {
    labels <- vapply(columns, function(column) catalog[[column]][[row]],
                     character(1L), USE.NAMES = FALSE)
    defined <- !is.na(labels) & nzchar(labels)
    data.frame(
      code = columns[defined], label = labels[defined],
      stringsAsFactors = FALSE, row.names = NULL
    )
  })
}

.naaa_choice_status <- function(choices) {
  levels <- c("\u5171\u901a", "\u7570\u306a\u308b", "\u60c5\u5831\u4e0d\u8db3")
  status <- rep(levels[[3L]], length(choices))
  if (length(choices) && nrow(choices[[1L]]) > 0L) {
    reference <- choices[[1L]]
    for (i in seq_along(choices)) {
      current <- choices[[i]]
      if (!nrow(current)) next
      index <- match(reference[["code"]], current[["code"]])
      common <- nrow(reference) == nrow(current) && !anyNA(index) &&
        identical(reference[["label"]], current[["label"]][index])
      status[[i]] <- levels[[if (common) 1L else 2L]]
    }
  }
  factor(status, levels = levels)
}
