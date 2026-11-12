# Source records identify rows within an unchanged UTF-8 CSV, not respondents
# across years. Keep one fingerprint per dataset rather than one per record.
.naaa_validate_sources_flag <- function(sources) {
  if (!is.logical(sources) || length(sources) != 1L ||
      !is.null(dim(sources)) || is.na(sources)) {
    stop("'sources'\u306b\u306fTRUE\u307e\u305f\u306fFALSE\u30921\u3064\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002", call. = FALSE)
  }
  invisible(sources)
}

.naaa_source_fingerprint <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !file.exists(path) || dir.exists(path) || file.access(path, 4L) != 0L) {
    stop("\u5143CSV\u3092\u8aad\u307f\u53d6\u308c\u307e\u305b\u3093: ", path, call. = FALSE)
  }
  canonical <- normalizePath(path, winslash = "/", mustWork = TRUE)
  digest <- tryCatch(
    withCallingHandlers(
      unname(tools::md5sum(canonical)),
      warning = function(e) stop(conditionMessage(e), call. = FALSE)
    ),
    error = function(e) {
      stop("\u5143CSV\u306e\u5185\u5bb9\u3092\u78ba\u8a8d\u3067\u304d\u307e\u305b\u3093: ", canonical, "\n",
           conditionMessage(e), call. = FALSE)
    }
  )
  if (length(digest) != 1L || is.na(digest) ||
      !grepl("^[[:xdigit:]]{32}$", digest)) {
    stop("\u5143CSV\u306e\u5185\u5bb9\u3092\u78ba\u8a8d\u3067\u304d\u307e\u305b\u3093: ", canonical, call. = FALSE)
  }
  list(path = canonical, md5 = tolower(digest))
}

.naaa_read_csv_with_source <- function(path, columns, dataset, sources = FALSE) {
  .naaa_validate_sources_flag(sources)
  if (!sources) {
    return(list(data = .naaa_read_csv_columns(path, columns), source = NULL))
  }
  before <- .naaa_source_fingerprint(path)
  data <- .naaa_read_csv_columns(path, columns)
  after <- .naaa_source_fingerprint(path)
  if (!identical(before$md5, after$md5)) {
    stop("\u8aad\u307f\u8fbc\u307f\u4e2d\u306b\u5143CSV\u306e\u5185\u5bb9\u304c\u5909\u308f\u308a\u307e\u3057\u305f\u3002\u30d5\u30a1\u30a4\u30eb\u3092\u5909\u66f4\u305b\u305a\u3001\u3082\u3046\u4e00\u5ea6\u8aad\u307f\u8fbc\u3093\u3067\u304f\u3060\u3055\u3044: ",
         before$path, call. = FALSE)
  }
  source <- data.frame(
    dataset_id = dataset$dataset_id[[1L]],
    category_code = dataset$category_code[[1L]],
    year_era = dataset$year_era[[1L]],
    year_ad = as.integer(dataset$year_ad[[1L]]),
    path = after$path,
    md5 = after$md5,
    n_records = nrow(data),
    stringsAsFactors = FALSE
  )
  list(data = data, source = source)
}
