# The analyst explicitly selects one metadata folder for the current R session.
# Catalogs and content versions are generated in memory on each metadata read.
.naaa_metadata_not_found <- function(path = NULL) {
  message <- paste0(
    "\u30e1\u30bf\u30c7\u30fc\u30bf\u304c\u898b\u3064\u304b\u308a\u307e\u305b\u3093\u3002 ",
    "Use set_naaa_metadata(\"meta\") to select the folder containing ",
    "metadata_long.csv."
  )
  if (!is.null(path)) message <- paste0(message, "\n", path)
  stop(message, call. = FALSE)
}

.naaa_catalog_path_present <- function(path) {
  if (file.exists(path)) return(TRUE)
  # Recognize broken links as supplied paths so they produce a read error.
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}

.naaa_readable_catalog_path <- function(path) {
  if (isTRUE(file.info(path)$isdir) || file.access(path, mode = 4L) != 0L) {
    stop("The metadata CSV must be a readable file: ", path, call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.naaa_catalog_path <- function() {
  directory <- getOption("NAAAntoka.metadata_dir")
  if (!is.character(directory) || length(directory) != 1L ||
      is.na(directory) || !nzchar(directory)) {
    .naaa_metadata_not_found()
  }
  path <- file.path(directory, "metadata_long.csv")
  if (!.naaa_catalog_path_present(path)) .naaa_metadata_not_found(path)
  .naaa_readable_catalog_path(path)
}

.naaa_catalog_file_hash <- function(path) {
  hash <- unname(tools::md5sum(path))
  if (length(hash) != 1L || is.na(hash) ||
      !grepl("^[a-f0-9]{32}$", hash)) {
    stop("Could not read the metadata CSV for versioning.",
         call. = FALSE)
  }
  hash
}

.naaa_catalog <- function() {
  .naaa_catalog_at(.naaa_catalog_path())
}

.naaa_catalog_at <- function(path) {
  before <- .naaa_catalog_file_hash(path)
  catalog <- .naaa_read_long_catalog(path)
  after <- .naaa_catalog_file_hash(path)
  if (!identical(before, after)) {
    stop("The metadata CSV changed while it was being read; retry ",
         "after saving the file.", call. = FALSE)
  }
  attr(catalog, "naaa_catalog_version") <- paste0("csv-", before)
  attr(catalog, "naaa_catalog_source") <- list(
    file = basename(path), md5 = before
  )
  catalog
}

.naaa_catalog_version <- function(catalog) {
  version <- attr(catalog, "naaa_catalog_version", exact = TRUE)
  # In-memory fixture catalogs can supply their version through the existing
  # manifest binding. Runtime CSV catalogs always carry their own version.
  if (is.null(version)) {
    version <- .naaa_catalog_manifest()[["Catalog-Version"]]
  }
  if (!is.character(version) || length(version) != 1L || is.na(version) ||
      !nzchar(version)) {
    stop("The catalog has no valid metadata version.", call. = FALSE)
  }
  version
}

.naaa_catalog_manifest <- function(catalog = NULL) {
  if (is.null(catalog)) catalog <- .naaa_catalog()
  source <- attr(catalog, "naaa_catalog_source", exact = TRUE)
  version <- attr(catalog, "naaa_catalog_version", exact = TRUE)
  if (!is.list(source) || !is.character(source$file) ||
      length(source$file) != 1L || is.na(source$file) ||
      !nzchar(source$file) || !is.character(source$md5) ||
      length(source$md5) != 1L || is.na(source$md5) ||
      !grepl("^[a-f0-9]{32}$", source$md5) ||
      !identical(version, paste0("csv-", source$md5))) {
    stop("The catalog has no valid CSV source information.", call. = FALSE)
  }
  data.frame(
    "Schema-Version" = "3",
    "Catalog-Version" = version,
    "Catalog-Status" = "provisional",
    "Source-File" = source$file,
    "Source-Format" = "long-csv",
    "Source-MD5" = source$md5,
    "Rows" = as.character(nrow(catalog)),
    "Columns" = as.character(ncol(catalog)),
    "Datasets" = as.character(length(unique(catalog[["dataset_id"]]))),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

.naaa_validate_catalog <- function(catalog) {
  required <- c(
    "dataset_id", "variable_name", "year_era", "year_ad", "category_code",
    "variable_id", "label_raw", "label_effective"
  )
  if (!is.data.frame(catalog) || anyNA(names(catalog)) ||
      any(!nzchar(names(catalog))) || anyDuplicated(names(catalog)) ||
      !all(required %in% names(catalog)) ||
      !all(vapply(catalog, is.character, logical(1L)))) {
    stop("The metadata catalog must contain the required character columns.",
         call. = FALSE)
  }
  for (name in c("dataset_id", "variable_id", "variable_name")) {
    values <- catalog[[name]]
    if (anyNA(values) || any(!nzchar(trimws(values)))) {
      stop("The metadata catalog contains a missing identifier: ", name,
           call. = FALSE)
    }
  }
  if (anyDuplicated(catalog[["variable_id"]]) ||
      anyDuplicated(catalog[c("dataset_id", "variable_name")])) {
    stop("The metadata catalog contains duplicate variable identifiers.",
         call. = FALSE)
  }
  choices <- attr(catalog, "naaa_choice_columns", exact = TRUE)
  if (!is.null(choices)) {
    reserved <- c(required, "category", "position_raw", "measurement_level_raw",
                  .naaa_long_metadata_columns())
    if (!is.character(choices) || anyNA(choices) || any(!nzchar(choices)) ||
        anyDuplicated(choices) || !all(choices %in% names(catalog)) ||
        any(choices %in% reserved) || any(startsWith(choices, "wrk_"))) {
      stop("The catalog choice-column declaration is invalid.", call. = FALSE)
    }
  }
  invisible(catalog)
}
