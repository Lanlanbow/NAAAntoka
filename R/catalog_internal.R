# The analyst explicitly selects a metadata folder or workbook for this session.
# Keep one validated catalog in memory, keyed by its absolute path and content.
.naaa_catalog_cache <- new.env(parent = emptyenv())

.naaa_metadata_not_found <- function(path = NULL) {
  message <- paste0(
    "\u30e1\u30bf\u30c7\u30fc\u30bf\u304c\u898b\u3064\u304b\u308a\u307e\u305b\u3093\u3002 ",
    "set_naaa_metadata(\"meta\")\u3067\u30e1\u30bf\u30c7\u30fc\u30bf\u306exlsx\u30d5\u30a1\u30a4\u30eb\u304c\u5165\u3063\u305f\u30d5\u30a9\u30eb\u30c0\u3001",
    "\u307e\u305f\u306fxlsx\u30d5\u30a1\u30a4\u30eb\u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002"
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
    stop("The metadata workbook must be a readable file: ", path, call. = FALSE)
  }
  if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    stop("\u30e1\u30bf\u30c7\u30fc\u30bf\u306f.xlsx\u30d5\u30a1\u30a4\u30eb\u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044: ", path, call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.naaa_find_metadata_file <- function(directory) {
  if (file.access(directory, mode = 4L) != 0L ||
      file.access(directory, mode = 1L) != 0L) {
    stop("The metadata folder must be readable: ", directory, call. = FALSE)
  }
  paths <- list.files(directory, pattern = "\\.xlsx$", full.names = TRUE,
                       ignore.case = TRUE, all.files = TRUE)
  paths <- paths[!startsWith(basename(paths), "~$")]
  info <- file.info(paths)
  paths <- paths[!is.na(info$isdir) & !info$isdir]
  if (!length(paths)) .naaa_metadata_not_found(directory)
  if (length(paths) > 1L) {
    stop("\u30d5\u30a9\u30eb\u30c0\u5185\u306bxlsx\u30d5\u30a1\u30a4\u30eb\u304c\u8907\u6570\u3042\u308a\u307e\u3059\u3002",
         "set_naaa_metadata(\"meta/\u30d5\u30a1\u30a4\u30eb\u540d.xlsx\")\u30671\u3064\u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002",
         call. = FALSE)
  }
  .naaa_readable_catalog_path(paths[[1L]])
}

.naaa_catalog_path <- function() {
  selected <- getOption("NAAAntoka.metadata_dir")
  if (!is.character(selected) || length(selected) != 1L ||
      is.na(selected) || !nzchar(selected)) {
    .naaa_metadata_not_found()
  }
  if (dir.exists(selected)) return(.naaa_find_metadata_file(selected))
  if (!.naaa_catalog_path_present(selected)) .naaa_metadata_not_found(selected)
  .naaa_readable_catalog_path(selected)
}

.naaa_catalog_file_hash <- function(path) {
  hash <- unname(tools::md5sum(path))
  if (length(hash) != 1L || is.na(hash) ||
      !grepl("^[a-f0-9]{32}$", hash)) {
    stop("Could not read the metadata workbook for versioning.",
         call. = FALSE)
  }
  hash
}

.naaa_catalog <- function() {
  .naaa_catalog_at(.naaa_catalog_path())
}

.naaa_catalog_at <- function(path) {
  path <- .naaa_readable_catalog_path(path)
  before <- .naaa_catalog_file_hash(path)
  cached <- .naaa_catalog_cache$entry
  if (!is.null(cached) && identical(cached$path, path) &&
      identical(cached$md5, before)) {
    return(cached$catalog)
  }
  catalog <- .naaa_read_long_catalog(path)
  after <- .naaa_catalog_file_hash(path)
  if (!identical(before, after)) {
    stop("The metadata workbook changed while it was being read; retry ",
         "after saving the file.", call. = FALSE)
  }
  attr(catalog, "naaa_catalog_version") <- paste0("xlsx-", before)
  attr(catalog, "naaa_catalog_source") <- list(
    file = basename(path), md5 = before
  )
  # Publish only a completely read and validated catalog. Errors above never
  # fall back to an old entry, and switching files retains only the latest one.
  .naaa_catalog_cache$entry <- list(path = path, md5 = before, catalog = catalog)
  catalog
}

.naaa_catalog_version <- function(catalog) {
  version <- attr(catalog, "naaa_catalog_version", exact = TRUE)
  # In-memory fixture catalogs can supply their version through the existing
  # manifest binding. Runtime workbook catalogs always carry their own version.
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
      !identical(version, paste0("xlsx-", source$md5))) {
    stop("The catalog has no valid workbook source information.", call. = FALSE)
  }
  data.frame(
    "Schema-Version" = "3",
    "Catalog-Version" = version,
    "Catalog-Status" = "provisional",
    "Source-File" = source$file,
    "Source-Format" = "long-xlsx",
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
