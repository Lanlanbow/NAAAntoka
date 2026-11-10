# The long CSV is a metadata dictionary, not respondent data. Keep every code
# name and label literal; in particular, "01" and "1" are different codes.
.naaa_survey_categories <- function() {
  c(
    "\u5150\u7ae5" = "pupil", "\u751f\u5f92" = "student", "\u5c0f\u5b66\u6821" = "primary_school",
    "\u4e2d\u5b66\u6821" = "junior_secondary_school", "\u5150\u7ae5\u4fdd\u8b77\u8005" = "pupil_parent",
    "\u751f\u5f92\u4fdd\u8b77\u8005" = "student_parent"
  )
}

.naaa_long_metadata_columns <- function() {
  c("\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea", "\u5e74\u5ea6", "\u5909\u6570", "\u5e74\u5ea6\u5909\u6570", "\u4f4d\u7f6e", "\u6e2c\u5b9a\u306e\u5c3a\u5ea6",
    "\u30e9\u30d9\u30eb_raw", "\u30e9\u30d9\u30eb_norm", "\u30e9\u30d9\u30eb_uni", "concept_id", "concept",
    "\u5927\u5206\u985e", "\u7814\u7a76\u63a2\u7d22\u7528\u5206\u985e")
}

.naaa_normalize_long_catalog <- function(catalog) {
  metadata_columns <- .naaa_long_metadata_columns()
  if (!is.data.frame(catalog) || anyNA(names(catalog)) ||
      any(!nzchar(names(catalog))) || anyDuplicated(names(catalog)) ||
      !all(metadata_columns %in% names(catalog)) ||
      !all(vapply(catalog, is.character, logical(1L)))) {
    stop("The long catalog must contain unique, named character columns, ",
         "including all 13 metadata columns.", call. = FALSE)
  }
  if (!nrow(catalog)) {
    stop("The long catalog must contain at least one variable.", call. = FALSE)
  }
  invalid_text <- vapply(catalog, function(x) {
    any(!validUTF8(x[!is.na(x)]))
  }, logical(1L))
  if (any(invalid_text) || any(!validUTF8(names(catalog)))) {
    stop("The long catalog must contain valid UTF-8 text.", call. = FALSE)
  }
  for (column in c("\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea", "\u5e74\u5ea6", "\u5909\u6570", "\u5e74\u5ea6\u5909\u6570")) {
    values <- catalog[[column]]
    if (anyNA(values) || any(!nzchar(trimws(values, whitespace = "[\\h\\v]")))) {
      stop("The long catalog contains a missing identifier: ", column,
           call. = FALSE)
    }
  }
  categories <- .naaa_survey_categories()
  category_code <- unname(categories[catalog[["\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea"]]])
  if (anyNA(category_code)) {
    stop("The long catalog contains an unknown survey category.", call. = FALSE)
  }
  source_year <- catalog[["\u5e74\u5ea6"]]
  year_era <- toupper(trimws(source_year, whitespace = "[\\h\\v]"))
  if (any(!grepl("^[HR][0-9]+$", year_era))) {
    stop("The long catalog '\u5e74\u5ea6' must use H/R era years.", call. = FALSE)
  }
  year_ad <- .naaa_parse_years(year_era, argument = "\u5e74\u5ea6")
  year_era <- paste0(substr(year_era, 1L, 1L),
                     as.numeric(substring(year_era, 2L)))
  if (any(catalog[["\u5e74\u5ea6\u5909\u6570"]] !=
          paste(source_year, catalog[["\u5909\u6570"]], sep = "_"))) {
    stop("The long catalog '\u5e74\u5ea6\u5909\u6570' must equal \u5e74\u5ea6 + '_' + \u5909\u6570.",
         call. = FALSE)
  }
  canonical <- data.frame(
    dataset_id = paste(category_code, year_ad, sep = "_"),
    variable_id = paste(category_code, year_era, catalog[["\u5909\u6570"]], sep = "|"),
    category_code = category_code,
    category = catalog[["\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea"]],
    year_ad = as.character(year_ad),
    year_era = year_era,
    variable_name = catalog[["\u5909\u6570"]],
    position_raw = catalog[["\u4f4d\u7f6e"]],
    label_raw = catalog[["\u30e9\u30d9\u30eb_raw"]],
    label_effective = catalog[["\u30e9\u30d9\u30eb_norm"]],
    measurement_level_raw = catalog[["\u6e2c\u5b9a\u306e\u5c3a\u5ea6"]],
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (any(names(canonical) %in% names(catalog))) {
    stop("The long catalog contains reserved internal column names.",
         call. = FALSE)
  }
  code_columns <- names(catalog)[
    !names(catalog) %in% metadata_columns &
      !startsWith(names(catalog), "wrk_")
  ]
  result <- cbind(canonical, catalog, stringsAsFactors = FALSE)
  rownames(result) <- NULL
  attr(result, "naaa_choice_columns") <- code_columns
  .naaa_validate_catalog(result)
  result
}

.naaa_read_long_catalog <- function(path) {
  .naaa_assert_utf8_file(path)
  header <- .naaa_csv_header(path)
  catalog <- as.data.frame(.naaa_read_csv_columns(path, header),
                           stringsAsFactors = FALSE)
  .naaa_normalize_long_catalog(catalog)
}
