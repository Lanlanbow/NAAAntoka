# Shared helpers for editable xlsx mappings. Mapping identities always come
# from current metadata, rather than code labels edited in the workbook.
.naaa_mapping_years <- function(years) {
  if (is.null(years) || !length(years) || !is.null(dim(years))) {
    stop("The mapping year filter must contain explicit years or a range.",
         call. = FALSE)
  }
  if (is.character(years) && any(grepl(":", years, fixed = TRUE))) {
    if (length(years) != 1L || is.na(years) ||
        !grepl("^[^:]+:[^:]+$", years)) {
      stop("A mapping year range must be one 'start:end' string.", call. = FALSE)
    }
    endpoints <- strsplit(years, ":", fixed = TRUE)[[1L]]
    if (length(endpoints) != 2L || any(!nzchar(trimws(endpoints)))) {
      stop("A mapping year range must have two endpoints: 'H24:R6'.",
           call. = FALSE)
    }
    return(.naaa_requested_years(year_range = endpoints))
  }
  .naaa_requested_years(years = years)
}

.naaa_check_mapping_output <- function(output, source, overwrite) {
  if (.naaa_same_path(output, source)) {
    stop("The mapping output must not replace the metadata workbook.", call. = FALSE)
  }
  if (dir.exists(output)) {
    stop("The mapping output is an existing directory: ", output, call. = FALSE)
  }
  if (.naaa_path_exists(output) && !overwrite) {
    stop("Output already exists; use overwrite = TRUE to replace it: ", output,
         call. = FALSE)
  }
  invisible(NULL)
}

.naaa_write_mapping_workbook <- function(table, output, source, overwrite) {
  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "mapping")
  openxlsx::writeData(workbook, "mapping", table, keepNA = FALSE,
                       withFilter = nrow(table) > 0L)
  openxlsx::addStyle(
    workbook, "mapping", openxlsx::createStyle(numFmt = "@"),
    rows = seq_len(nrow(table) + 1L), cols = seq_len(ncol(table)),
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::freezePane(workbook, "mapping", firstRow = TRUE)
  temporary <- tempfile(".naaa-mapping-", tmpdir = dirname(output),
                        fileext = ".xlsx")
  on.exit(unlink(temporary), add = TRUE)
  openxlsx::saveWorkbook(workbook, temporary, overwrite = FALSE)
  if (.Platform$OS.type != "windows") Sys.chmod(temporary, mode = "0600")
  .naaa_check_mapping_output(output, source, overwrite)
  # POSIX rename replaces an existing pathname without writing through a link.
  # Windows needs a backup because rename cannot replace an existing file there.
  backup <- NULL
  if (.Platform$OS.type == "windows" && .naaa_path_exists(output)) {
    backup <- tempfile(".naaa-mapping-backup-", tmpdir = dirname(output),
                       fileext = ".xlsx")
    if (!isTRUE(suppressWarnings(file.rename(output, backup)))) {
      stop("Could not preserve the existing mapping output; it was not changed.",
           call. = FALSE)
    }
  }
  published <- suppressWarnings(file.rename(temporary, output))
  if (!isTRUE(published)) {
    if (!is.null(backup) &&
        !isTRUE(suppressWarnings(file.rename(backup, output)))) {
      stop("Could not publish the mapping. The previous file is preserved at: ",
           backup, call. = FALSE)
    }
    stop("Could not publish the completed mapping; the previous output was ",
         "not changed: ", output, call. = FALSE)
  }
  if (!is.null(backup)) unlink(backup)
  invisible(output)
}

.naaa_read_mapping_workbook <- function(mapping) {
  .naaa_check_path_argument(mapping, "mapping")
  if (!grepl("\\.xlsx$", mapping, ignore.case = TRUE)) {
    stop("'mapping' must refer to an .xlsx file.", call. = FALSE)
  }
  mapping <- path.expand(mapping)
  if (!file.exists(mapping) || dir.exists(mapping) || file.access(mapping, 4L) != 0L) {
    stop("The mapping workbook is not a readable file: ", mapping, call. = FALSE)
  }
  tryCatch({
    sheets <- openxlsx::getSheetNames(mapping)
    sheet <- if ("mapping" %in% sheets) "mapping" else if (length(sheets) == 1L) {
      sheets[[1L]]
    } else {
      stop("Use a sheet named 'mapping' when the workbook has multiple sheets.")
    }
    # Read the header as ordinary cell text: openxlsx otherwise trims header
    # whitespace and replaces embedded spaces even with check.names = FALSE.
    table <- openxlsx::read.xlsx(
      mapping, sheet = sheet, colNames = FALSE, check.names = FALSE,
      detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE,
      na.strings = character()
    )
    if (!is.data.frame(table) || !nrow(table)) {
      stop("The mapping sheet must contain a header row.")
    }
    header <- vapply(table, function(column) as.character(column[[1L]]),
                     character(1L), USE.NAMES = FALSE)
    table <- table[-1L, , drop = FALSE]
    names(table) <- header
    for (column in seq_along(table)) table[[column]] <- as.character(table[[column]])
    rownames(table) <- NULL
    table
  }, error = function(e) {
    stop("Could not read the mapping workbook: ", conditionMessage(e),
         call. = FALSE)
  })
}

.naaa_selection_from_mapping <- function(catalog, mapping) {
  .naaa_validate_catalog(catalog)
  if (is.character(mapping)) mapping <- .naaa_read_mapping_workbook(mapping)
  required <- c("\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea", "\u5e74\u5ea6", "\u5909\u6570",
                "\u5206\u6790\u7528\u5909\u6570\u540d")
  if (!is.data.frame(mapping) || !nrow(mapping) || anyNA(names(mapping)) ||
      anyDuplicated(names(mapping)) || !all(required %in% names(mapping))) {
    stop("'mapping' must have at least one row and unique columns including: ",
         paste(required, collapse = ", "), ".", call. = FALSE)
  }
  for (column in required) {
    value <- mapping[[column]]
    if (!is.atomic(value) || !is.null(dim(value))) {
      stop("Mapping column '", column, "' must contain text values.", call. = FALSE)
    }
    value <- as.character(value)
    invalid <- which(is.na(value) | !nzchar(trimws(value, whitespace = "[\\h\\v]")))
    if (length(invalid)) {
      stop("Mapping column '", column, "' is blank at data row(s): ",
           paste(invalid, collapse = ", "), ".", call. = FALSE)
    }
    mapping[[column]] <- value
  }
  if (length(unique(mapping[[required[[1L]]]])) != 1L) {
    stop("A mapping must contain exactly one survey category. ",
         "Keep pupil and parent mappings separate.", call. = FALSE)
  }
  if (!all(required[1:3] %in% names(catalog))) {
    stop("The current catalog must retain its Japanese metadata columns.",
         call. = FALSE)
  }
  years <- .naaa_parse_years(mapping[[required[[2L]]]], "mapping year")
  catalog_years <- .naaa_parse_years(catalog[["year_ad"]], "metadata year_ad")
  index <- integer(nrow(mapping))
  for (i in seq_len(nrow(mapping))) {
    candidates <- which(
      catalog[[required[[1L]]]] == mapping[[required[[1L]]]][[i]] &
        catalog_years == years[[i]] &
        catalog[[required[[3L]]]] == mapping[[required[[3L]]]][[i]]
    )
    if (length(candidates) != 1L) {
      stop("No unique matching metadata variable for mapping data row ", i,
           " (Excel row ", i + 1L, "): ",
           paste(mapping[[required[[1L]]]][[i]], mapping[[required[[2L]]]][[i]],
                 mapping[[required[[3L]]]][[i]], sep = " / "), ".",
           call. = FALSE)
    }
    index[[i]] <- candidates[[1L]]
  }
  resolved <- .naaa_selection_mapping(catalog, index)
  resolved[["output_name"]] <- mapping[[required[[4L]]]]
  duplicate <- duplicated(resolved[c("dataset_id", "output_name")]) |
    duplicated(resolved[c("dataset_id", "output_name")], fromLast = TRUE)
  if (any(duplicate)) {
    stop("Each analysis name may select at most one variable per dataset. ",
         "Conflicting mapping data rows: ",
         paste(which(duplicate), collapse = ", "), " (Excel rows: ",
         paste(which(duplicate) + 1L, collapse = ", "), "). ",
         "Delete unwanted rows or use distinct analysis names.", call. = FALSE)
  }
  reserved <- c("dataset_id", "category_code", "year_era", "year_ad")
  conflicts <- intersect(unique(resolved[["output_name"]]), reserved)
  if (length(conflicts)) {
    stop("Analysis names conflict with reserved metadata columns: ",
         paste(conflicts, collapse = ", "), call. = FALSE)
  }
  list(
    mapping = resolved,
    output_names = unique(resolved[["output_name"]]),
    category_code = resolved[["category_code"]][[1L]],
    mode = "mapping",
    default_years = sort(unique(resolved[["year_ad"]])),
    concept_names = character(),
    raw_names = unique(resolved[["output_name"]])
  )
}

.naaa_mapping_choice_comparison <- function(catalog, resolved_mapping) {
  reports <- list()
  for (name in unique(resolved_mapping[["output_name"]])) {
    rows <- which(resolved_mapping[["output_name"]] == name)
    rows <- rows[order(resolved_mapping[["year_ad"]][rows])]
    current <- resolved_mapping[rows, , drop = FALSE]
    index <- match(current[["variable_id"]], catalog[["variable_id"]])
    if (anyNA(index)) {
      stop("The choice comparison contains an unknown metadata variable.",
           call. = FALSE)
    }
    choices <- .naaa_catalog_choices(catalog, index)
    report <- data.frame(
      analysis_name = rep(name, nrow(current)),
      dataset_id = current[["dataset_id"]],
      year_era = current[["year_era"]],
      variable_id = current[["variable_id"]],
      reference_variable_id = rep(current[["variable_id"]][[1L]], nrow(current)),
      choice_status = .naaa_choice_status(choices),
      stringsAsFactors = FALSE
    )
    report[["choices"]] <- choices
    reports[[length(reports) + 1L]] <- report
  }
  if (!length(reports)) {
    result <- data.frame(
      analysis_name = character(), dataset_id = character(), year_era = character(),
      variable_id = character(), reference_variable_id = character(),
      choice_status = .naaa_choice_status(list()), stringsAsFactors = FALSE
    )
    result[["choices"]] <- list()
    return(result)
  }
  result <- do.call(rbind, reports)
  rownames(result) <- NULL
  result
}
