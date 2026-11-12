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

.naaa_validate_mapping_name_col <- function(name_col) {
  if (!is.null(name_col) &&
      (!is.character(name_col) || length(name_col) != 1L ||
       !is.null(dim(name_col)) || is.na(name_col) ||
       !nzchar(trimws(name_col, whitespace = "[\\h\\v]")))) {
    stop("'name_col'\u306b\u306f\u5bfe\u5fdc\u8868\u306e\u5217\u540d\u30921\u3064\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002", call. = FALSE)
  }
  invisible(name_col)
}

.naaa_mapping_display <- function(values) {
  paste(unique(encodeString(as.character(values), quote = '"')), collapse = ", ")
}

.naaa_mapping_locations <- function(mapping, rows, excel_rows, name_col = NULL) {
  columns <- intersect(unique(c(
    "\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea", "\u5e74\u5ea6", "\u5909\u6570", "concept_id", "concept", name_col
  )), names(mapping))
  details <- vapply(utils::head(rows, 10L), function(row) {
    fields <- vapply(columns, function(column) {
      paste0(column, "=", .naaa_mapping_display(mapping[[column]][row]))
    }, character(1L))
    paste0("    \u5bfe\u5fdc\u8868\u30c7\u30fc\u30bf\u884c ", row,
           if (excel_rows) paste0("\uff08Excel\u884c ", row + 1L, "\uff09") else "",
           ": ", paste(fields, collapse = "\u3001"))
  }, character(1L))
  if (length(rows) > 10L) details <- c(details, paste0("    \u307b\u304b", length(rows) - 10L, "\u884c"))
  paste(details, collapse = "\n")
}

.naaa_stop_mapping_problems <- function(problems) {
  shown <- utils::head(problems, 30L)
  if (length(problems) > 30L) {
    shown <- c(shown, paste0("\u307b\u304b", length(problems) - 30L, "\u4ef6\u306e\u554f\u984c\u304c\u3042\u308a\u307e\u3059\u3002"))
  }
  stop(paste0("\u5bfe\u5fdc\u8868\u306e\u5bfe\u5fdc\u95a2\u4fc2\u3092\u78ba\u8a8d\u3057\u3066\u304f\u3060\u3055\u3044\u3002\n", paste(shown, collapse = "\n")),
       call. = FALSE)
}

.naaa_selection_from_mapping <- function(catalog, mapping, name_col = NULL) {
  .naaa_validate_catalog(catalog)
  .naaa_validate_mapping_name_col(name_col)
  excel_rows <- is.character(mapping)
  if (is.character(mapping)) mapping <- .naaa_read_mapping_workbook(mapping)
  identity_columns <- c("\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea", "\u5e74\u5ea6", "\u5909\u6570")
  use_concepts <- is.null(name_col)
  required <- unique(c(identity_columns, if (use_concepts) c("concept_id", "concept") else name_col))
  if (!is.data.frame(mapping) || !nrow(mapping) || anyNA(names(mapping)) ||
      anyDuplicated(names(mapping)) || !all(required %in% names(mapping))) {
    missing_columns <- setdiff(required, names(mapping))
    stop("\u5bfe\u5fdc\u8868\u306b\u306f1\u884c\u4ee5\u4e0a\u306e\u30c7\u30fc\u30bf\u3068\u3001\u91cd\u8907\u306e\u306a\u3044\u5217\u540d\u304c\u5fc5\u8981\u3067\u3059\u3002\u5fc5\u8981\u306a\u5217: ",
         paste(required, collapse = ", "),
         if (length(missing_columns)) paste0("\u3002\u4e0d\u8db3\u3057\u3066\u3044\u308b\u5217: ", paste(missing_columns, collapse = ", ")) else "",
         if (use_concepts) "\u3002\u5f93\u6765\u306e\u5bfe\u5fdc\u8868\u3092\u4f7f\u3046\u5834\u5408\u306f name_col = \"\u5206\u6790\u7528\u5909\u6570\u540d\" \u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002" else "\u3002",
         call. = FALSE)
  }
  if (use_concepts && "\u5206\u6790\u7528\u5909\u6570\u540d" %in% names(mapping)) {
    message("\u65e2\u5b9a\u3067\u306fconcept_id\u30fbconcept\u3092\u4f7f\u7528\u3057\u307e\u3059\u3002\u5bfe\u5fdc\u8868\u306e\u300c\u5206\u6790\u7528\u5909\u6570\u540d\u300d\u5217\u3092\u4f7f\u3046\u5834\u5408\u306f\u3001",
            "name_col = \"\u5206\u6790\u7528\u5909\u6570\u540d\" \u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002")
  }
  for (column in required) {
    value <- mapping[[column]]
    if (!is.atomic(value) || !is.null(dim(value))) {
      stop("\u5bfe\u5fdc\u8868\u306e\u5217 '", column, "'\u306b\u306f\u6587\u5b57\u5217\u307e\u305f\u306f\u6587\u5b57\u5217\u306b\u5909\u63db\u3067\u304d\u308b\u5024\u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002",
           call. = FALSE)
    }
    value <- as.character(value)
    invalid <- which(is.na(value) | !nzchar(trimws(value, whitespace = "[\\h\\v]")))
    if (length(invalid)) {
      stop("\u5bfe\u5fdc\u8868\u306e\u5fc5\u9808\u5217 '", column, "'\u304c\u7a7a\u6b04\u3067\u3059\u3002\n",
           .naaa_mapping_locations(mapping, invalid, excel_rows, name_col), call. = FALSE)
    }
    mapping[[column]] <- value
  }
  if (length(unique(mapping[[identity_columns[[1L]]]])) != 1L) {
    stop("\u5bfe\u5fdc\u8868\u306b\u306f1\u3064\u306e\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea\u3060\u3051\u3092\u6b8b\u3057\u3066\u304f\u3060\u3055\u3044\u3002\n",
         .naaa_mapping_locations(mapping, seq_len(nrow(mapping)), excel_rows, name_col),
         call. = FALSE)
  }
  if (!all(identity_columns %in% names(catalog))) {
    stop("The current catalog must retain its Japanese metadata columns.",
         call. = FALSE)
  }
  years <- .naaa_parse_years(mapping[[identity_columns[[2L]]]], "mapping year")
  catalog_years <- .naaa_parse_years(catalog[["year_ad"]], "metadata year_ad")
  index <- integer(nrow(mapping))
  for (i in seq_len(nrow(mapping))) {
    candidates <- which(
      catalog[[identity_columns[[1L]]]] == mapping[[identity_columns[[1L]]]][[i]] &
        catalog_years == years[[i]] &
        catalog[[identity_columns[[3L]]]] == mapping[[identity_columns[[3L]]]][[i]]
    )
    if (length(candidates) != 1L) {
      stop("\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea\u30fb\u5e74\u5ea6\u30fb\u5909\u6570\u306b\u4e00\u81f4\u3059\u308b\u5143\u5909\u6570\u3092\u3001\u30e1\u30bf\u30c7\u30fc\u30bf\u304b\u3089\u4e00\u610f\u306b\u7279\u5b9a\u3067\u304d\u307e\u305b\u3093\u3002\n",
           .naaa_mapping_locations(mapping, i, excel_rows, name_col), call. = FALSE)
    }
    index[[i]] <- candidates[[1L]]
  }
  resolved <- .naaa_selection_mapping(catalog, index)
  key <- if (use_concepts) mapping[["concept_id"]] else mapping[[name_col]]
  resolved[["output_name"]] <- if (use_concepts) mapping[["concept"]] else mapping[[name_col]]
  resolved[["mapping_key"]] <- key
  resolved[["concept_id"]] <- if (use_concepts) mapping[["concept_id"]] else rep(NA_character_, nrow(mapping))
  resolved[["concept"]] <- if (use_concepts) mapping[["concept"]] else rep(NA_character_, nrow(mapping))
  problems <- character()
  duplicate <- duplicated(resolved[c("dataset_id", "mapping_key")]) |
    duplicated(resolved[c("dataset_id", "mapping_key")], fromLast = TRUE)
  if (any(duplicate)) {
    group_codes <- paste(match(resolved$dataset_id, unique(resolved$dataset_id)),
                         match(key, unique(key)), sep = "/")
    groups <- split(which(duplicate), factor(group_codes[duplicate], levels = unique(group_codes[duplicate])))
    for (rows in groups) {
      identity <- if (use_concepts) {
        paste0("concept_id=", .naaa_mapping_display(key[rows]), "\u3001concept=",
               .naaa_mapping_display(mapping[["concept"]][rows]))
      } else paste0(name_col, "=", .naaa_mapping_display(key[rows]))
      problems <- c(problems, paste0(
        "- ", identity, " \u304c ", paste(unique(resolved$year_era[rows]), collapse = ", "),
        "\u5e74\u5ea6\uff08", paste(unique(resolved$year_ad[rows]), collapse = ", "), "\u5e74\uff09\u306e\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea ",
        .naaa_mapping_display(mapping[[identity_columns[[1L]]]][rows]), " \u306b\u8907\u6570\u3042\u308a\u307e\u3059\u3002\n",
        .naaa_mapping_locations(mapping, rows, excel_rows, name_col)
      ))
    }
  }
  if (use_concepts) {
    groups <- split(seq_along(key), factor(key, levels = unique(key)))
    for (rows in groups) {
      if (length(unique(mapping[["concept"]][rows])) > 1L) {
        problems <- c(problems, paste0(
          "- \u540c\u3058concept_id=", .naaa_mapping_display(key[rows]),
          " \u306b\u8907\u6570\u306econcept\u540d\u304c\u3042\u308a\u307e\u3059: ", .naaa_mapping_display(mapping[["concept"]][rows]), "\u3002\n",
          .naaa_mapping_locations(mapping, rows, excel_rows)
        ))
      }
    }
    labels <- mapping[["concept"]]
    groups <- split(seq_along(labels), factor(labels, levels = unique(labels)))
    for (rows in groups) {
      if (length(unique(key[rows])) > 1L) {
        problems <- c(problems, paste0(
          "- concept=", .naaa_mapping_display(labels[rows]),
          " \u306b\u7570\u306a\u308bconcept_id\u304c\u3042\u308a\u307e\u3059: ", .naaa_mapping_display(key[rows]),
          "\u3002\u51fa\u529b\u5217\u540d\u304c\u91cd\u8907\u3059\u308b\u305f\u3081\u540d\u79f0\u3092\u78ba\u8a8d\u3057\u3066\u304f\u3060\u3055\u3044\u3002\n",
          .naaa_mapping_locations(mapping, rows, excel_rows)
        ))
      }
    }
  }
  reserved <- c("dataset_id", "category_code", "year_era", "year_ad", "source_row")
  conflicts <- intersect(unique(resolved[["output_name"]]), reserved)
  if (length(conflicts)) {
    rows <- which(resolved[["output_name"]] %in% conflicts)
    problems <- c(problems, paste0(
      "- \u51fa\u529b\u5217\u540d\u304c\u5e74\u5ea6\u60c5\u5831\u306a\u3069\u306e\u4e88\u7d04\u5217\u540d\u3068\u91cd\u8907\u3057\u3066\u3044\u307e\u3059: ",
      .naaa_mapping_display(conflicts), "\u3002\n",
      .naaa_mapping_locations(mapping, rows, excel_rows, name_col)
    ))
  }
  if (length(problems)) .naaa_stop_mapping_problems(problems)

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
  keys <- if ("mapping_key" %in% names(resolved_mapping)) resolved_mapping[["mapping_key"]] else resolved_mapping[["output_name"]]
  for (key in unique(keys)) {
    rows <- which(keys == key)
    rows <- rows[order(resolved_mapping[["year_ad"]][rows])]
    current <- resolved_mapping[rows, , drop = FALSE]
    name <- current[["output_name"]][[1L]]
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
    report[["concept_id"]] <- if ("concept_id" %in% names(current)) current[["concept_id"]] else rep(NA_character_, nrow(current))
    report[["concept"]] <- if ("concept" %in% names(current)) current[["concept"]] else rep(NA_character_, nrow(current))
    reports[[length(reports) + 1L]] <- report
  }
  if (!length(reports)) {
    result <- data.frame(
      analysis_name = character(), dataset_id = character(), year_era = character(),
      variable_id = character(), reference_variable_id = character(),
      choice_status = .naaa_choice_status(list()), stringsAsFactors = FALSE
    )
    result[["choices"]] <- list()
    result[["concept_id"]] <- character()
    result[["concept"]] <- character()
    return(result)
  }
  result <- do.call(rbind, reports)
  rownames(result) <- NULL
  result
}
