#' 選んだ変数をCSVから読み込み、年度間で縦結合する
#'
#' 編集したxlsx対応表を`mapping`、変換済みCSVのフォルダを`data_dir`に
#' 指定できます。対応表の「調査カテゴリ」「年度」「変数」を照合し、
#' 「分析用変数名」を出力列名にします。残した行をすべて読み込みます。
#' 1表につき1カテゴリ、同じデータセット・分析用変数名につき1変数です。
#' 対応のない年度の項目は`NA`とし、表に残っていない年度は読みません。
#' `years`・`year_range`を指定した場合も、対応表に残った年度だけに絞ります。
#' 選択肢は実際に読み込んだ年度のうち項目ごとの最も古い年度を基準に比較し、
#' `choice_comparison`で返します。回答値は自動変換しません。
#'
#' UTF-8に変換済みのCSVから必要な列を読み込みます。`concepts`と、
#' `variables`または`variable_id`による元変数の指定を併用できます。
#' `variables`と`variable_id`は同時には指定できません。
#' concept指定時はconcept ID、直接指定時は元の変数名を出力列名にします。
#' 同名の元変数を縦に積む場合も、質問内容の同一性は判定しません。
#' 併用時も同じCSVから必要な列をまとめて取得し、元の行の対応を保持します。
#' 出力列は年度情報、concept、直接指定した元変数の順です。
#' concept IDと直接指定した元変数名が重複する場合はエラーになります。
#' 併用時、元変数を指定していない年度の値は構造上の`NA`になります。
#' 明示指定した元変数の列がCSVにない場合は、`on_missing`に従います。
#'
#' 元の値は文字列で読み、先頭0、空文字、文字列`NA`、欠損コードを保持します。
#' 回答値の変換、年度間の個人結合、複数質問の集約は行いません。
#'
#' 年度は昇順、各CSV内は元の行順を維持します。メタデータにない年度は
#' 読み込み対象から外して報告します。CSV不足などで年度を除外する場合は、
#' 同じ年度のすべてのデータセットを除外します。不正なID、曖昧な変数指定、
#' 不正なCSVやUTF-8では、`on_missing`の設定にかかわらず停止します。
#'
#' メタデータCSVを差し替えると、その内容から計算する版が変わります。
#' 更新前の版で作成したconceptは、内容を確認してから再作成してください。
#'
#' @param files 検索結果の`dataset_id`を名前、CSVパスを値に持つ名前付き
#'   文字列ベクトル。`data_dir`を指定する場合は不要です。
#'   IDは`pupil_2012`などのカテゴリコードと西暦です。
#' @param concepts 作成済みの`naaa_concept`、またはそのリスト。
#'   `variables`または`variable_id`と併用できます。
#' @param variables `variable_name`と年度、および`category_code`または
#'   `dataset_id`を持つ表。年度は`year_era`または`year_ad`で指定します。
#' @param variable_id 読み込む元変数IDの文字列ベクトル。
#' @param years 西暦の整数ベクトル、または`"H24"`・`"R3"`などの年度。
#'   H・Rは大文字小文字を区別しません。省略時は選択変数の年度を使います。
#' @param year_range 両端を含む年度範囲の端点2つ。例：`c("H24", "R6")`。
#'   `years`とは同時に指定できません。
#' @param on_missing `"error"`（既定）はCSV・必要列・concept対応の不足で停止。
#'   `"skip_year"`は警告して対象年度全体を除外します。
#' @param data_dir UTF-8のCSVが入ったフォルダ。メタデータの年度・カテゴリから
#'   `H24_児童.csv`などのファイル名を作ります。`files`とは併用できません。
#' @param mapping 編集したxlsxファイルのパス、または対応表の`data.frame`。
#'   xlsxのシート名は`mapping`です。「調査カテゴリ」「年度」「変数」
#'   「分析用変数名」が必要です。他の変数指定方法とは併用できません。
#'
#' @return `data`と`skipped`を持つリスト。`data`は年度・データセット情報と
#'   選択列を持つ`data.table`です。`year_ad`は整数、その他の列は文字列です。
#'   `skipped`は除外年度・変数・理由を持つ`data.frame`です。
#'   `mapping`指定時は、分析用変数名、データセット・年度、元変数ID、
#'   比較基準の元変数ID、選択肢の一致区分を持つ`choice_comparison`も返します。
#'   すべての対象年度が除外された場合はエラーになります。
#' @export
#'
#' @examples
#' \dontrun{
#' set_naaa_metadata("meta")
#' data <- read_naaa_data(data_dir = "utf8", mapping = "mapping.xlsx")
#' data$data
#' data$choice_comparison
#'
#' files <- c(pupil_2012 = "utf8/H24_児童.csv",
#'            pupil_2013 = "utf8/H25_児童.csv")
#' data <- read_naaa_data(
#'   files, concepts = list(breakfast), years = c("H24", "H25"),
#'   on_missing = "skip_year"
#' )
#' data$data
#' data$skipped
#' }
read_naaa_data <- function(
    files = NULL,
    concepts = NULL,
    variables = NULL,
    variable_id = NULL,
    years = NULL,
    year_range = NULL,
    on_missing = "error",
    data_dir = NULL,
    mapping = NULL
) {
  if (!is.character(on_missing) || length(on_missing) != 1L ||
      is.na(on_missing) || !on_missing %in% c("error", "skip_year")) {
    stop("'on_missing' must be 'error' or 'skip_year'.", call. = FALSE)
  }
  requested <- .naaa_requested_years(years, year_range)
  catalog <- .naaa_catalog()
  if (!is.null(mapping)) {
    if (!is.null(concepts) || !is.null(variables) || !is.null(variable_id)) {
      stop("'mapping' cannot be combined with 'concepts', 'variables', or ",
           "'variable_id'.", call. = FALSE)
    }
    selection <- .naaa_selection_from_mapping(catalog, mapping)
  } else {
    selection <- .naaa_resolve_selection(
      catalog, concepts = concepts, variable_id = variable_id, variables = variables
    )
  }
  if (!is.null(data_dir) && !is.null(files)) {
    stop("Specify only one of 'data_dir' and 'files'.", call. = FALSE)
  }
  if (is.null(requested)) requested <- sort(unique(selection$default_years))
  if (identical(selection$mode, "mapping")) {
    requested <- intersect(requested, selection$default_years)
    if (!length(requested)) {
      stop("No mapping rows match the requested years.", call. = FALSE)
    }
  }

  category_rows <- !is.na(catalog$category_code) &
    catalog$category_code == selection$category_code
  for (column in c("year_ad", "year_era")) {
    category_rows <- category_rows & !is.na(catalog[[column]]) &
      nzchar(trimws(catalog[[column]]))
  }
  info <- unique(catalog[category_rows,
                         c("dataset_id", "category_code", "year_era", "year_ad"),
                         drop = FALSE])
  if (anyDuplicated(info$dataset_id)) {
    stop("Dataset category/year metadata is not unique.", call. = FALSE)
  }
  info$year_ad <- .naaa_parse_years(info$year_ad, "metadata year_ad")
  era_year <- .naaa_parse_years(info$year_era, "metadata year_era")
  if (!identical(info$year_ad, era_year)) {
    stop("Dataset metadata year_era and year_ad do not agree.", call. = FALSE)
  }

  active <- selection$mapping[selection$mapping$year_ad %in% requested, ,
                              drop = FALSE]
  if (!is.null(data_dir)) {
    files <- .naaa_files_from_directory(data_dir, catalog, unique(active$dataset_id))
  } else {
    files <- .naaa_validate_data_files(files, catalog)
  }
  .naaa_check_reused_csv_paths(files, unique(active$dataset_id))
  chunks <- list()
  included_datasets <- character()
  skipped <- .naaa_empty_skipped()

  for (year in requested) {
    datasets <- info[info$year_ad == year, , drop = FALSE]
    if (nrow(datasets) == 0L) {
      skipped <- rbind(skipped, .naaa_skip_record(
        year, reason = "year_not_in_catalog"
      ))
      next
    }
    mapping <- active[active$year_ad == year, , drop = FALSE]
    if (selection$mode %in% c("raw", "mapping") || nrow(mapping) > 0L) {
      datasets <- datasets[datasets$dataset_id %in% mapping$dataset_id, ,
                           drop = FALSE]
    }
    issues <- .naaa_empty_skipped()
    plans <- list()
    if (nrow(datasets) == 0L) {
      issues <- .naaa_skip_record(year, reason = "no_selected_variables")
    }
    for (i in seq_len(nrow(datasets))) {
      dataset <- datasets[i, , drop = FALSE]
      id <- dataset$dataset_id[[1L]]
      era <- dataset$year_era[[1L]]
      selected <- mapping[mapping$dataset_id == id, , drop = FALSE]
      if (length(selection$concept_names)) {
        missing <- setdiff(selection$concept_names, selected$output_name)
        if (length(missing)) {
          for (concept in missing) {
            issues <- rbind(issues, .naaa_skip_record(
              year, era, id, variable = concept, reason = "concept_not_mapped"
            ))
          }
          next
        }
      }
      if (nrow(selected) == 0L) {
        issues <- rbind(issues, .naaa_skip_record(
          year, era, id, reason = "no_selected_variables"
        ))
        next
      }
      if (anyDuplicated(selected$output_name)) {
        stop("Selected output column names are ambiguous within dataset: ", id,
             call. = FALSE)
      }
      if (!id %in% names(files) || !file.exists(files[[id]])) {
        issues <- rbind(issues, .naaa_skip_record(
          year, era, id, reason = "csv_missing"
        ))
        next
      }
      path <- files[[id]]
      if (dir.exists(path)) {
        stop("A CSV path points to a directory: ", path, call. = FALSE)
      }
      header <- .naaa_csv_header(path)
      missing <- !selected$variable_name %in% header
      if (any(missing)) {
        for (j in which(missing)) {
          issues <- rbind(issues, .naaa_skip_record(
            year, era, id, selected$variable_id[[j]], selected$output_name[[j]],
            "column_missing"
          ))
        }
        next
      }
      plans[[length(plans) + 1L]] <- list(
        dataset = dataset, mapping = selected, path = path
      )
    }
    if (nrow(issues)) {
      if (identical(on_missing, "error")) {
        .naaa_stop_missing_year(year, issues)
      }
      skipped <- rbind(skipped, issues)
      next
    }

    # Read only after every dataset of this year passes the missing-data checks.
    for (plan in plans) {
      selected <- plan$mapping
      raw <- .naaa_read_csv_columns(plan$path, unique(selected$variable_name))
      n <- nrow(raw)
      dataset <- plan$dataset
      columns <- list(
        dataset_id = rep(dataset$dataset_id[[1L]], n),
        category_code = rep(dataset$category_code[[1L]], n),
        year_era = rep(dataset$year_era[[1L]], n),
        year_ad = rep(as.integer(year), n)
      )
      values <- lapply(selected$variable_name, function(name) raw[[name]])
      names(values) <- selected$output_name
      chunks[[length(chunks) + 1L]] <- data.table::as.data.table(c(columns, values))
      included_datasets <- c(included_datasets, dataset$dataset_id[[1L]])
    }
  }

  if (length(chunks) == 0L) {
    stop("No data could be created: all requested years were excluded. ",
         paste(unique(skipped$reason), collapse = ", "), call. = FALSE)
  }
  data <- data.table::rbindlist(chunks, use.names = TRUE, fill = TRUE)
  if (selection$mode %in% c("mixed", "mapping")) {
    for (name in setdiff(selection$raw_names, names(data))) {
      data.table::set(data, j = name, value = rep(NA_character_, nrow(data)))
    }
  }
  data.table::setcolorder(data, c(
    "dataset_id", "category_code", "year_era", "year_ad",
    selection$output_names[selection$output_names %in% names(data)]
  ))
  rownames(skipped) <- NULL
  if (nrow(skipped)) {
    warning("Excluded year(s): ", paste(sort(unique(skipped$year_ad)), collapse = ", "),
            ". See the returned 'skipped' table for reasons.", call. = FALSE)
  }
  result <- list(data = data, skipped = skipped)
  if (identical(selection$mode, "mapping")) {
    result$choice_comparison <- .naaa_mapping_choice_comparison(
      catalog, active[active$dataset_id %in% included_datasets, , drop = FALSE]
    )
  }
  result
}

.naaa_files_from_directory <- function(directory, catalog, datasets) {
  if (!is.character(directory) || length(directory) != 1L || is.na(directory) ||
      !nzchar(trimws(directory)) || !dir.exists(path.expand(directory))) {
    stop("'data_dir' must be an existing folder containing UTF-8 CSV files.",
         call. = FALSE)
  }
  directory <- normalizePath(path.expand(directory), winslash = "/", mustWork = TRUE)
  rows <- match(datasets, catalog$dataset_id)
  if (anyNA(rows)) stop("Unknown dataset selected for 'data_dir'.", call. = FALSE)
  category_column <- intersect(c("category", "\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea"),
                               names(catalog))
  if (!length(category_column)) {
    stop("Dataset metadata needs Japanese survey categories for 'data_dir'.",
         call. = FALSE)
  }
  categories <- catalog[[category_column[[1L]]]][rows]
  eras <- catalog$year_era[rows]
  if (anyNA(categories) || any(!nzchar(categories)) ||
      anyNA(eras) || any(!nzchar(eras)) ||
      any(grepl("[/\\\\]", categories)) || any(grepl("[/\\\\]", eras))) {
    stop("Dataset category/year metadata cannot be used as CSV file names.",
         call. = FALSE)
  }
  stats::setNames(file.path(directory, paste0(eras, "_", categories, ".csv")),
                  datasets)
}

.naaa_validate_data_files <- function(files, catalog) {
  if (!is.character(files) || length(files) == 0L || anyNA(files) ||
      any(!nzchar(trimws(files))) || is.null(names(files)) ||
      anyNA(names(files)) || any(!nzchar(trimws(names(files)))) ||
      anyDuplicated(names(files))) {
    stop("'files' must be a non-empty character vector with unique dataset_id names.",
         call. = FALSE)
  }
  unknown <- setdiff(names(files), unique(catalog$dataset_id))
  if (length(unknown)) {
    stop("Unknown dataset_id in 'files': ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  expanded <- path.expand(files)
  names(expanded) <- names(files)
  expanded
}

.naaa_check_reused_csv_paths <- function(files, datasets) {
  paths <- files[names(files) %in% datasets]
  paths <- paths[file.exists(paths) & !dir.exists(paths)]
  if (length(paths) < 2L) return(invisible(NULL))
  canonical <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  if (.Platform$OS.type == "windows" ||
      identical(unname(Sys.info()[["sysname"]]), "Darwin")) {
    canonical <- tolower(canonical)
  }
  if (anyDuplicated(canonical)) {
    stop("The same CSV file is assigned to multiple selected datasets.", call. = FALSE)
  }
  invisible(NULL)
}

.naaa_empty_skipped <- function() {
  data.frame(
    year_ad = integer(), year_era = character(), dataset_id = character(),
    variable_id = character(), variable = character(), reason = character(),
    stringsAsFactors = FALSE
  )
}

.naaa_skip_record <- function(
    year_ad, year_era = NA_character_, dataset_id = NA_character_,
    variable_id = NA_character_, variable = NA_character_, reason
) {
  data.frame(
    year_ad = as.integer(year_ad), year_era = year_era, dataset_id = dataset_id,
    variable_id = variable_id, variable = variable, reason = reason,
    stringsAsFactors = FALSE
  )
}

.naaa_stop_missing_year <- function(year, issues) {
  details <- paste(unique(paste(
    issues$dataset_id, issues$variable, issues$reason, sep = ": "
  )), collapse = "; ")
  stop("Required data are missing for year ", year, ": ", details,
       ". Use on_missing = 'skip_year' to exclude the entire year.", call. = FALSE)
}
