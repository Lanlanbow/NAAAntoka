#' 作成済みデータに、同じCSVから変数を追加する
#'
#' [read_naaa_data()]の返り値と追加用の対応表を指定し、元CSVの
#' `dataset_id`・`source_row`を使って新しい列を追加します。
#' 元データの行数・行順と既存列を保持し、追加用の対応表にない年度は
#' 新しい列を`NA`にします。追加側だけにある年度の行は増やしません。
#'
#' `source_row`は、CSVのヘッダーを除くデータレコード番号です。
#' 引用符内の改行を含むCSVでは、ファイル上の物理的な行番号とは異なります。
#' 年度間の個人を識別するIDではありません。
#'
#' 元の返り値の`data`を絞り込んだり並べ替えたりしていても追加できます。
#' `source_row`の値は変更したり振り直したりしないでください。
#' `dataset_id`・`category_code`・`year_era`・`year_ad`・`source_row`は保持してください。
#' キーの重複、既存列名との衝突、レコード番号の範囲外などは停止します。
#'
#' 既定の`sources = FALSE`では、CSVのMD5による内容確認と`sources`の出力を省略します。
#' 同じ元CSVを使い、ファイル内の行順や内容を変更しないでください。
#' 内容確認も行う場合は、初回の[read_naaa_data()]と追加時の両方で
#' `sources = TRUE`を指定します。この場合は元CSVのMD5・レコード数も照合し、
#' 不一致なら停止します。同じ内容ならCSVの保存場所の変更は可能です。
#'
#' @param data [read_naaa_data()]または本関数の返り値。
#'   `data`を持つリストを指定します。データ表だけは指定できません。
#' @param mapping 追加したい変数の編集済みxlsx対応表、またはその`data.frame`。
#'   対応表全体を検査し、元データに残っている年度・カテゴリの変数を読みます。
#'   既定では`concept_id`を対応キー、`concept`を出力列名にします。
#' @param data_dir UTF-8のCSVフォルダ。`files`とは併用できません。
#'   省略時に元の返り値が`sources`の記録を持っていれば、そのCSVの保存場所を使います。
#'   保存場所の記録がなければ、`data_dir`または`files`の指定が必要です。
#' @param files `dataset_id`を名前、UTF-8のCSVパスを値に持つ名前付き文字列ベクトル。
#' @param name_col 対応キー兼出力列名に使う対応表の列名。
#'   省略時は`concept_id`・`concept`を使います。
#' @param on_missing `"error"`（既定）は必要なCSV・列がない場合に停止します。
#'   `"skip_year"`は追加読み込みからその年度を除外し、元の行を保持して
#'   追加列を`NA`にします。すべての追加対象年度が除外された場合は停止します。
#' @param sources CSVのMD5による内容確認と、元CSV情報の出力を行うか。
#'   既定の`FALSE`ではどちらも省略します。`TRUE`には、初回から
#'   `read_naaa_data(..., sources = TRUE)`で作成した記録が必要です。
#'   `FALSE`で作成した返り値を後から`TRUE`に切り替えることはできません。
#' @return 元の返り値を引き継ぐリスト。`data`に新しい列を追加し、
#'   `skipped`・`choice_comparison`に追加読み込みの報告を加えます。
#'   `sources = FALSE`では、入力に記録があっても返り値の`sources`は省略します。
#'   `TRUE`では元CSVの記録を保持し、読み込みに成功したCSVの保存場所を更新します。
#'   `join_summary`は今回の追加について、`dataset_id`別の元の行数
#'   `n_existing`、対応した行数`n_matched`、対応しなかった行数`n_unmatched`を返します。
#'   これは回答値の欠損数ではなく、元CSVのレコードとの対応件数です。
#' @export
#' @examples
#' \dontrun{
#' set_naaa_metadata("meta")
#' data <- read_naaa_data(data_dir = "utf8", mapping = "mapping_edited.xlsx")
#' # 国語・算数の点数を指定した追加用対応表を編集して保存します。
#' data <- add_naaa_variables(
#'   data, mapping = "scores_edited.xlsx", data_dir = "utf8"
#' )
#' data$data
#' data$join_summary
#'
#' # 元CSVの内容も照合する場合は、最初の読み込みからTRUEを指定します。
#' data <- read_naaa_data(
#'   data_dir = "utf8", mapping = "mapping_edited.xlsx", sources = TRUE
#' )
#' data <- add_naaa_variables(data, mapping = "scores_edited.xlsx", sources = TRUE)
#' }
add_naaa_variables <- function(
    data, mapping, data_dir = NULL, files = NULL,
    name_col = NULL, on_missing = "error", sources = FALSE
) {
  .naaa_validate_sources_flag(sources)
  if (!is.character(on_missing) || length(on_missing) != 1L ||
      is.na(on_missing) || !on_missing %in% c("error", "skip_year")) {
    stop("'on_missing'\u306b\u306f'error'\u307e\u305f\u306f'skip_year'\u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002", call. = FALSE)
  }
  if (!is.null(data_dir) && !is.null(files)) {
    stop("'data_dir'\u3068'files'\u306f\u540c\u6642\u306b\u6307\u5b9a\u3067\u304d\u307e\u305b\u3093\u3002", call. = FALSE)
  }
  checked <- .naaa_add_validate_input(data, require_sources = sources)
  base <- data[["data"]]
  source_info <- checked$sources
  base_ids <- unique(base[["dataset_id"]])
  catalog <- .naaa_catalog()
  selection <- .naaa_selection_from_mapping(catalog, mapping, name_col)
  categories <- unique(base[["category_code"]])
  if (length(categories) != 1L || categories[[1L]] != selection$category_code) {
    stop("\u8ffd\u52a0\u7528\u5bfe\u5fdc\u8868\u306e\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea\u304c\u3001\u5143\u30c7\u30fc\u30bf\u306e\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea\u3068\u4e00\u81f4\u3057\u307e\u305b\u3093\u3002", call. = FALSE)
  }
  new_names <- selection$output_names
  conflicts <- intersect(new_names, names(base))
  if (length(conflicts)) {
    stop("\u8ffd\u52a0\u3059\u308b\u5217\u540d\u304c\u65e2\u5b58\u5217\u3068\u91cd\u8907\u3057\u3066\u3044\u307e\u3059: ",
         .naaa_mapping_display(conflicts),
         "\u3002\u8ffd\u52a0\u7528\u5bfe\u5fdc\u8868\u306econcept\u3001\u307e\u305f\u306fname_col\u3067\u6307\u5b9a\u3057\u305f\u5217\u3092\u4fee\u6b63\u3057\u3066\u304f\u3060\u3055\u3044\u3002",
         call. = FALSE)
  }
  overlap <- intersect(base_ids, selection$mapping[["dataset_id"]])
  if (!length(overlap)) {
    stop("\u8ffd\u52a0\u7528\u5bfe\u5fdc\u8868\u306b\u3001\u5143\u30c7\u30fc\u30bf\u306e\u5e74\u5ea6\u30fb\u30c7\u30fc\u30bf\u30bb\u30c3\u30c8\u306b\u5bfe\u5fdc\u3059\u308b\u884c\u304c\u3042\u308a\u307e\u305b\u3093\u3002",
         call. = FALSE)
  }
  if (is.null(data_dir) && is.null(files)) {
    if (is.null(source_info)) {
      stop("CSV\u306e\u4fdd\u5b58\u5834\u6240\u306e\u8a18\u9332\u304c\u3042\u308a\u307e\u305b\u3093\u3002'data_dir'\u307e\u305f\u306f'files'\u3067\u8ffd\u52a0\u8aad\u307f\u8fbc\u307f\u3059\u308bCSV\u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002",
           call. = FALSE)
    }
    records <- match(base_ids, source_info[["dataset_id"]])
    files <- stats::setNames(source_info[["path"]][records], base_ids)
  }
  if (sources) {
    resolved_files <- if (!is.null(data_dir)) {
      .naaa_files_from_directory(data_dir, catalog, overlap)
    } else {
      .naaa_validate_data_files(files, catalog)
    }
    # Check readable sources before missing-column handling can skip a year.
    # Post-read checks alone would miss changed files that lost a selected column.
    for (id in overlap) {
      if (!id %in% names(resolved_files) || !file.exists(resolved_files[[id]])) next
      current <- .naaa_source_fingerprint(resolved_files[[id]])
      previous <- match(id, source_info[["dataset_id"]])
      if (current$md5 != tolower(source_info[["md5"]][previous])) {
        stop("\u5143CSV\u306e\u5185\u5bb9\u304c\u4e00\u81f4\u3057\u307e\u305b\u3093: ", .naaa_mapping_display(id),
             "\u3002\u540c\u3058CSV\u304b\u3089\u30c7\u30fc\u30bf\u3092\u4f5c\u6210\u3057\u76f4\u3057\u3066\u304f\u3060\u3055\u3044\u3002", call. = FALSE)
      }
    }
  }
  added <- read_naaa_data(
    data_dir = data_dir, files = files, mapping = mapping,
    years = sort(unique(base[["year_ad"]])), name_col = name_col,
    on_missing = on_missing, sources = sources
  )
  added_checked <- .naaa_add_validate_input(added, require_sources = sources)
  if (sources) {
    added_sources <- added_checked$sources
    added_ids <- added_sources[["dataset_id"]]
    source_index <- match(added_ids, source_info[["dataset_id"]])
    if (anyNA(source_index)) {
      stop("\u8ffd\u52a0\u8aad\u307f\u8fbc\u307f\u306b\u3001\u5143\u30c7\u30fc\u30bf\u306b\u306a\u3044\u30c7\u30fc\u30bf\u30bb\u30c3\u30c8\u304c\u542b\u307e\u308c\u3066\u3044\u307e\u3059\u3002", call. = FALSE)
    }
    for (column in c("category_code", "year_era", "year_ad")) {
      disagreement <- added_sources[[column]] != source_info[[column]][source_index]
      if (any(disagreement)) {
        stop("\u8ffd\u52a0\u5143CSV\u306e\u5e74\u5ea6\u30fb\u30ab\u30c6\u30b4\u30ea\u60c5\u5831\u304c\u5143\u306e\u8a18\u9332\u3068\u4e00\u81f4\u3057\u307e\u305b\u3093: ", column,
             "\u3001dataset_id=", .naaa_mapping_display(added_ids[disagreement]),
             call. = FALSE)
      }
    }
    changed <- tolower(added_sources[["md5"]]) !=
      tolower(source_info[["md5"]][source_index]) |
      added_sources[["n_records"]] != source_info[["n_records"]][source_index]
    if (any(changed)) {
      stop("\u5143CSV\u306e\u5185\u5bb9\u307e\u305f\u306f\u30ec\u30b3\u30fc\u30c9\u6570\u304c\u4e00\u81f4\u3057\u307e\u305b\u3093: ",
           .naaa_mapping_display(added_ids[changed]),
           "\u3002\u540c\u3058CSV\u304b\u3089\u30c7\u30fc\u30bf\u3092\u4f5c\u6210\u3057\u76f4\u3057\u3066\u304f\u3060\u3055\u3044\u3002", call. = FALSE)
    }
  }
  if (!all(new_names %in% names(added[["data"]]))) {
    stop("\u8ffd\u52a0\u7528\u5bfe\u5fdc\u8868\u306e\u3059\u3079\u3066\u306e\u51fa\u529b\u5217\u3092\u53d6\u5f97\u3067\u304d\u307e\u305b\u3093\u3067\u3057\u305f\u3002", call. = FALSE)
  }
  # Use the exported merge method directly: this package otherwise uses fully
  # qualified data.table calls without importing its non-standard [ semantics.
  # sort = FALSE preserves the left table's order for a one-to-one left join.
  lookup <- data.table::copy(added_checked$keys)
  data.table::set(lookup, j = ".naaa_added_row", value = seq_len(nrow(lookup)))
  matched_table <- data.table::merge.data.table(
    checked$keys, lookup, by = c("dataset_id", "source_row"),
    all.x = TRUE, sort = FALSE
  )
  matched <- matched_table[[".naaa_added_row"]]
  # Missing rows in a successfully read dataset indicate an invalid record
  # key, including an out-of-range key when the optional source manifest is off.
  skipped_years <- added[["skipped"]][["year_ad"]]
  successful_ids <- unique(selection$mapping[["dataset_id"]][
    selection$mapping[["dataset_id"]] %in% overlap &
      !selection$mapping[["year_ad"]] %in% skipped_years
  ])
  invalid_rows <- is.na(matched) & base[["dataset_id"]] %in% successful_ids
  if (any(invalid_rows)) {
    stop("source_row\u306b\u3001\u8ffd\u52a0\u5143CSV\u306b\u5b58\u5728\u3057\u306a\u3044\u30ec\u30b3\u30fc\u30c9\u756a\u53f7\u304c\u3042\u308a\u307e\u3059\u3002dataset_id=",
         .naaa_mapping_display(unique(base[["dataset_id"]][invalid_rows])),
         "\u3002\u5143\u306e\u30ec\u30b3\u30fc\u30c9\u756a\u53f7\u3092\u4fdd\u6301\u3057\u305f\u30c7\u30fc\u30bf\u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002", call. = FALSE)
  }
  output <- data.table::copy(data.table::as.data.table(base))
  for (name in new_names) {
    data.table::set(output, j = name, value = added[["data"]][[name]][matched])
  }
  result <- data
  result[["data"]] <- output
  if (sources) {
    source_info[["path"]][source_index] <- added_sources[["path"]]
    result[["sources"]] <- source_info
  } else {
    result[["sources"]] <- NULL
  }
  result[["skipped"]] <- .naaa_add_bind_reports(data[["skipped"]], added[["skipped"]])
  previous_comparison <- data[["choice_comparison"]]
  if (!is.null(previous_comparison)) {
    if (!is.data.frame(previous_comparison) ||
        !"analysis_name" %in% names(previous_comparison)) {
      stop("\u5143\u30c7\u30fc\u30bf\u306echoice_comparison\u304c\u4e0d\u6b63\u3067\u3059\u3002\u5143\u306e\u8fd4\u308a\u5024\u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002",
           call. = FALSE)
    }
    previous_comparison <- previous_comparison[
      previous_comparison[["analysis_name"]] %in% names(base), , drop = FALSE
    ]
  }
  result[["choice_comparison"]] <- .naaa_add_bind_reports(
    previous_comparison, added[["choice_comparison"]]
  )
  grouping <- match(base[["dataset_id"]], base_ids)
  existing <- tabulate(grouping, nbins = length(base_ids))
  found <- tabulate(grouping[!is.na(matched)], nbins = length(base_ids))
  result[["join_summary"]] <- data.frame(
    dataset_id = base_ids, n_existing = existing, n_matched = found,
    n_unmatched = existing - found, stringsAsFactors = FALSE
  )
  result
}

.naaa_add_validate_input <- function(result, require_sources = FALSE) {
  required_info <- c("dataset_id", "category_code", "year_era", "year_ad", "source_row")
  required_sources <- c("dataset_id", "category_code", "year_era", "year_ad",
                        "path", "md5", "n_records")
  if (!is.list(result) || is.data.frame(result) ||
      anyDuplicated(names(result)) || !is.data.frame(result[["data"]]) ||
      !all(required_info %in% names(result[["data"]]))) {
    stop("'data'\u306b\u306fsource_row\u3092\u6301\u3064read_naaa_data()\u306e\u8fd4\u308a\u5024\u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002",
         "\u4ee5\u524d\u306e\u30d0\u30fc\u30b8\u30e7\u30f3\u3067\u4f5c\u6210\u3057\u305f\u5834\u5408\u306f\u3001\u65b0\u3057\u3044\u30d0\u30fc\u30b8\u30e7\u30f3\u3067\u8aad\u307f\u8fbc\u307f\u76f4\u3057\u3066\u304f\u3060\u3055\u3044\u3002",
         call. = FALSE)
  }
  base <- result[["data"]]
  source_info <- result[["sources"]]
  if (require_sources && is.null(source_info)) {
    stop("\u5143CSV\u3092\u78ba\u8a8d\u3059\u308b\u305f\u3081\u306esources\u306e\u8a18\u9332\u304c\u3042\u308a\u307e\u305b\u3093\u3002",
         "\u6700\u521d\u304b\u3089read_naaa_data(..., sources = TRUE)\u3067\u8aad\u307f\u8fbc\u307f\u76f4\u3057\u3001\u8ffd\u52a0\u6642\u3082sources = TRUE\u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002",
         call. = FALSE)
  }
  if (!nrow(base) || anyNA(names(base)) || anyDuplicated(names(base))) {
    stop("\u5143\u30c7\u30fc\u30bf\u306b\u306f1\u884c\u4ee5\u4e0a\u306e\u30c7\u30fc\u30bf\u3068\u91cd\u8907\u306e\u306a\u3044\u5217\u540d\u304c\u5fc5\u8981\u3067\u3059\u3002", call. = FALSE)
  }
  tables <- list(base)
  if (!is.null(source_info)) {
    if (!is.data.frame(source_info) || !nrow(source_info) ||
        anyNA(names(source_info)) || anyDuplicated(names(source_info)) ||
        !all(required_sources %in% names(source_info))) {
      stop("sources\u306e\u8a18\u9332\u304c\u4e0d\u6b63\u3067\u3059\u3002\u5143\u306e\u8fd4\u308a\u5024\u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002", call. = FALSE)
    }
    source_info <- as.data.frame(source_info, stringsAsFactors = FALSE)
    tables[[2L]] <- source_info
  }
  for (column in c("dataset_id", "category_code", "year_era")) {
    for (table in tables) {
      value <- table[[column]]
      if (!is.character(value) || !is.null(dim(value)) || anyNA(value) ||
          any(!nzchar(trimws(unique(value), whitespace = "[\\h\\v]")))) {
        stop("\u5143\u30c7\u30fc\u30bf\u30fbsources\u306e\u8b58\u5225\u5217\u304c\u4e0d\u6b63\u3067\u3059: ", column, call. = FALSE)
      }
    }
  }
  for (table in tables) {
    year <- table[["year_ad"]]
    if (!is.numeric(year) || !is.null(dim(year)) || anyNA(year) ||
        any(!is.finite(year)) || any(year < 1 | year != floor(year))) {
      stop("\u5143\u30c7\u30fc\u30bf\u30fbsources\u306eyear_ad\u306b\u306f\u897f\u66a6\u306e\u6574\u6570\u304c\u5fc5\u8981\u3067\u3059\u3002", call. = FALSE)
    }
  }
  ids <- unique(base[["dataset_id"]])
  first <- match(ids, base[["dataset_id"]])
  info <- data.frame(
    dataset_id = ids, category_code = base[["category_code"]][first],
    year_era = base[["year_era"]][first], year_ad = base[["year_ad"]][first],
    stringsAsFactors = FALSE
  )
  records <- match(base[["dataset_id"]], ids)
  for (column in c("category_code", "year_era", "year_ad")) {
    invalid <- base[[column]] != info[[column]][records]
    if (any(invalid)) {
      stop("\u540c\u3058dataset_id\u306b\u7570\u306a\u308b\u5e74\u5ea6\u30fb\u30ab\u30c6\u30b4\u30ea\u60c5\u5831\u304c\u3042\u308a\u307e\u3059: ", column,
           "\u3001dataset_id=", .naaa_mapping_display(unique(base[["dataset_id"]][invalid])),
           call. = FALSE)
    }
  }
  era_year <- .naaa_parse_years(info[["year_era"]], "year_era")
  if (any(era_year != info[["year_ad"]]) ||
      any(info[["dataset_id"]] != paste(info[["category_code"]], info[["year_ad"]], sep = "_"))) {
    stop("\u5143\u30c7\u30fc\u30bf\u306edataset_id\u30fb\u5e74\u5ea6\u30fb\u30ab\u30c6\u30b4\u30ea\u60c5\u5831\u304c\u4e00\u81f4\u3057\u307e\u305b\u3093\u3002", call. = FALSE)
  }
  rows <- base[["source_row"]]
  if (!is.numeric(rows) || !is.null(dim(rows)) || anyNA(rows) ||
      any(!is.finite(rows)) || any(rows < 1 | rows != floor(rows))) {
    stop("source_row\u306b\u306f\u3001\u5143CSV\u306e\u30ec\u30b3\u30fc\u30c9\u756a\u53f7\u3092\u8868\u3059\u6b63\u306e\u6574\u6570\u304c\u5fc5\u8981\u3067\u3059\u3002", call. = FALSE)
  }
  keys <- data.table::data.table(dataset_id = base[["dataset_id"]], source_row = rows)
  if (anyDuplicated(keys, by = c("dataset_id", "source_row"))) {
    duplicates <- duplicated(keys, by = c("dataset_id", "source_row")) |
      duplicated(keys, by = c("dataset_id", "source_row"), fromLast = TRUE)
    stop("\u5143\u30c7\u30fc\u30bf\u306edataset_id\u30fbsource_row\u306e\u7d44\u304c\u91cd\u8907\u3057\u3066\u3044\u307e\u3059\u3002dataset_id=",
         .naaa_mapping_display(unique(keys[["dataset_id"]][duplicates])),
         call. = FALSE)
  }
  if (!is.null(source_info)) {
    if (anyDuplicated(source_info[["dataset_id"]])) {
      stop("sources\u306b\u540c\u3058dataset_id\u304c\u8907\u6570\u3042\u308a\u307e\u3059\u3002", call. = FALSE)
    }
    for (column in c("path", "md5")) {
      value <- source_info[[column]]
      if (!is.character(value) || !is.null(dim(value)) || anyNA(value) ||
          any(!nzchar(trimws(value, whitespace = "[\\h\\v]")))) {
        stop("sources\u306e\u8a18\u9332\u304c\u4e0d\u8db3\u3057\u3066\u3044\u307e\u3059: ", column, call. = FALSE)
      }
    }
    if (any(!grepl("^[0-9a-fA-F]{32}$", source_info[["md5"]]))) {
      stop("sources\u306eMD5\u306e\u8a18\u9332\u304c\u4e0d\u6b63\u3067\u3059\u3002", call. = FALSE)
    }
    counts <- source_info[["n_records"]]
    if (!is.numeric(counts) || !is.null(dim(counts)) || anyNA(counts) ||
        any(!is.finite(counts)) || any(counts < 0 | counts != floor(counts))) {
      stop("sources\u306e\u5143\u30ec\u30b3\u30fc\u30c9\u6570\u306e\u8a18\u9332\u304c\u4e0d\u6b63\u3067\u3059\u3002", call. = FALSE)
    }
    source_rows <- match(base[["dataset_id"]], source_info[["dataset_id"]])
    if (anyNA(source_rows)) {
      stop("\u5143\u30c7\u30fc\u30bf\u306b\u5bfe\u5fdc\u3059\u308bdataset_id\u304csources\u306b\u3042\u308a\u307e\u305b\u3093: ",
           .naaa_mapping_display(unique(base[["dataset_id"]][is.na(source_rows)])),
           call. = FALSE)
    }
    for (column in c("category_code", "year_era", "year_ad")) {
      invalid <- base[[column]] != source_info[[column]][source_rows]
      if (any(invalid)) {
        stop("\u5143\u30c7\u30fc\u30bf\u306e\u5e74\u5ea6\u30fb\u30ab\u30c6\u30b4\u30ea\u60c5\u5831\u304csources\u3068\u4e00\u81f4\u3057\u307e\u305b\u3093: ", column,
             "\u3001dataset_id=", .naaa_mapping_display(unique(base[["dataset_id"]][invalid])),
             call. = FALSE)
      }
    }
    if (any(rows > counts[source_rows])) {
      stop("source_row\u306b\u306f\u3001\u5143CSV\u306e\u30ec\u30b3\u30fc\u30c9\u6570\u306e\u7bc4\u56f2\u5185\u306e\u6b63\u306e\u6574\u6570\u304c\u5fc5\u8981\u3067\u3059\u3002", call. = FALSE)
    }
  }
  list(keys = keys, sources = source_info)
}

.naaa_add_bind_reports <- function(previous, added) {
  for (report in list(previous, added)) {
    if (!is.null(report) && !is.data.frame(report)) {
      stop("\u8aad\u307f\u8fbc\u307f\u7d50\u679c\u306e\u5831\u544a\u8868\u304c\u4e0d\u6b63\u3067\u3059\u3002\u5143\u306e\u8fd4\u308a\u5024\u3092\u6307\u5b9a\u3057\u3066\u304f\u3060\u3055\u3044\u3002", call. = FALSE)
    }
  }
  result <- as.data.frame(data.table::rbindlist(
    list(previous, added), use.names = TRUE, fill = TRUE
  ))
  rownames(result) <- NULL
  result
}
