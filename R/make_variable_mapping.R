#' 年度・調査カテゴリ・検索語で対応表の候補をxlsxへ出力する
#'
#' [set_naaa_metadata()]で指定したメタデータを抽出し、Excelで編集できます。
#' `変数`・`ラベル_raw`・`ラベル_norm`を対象に、`contains()`で部分一致、
#' `matches()`で正規表現、`starts_with()`・`ends_with()`で前方・後方一致を
#' 指定できます。`c()`や`|`でOR、`&`でAND、`!`で除外を組み合わせられます。
#' 各条件は3つの項目のいずれかに一致した元変数を選び、組み合わせは元変数単位で
#' 判定します。ヘルパーは`tidyselect`の検索規則を使い、既定では大文字小文字を
#' 区別しません。同じ元変数が複数条件に一致しても1行だけ出力します。
#' 文字列ベクトルを直接渡す従来の指定は、文字どおりの部分一致・OR条件です。
#'
#' 既定では、指定した検索語・パターンの順に候補をまとめ、各まとまりを
#' 古い年度から並べます。同じ年度内は元のメタデータの行順を保ちます。
#' 複数の語に一致する行は最初の語のまとまりに一度だけ含めます。
#' 除外条件だけで選ばれた行など、肯定条件の語に一致しない行は最後にまとめます。
#' この並びは候補の比較用であり、質問内容の同一性を判定するものではありません。
#'
#' 出力後、不要な行を削除し、`分析用変数名`列を追加してください。
#' 同じ出力列として年度間で縦結合したい行には、同じ分析用変数名を
#' 入れます。編集後のパスは[read_naaa_data()]の`mapping`へ直接渡せます。
#' 候補が0件の場合も、元の列名と「西暦」を持つxlsxを出力します。
#'
#' @param 調査カテゴリ `"児童"`・`"生徒"`・`"小学校"`・`"中学校"`・
#'   `"児童保護者"`・`"生徒保護者"`の6種類から1つを完全一致で指定します。
#' @param 年度 `"H24:R6"`のような範囲、または
#'   `c("H24", "H25", "R6")`のような個別年度を指定します。
#'   西暦も指定でき、平成・令和をまたぐ範囲や将来の令和年度にも対応します。
#' @param 変数 `contains("朝食")`・`matches("朝食.*毎日")`などの検索条件、
#'   または1つ以上の検索語を持つ文字列ベクトルです。
#'   ヘルパーは名前だけでも`tidyselect::contains()`などの形式でも指定できます。
#'   条件による列名の変更や`where()`による型指定には対応しません。
#' @param output 保存するxlsxのパス。親フォルダは事前に作成してください。
#' @param overwrite 既存ファイルを置き換えるか。既定は`FALSE`です。
#' @param sort_by 出力順。`"keyword"`（既定）は検索語・パターンの指定順、
#'   その中で年度の昇順です。`"source"`は元のメタデータの行順です。
#'
#' @return 保存したxlsxの絶対パスを非表示で返します。
#'   元の値は文字列セルで保存し、コード列名の`01`、文字列`NA`、空白や
#'   改行を保持します。
#' @export
#'
#' @examples
#' \dontrun{
#' set_naaa_metadata("meta")
#' make_variable_mapping(
#'   調査カテゴリ = "児童",
#'   年度 = "H24:R6",
#'   変数 = c(contains("朝食"), matches("寝る|起きる"), contains("性別")),
#'   output = "mapping.xlsx"
#' )
#' # Excelで不要な行を削除し、「分析用変数名」列を追加して保存します。
#' data <- read_naaa_data(data_dir = "utf8", mapping = "mapping.xlsx")
#' }
make_variable_mapping <- function(
    調査カテゴリ, 年度, 変数, output, overwrite = FALSE, sort_by = "keyword"
) {
  if (!is.character(sort_by) || length(sort_by) != 1L || is.na(sort_by) ||
      !sort_by %in% c("keyword", "source")) {
    stop("'sort_by' must be 'keyword' or 'source'.", call. = FALSE)
  }
  expected <- c("\u8abf\u67fb\u30ab\u30c6\u30b4\u30ea", "\u5e74\u5ea6", "\u5909\u6570")
  filters <- stats::setNames(list(
    rlang::enquo(調査カテゴリ), rlang::enquo(年度), rlang::enquo(変数)
  ), expected)
  missing_filters <- vapply(filters, rlang::quo_is_missing, logical(1L))
  if (any(missing_filters)) {
    stop("次の必須引数を指定してください: ",
         paste(expected[missing_filters], collapse = ", "), call. = FALSE)
  }
  category <- rlang::eval_tidy(filters[[expected[[1L]]]])
  category_choices <- names(.naaa_survey_categories())
  if (!is.character(category) || length(category) != 1L || is.na(category) ||
      !category %in% category_choices) {
    stop("'", expected[[1L]], "' must exactly match one of: ",
         paste(category_choices, collapse = ", "), ".",
         call. = FALSE)
  }
  years <- .naaa_mapping_years(rlang::eval_tidy(filters[[expected[[2L]]]]))
  .naaa_check_path_argument(output, "output")
  if (!grepl("\\.xlsx$", output, ignore.case = TRUE)) {
    stop("'output' must have the .xlsx extension.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }
  output <- path.expand(output)
  directory <- dirname(output)
  if (!dir.exists(directory)) {
    stop("The output parent directory does not exist: ", directory,
         call. = FALSE)
  }
  output <- file.path(normalizePath(directory, winslash = "/", mustWork = TRUE),
                      basename(output))
  source <- .naaa_catalog_path()
  .naaa_check_mapping_output(output, source, overwrite)
  catalog <- .naaa_catalog_at(source)
  if (!category %in% unique(catalog[[expected[[1L]]]])) {
    stop("Survey category is not available in the selected metadata: ",
         category, call. = FALSE)
  }
  catalog_years <- .naaa_parse_years(catalog[["year_ad"]], "metadata year_ad")
  in_scope <- catalog[[expected[[1L]]]] == category & catalog_years %in% years
  candidates <- which(in_scope)
  by_keyword <- identical(sort_by, "keyword")
  selected <- .naaa_mapping_search(
    catalog[candidates, , drop = FALSE], filters[[expected[[3L]]]],
    details = by_keyword
  )
  matched <- if (by_keyword) selected$matched else selected
  rows <- candidates[matched]
  if (by_keyword) {
    rows <- rows[order(selected$group_rank[matched], catalog_years[rows], rows)]
  }
  # The normalizer prepends canonical fields; every remaining field is source
  # metadata, including optional work columns and literal code-column names.
  canonical <- c("dataset_id", "variable_id", "category_code", "category",
                 "year_ad", "year_era", "variable_name", "position_raw",
                 "label_raw", "label_effective", "measurement_level_raw")
  original <- names(catalog)[!names(catalog) %in% canonical]
  gregorian <- "\u897f\u66a6"
  if (gregorian %in% original) {
    stop("The metadata already contains a column named '", gregorian,
         "'; the derived mapping column cannot replace it.", call. = FALSE)
  }
  result <- as.data.frame(catalog[rows, original, drop = FALSE],
                          stringsAsFactors = FALSE)
  result[[gregorian]] <- as.character(catalog_years[rows])
  columns <- append(original, gregorian, after = match(expected[[2L]], original))
  result <- result[, columns, drop = FALSE]
  rownames(result) <- NULL
  .naaa_write_mapping_workbook(result, output, source, overwrite)
  message("Wrote ", nrow(result), " metadata candidate row(s): ", output)
  invisible(output)
}
