#' 選んだ変数を分析者独自のconceptにまとめる
#'
#' 同じカテゴリ内の変数を、1データセットにつき1変数ずつ選び、
#' 年度をまたいで同じものとみなすconceptの対応表を作成します。
#' `name`はconceptの名前、`variable_id`はまとめたい元の変数IDです。
#' 年度は使用するメタデータの`year_era`から取得するため、指定は不要です。
#' IDの文字列を分解して年度を推測する処理は行いません。
#'
#' 最初に指定した変数を基準に、コードと値ラベルの組全体を比較します。
#' 無回答・非該当などのコードも含め、句読点だけの差も「異なる」とします。
#' コードの`01`と`1`は区別し、メタデータ内の列順序は比較に影響しません。
#' 基準または比較対象の値ラベルが全くない場合は「情報不足」とします。
#' メタデータで値ラベルが空欄のコードは定義なしとして扱います。
#' 回答値の再符号化や複数質問の集約は行わず、分析者が別途指定します。
#'
#' 変数は指定した順序で保持します。比較は指定した変数間で行い、
#' メタデータにあるconcept分類から同一性を自動判定しません。
#' メタデータCSVの内容から自動計算した版を記録します。
#' 作成結果は[base::saveRDS()]で保存し、[base::readRDS()]で再利用できます。
#'
#' @param name conceptの名前を表す1つの空でない文字列。
#' @param variable_id まとめたい元の`variable_id`の文字列ベクトル。
#'   同じカテゴリ内で、1データセットにつき1変数まで指定できます。
#' @param reason 判断理由。省略時は`NA`です。1つの文字列を全変数に共通で
#'   指定するか、`variable_id`と同じ長さで変数ごとに指定できます。
#' @param concept_id conceptの識別子。省略時は`name`を使います。
#'   名前とは別の固定IDを使いたい場合に指定します。
#'
#' @return `naaa_concept`オブジェクト。concept ID・名前・カテゴリ・
#'   メタデータ版・比較基準の変数IDと、`variables`という4列表を保持します。
#'   表の列は`variable_id`、`reason`、`year`、`choice_status`です。
#'   `choice_status`は3水準のfactor、その他の列は文字列です。
#' @export
#'
#' @examples
#' \dontrun{
#' # selected_idsは検索結果を確認して分析者が選んだvariable_id
#' breakfast <- create_naaa_concept(
#'   name = "朝食", variable_id = selected_ids,
#'   reason = "質問文を確認して同じ項目と判断"
#' )
#' breakfast$variables
#' saveRDS(breakfast, "breakfast_concept.rds")
#' }
create_naaa_concept <- function(
    name,
    variable_id,
    reason = NULL,
    concept_id = name
) {
  .naaa_concept_scalar(name, "name")
  .naaa_concept_scalar(concept_id, "concept_id")
  if (!is.character(variable_id) || length(variable_id) == 0L ||
      anyNA(variable_id) ||
      any(!nzchar(trimws(variable_id, whitespace = "[\\h\\v]")))) {
    stop("'variable_id' must contain one or more non-empty, non-missing IDs.",
         call. = FALSE)
  }
  if (anyDuplicated(variable_id)) {
    stop("'variable_id' must not contain duplicate IDs.", call. = FALSE)
  }
  variable_id <- as.character(variable_id)
  n <- length(variable_id)
  if (is.null(reason)) {
    reasons <- rep(NA_character_, n)
  } else if (!is.character(reason) || !length(reason) %in% c(1L, n)) {
    stop("'reason' must be NULL, one character value, or one value per ID.",
         call. = FALSE)
  } else {
    reasons <- if (length(reason) == 1L) rep(as.character(reason), n) else {
      as.character(reason)
    }
  }

  catalog <- .naaa_catalog()
  index <- match(variable_id, catalog[["variable_id"]])
  if (anyNA(index)) {
    stop("Unknown 'variable_id': ", paste(variable_id[is.na(index)], collapse = ", "),
         call. = FALSE)
  }
  selected <- catalog[index, , drop = FALSE]
  categories <- selected[["category_code"]]
  years <- selected[["year_era"]]
  if (!is.character(categories) || length(categories) != n ||
      anyNA(categories) ||
      any(!nzchar(trimws(categories, whitespace = "[\\h\\v]")))) {
    stop("The selected variables need a non-missing metadata category.",
         call. = FALSE)
  }
  if (length(unique(categories)) != 1L) {
    stop("All selected variables must belong to the same category.",
         call. = FALSE)
  }
  if (!is.character(years) || length(years) != n || anyNA(years) ||
      any(!nzchar(trimws(years, whitespace = "[\\h\\v]")))) {
    stop("The selected variables need a non-missing metadata year.",
         call. = FALSE)
  }
  if (anyDuplicated(selected[["dataset_id"]])) {
    stop("Select at most one variable from each dataset for a concept.",
         call. = FALSE)
  }

  catalog_version <- .naaa_catalog_version(catalog)
  .naaa_concept_scalar(catalog_version, "catalog_version")
  variables <- data.frame(
    variable_id = variable_id,
    reason = reasons,
    year = unname(years),
    choice_status = .naaa_choice_status(.naaa_catalog_choices(catalog, index)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  structure(list(
    concept_id = as.character(concept_id),
    name = as.character(name),
    category_code = unname(categories[[1L]]),
    catalog_version = as.character(catalog_version),
    reference_variable_id = variable_id[[1L]],
    variables = variables
  ), class = "naaa_concept")
}

.naaa_concept_scalar <- function(value, argument) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value, whitespace = "[\\h\\v]"))) {
    stop("'", argument, "' must be a single non-empty, non-missing string.",
         call. = FALSE)
  }
  invisible(value)
}

#' @export
print.naaa_concept <- function(x, ...) {
  cat("NAAA concept: ", x$name, "\n", sep = "")
  cat("ID: ", x$concept_id, "\n", sep = "")
  cat("Category: ", x$category_code, "\n", sep = "")
  cat("Catalog version: ", x$catalog_version, "\n", sep = "")
  cat("Reference variable: ", x$reference_variable_id, "\n", sep = "")
  print(x$variables, ...)
  invisible(x)
}
