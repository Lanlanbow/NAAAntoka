#' キーワードでNAAAの変数を検索する
#'
#' [set_naaa_metadata()]で指定したメタデータのxlsxファイルから
#' 変数名・原ラベル・有効ラベルを検索します。
#' [make_variable_mapping()]と同じ検索条件を使えます。`contains()`は
#' 文字どおりの部分一致、`matches()`は正規表現、`starts_with()`と
#' `ends_with()`は前方・後方一致です。各条件は変数名・原ラベル・有効ラベルの
#' いずれかに一致した元変数を選びます。`c()`や`|`でOR、`&`でAND、
#' `!`で除外を組み合わせ、元変数単位で判定します。
#' ヘルパーは名前だけでも`tidyselect::contains()`などの形式でも使えます。
#' 既定では英字の大文字小文字を区別しません。
#'
#' 同じラベルでも、年度やデータセットが異なる変数は別々の行として返します。
#' 検索結果は使用するメタデータファイルの順序を維持します。
#'
#' @param keyword `contains("朝食")`・`matches("朝食.*毎日")`などの検索条件、
#'   または1つ以上の検索語を持つ文字列ベクトル。文字列指定では空文字、
#'   空白だけの文字列、`NA`は指定できません。
#'
#' @return 次の9列を持つ`data.frame`：`dataset_id`、`variable_name`、
#'   `year_era`、`year_ad`、`category_code`、`variable_id`、`label_raw`、
#'   `label_effective`、`choices`。最初の8列は文字列です。`choices`は
#'   各変数のコードと値ラベルを文字列の`code`・`label`列に持つ
#'   `data.frame`のリスト列です。無回答・非該当なども含め、コードの
#'   先頭0や値ラベルの句読点を保持します。選択肢情報がない場合は
#'   0行の表になります。1変数につき1行で、該当する変数がない場合も
#'   同じ列構成の0行の表を返します。
#' @export
#'
#' @examples
#' \dontrun{
#' set_naaa_metadata("meta")
#' vars <- search_naaa_variables(contains("朝食"))
#' vars_regex <- search_naaa_variables(matches("朝食.*毎日"))
#' vars_multiple <- search_naaa_variables(
#'   c(contains("朝食"), matches("寝る|起きる"))
#' )
#' head(vars)
#' }
search_naaa_variables <- function(keyword) {
  query <- rlang::enquo(keyword)
  if (!.naaa_mapping_has_search_helper(rlang::quo_get_expr(query))) {
    keywords <- rlang::eval_tidy(query)
    .naaa_validate_search_keywords(keywords, "keyword")
    # Evaluate a caller's literal keyword expression once, then reuse its value.
    query <- rlang::new_quosure(keywords, rlang::empty_env())
  }

  catalog <- .naaa_catalog()
  result_columns <- c(
    "dataset_id", "variable_name", "year_era", "year_ad", "category_code",
    "variable_id", "label_raw", "label_effective"
  )
  matched <- .naaa_mapping_search(catalog, query)

  result <- catalog[matched, result_columns, drop = FALSE]
  result[["choices"]] <- .naaa_catalog_choices(catalog, which(matched))
  rownames(result) <- NULL
  result
}
