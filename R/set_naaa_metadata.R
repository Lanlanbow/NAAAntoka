#' 使用するメタデータのフォルダを指定する
#'
#' `metadata_long.csv`が入っているフォルダを指定します。
#' 相対パスは指定時点の作業フォルダを基準に絶対パスへ変換する
#' ため、その後に作業フォルダを変更しても選択したメタデータは変わりません。
#'
#' 検索・concept作成・データ作成の前に実行してください。
#' 未指定の場合は、作業フォルダや同梱CSVを自動で選択せずエラーにします。
#' CSV変換だけを使う場合は、この設定は不要です。
#'
#' @param path `metadata_long.csv`が入ったフォルダのパス。CSVファイル自体
#'   ではなく、フォルダを指定します。相対パスと絶対パスの両方に対応します。
#'
#' @return 設定したフォルダの絶対パスを不可視で返します。
#' @export
#'
#' @examples
#' \dontrun{
#' library(NAAAntoka)
#' set_naaa_metadata("meta")
#' vars <- search_naaa_variables("朝食")
#' }
set_naaa_metadata <- function(path) {
  if (!is.character(path) || length(path) != 1L || !is.null(dim(path)) || is.na(path) ||
      !nzchar(trimws(path, whitespace = "[\\h\\v]"))) {
    stop("'path' must be a single non-empty folder path.", call. = FALSE)
  }
  directory <- path.expand(path)
  if (!dir.exists(directory)) {
    .naaa_metadata_not_found(file.path(directory, "metadata_long.csv"))
  }
  if (file.access(directory, mode = 4L) != 0L ||
      file.access(directory, mode = 1L) != 0L) {
    stop("The metadata folder must be readable: ", directory, call. = FALSE)
  }
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  candidate <- file.path(directory, "metadata_long.csv")
  if (!.naaa_catalog_path_present(candidate)) {
    .naaa_metadata_not_found(candidate)
  }
  candidate <- .naaa_readable_catalog_path(candidate)
  .naaa_catalog_at(candidate)
  options(NAAAntoka.metadata_dir = directory)
  invisible(directory)
}
