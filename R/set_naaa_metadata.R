#' 使用するメタデータのxlsxファイルまたはフォルダを指定する
#'
#' メタデータのxlsxファイルが入ったフォルダ、またはxlsxファイルを指定します。
#' ファイル名は任意です。フォルダ内のxlsxが1つなら、そのファイルを使います。
#' Excelの作業用ファイル（`~$`で始まるもの）は対象外です。
#' xlsxが複数ある場合は、使うファイルのパスを直接指定してください。
#' 相対パスは指定時点の作業フォルダを基準に絶対パスへ変換する
#' ため、その後に作業フォルダを変更しても選択したメタデータは変わりません。
#'
#' 検索・concept作成・データ作成の前に実行してください。
#' 未指定の場合は、作業フォルダや同梱ファイルを自動で選択せずエラーにします。
#' CSV変換だけを使う場合は、この設定は不要です。
#'
#' メタデータが1シートだけならシート名は任意です。複数シートがある場合は
#' `metadata`シートを読みます。1行目を列名とし、コード列名の`01`などは
#' Excel上でも文字列として保存してください。
#'
#' 読み込み・検証済みのメタデータはRセッション内に保持します。
#' 検索などの呼び出し時にファイルの内容が変更されているか確認し、
#' 同じ内容なら再利用します。
#' 内容や指定先が変わった場合は、自動で読み込み・検証し直します。
#' フォルダを指定した場合は、ファイル名の変更も次の呼び出しで反映します。
#'
#' @param path メタデータのxlsxファイル、またはそのファイルが1つ入った
#'   フォルダのパス。相対パスと絶対パスの両方に対応します。
#'
#' @return 設定したフォルダまたはファイルの絶対パスを不可視で返します。
#' @export
#'
#' @examples
#' \dontrun{
#' library(NAAAntoka)
#' set_naaa_metadata("meta")
#' # ファイルを直接指定することもできます。
#' set_naaa_metadata("meta/metadata_long.xlsx")
#' vars <- search_naaa_variables("朝食")
#' }
set_naaa_metadata <- function(path) {
  if (!is.character(path) || length(path) != 1L || !is.null(dim(path)) || is.na(path) ||
      !nzchar(trimws(path, whitespace = "[\\h\\v]"))) {
    stop("'path' must be a single non-empty folder or xlsx file path.", call. = FALSE)
  }
  selected <- path.expand(path)
  if (dir.exists(selected)) {
    selected <- normalizePath(selected, winslash = "/", mustWork = TRUE)
    candidate <- .naaa_find_metadata_file(selected)
  } else {
    if (!.naaa_catalog_path_present(selected)) .naaa_metadata_not_found(selected)
    candidate <- .naaa_readable_catalog_path(selected)
    selected <- candidate
  }
  .naaa_catalog_at(candidate)
  options(NAAAntoka.metadata_dir = selected)
  invisible(selected)
}
