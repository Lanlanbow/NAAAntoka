#' NAAAのCSVファイルをUTF-8へ変換して別フォルダに保存する
#'
#' CSVファイルをUTF-8へ変換します。ファイル全体が正しいUTF-8なら
#' 内容をそのままコピーし、それ以外はCP932として変換を試みます。
#'対応する文字コードはUTF-8とCP932です。
#'
#' macOSでは`iconv`、WindowsではPowerShellの.NET機能を使用します。
#' 必要なコマンドが見つからなければ、エラーで停止します。自動インストールは行いません。
#'
#' @param input 変換するCSVファイルのパス。
#' @param out_dir 変換後のCSVを保存するフォルダ。入力元とは別のフォルダを
#'   指定してください。存在しなければ作成します。
#' @param os `"auto"`（既定）、`"mac"`、`"win"`。
#'   `"auto"`は実行環境を判定します。実行環境と異なるOSは指定できません。
#' @param overwrite 既存の出力を置き換えるか。既定は`FALSE`です。
#'
#' @return 保存したCSVの絶対パスを不可視で返します。
#' @export
#'
#' @examples
#' \dontrun{
#' convert_utf8("raw/H28_児童.csv", "utf8")
#' # 既存の変換結果を明示的に置き換える
#' convert_utf8("raw/H28_児童.csv", "utf8", overwrite = TRUE)
#' }
convert_utf8 <- function(
    input,
    out_dir,
    os = "auto",
    overwrite = FALSE
) {
  .naaa_check_path_argument(input, "input")
  .naaa_check_path_argument(out_dir, "out_dir")
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }
  resolved_os <- .naaa_resolve_os(os)

  input <- path.expand(input)
  if (!file.exists(input) || dir.exists(input)) {
    stop("'input' must be an existing single CSV file: ", input, call. = FALSE)
  }
  output_name <- basename(input)
  input <- normalizePath(input, winslash = "/", mustWork = TRUE)
  if (file.access(input, mode = 4L) != 0L) {
    stop("The input file is not readable: ", input, call. = FALSE)
  }

  out_dir <- path.expand(out_dir)
  if (.naaa_path_exists(out_dir) && !dir.exists(out_dir)) {
    stop("'out_dir' must be a directory, not a file or broken link.",
         call. = FALSE)
  }
  if (!dir.exists(out_dir)) {
    created <- dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(created) && !dir.exists(out_dir)) {
      stop("Could not create the output directory: ", out_dir, call. = FALSE)
    }
  }
  out_dir <- normalizePath(out_dir, winslash = "/", mustWork = TRUE)
  if (.naaa_same_path(out_dir, dirname(input))) {
    stop("'out_dir' must be different from the input directory.", call. = FALSE)
  }

  output <- file.path(out_dir, output_name)
  if (.naaa_same_path(output, input)) {
    stop("The output path resolves to the input file; choose another directory.",
         call. = FALSE)
  }
  if (dir.exists(output)) {
    stop("The output path is an existing directory: ", output, call. = FALSE)
  }
  if (.naaa_path_exists(output) && !overwrite) {
    stop("Output already exists; use overwrite = TRUE to replace it: ", output,
         call. = FALSE)
  }

  temporary <- tempfile(pattern = ".naaa-utf8-", tmpdir = out_dir)
  on.exit(unlink(temporary), add = TRUE)
  if (!file.create(temporary, showWarnings = FALSE)) {
    stop("Could not create a temporary file in the output directory.",
         call. = FALSE)
  }
  # Restrict permissions before writing respondent data on POSIX filesystems.
  if (resolved_os == "mac" && !Sys.chmod(temporary, mode = "0600")) {
    stop("Could not set permissions on the temporary output file.",
         call. = FALSE)
  }
  source_encoding <- .naaa_convert_csv_file(input, temporary, resolved_os)

  # Recheck before publishing: never directly write through an existing link.
  if (.naaa_same_path(output, input)) {
    stop("The output path now resolves to the input file.", call. = FALSE)
  }
  if (dir.exists(output)) {
    stop("The output path is an existing directory: ", output, call. = FALSE)
  }
  if (.naaa_path_exists(output) && !overwrite) {
    stop("Output already exists; the existing file was not changed: ", output,
         call. = FALSE)
  }
  published <- suppressWarnings(file.rename(temporary, output))
  if (!isTRUE(published)) {
    stop("Could not move the completed CSV into place; the existing output ",
         "was not changed: ", output, call. = FALSE)
  }

  message(if (identical(source_encoding, "UTF-8")) {
    paste0("Copied UTF-8 CSV: ", output)
  } else {
    paste0("Converted CP932 CSV to UTF-8: ", output)
  })
  invisible(output)
}

.naaa_check_path_argument <- function(path, argument) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(trimws(path))) {
    stop("'", argument, "' must be a single non-empty file-system path.",
         call. = FALSE)
  }
}

.naaa_resolve_os <- function(os) {
  if (!is.character(os) || length(os) != 1L || is.na(os) ||
      !os %in% c("auto", "mac", "win")) {
    stop("'os' must be 'auto', 'mac', or 'win'.", call. = FALSE)
  }
  actual <- if (.Platform$OS.type == "windows") {
    "win"
  } else if (identical(unname(Sys.info()[["sysname"]]), "Darwin")) {
    "mac"
  } else {
    stop("This function currently supports macOS and Windows only.",
         call. = FALSE)
  }
  if (os != "auto" && os != actual) {
    stop("The specified OS does not match this computer; use os = 'auto'.",
         call. = FALSE)
  }
  actual
}

.naaa_path_exists <- function(path) {
  link <- suppressWarnings(Sys.readlink(path))
  file.exists(path) || (!is.na(link) && nzchar(link))
}

.naaa_same_path <- function(first, second) {
  normalize <- function(path) {
    tolower(suppressWarnings(normalizePath(
      path, winslash = "/", mustWork = FALSE
    )))
  }
  identical(normalize(first), normalize(second))
}
