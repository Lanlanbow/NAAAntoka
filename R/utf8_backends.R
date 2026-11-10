# Internal conversion backends. The public wrapper owns output transactions.
.naaa_convert_csv_file <- function(input, output, os) {
  .naaa_check_csv_bytes(input)
  if (identical(os, "mac")) {
    return(.naaa_convert_csv_mac(input, output))
  }
  if (identical(os, "win")) {
    return(.naaa_convert_csv_win(input, output))
  }
  stop("Internal error: conversion OS must be 'mac' or 'win'.", call. = FALSE)
}

# UTF-16/32 and NUL-containing input is outside the supported CSV encodings.
# Scan the entire file, not a prefix: an ASCII header says nothing about later
# bytes, and accepting NULs could silently mistake UTF-16 for CP932.
.naaa_check_csv_bytes <- function(input) {
  con <- file(input, open = "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "raw", n = 4L)
  prefix <- as.integer(bytes)
  utf16 <- length(prefix) >= 2L &&
    (identical(prefix[1:2], c(255L, 254L)) ||
       identical(prefix[1:2], c(254L, 255L)))
  utf32 <- length(prefix) == 4L &&
    identical(prefix, c(0L, 0L, 254L, 255L))
  if (utf16 || utf32) {
    stop("UTF-16/UTF-32 input is not supported; expected UTF-8 or CP932.",
         call. = FALSE)
  }
  repeat {
    if (any(bytes == as.raw(0L))) {
      stop("Input contains NUL bytes; expected a UTF-8 or CP932 CSV file.",
           call. = FALSE)
    }
    bytes <- readBin(con, what = "raw", n = 1048576L)
    if (length(bytes) == 0L) break
  }
  invisible(NULL)
}

.naaa_convert_csv_mac <- function(input, output) {
  command <- unname(Sys.which("iconv"))
  if (!nzchar(command)) {
    stop("The 'iconv' command is required for conversion on macOS.",
         call. = FALSE)
  }
  diagnostic <- tempfile("naaa-iconv-", fileext = ".log")
  on.exit(unlink(diagnostic), add = TRUE)
  run_iconv <- function(from) {
    suppressWarnings(system2(
      command,
      args = c("-f", from, "-t", "UTF-8", shQuote(input)),
      stdout = output,
      stderr = diagnostic
    ))
  }

  # With identical source/target encodings, iconv validates the whole file and
  # preserves its bytes, including a UTF-8 BOM and original line endings.
  status <- run_iconv("UTF-8")
  if (identical(status, 0L)) return("UTF-8")

  # Re-running system2 truncates only the new temporary output. There is no -c
  # or transliteration flag: invalid or incomplete input must fail.
  status <- run_iconv("CP932")
  if (!identical(status, 0L)) {
    detail <- if (file.exists(diagnostic)) {
      paste(readLines(diagnostic, warn = FALSE), collapse = " ")
    } else {
      ""
    }
    stop(paste0(
      "Input could not be converted as UTF-8 or CP932",
      if (nzchar(detail)) paste0(": ", detail) else "."
    ), call. = FALSE)
  }
  "CP932"
}

.naaa_convert_csv_win <- function(input, output) {
  commands <- Sys.which(c("powershell", "pwsh"))
  commands <- unname(commands[nzchar(commands)])
  if (length(commands) == 0L) {
    stop("PowerShell is required for conversion on Windows.", call. = FALSE)
  }
  script <- tempfile("naaa-convert-", fileext = ".ps1")
  result <- tempfile("naaa-convert-", fileext = ".txt")
  diagnostic <- tempfile("naaa-convert-", fileext = ".log")
  on.exit(unlink(c(script, result, diagnostic)), add = TRUE)
  writeLines(.naaa_powershell_conversion_script(), script, useBytes = TRUE)
  args <- c(
    "-NoLogo", "-NoProfile", "-NonInteractive", "-ExecutionPolicy", "Bypass",
    "-File", shQuote(script, type = "cmd"),
    "-InputPath", shQuote(input, type = "cmd"),
    "-OutputPath", shQuote(output, type = "cmd")
  )
  status <- suppressWarnings(system2(
    commands[[1L]], args = args, stdout = result, stderr = diagnostic
  ))
  if (!identical(status, 0L)) {
    detail <- if (file.exists(diagnostic)) {
      paste(readLines(diagnostic, warn = FALSE), collapse = " ")
    } else {
      ""
    }
    stop(paste0(
      "PowerShell could not convert the input as UTF-8 or CP932",
      if (nzchar(detail)) paste0(": ", detail) else "."
    ), call. = FALSE)
  }
  source_encoding <- trimws(readLines(result, warn = FALSE))
  source_encoding <- source_encoding[nzchar(source_encoding)]
  if (length(source_encoding) != 1L ||
      !source_encoding %in% c("UTF-8", "CP932") || !file.exists(output)) {
    stop("PowerShell did not return a valid conversion result.", call. = FALSE)
  }
  source_encoding
}

# All source is ASCII so Windows PowerShell 5.1 does not depend on a script BOM.
# Paths are command-line parameters, never interpolated as PowerShell source.
.naaa_powershell_conversion_script <- function() {
  c(
    "#requires -Version 5.0",
    "param([string]$InputPath, [string]$OutputPath)",
    "$ErrorActionPreference = 'Stop'",
    "$strictUtf8 = [System.Text.UTF8Encoding]::new($false, $true)",
    "function Test-StrictUtf8([string]$path) {",
    "  $reader = $null",
    "  try {",
    "    $reader = [System.IO.StreamReader]::new($path, $strictUtf8, $false, 1048576)",
    "    $buffer = New-Object char[] 1048576",
    "    while ($reader.Read($buffer, 0, $buffer.Length) -gt 0) { }",
    "    return $true",
    "  } catch {",
    "    $cause = $_.Exception",
    "    while ($null -ne $cause.InnerException) { $cause = $cause.InnerException }",
    "    if ($cause -is [System.Text.DecoderFallbackException]) { return $false }",
    "    throw",
    "  } finally {",
    "    if ($null -ne $reader) { $reader.Dispose() }",
    "  }",
    "}",
    "$reader = $null",
    "$writer = $null",
    "try {",
    "  if (Test-StrictUtf8 $InputPath) {",
    "    [System.IO.File]::Copy($InputPath, $OutputPath, $true)",
    "    [Console]::Out.WriteLine('UTF-8')",
    "  } else {",
    "    $providerType = [Type]::GetType('System.Text.CodePagesEncodingProvider, System.Text.Encoding.CodePages')",
    "    if ($null -ne $providerType) {",
    "      $provider = $providerType.GetProperty('Instance').GetValue($null, $null)",
    "      [System.Text.Encoding]::RegisterProvider($provider)",
    "    }",
    "    $encoderFallback = [System.Text.EncoderExceptionFallback]::new()",
    "    $decoderFallback = [System.Text.DecoderExceptionFallback]::new()",
    "    $sourceEncoding = [System.Text.Encoding]::GetEncoding(932, $encoderFallback, $decoderFallback)",
    "    $reader = [System.IO.StreamReader]::new($InputPath, $sourceEncoding, $false, 1048576)",
    "    $writer = [System.IO.StreamWriter]::new($OutputPath, $false, $strictUtf8, 1048576)",
    "    $buffer = New-Object char[] 1048576",
    "    while (($count = $reader.Read($buffer, 0, $buffer.Length)) -gt 0) {",
    "      $writer.Write($buffer, 0, $count)",
    "    }",
    "    $writer.Dispose()",
    "    $writer = $null",
    "    [Console]::Out.WriteLine('CP932')",
    "  }",
    "} catch {",
    "  [Console]::Error.WriteLine($_.Exception.Message)",
    "  exit 1",
    "} finally {",
    "  if ($null -ne $writer) { $writer.Dispose() }",
    "  if ($null -ne $reader) { $reader.Dispose() }",
    "}",
    "exit 0"
  )
}
