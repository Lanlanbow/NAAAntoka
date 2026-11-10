# Run tidyselect's text helpers against metadata rows. Each proxy column is one
# source variable, so Boolean/exclusion operations act on whole variables even
# when separate conditions match separate label/name fields.
.naaa_mapping_search_helpers <- function() {
  c("contains", "matches", "starts_with", "ends_with")
}

.naaa_mapping_search_helper_name <- function(expression) {
  if (!is.call(expression)) return(NULL)
  head <- expression[[1L]]
  helpers <- .naaa_mapping_search_helpers()
  if (is.symbol(head) && as.character(head) %in% helpers) {
    return(as.character(head))
  }
  if (is.call(head) && length(head) == 3L &&
      identical(head[[1L]], as.name("::")) &&
      is.symbol(head[[2L]]) &&
      as.character(head[[2L]]) %in% c("tidyselect", "dplyr") &&
      is.symbol(head[[3L]]) && as.character(head[[3L]]) %in% helpers) {
    return(as.character(head[[3L]]))
  }
  NULL
}

.naaa_mapping_has_search_helper <- function(expression) {
  if (!is.null(.naaa_mapping_search_helper_name(expression))) return(TRUE)
  if (!is.call(expression)) return(FALSE)
  arguments <- as.list(expression)[-1L]
  any(vapply(arguments, .naaa_mapping_has_search_helper, logical(1L)))
}

.naaa_mapping_rewrite_search <- function(
    expression, selection = TRUE, positive = TRUE, register = NULL
) {
  helper <- .naaa_mapping_search_helper_name(expression)
  if (!is.null(helper)) {
    # Private names bypass tidyselect's own helper mask; leave helper arguments
    # untouched so patterns, ignore.case/perl and caller expressions keep their
    # usual R meaning.
    expression[[1L]] <- as.name(if (is.null(register)) {
      paste0(".naaa_search_", helper)
    } else register(helper, positive))
    return(expression)
  }
  if (!is.call(expression)) {
    if (selection && (is.character(expression) || is.symbol(expression))) {
      name <- if (is.null(register)) ".naaa_search_contains" else {
        register("contains", positive)
      }
      return(as.call(list(as.name(name), expression)))
    }
    return(expression)
  }
  head <- expression[[1L]]
  operators <- c("c", "&", "|", "!", "-", "(")
  arguments_are_selections <- is.symbol(head) && as.character(head) %in% operators
  arguments <- as.list(expression)[-1L]
  negate <- is.symbol(head) && as.character(head) %in% c("!", "-") &&
    length(arguments) == 1L
  for (i in seq_along(arguments)) {
    arguments[[i]] <- .naaa_mapping_rewrite_search(
      arguments[[i]], selection = arguments_are_selections,
      positive = if (negate) !positive else positive, register = register
    )
  }
  as.call(c(list(head), arguments))
}

.naaa_mapping_helper_positions <- function(catalog, helper, ...) {
  arguments <- list(...)
  if ("vars" %in% names(arguments)) {
    stop("The search helper's 'vars' argument is supplied by NAAAntoka; ",
         "do not set it manually.", call. = FALSE)
  }
  search <- getExportedValue("tidyselect", helper)
  result <- integer()
  for (column in c("variable_name", "label_raw", "label_effective")) {
    values <- catalog[[column]]
    present <- which(!is.na(values))
    warning_message <- character()
    index <- withCallingHandlers(
      do.call(search, c(arguments, list(vars = values[present]))),
      warning = function(condition) {
        warning_message <<- c(warning_message, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    )
    if (length(warning_message)) {
      stop("Could not apply ", helper, "(): ",
           paste(unique(warning_message), collapse = "; "), call. = FALSE)
    }
    result <- union(result, present[index])
  }
  result
}

.naaa_validate_search_keywords <- function(keywords, argument = "variable filter") {
  if (!is.character(keywords) || !is.null(dim(keywords)) ||
      !length(keywords) || anyNA(keywords) ||
      any(!nzchar(trimws(keywords, whitespace = "[\\h\\v]")))) {
    stop("'", argument, "' must contain non-empty keyword strings or ",
         "a contains()/matches()/starts_with()/ends_with() expression.",
         call. = FALSE)
  }
  invisible(keywords)
}

.naaa_mapping_literal_search <- function(catalog, keywords) {
  .naaa_validate_search_keywords(keywords)
  fold <- function(x) chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)
  matched <- rep(FALSE, nrow(catalog))
  for (column in c("variable_name", "label_raw", "label_effective")) {
    values <- catalog[[column]]
    present <- !is.na(values)
    text <- fold(values[present])
    for (keyword in unique(fold(keywords))) {
      matched[present] <- matched[present] | grepl(keyword, text, fixed = TRUE)
    }
  }
  matched
}

.naaa_mapping_search_group_ranks <- function(groups, n) {
  # Rows without a positive search term (e.g. an all-negative selection) form
  # one final group. Empty or overlapping terms never duplicate source rows.
  rank <- rep.int(length(groups) + 1L, n)
  assigned <- rep(FALSE, n)
  for (i in seq_along(groups)) {
    rows <- groups[[i]]
    rows <- rows[!assigned[rows]]
    rank[rows] <- i
    assigned[rows] <- TRUE
  }
  rank
}

.naaa_mapping_helper_groups <- function(catalog, helper, arguments) {
  search <- getExportedValue("tidyselect", helper)
  # All arguments have already been evaluated and accepted by the helper.
  # Match formal names without evaluating caller expressions a second time.
  call <- as.call(c(list(as.name(helper)), arguments))
  bound <- as.list(match.call(definition = search, call = call))[-1L]
  patterns <- bound[["match"]]
  lapply(seq_along(patterns), function(i) {
    pattern <- patterns[i]
    attributes(pattern) <- attributes(patterns)[
      !names(attributes(patterns)) %in% c("names", "dim", "dimnames")
    ]
    current <- bound
    current[["match"]] <- pattern
    do.call(.naaa_mapping_helper_positions,
            c(list(catalog = catalog, helper = helper), current))
  })
}

.naaa_mapping_search <- function(catalog, expression, details = FALSE) {
  query <- rlang::quo_get_expr(expression)
  if (!.naaa_mapping_has_search_helper(query)) {
    keywords <- rlang::eval_tidy(expression)
    matched <- .naaa_mapping_literal_search(catalog, keywords)
    if (!details) return(matched)
    groups <- lapply(seq_along(keywords), function(i) {
      which(.naaa_mapping_literal_search(catalog, keywords[[i]]))
    })
    return(list(matched = matched, group_rank =
                  .naaa_mapping_search_group_ranks(groups, nrow(catalog))))
  }
  context <- new.env(parent = rlang::quo_get_env(expression))
  grouped_calls <- list()
  register <- NULL
  if (details) {
    register <- function(helper, positive) {
      id <- length(grouped_calls) + 1L
      grouped_calls[id] <<- list(list())
      name <- paste0(".naaa_search_term_", id)
      wrapper <- function(...) {
        arguments <- list(...)
        selected <- do.call(.naaa_mapping_helper_positions,
                           c(list(catalog = catalog, helper = helper), arguments))
        if (positive) {
          grouped_calls[id] <<- list(.naaa_mapping_helper_groups(
            catalog, helper, arguments
          ))
        }
        selected
      }
      assign(name, wrapper, envir = context)
      name
    }
  } else {
    for (helper in .naaa_mapping_search_helpers()) {
      wrapper <- local({
        current_helper <- helper
        function(...) .naaa_mapping_helper_positions(catalog, current_helper, ...)
      })
      assign(paste0(".naaa_search_", helper), wrapper, envir = context)
    }
  }
  query <- .naaa_mapping_rewrite_search(query, register = register)
  proxy <- stats::setNames(rep(list(character()), nrow(catalog)), catalog$variable_id)
  selected <- tidyselect::eval_select(
    rlang::new_quosure(query, context), proxy,
    allow_rename = FALSE, allow_predicates = FALSE
  )
  matched <- rep(FALSE, nrow(catalog))
  matched[unname(selected)] <- TRUE
  if (!details) return(matched)
  groups <- unlist(grouped_calls, recursive = FALSE)
  list(matched = matched, group_rank =
         .naaa_mapping_search_group_ranks(groups, nrow(catalog)))
}
