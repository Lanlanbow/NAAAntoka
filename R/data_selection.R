# Resolve concept and raw-variable selections without reading respondent data.
.naaa_resolve_selection <- function(
    catalog,
    concepts = NULL,
    variable_id = NULL,
    variables = NULL
) {
  has_concepts <- !is.null(concepts)
  has_ids <- !is.null(variable_id)
  has_variables <- !is.null(variables)
  if (!has_concepts && !has_ids && !has_variables) {
    stop("Specify at least one of 'concepts', 'variable_id', or 'variables'.",
         call. = FALSE)
  }
  if (has_ids && has_variables) {
    stop("Specify only one of 'variable_id' and 'variables'; either may be ",
         "combined with 'concepts'.", call. = FALSE)
  }
  .naaa_validate_catalog(catalog)

  concept_mapping <- if (has_concepts) {
    .naaa_selection_concepts(catalog, concepts)
  } else NULL
  raw_mapping <- NULL
  if (has_ids) {
    ids <- .naaa_selection_ids(variable_id)
    index <- match(ids, catalog[["variable_id"]])
    if (anyNA(index)) {
      stop("Unknown 'variable_id': ", paste(ids[is.na(index)], collapse = ", "),
           call. = FALSE)
    }
    raw_mapping <- .naaa_selection_mapping(catalog, index)
  } else if (has_variables) {
    index <- .naaa_selection_rows(catalog, variables)
    raw_mapping <- .naaa_selection_mapping(catalog, index)
  }
  concept_names <- if (has_concepts) unique(concept_mapping[["output_name"]]) else {
    character()
  }
  raw_names <- if (!is.null(raw_mapping)) unique(raw_mapping[["output_name"]]) else {
    character()
  }
  mode <- if (has_concepts && !is.null(raw_mapping)) "mixed" else if (has_concepts) {
    "concepts"
  } else "raw"
  mapping <- if (identical(mode, "mixed")) {
    rbind(concept_mapping, raw_mapping)
  } else if (has_concepts) concept_mapping else raw_mapping

  if (length(unique(mapping[["category_code"]])) != 1L) {
    stop("All selected variables must belong to the same category.",
         call. = FALSE)
  }
  reserved <- c("dataset_id", "category_code", "year_era", "year_ad", "source_row")
  conflicts <- intersect(unique(mapping[["output_name"]]), reserved)
  if (length(conflicts)) {
    stop("Selected output names conflict with reserved metadata columns: ",
         paste(conflicts, collapse = ", "), call. = FALSE)
  }
  overlaps <- intersect(concept_names, raw_names)
  if (length(overlaps)) {
    stop("Concept and raw-variable output names overlap: ",
         paste(overlaps, collapse = ", "),
         ". Use distinct concept IDs or remove the overlapping raw selection.",
         call. = FALSE)
  }
  rownames(mapping) <- NULL
  list(
    mapping = mapping,
    output_names = unique(mapping[["output_name"]]),
    category_code = mapping[["category_code"]][[1L]],
    mode = mode,
    default_years = sort(unique(mapping[["year_ad"]])),
    concept_names = concept_names,
    raw_names = raw_names
  )
}

.naaa_selection_ids <- function(ids, argument = "variable_id") {
  if (!is.character(ids) || length(ids) == 0L || anyNA(ids) ||
      any(!nzchar(trimws(ids, whitespace = "[\\h\\v]")))) {
    stop("'", argument, "' must contain non-empty, non-missing character IDs.",
         call. = FALSE)
  }
  if (anyDuplicated(ids)) {
    stop("'", argument, "' must not contain duplicate IDs.", call. = FALSE)
  }
  unname(as.character(ids))
}

.naaa_selection_mapping <- function(catalog, index) {
  columns <- c(
    "dataset_id", "variable_id", "variable_name", "category_code", "year_era",
    "year_ad"
  )
  mapping <- as.data.frame(catalog[index, columns, drop = FALSE],
                           stringsAsFactors = FALSE)
  for (column in columns) {
    value <- mapping[[column]]
    if (!is.character(value) || anyNA(value) ||
        any(!nzchar(trimws(value, whitespace = "[\\h\\v]")))) {
      stop("Selected metadata has a missing or invalid '", column, "'.",
           call. = FALSE)
    }
  }
  years <- .naaa_parse_years(mapping[["year_ad"]], "metadata year_ad")
  era_years <- .naaa_parse_years(mapping[["year_era"]], "metadata year_era")
  if (!identical(unname(years), unname(era_years))) {
    stop("Selected metadata has inconsistent year_ad and year_era values.",
         call. = FALSE)
  }
  mapping[["year_ad"]] <- unname(years)
  mapping[["output_name"]] <- mapping[["variable_name"]]
  rownames(mapping) <- NULL
  mapping
}

.naaa_selection_concepts <- function(catalog, concepts) {
  if (inherits(concepts, "naaa_concept")) {
    concepts <- list(concepts)
  }
  if (!is.list(concepts) || length(concepts) == 0L) {
    stop("'concepts' must be a concept or a non-empty list of concepts.",
         call. = FALSE)
  }
  version <- .naaa_catalog_version(catalog)
  .naaa_concept_scalar(version, "current catalog version")
  concept_ids <- character(length(concepts))
  mappings <- vector("list", length(concepts))
  for (i in seq_along(concepts)) {
    concept <- concepts[[i]]
    if (!inherits(concept, "naaa_concept") || !is.list(concept)) {
      stop("Every entry of 'concepts' must be a naaa_concept object.",
           call. = FALSE)
    }
    required <- c(
      "concept_id", "name", "category_code", "catalog_version",
      "reference_variable_id", "variables"
    )
    if (!all(required %in% names(concept)) || anyDuplicated(names(concept))) {
      stop("The concept object is incomplete or has duplicate fields.",
           call. = FALSE)
    }
    for (field in setdiff(required, "variables")) {
      .naaa_concept_scalar(concept[[field]], paste0("concept ", field))
    }
    if (!identical(concept[["catalog_version"]], version)) {
      stop("Concept '", concept[["concept_id"]], "' uses catalog version '",
           concept[["catalog_version"]], "', not the current version '",
           version, "'. Review and recreate the concept with the current ",
           "catalog before reading data.", call. = FALSE)
    }
    members <- concept[["variables"]]
    member_columns <- c("variable_id", "reason", "year", "choice_status")
    if (!is.data.frame(members) || nrow(members) == 0L ||
        anyDuplicated(names(members)) ||
        !all(member_columns %in% names(members)) ||
        !is.character(members[["reason"]]) ||
        !is.character(members[["year"]])) {
      stop("The concept's variables table is incomplete or invalid.",
           call. = FALSE)
    }
    ids <- .naaa_selection_ids(members[["variable_id"]], "concept variable_id")
    if (!concept[["reference_variable_id"]] %in% ids) {
      stop("The concept's reference variable is not one of its selected IDs.",
           call. = FALSE)
    }
    index <- match(ids, catalog[["variable_id"]])
    if (anyNA(index)) {
      stop("Concept contains unknown variable IDs: ",
           paste(ids[is.na(index)], collapse = ", "), call. = FALSE)
    }
    mapping <- .naaa_selection_mapping(catalog, index)
    if (length(unique(mapping[["category_code"]])) != 1L ||
        !identical(mapping[["category_code"]][[1L]],
                   concept[["category_code"]])) {
      stop("The concept category does not match its current variable metadata.",
           call. = FALSE)
    }
    if (anyDuplicated(mapping[["dataset_id"]])) {
      stop("A concept may select at most one variable from each dataset.",
           call. = FALSE)
    }
    if (!identical(unname(members[["year"]]), mapping[["year_era"]])) {
      stop("The concept years do not match the current variable metadata.",
           call. = FALSE)
    }
    concept_ids[[i]] <- concept[["concept_id"]]
    mapping[["output_name"]] <- rep(concept_ids[[i]], nrow(mapping))
    mappings[[i]] <- mapping
  }
  if (anyDuplicated(concept_ids)) {
    stop("Selected concepts must have distinct concept IDs.", call. = FALSE)
  }
  mapping <- do.call(rbind, mappings)
  rownames(mapping) <- NULL
  mapping
}

.naaa_selection_rows <- function(catalog, variables) {
  if (!is.data.frame(variables) || nrow(variables) == 0L ||
      anyDuplicated(names(variables)) ||
      !"variable_name" %in% names(variables)) {
    stop("'variables' must be a non-empty table with variable_name, year, ",
         "and category_code or dataset_id columns.", call. = FALSE)
  }
  year_columns <- intersect(c("year_era", "year_ad"), names(variables))
  key_columns <- intersect(c("category_code", "dataset_id"), names(variables))
  if (!length(year_columns) || !length(key_columns)) {
    stop("'variables' needs year_era or year_ad, and category_code or dataset_id.",
         call. = FALSE)
  }
  for (column in c("variable_name", key_columns)) {
    value <- variables[[column]]
    if (!is.character(value) || anyNA(value) ||
        any(!nzchar(trimws(value, whitespace = "[\\h\\v]")))) {
      stop("'variables$", column, "' must contain non-empty character values.",
           call. = FALSE)
    }
  }
  years <- .naaa_parse_years(variables[[year_columns[[1L]]]],
                            paste0("variables$", year_columns[[1L]]))
  if (length(year_columns) == 2L) {
    other_years <- .naaa_parse_years(variables[[year_columns[[2L]]]],
                                    paste0("variables$", year_columns[[2L]]))
    if (!identical(unname(years), unname(other_years))) {
      stop("'variables' has inconsistent year_era and year_ad values.",
           call. = FALSE)
    }
  }
  index <- integer(nrow(variables))
  for (i in seq_len(nrow(variables))) {
    keep <- catalog[["variable_name"]] == variables[["variable_name"]][[i]]
    for (column in key_columns) {
      keep <- keep & !is.na(catalog[[column]]) &
        catalog[[column]] == variables[[column]][[i]]
    }
    candidates <- which(keep)
    if (length(candidates)) {
      candidate_years <- .naaa_parse_years(
        catalog[["year_ad"]][candidates], "matching metadata year_ad"
      )
      candidates <- candidates[candidate_years == years[[i]]]
    }
    if (length(candidates) == 0L) {
      stop("No matching variable for row ", i, " of 'variables': ",
           variables[["variable_name"]][[i]], ".", call. = FALSE)
    }
    if (length(candidates) != 1L) {
      stop("Ambiguous variable at row ", i,
           " of 'variables'; specify dataset_id to select one dataset.",
           call. = FALSE)
    }
    index[[i]] <- candidates[[1L]]
  }
  if (anyDuplicated(catalog[["variable_id"]][index])) {
    stop("'variables' selects the same variable more than once.", call. = FALSE)
  }
  index
}
