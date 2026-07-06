save_manifest <- function(
    board,
    current_dates,
    dados_bronze_refreshed,
    dados_silver_refreshed,
    dados_gold_refreshed,
    previous_manifest = NULL,
    commit = NULL,
    source = "internal",
    manifest_pin_name = "manifest",
    object_pin_names = c(
      dados_bronze_refreshed = "dados_bronze",
      dados_silver_refreshed = "dados_silver",
      dados_gold_refreshed   = "dados_gold"
    ),
    comparison = c("identical", "all.equal")
) {

  comparison <- base::match.arg(comparison)

  if (is.null(commit)) {
    commit <- readline(prompt = "Enter manifest commit hash or description: ")

    if (identical(commit, "")) {
      stop("Commit is required. Aborting.", call. = FALSE)
    }
  }

  if (!all(names(object_pin_names) %in% c(
    "dados_bronze_refreshed",
    "dados_silver_refreshed",
    "dados_gold_refreshed"
  ))) {
    stop(
      "`object_pin_names` must be a named character vector with names: ",
      "dados_bronze_refreshed, dados_silver_refreshed, dados_gold_refreshed.",
      call. = FALSE
    )
  }

  target_objects <- list(
    dados_bronze_refreshed = dados_bronze_refreshed,
    dados_silver_refreshed = dados_silver_refreshed,
    dados_gold_refreshed   = dados_gold_refreshed
  )

  versions_table <- purrr::imap_dfr(
    object_pin_names,
    function(pin_name, object_name) {
      check_latest_pin_matches_target(
        board         = board,
        object_name   = object_name,
        pin_name      = pin_name,
        target_object = target_objects[[object_name]],
        comparison    = comparison
      )
    }
  )

  git_hash <- tryCatch(
    get_git_hash(short = TRUE),
    error = function(e) NA_character_
  )

  manifest_refreshed <- list(
    source            = source,
    commit_msg        = commit,
    commit_hash       = digest::digest(commit),
    git_hash          = git_hash,
    current_dates     = sort(unique(as.Date(current_dates))),
    current_date_min  = min(as.Date(current_dates)),
    current_date_max  = max(as.Date(current_dates)),
    versions_table    = versions_table,
    previous_manifest = previous_manifest,
    created_at        = Sys.time()
  )

  ## Save it to  data/prod/manifests/manifest
  max_current_date <- as.Date(max(current_dates))

  year  <- lubridate::year(max_current_date)
  month <- lubridate::month(max_current_date)
  day   <- lubridate::day(max_current_date)

  manifest_dir <- here::here("data", "prod", "manifests")

  ## Create dir if it does not exist yet
  if (!dir.exists(manifest_dir)) {
    dir.create(
      path = manifest_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  manifest_path <- here::here(
    "data", "prod", "manifests",
    paste0(
      "manifest_",
      year,
      "_",
      stringr::str_pad(month, width = 2, pad = "0"),
      "_",
      stringr::str_pad(day, width = 2, pad = "0"),
      ".rds"
    )
  )

  saveRDS(
    object = manifest_refreshed,
    file = manifest_path
  )

  manifest_refreshed

  message("Manifest saved locally: ", manifest_path)
}

#Helpers-----------------------------------------------------------------------
check_latest_pin_matches_target <- function(
    board,
    object_name,
    pin_name,
    target_object,
    comparison = c("identical", "all.equal")
) {

  comparison <- base::match.arg(comparison)

  message("\nChecking object: ", object_name)
  message("Expected pin: ", pin_name)

  latest_version <- get_latest_pin_version(
    board    = board,
    pin_name = pin_name
  )

  latest_pinned_object <- pins::pin_read(
    board   = board,
    name    = pin_name,
    version = latest_version
  )

  objects_match <- switch(
    comparison,
    identical = base::identical(latest_pinned_object, target_object),
    all.equal = isTRUE(base::all.equal(latest_pinned_object, target_object))
  )

  if (!objects_match) {

    message("Pinned object does not match target object: ", object_name)

    if (rlang::is_installed("waldo")) {
      diffs <- waldo::compare(
        latest_pinned_object,
        target_object,
        max_diffs = 20
      )

      print(diffs)
    } else {
      print(base::all.equal(latest_pinned_object, target_object))
    }

    stop(
      "Latest pinned version does not match the targets-store object for: ",
      object_name,
      call. = FALSE
    )
  }

  message("Latest pinned version matches target object for: ", object_name)

  data.frame(
    object_name        = object_name,
    pin_name           = pin_name,
    latest_pin_version = latest_version,
    object_digest      = digest::digest(target_object),
    checked_at         = as.character(Sys.time()),
    stringsAsFactors   = FALSE
  )
}
