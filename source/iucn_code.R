# Preparation----
# This file is sourced by both app.R (top-level source/iucn_code.R, before
# runApp starts) and ui.R (shiny/source/iucn_code.R). It provides the three
# API-lookup functions shared by the Fauna and Flora Conservation Status
# tabs, plus the GBIF validation helper used by the Data Processing tabs.

## Load library----
library(shiny)
library(iucnredlist)
library(rcites)
library(purrr)

## Set up token----
# IUCN and CITES API tokens - replace these if they expire or need rotating.
api <- init_api("T3SzxqvSwGgAQjfGuWK8tWVaNq361o1dcSoT")
# CITES
set_token("kUydW4HMDXY9AvDFSThxMwtt")

# Set argument for the functions

# IUCN: looks up Red List status + common name for each genus/species pair
# in species_df (needs `genus` and `species` columns). Called in batches of
# up to 50 species from server.R's conservation-status handlers.
get_iucn_species_data <- function(api, species_df, wait_time = 0.5) {
  
  # Safely wrap assessments_by_name
  safe_assessments <- safely(function(genus, species) {
    assessments_by_name(api, genus = genus, species = species)
  }, otherwise = NULL)
  
  # Safely get assessments
  iucn_as_list <- pmap(
    list(species_df$genus, species_df$species),
    ~ {
      Sys.sleep(wait_time)
      result <- safe_assessments(..1, ..2)
      if (is.null(result$result)) {
        tibble(
          genus = ..1,
          species = ..2,
          assessment_id = NA,
          latest = NA,
          scopes_code = NA,
          status = "Not found"
        )
      } else {
        result$result %>%
          mutate(genus = ..1, species = ..2)
      }
    }
  )
  
  # Combine all results
  iucn_as <- bind_rows(iucn_as_list)
  
  # Filter for latest global assessments only
  iucn_as_latest <- iucn_as %>%
    filter(!is.na(assessment_id), latest == TRUE, scopes_code == 1)
  
  # Safely download full assessment data
  safe_assessment_data <- safely(assessment_data_many)
  full_data_result <- safe_assessment_data(
    api,
    unique(iucn_as_latest$assessment_id),
    wait_time = wait_time
  )
  
  full_data <- full_data_result$result
  if (is.null(full_data)) full_data <- list()
  
  # Extract elements (if available)
  full_taxon <- extract_element(full_data, "taxon")
  full_rlc <- extract_element(full_data, "red_list_category")
  common_name <- extract_element(full_data, "taxon_common_names") %>%
    filter(main == TRUE, language == "eng")
  
  # Join and clean
  final_species_df <- full_taxon %>%
    inner_join(full_rlc, by = "assessment_id") %>%
    inner_join(common_name, by = "assessment_id") %>%
    rename(
      Species = scientific_name,
      Class = class_name,
      Order = order_name,
      Family = family_name,
      Status = code,
      `Common name` = name
    )
  
  # Handle "not found" cases only if such column exists
  if ("status" %in% colnames(iucn_as) && any(iucn_as$status == "Not found")) {
    not_found_df <- iucn_as %>%
      filter(status == "Not found") %>%
      transmute(
        Species = paste(genus, species),
        Class = NA,
        Order = NA,
        Family = NA,
        Status = "Not found",
        `Common name` = NA
      )
    final_species_df <- bind_rows(final_species_df, not_found_df)
  }
  
  return(final_species_df)
}


# GBIF: validates a vector of user-submitted species names against the
# GBIF taxonomic backbone, returning a suggested/canonical name and
# matchType (EXACT/FUZZY/VARIANT/HIGHERRANK/NONE) for each. Used by the
# Species Validation step in both Fauna and Flora Data Processing.
#
# "Status: 0 - try lower bucket_size or larger sleep" is GBIF's own rate-
# limit error, thrown by name_backbone_checklist() when requests come in
# too large or too fast - which is exactly what happens if a user re-runs
# validation several times in a row (each run is its own burst of calls;
# GBIF sees the cumulative rate, not just this one run). batch_size/
# wait_time below are deliberately conservative per GBIF's own suggestion,
# and each batch now retries with exponential backoff instead of crashing
# the whole Species Validation table on a single transient failure - a
# batch that still fails after every retry is marked matchType = "NONE" /
# status = "GBIF lookup failed (rate-limited or offline)" for its species
# rather than silently dropping them, so the user can tell those rows need
# a re-run instead of mistaking them for a real "not found".
gbif_checklist_batched <- function(species_names, batch_size = 25, wait_time = 1.5,
                                    max_retries = 3) {

  batch_id <- ceiling(seq_along(species_names) / batch_size)
  batches <- split(species_names, batch_id)

  results <- lapply(batches, function(batch) {
    for (attempt in seq_len(max_retries)) {
      Sys.sleep(wait_time * attempt)  # back off a little more each retry
      res <- tryCatch(
        name_backbone_checklist(batch),
        error = function(e) {
          message("GBIF batch failed (attempt ", attempt, "/", max_retries, "): ",
                   conditionMessage(e))
          NULL
        }
      )
      if (!is.null(res)) return(res)
    }

    # every retry failed - return a placeholder row per species instead of
    # dropping them or crashing the caller
    tibble(
      verbatim_name = batch,
      scientificName = NA_character_,
      canonicalName  = NA_character_,
      rank           = NA_character_,
      status         = "GBIF lookup failed (rate-limited or offline)",
      matchType      = "NONE",
      class          = NA_character_,
      order          = NA_character_,
      family         = NA_character_,
      genus          = NA_character_,
      species        = NA_character_
    )
  })

  dplyr::bind_rows(results)
}


# CITES: looks up the CITES Appendix (I/II/III) for each species in
# speciesList. Species with no CITES listing (or a failed lookup) get
# CITES_Appendix = NA rather than an error, so one bad species never stops
# the batch - the caller turns NA into "Not identified" for display.
retrieve_CITES_data <- function(speciesList) {
  
  get_cites_status <- function(sp) {

    res <- tryCatch(
      spp_taxonconcept(query_taxon = sp, raw = TRUE),
      error = function(e) {
        message(sp, " ----- CITES lookup failed: ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(res) || length(res) == 0 || is.null(res[[1]]$cites_listing)) {
      message(sp, " ----- CHECK (not found or no CITES listing)")
      tibble(Species = sp, CITES_Appendix = NA_character_)

    } else {
      tibble(
        Species = res[[1]]$full_name,
        CITES_Appendix = res[[1]]$cites_listing
      )
    }
  }
  
  purrr::map_dfr(speciesList, get_cites_status)
}


# Indonesia's protected-species list (PP106), joined onto the IUCN/CITES
# result in server.R to add the Protected/Endemic/Migratory columns.
urlfile <- 'https://raw.githubusercontent.com/ryanavri/GetTaxonCS/main/PSG_v3.csv'
db <- read.csv(urlfile)