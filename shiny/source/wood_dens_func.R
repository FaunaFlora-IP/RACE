#' Attach a wood density (g/cm^3) to every tree in FI_set, needed by several
#' allometric equations in allo_func.R.
#'
#' Looks up each tree's density via BIOMASS::getWoodDensity(), then falls
#' back in this priority order when no exact match is found:
#'  1. species-level match from BIOMASS
#'  2. family mean, computed here from this dataset's own species-level rows
#'  3. genus-level match from BIOMASS
#'  4. default_rho, for anything still missing (or with no Taxon.Rank)
#' Adds `wood_density` (the value used) and `wd_level` (which of the above
#' it came from) columns. Called from the Carbon Stock tab's "Weight
#' Density" step, before any AGB calculation.
attach_wood_density <- function(
    FI_set,
    default_rho = 0.57
) {
  stopifnot(is.data.frame(FI_set))
  
  df <- FI_set %>%
    dplyr::mutate(
      Scientific.Name = stringr::str_squish(Scientific.Name),
      Genus = if ("Genus" %in% names(.)) Genus else stringr::word(Scientific.Name, 1),
      Species_raw = stringr::str_remove(Scientific.Name, paste0("^", Genus, "\\s*")),
      Species_clean = dplyr::case_when(
        Taxon.Rank == "Species" &
          stringr::str_detect(Species_raw, "^[A-Za-z-]+(\\s+[A-Za-z-]+)?$") ~ stringr::word(Species_raw, 1),
        TRUE ~ NA_character_
      )
    )
  
  # Run BIOMASS once to get species/genus info + family tags
  wd_res <- BIOMASS::getWoodDensity(genus = df$Genus, species = df$Species_clean)
  
  wd_mean   <- suppressWarnings(as.numeric(wd_res$meanWD))
  wd_level  <- wd_res$levelWD           # "species" / "genus" / NA
  fam_tag   <- wd_res$family
  
  # Ensure Family present; prefer user's Family if given, else BIOMASS family
  if (!"Family" %in% names(df)) df$Family <- fam_tag
  df$Family <- dplyr::coalesce(df$Family, fam_tag)
  
  # === Build family means using SPECIES-level densities only ==================
  wd_species_only <- ifelse(wd_level == "species", wd_mean, NA_real_)
  fam_means_tbl <- dplyr::tibble(Family = df$Family, wd_species_only = wd_species_only) %>%
    dplyr::filter(!is.na(Family), !is.na(wd_species_only)) %>%
    dplyr::group_by(Family) %>%
    dplyr::summarise(family_meanWD = mean(wd_species_only, na.rm = TRUE), .groups = "drop")
  
  # Make a fast lookup for family mean
  fam_vec <- stats::setNames(fam_means_tbl$family_meanWD, fam_means_tbl$Family)
  
  # === Initialize outputs =====================================================
  final_wd   <- rep(NA_real_, nrow(df))
  final_level <- rep(NA_character_, nrow(df))
  
  # 0) If Taxon.Rank is empty/NA/blank -> DEFAULT directly (as requested)
  rank_blank <- is.na(df$Taxon.Rank) | trimws(df$Taxon.Rank) == ""
  final_wd[rank_blank]    <- default_rho
  final_level[rank_blank] <- "default"
  
  # 1) SPECIES exact (highest priority)
  use_species <- !rank_blank & (wd_level == "species") & !is.na(wd_mean)
  final_wd[use_species]    <- wd_mean[use_species]
  final_level[use_species] <- "species"
  
  # 2) FAMILY mean (computed from species-only rows), for rows still NA
  has_family_mean <- !is.na(df$Family) & !is.na(fam_vec[df$Family])
  use_family <- !rank_blank & is.na(final_wd) & has_family_mean
  final_wd[use_family]    <- fam_vec[df$Family[use_family]]
  final_level[use_family] <- "family"
  
  # 3) GENUS fallback (only now), for rows still NA and with BIOMASS genus result
  use_genus <- !rank_blank & is.na(final_wd) & (wd_level == "genus") & !is.na(wd_mean)
  final_wd[use_genus]    <- wd_mean[use_genus]
  final_level[use_genus] <- "genus"
  
  # 4) DEFAULT for anything still NA
  still_na <- is.na(final_wd)
  if (any(still_na)) {
    final_wd[still_na]    <- default_rho
    final_level[still_na] <- "default"
  }
  
  # Return clean table
  df %>%
    dplyr::mutate(
      wood_density = final_wd,
      wd_level     = final_level
    ) %>%
    dplyr::select(-Species_raw, -Species_clean)
}
