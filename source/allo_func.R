# --- Allometric selection & carbon stock -------------------------------------

#' Compute per-tree AGB/Carbon/CO2e using a chosen allometric equation
#'
#' Methods supported (D = DBH cm, H = height m, ρ/WD/G = wood density g/cm^3
#' - "G" and "WD" in the source papers are the same quantity as ρ here):
#'
#'  Mangrove:
#'  - "Komiyama2005_Mangrove"      : AGB = 0.251 * ρ * D^2.46                          (kg)
#'  - "Chave2005_Mangrove"         : AGB = ρ * exp(-1.349 + 1.980 ln D + 0.207 ln^2 D
#'                                          - 0.0281 ln^3 D)                           (kg)
#'  Peat:
#'  - "Manuri2014_Peat_D"          : AGB = 0.136 * D^2.513                             (kg)
#'  - "Manuri2014_Peat_DWD"        : AGB = 0.242 * D^2.473 * WD^0.736                  (kg)
#'  - "Manuri2014_Peat_DH"         : AGB = 0.081 * D^2.049 * H^0.672                   (kg)
#'  - "Manuri2014_Peat_DWDH"       : AGB = 0.150 * D^2.095 * WD^0.664 * H^0.552        (kg)
#'  - "Widyasari2010_Peat"         : AGB = 0.153108 * D^2.40                           (kg)
#'  - "Istomo2002_Peat_Power"      : AGB = 0.1886 * D^2.3702                           (kg)
#'  - "Istomo2006_Peat_Polynomial" : AGB = 0.0145*D^3 - 0.4659*D^2 + 30.64*D - 263.32   (kg)
#'  Mineral:
#'  - "Manuri2017_DG1"             : AGB = 0.171 * D^2.564 * G^0.909                   (kg)
#'  - "Manuri2017_DGH1"            : AGB = 0.088 * (D^2 * G * H)^0.954                 (kg)
#'  - "Manuri2017_DG2_W"           : AGB = 0.167 * D^2.560 * G^0.889 (Sumatra-Kalimantan)
#'  - "Manuri2017_DG2_M"           : AGB = 0.151 * D^2.560 * G^0.889 (Java/Bali/NT/Sulawesi/Maluku)
#'  - "Manuri2017_DG2_E"           : AGB = 0.206 * D^2.560 * G^0.889 (Papua)
#'  - "Basuki2009_DWD"             : ln(AGB) = -0.744 + 2.188 ln D + 0.832 ln WD       (kg)
#'  - "Ketterings2001"             : AGB = 0.11 * ρ * D^2.62                           (kg)
#'  - "Chave2014_DWDH"             : AGB = 0.0673 * (ρ D^2 H)^0.976                    (kg)
#'  - "Chave2005_Moist_DWD"        : AGB = ρ * exp(-1.499 + 2.148 ln D + 0.207 ln^2 D
#'                                          - 0.0281 ln^3 D)                           (kg)
#'  - "Brown1997_Moist"            : AGB = exp(-2.134 + 2.530 ln D)                    (kg)
#'
#' Assumes DBH in cm; Height in m; wood_density (ρ/WD/G) in g/cm^3.
calc_AGB <- function(
    data,
    method = c("Komiyama2005_Mangrove","Chave2005_Mangrove",
               "Manuri2014_Peat_D","Manuri2014_Peat_DWD","Manuri2014_Peat_DH","Manuri2014_Peat_DWDH",
               "Widyasari2010_Peat","Istomo2002_Peat_Power","Istomo2006_Peat_Polynomial",
               "Manuri2017_DG1","Manuri2017_DGH1","Manuri2017_DG2_W","Manuri2017_DG2_M","Manuri2017_DG2_E",
               "Basuki2009_DWD","Ketterings2001","Chave2014_DWDH","Chave2005_Moist_DWD","Brown1997_Moist"),
    dbh_col = "DBH",
    h_col   = "Total.Height",
    rho_col = "wood_density",
    carbon_frac = 0.47
) {
  method <- match.arg(method)

  if (!dbh_col %in% names(data)) stop("Column '", dbh_col, "' not found.")
  needs_H   <- method %in% c("Manuri2014_Peat_DH","Manuri2014_Peat_DWDH",
                             "Manuri2017_DGH1","Chave2014_DWDH")
  needs_rho <- method %in% c("Komiyama2005_Mangrove","Chave2005_Mangrove",
                             "Manuri2014_Peat_DWD","Manuri2014_Peat_DWDH",
                             "Manuri2017_DG1","Manuri2017_DGH1",
                             "Manuri2017_DG2_W","Manuri2017_DG2_M","Manuri2017_DG2_E",
                             "Basuki2009_DWD","Ketterings2001","Chave2014_DWDH","Chave2005_Moist_DWD")
  if (needs_H && !h_col %in% names(data))
    stop(method, " requires '", h_col, "'.")
  if (needs_rho && !rho_col %in% names(data))
    stop(method, " requires '", rho_col, "'. Run attach_wood_density() first.")

  df  <- data
  D   <- as.numeric(df[[dbh_col]])
  H   <- if (needs_H) as.numeric(df[[h_col]]) else NA_real_
  rho <- if (needs_rho) as.numeric(df[[rho_col]]) else NA_real_

  # log(D) and log(WD), used by the ln-based equations (Chave/Basuki/Brown)
  lnD  <- ifelse(D > 0, log(D), NA_real_)
  lnWD <- if (needs_rho) ifelse(rho > 0, log(rho), NA_real_) else NA_real_

  # AGB in kg
  AGB_kg <- switch(
    method,
    # Mangrove
    Komiyama2005_Mangrove      = 0.251 * rho * (D^2.46),
    Chave2005_Mangrove         = exp(-1.349 + 1.980*lnD + 0.207*(lnD^2) - 0.0281*(lnD^3)) * rho,
    # Peat
    Manuri2014_Peat_D          = 0.136 * (D^2.513),
    Manuri2014_Peat_DWD        = 0.242 * (D^2.473) * (rho^0.736),
    Manuri2014_Peat_DH         = 0.081 * (D^2.049) * (H^0.672),
    Manuri2014_Peat_DWDH       = 0.150 * (D^2.095) * (rho^0.664) * (H^0.552),
    Widyasari2010_Peat         = 0.153108 * (D^2.40),
    Istomo2002_Peat_Power      = 0.1886 * (D^2.3702),
    Istomo2006_Peat_Polynomial = (0.0145 * (D^3)) - (0.4659 * (D^2)) + (30.64 * D) - 263.32,
    # Mineral
    Manuri2017_DG1        = 0.171 * (D^2.564) * (rho^0.909),
    Manuri2017_DGH1       = 0.088 * ( (D^2) * rho * H )^0.954,
    Manuri2017_DG2_W      = 0.167 * (D^2.560) * (rho^0.889),
    Manuri2017_DG2_M      = 0.151 * (D^2.560) * (rho^0.889),
    Manuri2017_DG2_E      = 0.206 * (D^2.560) * (rho^0.889),
    Basuki2009_DWD        = exp(-0.744 + 2.188*lnD + 0.832*lnWD),
    Ketterings2001        = 0.11 * rho * (D^2.62),
    Chave2014_DWDH        = 0.0673 * (rho * (D^2) * H)^0.976,
    Chave2005_Moist_DWD   = exp(-1.499 + 2.148*lnD + 0.207*(lnD^2) - 0.0281*(lnD^3)) * rho,
    Brown1997_Moist       = exp(-2.134 + 2.530*lnD)
  )

  # Guardrails
  AGB_kg[D <= 0] <- NA_real_
  if (needs_H) AGB_kg[H <= 0] <- NA_real_
  # ln(D)-based equations need a valid, finite log(D)
  if (method %in% c("Chave2005_Mangrove","Chave2005_Moist_DWD","Basuki2009_DWD","Brown1997_Moist"))
    AGB_kg[!is.finite(lnD)] <- NA_real_
  # Basuki2009 also needs a valid, finite log(wood density)
  if (method == "Basuki2009_DWD") AGB_kg[!is.finite(lnWD)] <- NA_real_
  # polynomial dips below zero at small/large D
  if (method == "Istomo2006_Peat_Polynomial") AGB_kg[AGB_kg < 0] <- NA_real_

  AGB_Mg    <- AGB_kg / 1000
  Carbon_Mg <- AGB_Mg * carbon_frac
  CO2e_Mg   <- Carbon_Mg * (44/12)

  dplyr::mutate(df,
                AGB_kg    = AGB_kg,
                AGB_Mg    = AGB_Mg,
                Carbon_Mg = Carbon_Mg,
                CO2e_Mg   = CO2e_Mg,
                .method   = method
  )
}

#' Run calc_AGB() once per method and stack the results (long format)
#'
#' Used by the "Compare Allometric Equations" selector on the Carbon Stock
#' tab so up to 5 equations can be plotted/compared side by side.
calc_AGB_compare <- function(
    data,
    methods = c("Komiyama2005_Mangrove","Chave2005_Mangrove",
                "Manuri2014_Peat_D","Manuri2014_Peat_DWD","Manuri2014_Peat_DH","Manuri2014_Peat_DWDH",
                "Widyasari2010_Peat","Istomo2002_Peat_Power","Istomo2006_Peat_Polynomial",
                "Manuri2017_DG1","Manuri2017_DGH1","Manuri2017_DG2_W","Manuri2017_DG2_M","Manuri2017_DG2_E",
                "Basuki2009_DWD","Ketterings2001","Chave2014_DWDH","Chave2005_Moist_DWD","Brown1997_Moist"),
    dbh_col = "DBH",
    h_col   = "Total.Height",
    rho_col = "wood_density",
    carbon_frac = 0.47
) {
  dplyr::bind_rows(lapply(
    methods,
    function(m) calc_AGB(data, method = m, dbh_col = dbh_col, h_col = h_col,
                         rho_col = rho_col, carbon_frac = carbon_frac)
  ))
}

#' Summarize AGB/Carbon/CO2e to totals and per-hectare values by group.
#'
#' NOT CURRENTLY CALLED from server.R - the app computes its per-plot/
#' per-stratum summaries a different way (see compute_stratum_stats() in
#' server.R). Kept here as a ready-to-use alternative grouped summary if
#' that's ever needed; safe to delete if it stays unused.
summarize_carbon <- function(
    data,
    area_ha,
    group_vars = c("Landscape","Site","Transect","Plot.Name","LULC",".method")
) {
  if (!all(c("AGB_Mg","Carbon_Mg","CO2e_Mg") %in% names(data)))
    stop("Run calc_AGB() first; missing AGB_Mg/Carbon_Mg/CO2e_Mg.")
  stopifnot(is.numeric(area_ha), length(area_ha) == 1, is.finite(area_ha), area_ha > 0)

  keep <- group_vars[group_vars %in% names(data)]
  syms <- rlang::syms(keep)

  data |>
    dplyr::group_by(!!!syms) |>
    dplyr::summarise(
      Trees            = dplyr::n(),
      AGB_Mg_total     = sum(AGB_Mg, na.rm = TRUE),
      Carbon_Mg_total  = sum(Carbon_Mg, na.rm = TRUE),
      CO2e_Mg_total    = sum(CO2e_Mg, na.rm = TRUE),
      AGB_Mg_per_ha    = AGB_Mg_total / area_ha,
      Carbon_Mg_per_ha = Carbon_Mg_total / area_ha,
      CO2e_Mg_per_ha   = CO2e_Mg_total / area_ha,
      .groups = "drop"
    )
}

#' Expand per-tree AGB (kg) to AGB per hectare using the nested-plot DBH-class
#' expansion factors (A x4, B x25, C x100), based on the DBH.G column added
#' during Flora Data Processing. Called right after calc_AGB() when building
#' the final AGC-by-plot table.
add_agb_tpha <- function(data, agb_kg_col = "AGB_kg") {
  stopifnot("DBH.G" %in% names(data), agb_kg_col %in% names(data))
  dplyr::mutate(
    data,
    `AGB(ton/ha)` = dplyr::case_when(
      DBH.G == "A" ~ 4   * .data[[agb_kg_col]] / 1000,
      DBH.G == "B" ~ 25  * .data[[agb_kg_col]] / 1000,
      DBH.G == "C" ~ 100 * .data[[agb_kg_col]] / 1000,
      TRUE         ~ NA_real_
    )
  )
}

