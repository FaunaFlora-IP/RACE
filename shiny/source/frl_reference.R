## FRL (Forest Reference Level) 2nd - Indonesia national AGB/AGC means per
## stratum. Displayed as-is in the "AGC Reference (Indonesia FRL)" card on
## the Carbon Stock tab (filterable by Category), for the user to compare
## their own calculated AGC-per-stratum results against.
frl_stratum_ref <- data.frame(
  Stratum = c(
    "Primary dryland forest", "Primary swamp forest", "Primary mangrove forest",
    "Secondary swamp forest", "Secondary dryland forest", "Plantation forest",
    "Mixed dry agriculture", "Dry shrub", "Secondary mangrove forest", "Estate crop",
    "Wet shrub", "Pure dry agriculture", "Transmigration areas", "Paddy field",
    "Savanna and grasses", "Bare ground", "Settlement", "Open water",
    "Fish pond / aquaculture", "Port and harbour", "Mining areas", "Open swamps"
  ),
  # Indonesia's standard (KLHK) land-cover grouping: the 7 forest classes vs.
  # everything else, used to tell forested vs. non-forested strata apart at a glance
  Category = c(
    "Forest", "Forest", "Forest",
    "Forest", "Forest", "Forest",
    "Non-forest", "Non-forest", "Forest", "Non-forest",
    "Non-forest", "Non-forest", "Non-forest", "Non-forest",
    "Non-forest", "Non-forest", "Non-forest", "Non-forest",
    "Non-forest", "Non-forest", "Non-forest", "Non-forest"
  ),
  `AGB mean (Mg d.m. ha-1)` = c(
    291.24, 248.80, 236.17, 204.61, 204.10, 161.23, 137.52, 128.49, 118.02, 102.35,
    41.15, 29.95, 29.95, 21.27, 8.64, 5.11, 4.61, 0, 0, 0, 0, 0
  ),
  `AGC mean (Mg C ha-1)` = c(
    136.88, 116.94, 111.00, 96.17, 95.93, 75.78, 64.63, 60.39, 55.47, 48.10,
    19.34, 14.08, 14.08, 10.00, 4.06, 2.40, 2.17, 0, 0, 0, 0, 0
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
