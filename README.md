# RACE — Rapid Assessment for Carbon Stock and Wildlife Ecology

RACE is an R Shiny application for processing raw field-survey data from fauna transects and vegetation plots. It is developed and maintained by Fauna & Flora's Indonesia Programme.

## What it does

RACE is organized into two parallel workflows, each a sequence of tabs meant to be worked through top to bottom (later steps build on the output of earlier ones).

### Fauna workflow

1.  **Data Processing** — Upload a transect CSV and map its columns (Transect, Scientific Name, Taxon Rank, plus Observation Type and Individual Count for bird surveys). Species names are validated against the live GBIF taxonomic backbone, with a dataset-quality report (`data.validator`) before you proceed.
2.  **Results** — Species richness, abundance, and diversity indices (Shannon, Simpson, Margalef, Evenness) per transect; a Chao1 richness estimate; an iNEXT species-accumulation curve; and hierarchical clustering/dissimilarity between transects.
3.  **Conservation Status** — Upload the validated species list exported from Data Processing to batch-query IUCN Red List status, CITES Appendix, and PP106 protection flags (processed automatically in batches of up to 50 species), reviewed in a results table and a taxonomic-composition treemap.

### Flora workflow

1.  **Data Processing** — Upload a vegetation plot CSV (Transect, Plot ID, Tree ID, Scientific Name, Taxon Rank, size Class A/B/C, Girth, Tree Height), validate species names via GBIF, and review the DBH distribution and DBH-vs-height relationship.
2.  **Results Vegetation** — Richness/abundance per transect, clustering/dissimilarity between transects, and the Importance Value Index (IVI) by size class.
3.  **Carbon Stock Estimation** — Compare up to 5 allometric equations (19 built in, covering mangrove, peat, and mineral-soil forest types) against a built-in reference table, attach wood density (via `BIOMASS::getWoodDensity()`, with a manual-revision path), pick a final equation, upload a stratum file, then calculate aboveground biomass (AGB) and aboveground carbon (AGC) at the plot and stratum level. Results are checked with Q-Q/normality diagnostics, an editable AGC-by-plot table, and compared against Indonesia's national FRL reference table.
4.  **Conservation Status** — Same IUCN/CITES/PP106 lookup as Fauna, using the species list exported from the Flora Data Processing step.

## Getting started

### Prerequisites

- R (developed on R 4.6.1)
- The packages listed under **Dependencies** below

### Running the app

From the project root:

``` r
source("app.R")
```

This sources the shared IUCN/CITES/GBIF helper functions and launches the app via `shiny::runApp("./shiny/")`.

## Project structure

```         
Rapid_ffn/
├── app.R                    # Entry point
├── example/                 # Example input CSVs for Fauna/Flora
├── source/                  # Reference material + the top-level copy of
│                             # iucn_code.R (loaded by app.R before runApp)
└── shiny/
    ├── ui.R                 # UI layout 
    ├── server.R              # Server logic
    ├── source/               # Helper functions/data sourced by ui.R:
    │   ├── iucn_code.R        #   GBIF/IUCN/CITES lookup functions
    │   ├── allo_func.R         #   Allometric AGB/AGC calculation
    │   ├── wood_dens_func.R     #   Wood density attachment
    │   ├── allometric_ref.R      #   Allometric equation reference table
    │   └── frl_reference.R        #   Indonesia FRL national reference table
    └── www/                  # Static assets
```

## Data requirements

| Workflow | Required columns |
|------------------------------------|------------------------------------|
| Fauna | Transect, Scientific Name, Taxon Rank (+ Observation Type, Individual Count for Avifauna) |
| Flora | Transect, Plot ID, Tree ID, Scientific Name, Taxon Rank, size Class (A/B/C), Girth (DBH), Tree Height |
| Carbon Stock — Stratum file | A CSV with a Plot ID column and a Stratum column (user-mapped) |

See `example/` for sample CSVs in the expected format.

## Dependencies

Shiny/dashboard: `shiny`, `bs4Dash`, `shinybusy`

Tables & visualization: `DT`, `treemap`, `tidyquant`, `ggdist`, `qqplotr`

Data wrangling: `tidyverse`, `assertr`, `data.validator`

Biodiversity analysis: `iNEXT`, `SpadeR`, `vegan`, `BiodiversityR`

Taxonomy & conservation status: `rgbif`, `iucnredlist`, `rcites`

Carbon stock: `BIOMASS`, `moments`, `mgcv`

## Citation

Fauna & Flora's Indonesia Programme. (2026). *RACE: Rapid Assessment for Carbon Stock and Wildlife Ecology* [Software]. *(Suggested citation — update with a DOI or formal reference if one is registered.)*
