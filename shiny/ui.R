# Library----

# Shiny Framework and Dashboard
library(shiny)
library(bs4Dash)
library(shinybusy)

# UI and Output Formatting
library(DT)            # Interactive tables
library(treemap)       # Treemap visualization
library(tidyquant)
library(ggdist)

# Data Processing and Tidy Helpers
library(tidyverse)     # ggplot2, dplyr, tidyr, etc.
library(assertr)       # not_na()/in_set() predicates used by data.validator checks

# Biodiversity and Ecological Analysis
library(iNEXT)         # Rarefaction and extrapolation
library(SpadeR)        # Species diversity estimation
library(vegan)         # Ecological analysis and ordination
library(BiodiversityR) # importancevalue.comp() - Importance Value Index table

# Data Validation and Taxonomic Standardization
library(rgbif)         # GBIF taxonomic backbone matching
library(data.validator) # Data quality checks

# Carbon Stock Estimation
library(BIOMASS)       # Wood density lookup
library(qqplotr)       # Normal Q-Q plot layers
library(moments)       # Skewness
library(mgcv)          # GAM smoothing

source("./source/iucn_code.R")
source("./source/allo_func.R")
source("./source/wood_dens_func.R")
source("./source/allometric_ref.R")
source("./source/frl_reference.R")

# "Last built" timestamp shown in the footer - the most recent modification
# time across the app's own .R files, so it updates automatically whenever
# ui.R/server.R/source/*.R change rather than needing to be bumped by hand.
# Computed once here (ui.R local scope) at app startup, not per session.
app_build_files <- list.files(".", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
app_build_time <- max(file.info(app_build_files)$mtime, na.rm = TRUE)

# UI----
ui <- dashboardPage(
  title = "RACE",
  fullscreen = TRUE,

  header = dashboardHeader(
    title = "RACE",
    titleWidth = 300
  ),

## Sidebar----
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Fauna", tabName = "fauna", icon = icon("paw"),
               startExpanded = TRUE,
               menuSubItem("Data Processing", tabName = "Fa_dat_proc"),
               menuSubItem("Results", tabName = "Fa_dat_res"),
               menuSubItem("Conservation Status", tabName = "Fa_res_cs")
      ),
      menuItem("Flora", tabName = "flora", icon = icon("leaf"),
               startExpanded = F,
               menuSubItem("Data Processing", tabName = "Flo_dat_proc"),
               menuSubItem("Results Vegetation", tabName = "Flo_dat_veg"),
               menuSubItem("Carbon Stock Estimation", tabName = "Flo_dat_car"),
               menuSubItem("Conservation Status", tabName = "Flo_res_cs")
      )
    )
  ),

## Sidebar Content----
  body = dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper .row {
        display: flex;
        flex-wrap: wrap;
      }
      .content-wrapper .row > div {
        display: flex;
        flex-direction: column;
      }
      .content-wrapper .row > div > .card {
        flex: 1 1 auto;
      }
      .card-body strong, .card-body b {
        font-weight: 700 !important;
      }
    "))),
    tags$script(HTML("
      $(document).on('maximized.lte.cardwidget', function(e) {
        var $plots = $(e.target).closest('.card').find('.shiny-plot-output, .shiny-image-output');
        $plots.each(function() {
          var $el = $(this);
          if ($el.data('orig-height') === undefined) {
            $el.data('orig-height', $el.css('height'));
          }
          $el.css('height', 'calc(100vh - 120px)');
        });
        setTimeout(function() { $(window).trigger('resize'); }, 50);
        setTimeout(function() { $(window).trigger('resize'); }, 350);
      });

      $(document).on('minimized.lte.cardwidget', function(e) {
        var $plots = $(e.target).closest('.card').find('.shiny-plot-output, .shiny-image-output');
        $plots.each(function() {
          var $el = $(this);
          var orig = $el.data('orig-height');
          if (orig !== undefined) { $el.css('height', orig); }
        });
        setTimeout(function() { $(window).trigger('resize'); }, 50);
        setTimeout(function() { $(window).trigger('resize'); }, 350);
      });
    ")),
    tabItems(
      tabItem(tabName = "home",

              fluidRow(
                bs4Card(
                  title = "Welcome to RACE",
                  status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  h4(strong("RACE — Rapid Assessment for Carbon Stock and Wildlife Ecology")),
                  p("A field-data analysis tool for biodiversity assessment, conservation-status screening, vegetation analysis, and carbon-stock estimation."),
                  p("RACE is designed to process raw field-survey data from fauna and vegetation transects. For fauna surveys, it supports species validation and diversity analysis, followed by conservation-status checks against the IUCN Red List, CITES Appendices, and Indonesia’s protected-species list (PP 106)."),
                  p("For vegetation surveys, RACE also supports carbon-stock estimation using a range of allometric equations, allowing users to select equations appropriate to the available field measurements and vegetation type."),
                  p("Use the sidebar to switch between the Fauna and Flora workflows. Each workflow consists of a sequence of tabs designed to be completed from top to bottom, as later steps generally build on the outputs generated in earlier steps.")
                )
              ),

              fluidRow(
                bs4Card(
                  title = "Fauna Workflow",
                  status = "primary",
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  tags$ol(
                    tags$li(strong("Data Processing"), " - Upload your transect CSV and map the relevant columns, including Transect, Scientific Name, Taxon Rank, and, where applicable, Observation Type and Individual Count. RACE then validates scientific names against the GBIF taxonomic backbone. Review the validation results and make any necessary corrections. Once the dataset is ready and you are satisfied with the validation, click Calculate to proceed to the Results tab."),
                    tags$li(strong("Results"), " - Explore the calculated biodiversity metrics, including species richness and a range of diversity indices such as Shannon, Simpson, Margalef, and Evenness. This section also provides species-richness estimation, species accumulation curves, and hierarchical clustering based on dissimilarity among transects."),
                    tags$li(strong("Conservation Status"), " - Use the validated species list from the Data Processing step to retrieve conservation and protection information, including IUCN Red List status, CITES Appendix, and Indonesia’s PP 106 protection status. Results are presented in a summary table together with a taxonomic composition treemap.")
                  )
                ),

                bs4Card(
                  title = "Flora Workflow",
                  status = "info",
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  tags$ol(
                    tags$li(strong("Data Processing"), " - Upload your vegetation plot CSV and map the relevant columns, including Transect, Plot ID, Tree ID, Scientific Name, Taxon Rank, Size Class, Girth, and Tree Height. RACE then validates scientific names against the GBIF taxonomic backbone and provides basic data-quality checks, including the DBH distribution and DBH–height relationship. Review the validation results and make any necessary corrections. Once the dataset is ready and you are satisfied with the validation, click Calculate to proceed to the Results tab."),
                    tags$li(strong("Vegetation Results"), " - Explore the calculated vegetation metrics, including species richness and abundance by transect, hierarchical clustering based on dissimilarity among transects, and the Importance Value Index (IVI) for each vegetation size class."),
                    tags$li(strong("Carbon Stock Estimation"), " - Estimate aboveground biomass and carbon stock using a selection of allometric equations. RACE allows you to compare candidate equations against a built-in reference table, attach species-specific wood-density values, and select the most appropriate equation for the dataset. After assigning vegetation strata, RACE calculates aboveground biomass (AGB) and aboveground carbon (AGC) at the plot and stratum levels. The results can then be reviewed through diagnostic checks and compared with Indonesian national reference values, including tree density and mean weighted carbon stock."),
                    tags$li(strong("Conservation Status"), " - Use the validated flora species list to retrieve conservation and protection information, including IUCN Red List status, CITES Appendix, and Indonesia’s PP 106 protection status. Results are presented in a summary table together with a taxonomic composition treemap.")
                  )
                )
              ),

              fluidRow(
                bs4Card(
                  title = "Before You Start",
                  status = "warning",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  tags$ul(
                    tags$li(strong("Species validation:"), " Scientific names are checked against the live GBIF taxonomic backbone. Review the ", strong("Comparison"), " column (Match / Different / Not Found) before proceeding with the analysis."),
                    tags$li(strong("Conservation-status lookup:"), " IUCN and CITES queries are processed automatically in batches of up to 50 species. Larger species lists may therefore take a little longer to complete."),
                    tags$li(strong("Export and revision:"), " Tables with Copy / CSV / Excel options can be exported and edited outside RACE. Where supported, such as for wood-density data, revised tables can be uploaded again."),
                    tags$li(strong("Re-running analyses:"), " If a result needs adjustment, you usually do not need to restart the workflow. Return to the relevant earlier step, revise the input or settings, and run that step again."),
                    tags$li(strong("Expanded view:"), " Cards with the expand icon (", icon("expand"), ") in the top-right corner can be maximized for easier viewing of large tables and plots.")
                  )
                )
              ),

              fluidRow(
                bs4Card(
                  title = "About & Support",
                  status = "secondary",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  maximizable = TRUE,
                  tags$table(
                    style = "width: 100%;",
                    tags$tr(
                      tags$td(style = "width: 140px; vertical-align: top;", strong("Version")),
                      tags$td("RACE v1.0.0")
                    ),
                    tags$tr(
                      tags$td(style = "vertical-align: top;", strong("Documentation")),
                      # PLACEHOLDER LINK - replace href with the real docs URL when available
                      tags$td(tags$a(href = "#REPLACE_WITH_DOCUMENTATION_URL", target = "_blank", "User guide and methods documentation"))
                    ),
                    tags$tr(
                      tags$td(style = "vertical-align: top;", strong("Citation")),
                      tags$td("Fauna & Flora's Indonesia Programme. (2026). RACE: Rapid Assessment for Carbon Stock and Wildlife Ecology [Software]. ",
                              em("(Suggested citation - update with a DOI or formal reference if one is registered.)"))
                    )
                  )
                )
              )
      ),

## Fauna----      
### Fauna data input----
      tabItem(tabName = "Fa_dat_proc",
              fluidRow(
                bs4Card(
                  title = "Data Input and Options",
                  status = "primary",
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  fluidRow(
                    column(
                      width = 12,
                      fileInput("fauna_csv", "Choose CSV File",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),

                      selectInput(inputId = "validationType", label = "Dataset",
                                  choices = c("Avifauna", "Herpetofauna", "Mammals")),

                      textOutput("columnHintText"),

                      selectInput("selected_columns", "Select Columns to Use",
                                  choices = NULL, multiple = TRUE),

                      tags$hr(),
                      actionButton(inputId = "faunastart", label = "1. Validate"),
                      tags$br(), tags$br(),
                      actionButton(inputId = "faunacalculate", label = "2. Calculate")
                    )
                  )
                ),

                bs4Card(
                  title = "Species Validation",
                  status = "primary",
                  width = 8,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  uiOutput(outputId = "uspecies", width = "100%")
                )
              ),

              fluidRow(
                bs4Card(
                  title = "Dataset Validation",
                  status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  uiOutput(outputId = "validation", width = "100%")
                )
              )
            ),
      
### Fauna data results----
      tabItem(tabName = "Fa_dat_res",
              fluidRow(
                bs4Card(
                  title = "Species Richness Indices",
                  status = "primary",
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  DTOutput(outputId = "spindex")
                ),
                bs4Card(
                  title = "Richness and Abundance Plot",
                  status = "primary",
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  plotOutput(outputId = "rs_abd_plot")
                )
              ),
              
              fluidRow(
                bs4Card(
                  title = "Species Richness Estimation",
                  status = "primary",
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  DTOutput(outputId = "est_table")
                ),
                bs4Card(
                  title = "Species Accumulation Curve",
                  status = "primary",
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  plotOutput(outputId = "inext")
                )
              ),
              
              fluidRow(
                bs4Card(
                  title = "Cluster Plot",
                  status = "primary",
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  
                  # Add method selector
                  selectInput(
                    inputId = "distance_method",
                    label = "Distance Method",
                    choices = c("Bray-Curtis" = "bray", "Jaccard" = "jaccard"),
                    selected = "bray"
                  ),
                  
                  # Plot output
                  plotOutput("cluster_plot_b")
                ),
                
                bs4Card(
                  title = "Dissimilarity",
                  status = "primary",
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  selectInput(
                    inputId = "distance_method_table",
                    label = "Distance Method",
                    choices = c("Bray-Curtis" = "bray", "Jaccard" = "jaccard"),
                    selected = "bray"
                  ),
                  
                  # Table output
                  DTOutput("cluster_table_b")
                )
              )
      ),

### Fauna conservation status----
      tabItem(tabName = "Fa_res_cs",
              fluidRow(
                bs4Card(
                  title = "Upload Validated Species List",
                  status = "danger",
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  p("Upload the CSV exported from the Species Validation step (Step 1), with any corrections applied."),
                  fileInput(
                    inputId = "species_cs_csv",
                    label = "Choose CSV File",
                    accept = c(".csv")
                  ),
                  actionButton(inputId = "searchcs", label = "Retrieve Conservation Status"),
                  tags$br(), tags$br(),
                  div(
                    style = "color: #92400e; background-color: #fef3c7; padding: 10px; border-radius: 4px; border: 1px solid #fde68a;",
                    icon("triangle-exclamation"),
                    strong(" Please click only once."),
                    " Clicking again while it's running may cause it to run multiple times. Retrieving IUCN and CITES data can take several minutes for large species lists — please stand by."
                  )
                ),
                bs4Card(
                  title = "Species and Conservation Status",
                  status = "danger",
                  width = 8,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  DTOutput(outputId = "splistcs", width = "100%")
                )
              ),
              fluidRow(
                bs4Card(
                  title = "Species Composition",
                  status = "danger",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  plotOutput(outputId = "treemap", height = "550px")
                )
              )
            ),

## Flora----      
### Flora data input----
      tabItem(tabName = "Flo_dat_proc",

              fluidRow(
                bs4Card(
                  title = "Data Input and Options",
                  status = "info",
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  fluidRow(
                    column(
                      width = 12,
                      fileInput("Flo_csv_file", "Choose CSV File",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),

                      p("Please select the column(s) that indicate Transect, Plot ID, Tree ID, Scientific Name, Taxonomic Rank, Class group, Girth and Tree Height."),

                      selectInput("Flo_selected_columns", "Select Columns to Use",
                                  choices = NULL, multiple = TRUE),
                      tags$hr(),
                      actionButton(inputId = "florastart", label = "1. Validate dataset"),
                      tags$br(), tags$br(),
                      actionButton(inputId = "floracalculate", label = "2. Calculate vegetation")
                    )
                  )
                ),

                bs4Card(
                  title = "Species Validation",
                  status = "info",
                  width = 8,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  uiOutput(outputId = "uspeciesflora", width = "100%")
                )
              ),

              fluidRow(
                bs4Card(
                  title = "Dataset Validation",
                  status = "info",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  uiOutput(outputId = "validationflora", width = "100%")
                )
              ),

              fluidRow(
                bs4Card(
                  title = "Raincloud Plot",
                  status = "info",
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  plotOutput(outputId = "raincloudbh", width = "100%")
                ),
                bs4Card(
                  title = "Tree Girth vs Clear Bole",
                  status = "info",
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  plotOutput(outputId = "lmddbhtt", width = "100%")
                )
              )
            ),

### Flora veg results----
tabItem(tabName = "Flo_dat_veg",
        fluidRow(
          bs4Card(
            title = "Richness Indices Across Transect",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput(outputId = "spindexflora")
          ),
          bs4Card(
            title = "Richness and Abundance Plot",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput(outputId = "rs_abd_plot_flora")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Cluster Plot",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput("cluster_plot_f")
          ),
          bs4Card(
            title = "Dissimilarity",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput("cluster_table_f")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Importance Value Index",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput(outputId = "IV")
          )
        )
      ),

### Flora carbon stock----
tabItem(tabName = "Flo_dat_car",

        fluidRow(
          bs4Card(
            title = "Allometric Method & Validation",
            status = "info",
            width = 4,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            p("Carbon stock estimation continues from the calculated vegetation dataset (Results Vegetation step)."),
            selectizeInput(
              inputId = "allometric_compare",
              label = "Compare Allometric Equations (up to 5)",
              choices = allometric_choices,
              multiple = TRUE,
              options = list(maxItems = 5, placeholder = "Select up to 5 equations to compare")
            ),
            tags$hr(),
            actionButton(inputId = "carbonvalidate", label = "Validate Dataset")
          ),

          bs4Card(
            title = "Allometric Equation Reference",
            status = "info",
            width = 8,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput(outputId = "allometric_ref_table", width = "100%")
          )
        ),

        fluidRow(
          bs4Card(
            title = "Wood Density Plot",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput(outputId = "wd_dist_plot", width = "100%")
          ),

          bs4Card(
            title = "Wood Density Data",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput(outputId = "wd_data_table", width = "100%")
          )
        ),

        fluidRow(
          bs4Card(
            title = "Allometric plot",
            status = "info",
            width = 8,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput(outputId = "agb_dbh_fit", width = "100%")
          ),

          bs4Card(
            title = "R² Comparison",
            status = "info",
            width = 4,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput(outputId = "agb_r2_table", width = "100%")
          )
        ),

        fluidRow(
          bs4Card(
            title = "Final Equation Selection",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            p("Based on the comparison above, choose the single allometric equation to use for the final carbon stock calculation (Q-Q plot and distribution check below)."),
            selectInput(
              inputId = "final_allometric_method",
              label = "Final Allometric Equation",
              choices = allometric_choices
            )
          )
        ),

        fluidRow(
          bs4Card(
            title = "Stratum Input and Options",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            fluidRow(
              column(
                width = 6,
                fileInput("stratum_csv_file", "Choose CSV File (Plot ID + Stratum)",
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),

                selectInput("stratum_plotid_col", "Plot ID Column", choices = NULL),
                selectInput("stratum_stratum_col", "Stratum Column", choices = NULL)
              ),
              column(
                width = 6,
                radioButtons(
                  inputId = "wd_source",
                  label = "Wood Density Source",
                  choices = c(
                    "Use app-calculated wood density (from Validate Dataset step)" = "app",
                    "Upload revised wood density (edited from Wood Density Data download)" = "upload"
                  ),
                  selected = "app"
                ),
                conditionalPanel(
                  condition = "input.wd_source == 'upload'",
                  fileInput("wd_revised_csv", "Upload Revised Wood Density CSV",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"))
                ),
                tags$hr(),
                actionButton(inputId = "carbonconfirm", label = "Confirm Stratum & Wood Density")
              )
            )
          )
        ),

        fluidRow(
          bs4Card(
            title = "QQ plot of AGB",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput(outputId = "agb_qq_plot", width = "100%")
          ),

          bs4Card(
            title = "Normal distibution of AGB",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput(outputId = "agb_normality_plot", width = "100%")
          )
        ),

        fluidRow(
          bs4Card(
            title = "AGC Reference (Indonesia FRL)",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput(outputId = "frl_ref_table", width = "100%")
          ),

          bs4Card(
            title = "Distribution of AGC across Plot and Stratum",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            p("Plot.ID, AGC (tC/ha) and Stratum - double-click a cell to revise if needed. Changes apply to the outputs below."),
            DTOutput(outputId = "agc_plot_data_table", width = "100%")
          )
        ),

        fluidRow(
          bs4Card(
            title = "AGC Distribution Plot",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput(outputId = "agc_by_stratum_plot", width = "100%")
          )
        ),

        fluidRow(
          bs4Card(
            title = "Mean Tree Density",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            p("Mean tree density by DBH size class and stratum to sanity-check whether the nested-plot expansion looks reasonable (smaller size classes are typically denser)."),
            DTOutput(outputId = "tree_density_by_class_table", width = "100%")
          ),

          bs4Card(
            title = "Mean Tree Density by Stratum",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput(outputId = "tree_density_table", width = "100%")
          )
        ),

        fluidRow(
          bs4Card(
            title = "Mean Carbon Stock",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput(outputId = "mean_weighted_carbon_table", width = "100%")
          )
        ),

        fluidRow(
          bs4Card(
            title = "Before You Finalize: Review Your Results",
            status = "warning",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            div(
              style = "color: #92400e; background-color: #fef3c7; padding: 12px; border-radius: 4px; border: 1px solid #fde68a;",
              icon("triangle-exclamation"),
              strong(" Please review everything above carefully before treating these results as final."),
              tags$ul(
                tags$li("Does AGC look overestimated (or underestimated) compared to the FRL reference table?"),
                tags$li("Is the AGC/tree density distribution too skewed, or driven by one or two outlier plots?"),
                tags$li("Does the Stratum assigned to each plot actually make sense for that location?"),
                tags$li("Does tree density (trees/ha) look reasonable across DBH classes A/B/C - and across strata?")
              ),
              "If anything looks off, you don't need to start over from the beginning: go back to ",
              strong("Stratum Input and Options"),
              " above - re-upload or remap the stratum file, switch the wood density source, or edit the AGC table directly - then click ",
              strong("Confirm Stratum & Wood Density"),
              " again to recalculate everything below it."
            )
          )
        )
      ),

### Flora conservation status----
tabItem(tabName = "Flo_res_cs",
        fluidRow(
          bs4Card(
            title = "Upload Validated Species List",
            status = "danger",
            width = 4,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            p("Upload the CSV exported from the Species Validation step (Data Processing tab), with any corrections applied."),
            fileInput(
              inputId = "flo_species_cs_csv",
              label = "Choose CSV File",
              accept = c(".csv")
            ),
            actionButton(inputId = "flo_searchcs", label = "Retrieve Conservation Status"),
            tags$br(), tags$br(),
            div(
              style = "color: #92400e; background-color: #fef3c7; padding: 10px; border-radius: 4px; border: 1px solid #fde68a;",
              icon("triangle-exclamation"),
              strong(" Please click only once."),
              " Clicking again while it's running may cause it to run multiple times. Retrieving IUCN and CITES data can take several minutes for large species lists — please stand by."
            )
          ),
          bs4Card(
            title = "Species and Conservation Status",
            status = "danger",
            width = 8,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput(outputId = "flo_splistcs", width = "100%")
          )
        ),

        fluidRow(
          bs4Card(
            title = "Species Composition",
            status = "danger",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput(outputId = "flo_treemap", height = "550px")
          )
        )
      )
    )
  ),

  controlbar = dashboardControlbar(),
  footer = dashboardFooter(
    left = "Developed and maintained by Fauna & Flora's Indonesia Programme",
    right = paste("Last built:", format(app_build_time, "%Y-%m-%d %H:%M"))
  )
)
