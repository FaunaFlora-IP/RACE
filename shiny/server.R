server <- function(input, output, session) {

# Shared plot styling----
legend_theme <- theme(
  legend.title   = element_text(size = 15, face = "bold"),
  legend.text    = element_text(size = 13),
  legend.key.size = unit(1, "lines")
)

# Shared table export buttons----
dt_export_buttons <- function(types = c("copy", "csv", "excel", "pdf")) {
  lapply(types, function(type) {
    list(extend = type, exportOptions = list(modifier = list(page = "all")))
  })
}

#Fauna----

## Fauna data input----  
  
### Reactivity for uploaded data----
   
  fauna_data <- reactive({
    req(input$fauna_csv)
    read.csv(input$fauna_csv$datapath, stringsAsFactors = FALSE)
  })
  
  observeEvent({
    input$fauna_csv
    input$validationType
  }, {
    req(fauna_data())
    req(input$validationType != "")
    
    updateSelectInput(
      session,
      inputId = "selected_columns",
      choices = names(fauna_data()),
      selected = NULL
    )
  })
  
  output$columnHintText <- renderText({
    req(input$validationType)
    
    base_msg <- "Please select the column(s) that indicate transect, scientific name, and taxonomic rank."
    
    if (input$validationType == "Avifauna") {
      paste(base_msg, "Also include observation type and number of individuals.")
    } else {
      base_msg
    }
  })
  
### Column selections ----  

  validated_data <- eventReactive(input$faunastart, {
    req(fauna_data())
    req(input$selected_columns)
    
    data <- fauna_data()
    selected <- input$selected_columns
    
    required_3 <- c("Transect", "Scientific.Name", "Taxon.Rank")
    required_5 <- c(required_3, "Observation.Type", "Indv")
    
    if (length(selected) == 5) {
      out <- data[, selected]
      names(out) <- required_5
    } else if (length(selected) == 3) {
      out <- data[, selected]
      names(out) <- required_3
    } else {
      showNotification("Please select either 3 or 5 columns.", type = "error")
      return(NULL)
    }
    
    return(out)
  })
  
### Species validation----

  output$uspecies <- renderUI({

    req(validated_data())

    show_modal_spinner(spin = "cube-grid", text = "Validating species names via GBIF...")
    on.exit(remove_modal_spinner(), add = TRUE)

    uspl <- validated_data() %>%
      arrange(Scientific.Name)

    uspl <- unique(uspl$Scientific.Name)

    res <- gbif_checklist_batched(uspl) %>%
      select(originalName = verbatim_name,
             userScientificName = canonicalName,
             referenceScientificName = scientificName,
             rank, status, matchType,
             class, order, family, genus, suggestedName = species
      ) %>%
      mutate(Comparison = case_when(
        is.na(userScientificName) ~ "Not Found",
        originalName == userScientificName ~ "Match",
        TRUE ~ "Different"
      ))

    DT::datatable(res %>% select("Submitted Name" = originalName,
                                  "Comparison" = Comparison,
                                  "Match Type" = matchType,
                                  "Taxonomic Status" = status,
                                  Rank = rank,
                                  "GBIF Canonical Name" = userScientificName,
                                  "GBIF Reference Name" = referenceScientificName,
                                  "Suggested Name" = suggestedName,
                                  Class = class, Order = order, Family = family, Genus = genus),
                  extensions = 'Buttons', filter = "top",
                  options = list(
                    paging = FALSE,
                    scrollX = TRUE,
                    searching = TRUE,
                    ordering = TRUE,
                    autoWidth = TRUE,
                    dom = 'Bfrtip',
                    buttons = dt_export_buttons(),
                    scrollY = "500px"),
                  escape = FALSE) %>%
      DT::formatStyle("Comparison",
                      target = 'cell',
                      color = styleEqual(
                        c("Match", "Different", "Not Found"),
                        c('forestgreen', 'darkorange', 'red')
                      ))

    })


### Dataset validation----
  output$validation <- renderUI({
    
    req(validated_data()) 
    
    raw <- validated_data()
    
    report <- data_validation_report() 
    
    data.validator::validate(raw, description = "Dataset validation") %>%
      validate_cols(predicate = not_na, "Transect", description = "No missing values in Transect") %>%
      validate_cols(predicate = not_na, c("Scientific.Name", "Taxon.Rank"), description = "No missing values in taxonomic fields") %>%
      validate_cols(in_set(c("Species", "Genus", "Family", "Ordo")), "Taxon.Rank", description = "Correct Taxon Rank category") %>%
      add_results(report)
    
    render_semantic_report_ui(get_results(report = report))
    
 })   
  
  
## Fauna data results----

# Data calculation  
  calculateData <- eventReactive(input$faunacalculate, {
    
    req(validated_data())
    
    calculateData <- req(validated_data())
    
  })
  
### Table of species richness----
  output$spindex <- renderDT({
    
    req(calculateData())
    
    dataspr <- calculateData() %>%
      filter(Taxon.Rank %in% c("Species", "Genus")) %>%
      {
        if ("Observation.Type" %in% names(.)) {
          filter(., Observation.Type == "PointLoc")
        } else {
          .
        }
      } %>%
      {
        if ("Indv" %in% names(.)) {
          drop_na(., Indv)
        } else {
          mutate(., Indv = 1)
        }
      } %>%
      group_by(Transect, Scientific.Name) %>%
      summarize(n = sum(Indv, na.rm = TRUE)) %>%
      summarize(Richness = n_distinct(Scientific.Name),
                Abundance = sum(n),
                Shannon = -sum(prop.table(n) * log(prop.table(n))),
                Margalef = (n_distinct(Scientific.Name) - 1) / log(sum(n)),
                Evenness = (-sum(prop.table(n) * log(prop.table(n))))/log(length(n)),
                Simpson = sum(prop.table(n)^2)) %>%
      mutate(across(4:last_col(), round, 2))
    
    datatable(dataspr, extensions = "Buttons", filter = "top",
              options = list(
                paging = TRUE,
                scrollX = TRUE,
                searching = TRUE,
                ordering = TRUE,
                dom = 'Bfrtip',
                buttons = dt_export_buttons(),
                pageLength = 10,
                lengthMenu = c(3, 5, 10)))
  })  
  
  ### abundance plot----
  output$rs_abd_plot <- renderPlot({
    req(calculateData())
    
    dataspr <- calculateData() %>%
      filter(Taxon.Rank %in% c("Species", "Genus")) %>%
      {
        if ("Observation.Type" %in% names(.)) {
          filter(., Observation.Type == "PointLoc")
        } else {
          .
        }
      } %>%
      {
        if ("Indv" %in% names(.)) {
          drop_na(., Indv)
        } else {
          mutate(., Indv = 1)
        }
      } %>%
      group_by(Transect, Scientific.Name) %>%
      summarize(n = sum(Indv, na.rm = TRUE)) %>%
      summarize(Richness = n_distinct(Scientific.Name),
                Abundance = sum(n),
                Shannon = -sum(prop.table(n) * log(prop.table(n))),
                Margalef = (n_distinct(Scientific.Name) - 1) / log(sum(n)),
                Evenness = (-sum(prop.table(n) * log(prop.table(n))))/log(length(n)),
                Simpson = sum(prop.table(n)^2)) %>%
      mutate(across(4:last_col(), round, 2))
    
    dataspr %>%
      select(c(Transect, Richness, Abundance)) %>% 
      pivot_longer(-Transect, names_to = "Category", values_to = "values") %>%
      ggplot(aes(fill=Category, y=values, x=Transect)) +
      geom_col(position="dodge", width = 0.8) +
      geom_text(aes(label = round(values,1)),
                position = position_dodge(0.8), vjust = -0.5, hjust = 0.5) +
      theme_bw() +
      legend_theme
  })

  ### Estimate species richness----
  output$est_table <- renderDT({
    req(calculateData())
    
    # Calculate observed species richness
    observed <- calculateData() %>% 
      filter(Taxon.Rank %in% c("Species", "Genus")) %>%
      distinct(Scientific.Name) %>%
      nrow()
    
    new_row <- data.frame(
      Method = "    Observed Species",  # Name of the method/row
      Estimate = observed,          # Count value to be added
      s.e. = NA,                       # Fill with NA if not applicable
      "95%Lower" = NA,                 # Fill with NA if not applicable
      "95%Upper" = NA                  # Fill with NA if not applicable
    )
    
    colnames(new_row) <- c("Method", "Estimate", "s.e.", "95%Lower", "95%Upper")
    
    out1 <- calculateData() %>%
      filter(Taxon.Rank %in% c("Species", "Genus")) %>%
      {
        if ("Indv" %in% names(.)) {
          drop_na(., Indv)
        } else {
          mutate(., Indv = 1)
        }
      }  %>%
      group_by(Scientific.Name) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      as.data.frame()
    
    out1 <- as.data.frame(out1)
    rownames(out1) <- out1$Scientific.Name
    out1$Scientific.Name <- NULL
    
    out2 <- SpadeR::ChaoSpecies(out1, datatype = "abundance")
    
    out3 <- as.data.frame(out2$Species_table)
    out3 <- tibble::rownames_to_column(out3, "Method")
    out3 <- rbind(new_row, out3)
    
    final_table <- out3[c(1, 2, 6,9), ]
    
    datatable(final_table, extensions = "Buttons", filter = "top",
              options = list(
                paging = FALSE,
                scrollX = TRUE,
                searching = TRUE,
                ordering = TRUE,
                dom = 'Bfrtip',
                buttons = dt_export_buttons()),
              escape = FALSE)
  })
  
  
### Species accumulation curve----
  output$inext <- renderPlot({
    
    req(calculateData())
    
    out1 <- calculateData() %>%
      filter(Taxon.Rank %in% c("Species", "Genus")) %>%
      {
        if ("Indv" %in% names(.)) {
          drop_na(., Indv)
        } else {
          mutate(., Indv = 1)
        }
      }  %>%
      group_by(Scientific.Name) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      as.data.frame()
    
    rownames(out1) <- out1$Scientific.Name
    out1$Scientific.Name <- NULL
    
    out1 <- iNEXT(out1, q = 0, datatype = "abundance")
    
    ggiNEXT(x = out1, type = 1, color.var = "Order.q") +
      labs(x = "Number of Individuals", y = "Cumulative Species Richness") +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.position = "bottom") +
      legend_theme

  })

  
### Cluster plot---- 
  output$cluster_plot_b <- renderPlot({
    req(calculateData(), input$distance_method)
    
    rawsp <- calculateData() %>%
      filter(Taxon.Rank %in% c("Species", "Genus")) %>%
      {
        if ("Observation.Type" %in% names(.)) {
          filter(., Observation.Type == "PointLoc")
        } else {
          .
        }
      } %>%
      {
        if ("Indv" %in% names(.)) {
          drop_na(., Indv)
        } else {
          mutate(., Indv = 1)
        }
      } 
    
    aggregated_data <- aggregate(Indv ~ Transect + Scientific.Name, data = rawsp, sum)
    data_matrix <- reshape2::dcast(aggregated_data, Transect ~ Scientific.Name, value.var = "Indv")
    data_matrix[is.na(data_matrix)] <- 0
    data_matrix_table <- as.matrix(data_matrix[, -1])  # Exclude the Transect column
    rownames(data_matrix_table) <- data_matrix$Transect
    
    hc_transect <- data_matrix_table %>%
      vegdist(method = input$distance_method) %>%
      hclust(method = "average")
    
    plot(hc_transect, xlab = "", ylab = "Dissimilarity", sub = "Transect", hang = -1)
    
  })

### Cluster table----
  output$cluster_table_b <- renderDT({
    req(calculateData(), input$distance_method_table)
    
    rawsp <- calculateData() %>%
      filter(Taxon.Rank %in% c("Species", "Genus")) %>%
      {
        if ("Observation.Type" %in% names(.)) {
          filter(., Observation.Type == "PointLoc")
        } else {
          .
        }
      } %>%
      {
        if ("Indv" %in% names(.)) {
          drop_na(., Indv)
        } else {
          mutate(., Indv = 1)
        }
      }
    
    aggregated_data <- aggregate(Indv ~ Transect + Scientific.Name, data = rawsp, sum)
    data_matrix <- reshape2::dcast(aggregated_data, Transect ~ Scientific.Name, value.var = "Indv")
    data_matrix[is.na(data_matrix)] <- 0
    data_matrix_table <- as.matrix(data_matrix[, -1])
    rownames(data_matrix_table) <- data_matrix$Transect
    
    hc_table <- vegdist(data_matrix_table, method = input$distance_method_table)
    
    hct <- as.matrix(hc_table) %>%
      as.data.frame() %>%
      round(2)
    
    datatable(hct, extensions = "Buttons", filter = "top",
              options = list(
                paging = FALSE,
                scrollX = TRUE,
                searching = TRUE,
                ordering = TRUE,
                dom = 'Bfrtip',
                buttons = dt_export_buttons(),
                scrollY = "500px"),
              escape = FALSE)
  })

## Fauna conservation----
### Upload validated species list (export from Species Validation step)----
  validated_species_data <- reactive({
    req(input$species_cs_csv)
    read.csv(input$species_cs_csv$datapath, stringsAsFactors = FALSE)
  })

### Retrieve conservation status----
  species_list <- eventReactive(input$searchcs, {

    speciesCons <- req(validated_species_data())

    show_modal_spinner(spin = "cube-grid", text = "Preparing species list...")
    on.exit(remove_modal_spinner(), add = TRUE)

    # Reframe dataset - Class, Order and Family come directly from the
    # species validation export (GBIF-derived) instead of being re-derived
    # from IUCN, since IUCN doesn't have an entry for every species
    Species_df <- speciesCons %>%
      distinct(Species = Suggested.Name, Class, Order, Family) %>%
      mutate(
        genus = word(Species, 1),
        species = word(Species, 2))

    # IUCN and CITES only accept up to 50 species per request, so split into
    # batches of 50, query each batch separately, then recombine
    batch_id <- ceiling(seq_len(nrow(Species_df)) / 50)
    batches <- split(Species_df, batch_id)
    n_batches <- length(batches)

    batch_results <- purrr::imap(batches, function(batch_df, idx) {

      # Retrieve IUCN data (drop its own Class/Order/Family - we keep the
      # ones carried over from the species validation export instead)
      update_modal_spinner(text = paste0("Batch ", idx, "/", n_batches, ": querying IUCN Red List..."))
      sp1 <- get_iucn_species_data(api, batch_df) %>%
        select(-any_of(c("Class", "Order", "Family")))

      # Retrieve CITES data - some species aren't CITES-listed at all, which
      # retrieve_CITES_data() already handles by returning NA rather than
      # failing, so a partial miss here never stops the batch
      update_modal_spinner(text = paste0("Batch ", idx, "/", n_batches, ": querying CITES..."))
      sp2 <- retrieve_CITES_data(batch_df$Species) %>%
        distinct(Species, .keep_all = TRUE)

      left_join(sp1, sp2, by = "Species")
    })

    combined <- bind_rows(batch_results)

    # Combine IUCN, CITES and PP106 with the taxonomy carried over from upload
    result <- Species_df %>%
      select(Species, Class, Order, Family) %>%
      left_join(combined, by = "Species") %>%
      left_join(db, by = "Species") %>%
      select(Class, Order, Family, Species, `Common name`, Status, CITES_Appendix, Protected, Endemic, Migratory) %>%
      mutate(across(c(Class, Order, Family), tolower)) %>%
      mutate(across(c(Class, Order, Family), tools::toTitleCase)) %>%
      mutate(CITES_Appendix = ifelse(is.na(CITES_Appendix), "Not identified", CITES_Appendix)) %>%
      rename(Appendix = CITES_Appendix) %>%
      arrange(Order, Family, Species)

    result # Return the result
  })
  
### Table of taxon and conservation status ----
  output$splistcs <- renderDT({
    
    req(species_list()) 
    
    datatable(species_list(), extensions = "Buttons", filter = "top",
      options = list(
        paging = TRUE,
        pageLength = 20,
        lengthMenu = c(20, 50, 100),
        scrollX = TRUE,
        searching = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = dt_export_buttons()
      ),
      escape = FALSE
    )

  })

### Treemap ----
  output$treemap <- renderPlot({
    
    req(species_list) 
    
    rearranged_species <- species_list() %>%
      group_by(Order, Family) %>%
      filter(Order != "Na") %>%
      summarise(Num_Species = n(), .groups = 'drop')
    
    treemap(rearranged_species, index = c("Order", "Family"),
            vSize = "Num_Species", type = "index",
            title = "",
            fontsize.labels = c(16, 12),
            fontcolor.labels = c("white", "white"),
            fontface.labels = c(2, 2),
            bg.labels = c("transparent"),
            align.labels = list(
              c("center", "center"),
              c("center", "bottom")
            ),
            overlap.labels = 0.3,
            lowerbound.cex.labels = 0.6,
            inflate.labels = F
            )

  })
  
# Flora---- 
  
## Flora data input----  

### Reactivity for uploaded data----
  
  flora_data <- reactive({
    req(input$Flo_csv_file)
    read.csv(input$Flo_csv_file$datapath, stringsAsFactors = FALSE)
  })  
  
  observeEvent({
    input$Flo_csv_file
  }, {
    req(flora_data())
    
    updateSelectInput(
      session,
      inputId = "Flo_selected_columns",
      choices = names(flora_data()),
      selected = NULL
    )
  })

### Column selections ----  
  
  validated_flora <- eventReactive(input$florastart, {
    req(flora_data())
    req(input$Flo_selected_columns)

    data <- flora_data()
    selected <- input$Flo_selected_columns

    required_8 <- c("Transect", "Plot.ID", "Tree.ID", "Scientific.Name", "Taxon.Rank", "Class", "DBH", "TT")

    if (length(selected) == 8) {
      out <- data[, selected]
      names(out) <- required_8
    } else {
      showNotification("Please select exactly 8 columns: Transect, Plot ID, Tree ID, Scientific Name, Taxon Rank, Class, Girth, Tree Height", type = "error")
      return(NULL)
    }

    return(out)
  })

### Species validation----

  output$uspeciesflora <- renderUI({

    req(validated_flora())

    show_modal_spinner(spin = "cube-grid", text = "Validating species names via GBIF...")
    on.exit(remove_modal_spinner(), add = TRUE)

    uspl <- validated_flora() %>%
      arrange(Scientific.Name)

    uspl <- unique(uspl$Scientific.Name)

    res <- gbif_checklist_batched(uspl) %>%
      select(originalName = verbatim_name,
             userScientificName = canonicalName,
             referenceScientificName = scientificName,
             rank, status, matchType,
             class, order, family, genus, suggestedName = species
      ) %>%
      mutate(Comparison = case_when(
        is.na(userScientificName) ~ "Not Found",
        originalName == userScientificName ~ "Match",
        TRUE ~ "Different"
      ))

    DT::datatable(res %>% select("Submitted Name" = originalName,
                                  "Comparison" = Comparison,
                                  "Match Type" = matchType,
                                  "Taxonomic Status" = status,
                                  Rank = rank,
                                  "GBIF Canonical Name" = userScientificName,
                                  "GBIF Reference Name" = referenceScientificName,
                                  "Suggested Name" = suggestedName,
                                  Class = class, Order = order, Family = family, Genus = genus),
                  extensions = 'Buttons', filter = "top",
                  options = list(
                    paging = FALSE,
                    scrollX = TRUE,
                    searching = TRUE,
                    ordering = TRUE,
                    autoWidth = TRUE,
                    dom = 'Bfrtip',
                    buttons = dt_export_buttons(),
                    scrollY = "500px"),
                  escape = FALSE) %>%
      DT::formatStyle("Comparison",
                      target = 'cell',
                      color = styleEqual(
                        c("Match", "Different", "Not Found"),
                        c('forestgreen', 'darkorange', 'red')
                      ))

    })

### Dataset validation----
  output$validationflora <- renderUI({

    req(validated_flora())

    raw <- validated_flora()

    report <- data_validation_report()

    data.validator::validate(raw, description = "Dataset validation") %>%
      validate_cols(predicate = not_na, c("Transect", "Plot.ID", "Tree.ID"), description = "No missing values in Transect, Plot ID or Tree ID") %>%
      validate_cols(predicate = is.character, c("Transect", "Plot.ID", "Tree.ID"), description = "Transect, Plot ID and Tree ID must be text") %>%
      validate_cols(predicate = not_na, c("Scientific.Name", "Taxon.Rank"), description = "No missing values in taxonomic fields") %>%
      validate_cols(in_set(c("Order", "Family", "Genus", "Species")), "Taxon.Rank", description = "Correct Taxon Rank category") %>%
      validate_cols(in_set(c("A", "B", "C")), "Class", description = "Class group must be A, B, or C") %>%
      validate_cols(predicate = not_na, c("DBH", "TT"), description = "No missing values in Girth and Tree Height") %>%
      validate_cols(predicate = is.numeric, c("DBH", "TT"), description = "Girth and Tree Height must be numeric") %>%
      add_results(report)

    render_semantic_report_ui(get_results(report = report))

  })

### Raincloud----
  output$raincloudbh <- renderPlot({
    
    req(validated_flora())
    
    validated_flora() %>%
      ggplot(aes(x = factor(Class, levels = c("C", "B", "A")), y = DBH, fill=factor(Class))) +
      
      ggdist::stat_halfeye(
        adjust = 0.5,
        justification = -.2,
        .width = 0,
        point_colour = NA
      ) + 
      
      geom_boxplot(
        width =.12,
        outlier.colour = NA,
        alpha = .5
      ) +  
      
      ggdist::stat_dots(
        side = "left",
        justification = 1.1,
        binwidth = .25
      ) + 
      
      scale_fill_tq() + 
      
      theme_tq() +
      
      labs(
        title = "Raincloud Plot of DBH",
        subtitle = "",
        x = "Class",
        y = "DBH (cm)",
        fill = "Class"
      ) +
      coord_flip() +
      legend_theme
  })
  
# Eda for relationship between heigth and girth
  output$lmddbhtt <- renderPlot({
    
    req(validated_flora())
    
    raw <- validated_flora()
    # Calculate R-squared value
    fit <- lm(TT ~ DBH, raw)
    r_squared <- summary(fit)$r.squared
    
    raw %>%
      ggplot(aes(x = DBH, y = TT)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) + 
      scale_fill_tq() + 
      theme_tq() +
      geom_label(aes(x = max(DBH), y = max(TT), label = paste("R² =", round(r_squared, 2))), 
                 hjust = 1, vjust = 1)
  })  
  
## Flora Result----  

# Data calculation  
  calculateDataFlora <- eventReactive(input$floracalculate, {
    
    req(validated_flora())
    
    calculateDataFlora <- req(validated_flora())
    
  })
  
### Table of species richness----
  output$spindexflora <- renderDT({
    
    req(calculateDataFlora())
    
    dataspr <- calculateDataFlora() %>%
      filter(Transect != "") %>% 
      filter(Taxon.Rank == "Species" | Taxon.Rank == "Genus") %>%
      count(Scientific.Name, Transect) %>%
      group_by(Transect) %>%
      summarize(Richness = n_distinct(Scientific.Name),
                Abundance = sum(n),
                Shannon = -sum(prop.table(n) * log(prop.table(n))),
                Margalef = (n_distinct(Scientific.Name) - 1) / log(sum(n)),
                Evenness = (-sum(prop.table(n) * log(prop.table(n))))/log(length(n)),
                Simpson = sum(prop.table(n)^2)) %>%
      mutate(across(4:last_col(), ~round(., 2)))
    
    datatable(dataspr, extensions = "Buttons", filter = "top",
              options = list(paging = TRUE,
                             scrollX=TRUE,
                             searching = TRUE,
                             ordering = TRUE,
                             dom = 'Bfrtip',
                             buttons = dt_export_buttons(),
                             pageLength=10,
                             lengthMenu=c(3,5,10) ))
  })  
  
  ### abundance plot----
  output$rs_abd_plot_flora <- renderPlot({
    
    req(calculateDataFlora())
    
    dataspr <- calculateDataFlora() %>%
      filter(Transect != "") %>% 
      filter(Taxon.Rank == "Species" | Taxon.Rank == "Genus") %>%
      count(Scientific.Name, Transect) %>%
      group_by(Transect) %>%
      summarize(Richness = n_distinct(Scientific.Name),
                Abundance = sum(n),
                Shannon = -sum(prop.table(n) * log(prop.table(n))),
                Margalef = (n_distinct(Scientific.Name) - 1) / log(sum(n)),
                Evenness = (-sum(prop.table(n) * log(prop.table(n))))/log(length(n)),
                Simpson = sum(prop.table(n)^2)) %>%
      mutate(across(4:last_col(), ~round(., 2)))
    
    dataspr %>%
      select(c(Transect, Richness, Abundance)) %>% 
      pivot_longer(-Transect, names_to = "Category", values_to = "values") %>%
      ggplot(aes(fill=Category, y=values, x=Transect)) + 
      geom_col(position="dodge", width = 0.8) + 
      geom_text(aes(label = round(values,1)),
                position = position_dodge(0.8), vjust = -0.5, hjust = 0.5) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      legend_theme
  })

### Cluster plot----
  output$cluster_plot_f <- renderPlot({
    req(calculateDataFlora())
    
    rawsp <- calculateDataFlora() %>%
      filter(Taxon.Rank %in% c("Species", "Genus")) %>%
      mutate(Indv = 1)  # add 1 per observation
    
    aggregated_data <- aggregate(Indv ~ Transect + Scientific.Name, data = rawsp, sum)
    data_matrix <- reshape2::dcast(aggregated_data, Transect ~ Scientific.Name, value.var = "Indv")
    data_matrix[is.na(data_matrix)] <- 0
    data_matrix_table <- as.matrix(data_matrix[, -1])  # remove Transect column
    rownames(data_matrix_table) <- data_matrix$Transect
    
    hc_transect <- vegdist(data_matrix_table, method = "bray") %>%
      hclust(method = "average")
    
    plot(hc_transect, xlab = "", ylab = "Dissimilarity", sub = "Transect", hang = -1)
  })
  
  ### Cluster table----
  output$cluster_table_f <- renderDT({
    req(calculateDataFlora())
    
    rawsp <- calculateDataFlora() %>%
      filter(Taxon.Rank %in% c("Species", "Genus")) %>%
      mutate(Indv = 1)  # add 1 per observation
      
    aggregated_data <- aggregate(Indv ~ Transect + Scientific.Name, data = rawsp, sum)
    data_matrix <- reshape2::dcast(aggregated_data, Transect ~ Scientific.Name, value.var = "Indv")
    data_matrix[is.na(data_matrix)] <- 0
    data_matrix_table <- as.matrix(data_matrix[, -1])
    rownames(data_matrix_table) <- data_matrix$Transect
    
    hc_table <- vegdist(data_matrix_table, method = "bray")
    
    hct <- as.matrix(hc_table) %>%
      as.data.frame() %>%
      round(2)
    
    datatable(hct, extensions = "Buttons", filter = "top",
              options = list(
                paging = FALSE,
                scrollX = TRUE,
                searching = TRUE,
                ordering = TRUE,
                dom = 'Bfrtip',
                buttons = dt_export_buttons()),
              escape = FALSE)
  })
  
### IVI----
output$IV <- renderDT({  
  
  req(calculateDataFlora())
  
  sayur <- calculateDataFlora()
  
  sayur$Class <- as.factor(sayur$Class)
  
  impset <- sayur %>% 
    group_by(Transect, Class, Scientific.Name) %>% 
    summarise(count = n(),
              basal = sum(0.7854*(DBH/100)^2)) %>% #konversi dbh (cm) ke basal area dalam meter persegi
    ungroup() %>% 
    as.data.frame
  
  imp <- importancevalue.comp(impset, site='Transect', species='Scientific.Name', count='count', 
                              basal='basal', factor='Class') 
  
  selected_column <- 'importance.value'  
  
  # Create an empty data frame to store the results
  result_df <- data.frame()
  
  # Iterate through each element in the list
  for (i in 2:length(imp)) {
    # Extract the species names and the selected column data
    species <- rownames(imp[[i]])
    values <- imp[[i]][, selected_column]
    
    # Create a temporary data frame for the current LOTP and sort by the selected column
    temp_df <- data.frame(
      Class = rep(names(imp)[i], length(species)),
      Species = species,
      Importancevalue = values
    )
    temp_df <- temp_df[order(-temp_df$Importancevalue), ]  # Sort by importancevalue
    
    # Take the top 5 species for the current LOTP
    top_5 <- temp_df[1:min(10, nrow(temp_df)), ]
    
    # Append the top 5 species data to the result dataframe
    result_df <- rbind(result_df, top_5)
    result_df$Importancevalue <- round(result_df$Importancevalue, 2)
    rownames(result_df) <- NULL
  }
  
  datatable(result_df, extensions = "Buttons", filter = "top",
            options = list(paging = TRUE,
                           scrollX=TRUE,
                           searching = TRUE,
                           ordering = TRUE,
                           dom = 'Bfrtip',
                           buttons = dt_export_buttons(),
                           pageLength=10,
                           lengthMenu=c(3,5,10)))
})

# Carbon Stock Estimation----

## Allometric equation reference table----
output$allometric_ref_table <- renderDT({

  datatable(allometric_ref, extensions = "Buttons", filter = "top",
            options = list(
              paging = TRUE,
              pageLength = 10,
              scrollX = TRUE,
              searching = TRUE,
              ordering = TRUE,
              dom = 'Bfrtip',
              buttons = dt_export_buttons()),
            escape = FALSE)
})

## Attach wood density to each tree----
flora_with_wd <- eventReactive(input$carbonvalidate, {

  req(calculateDataFlora())

  show_modal_spinner(spin = "cube-grid", text = "Attaching wood density values...")
  on.exit(remove_modal_spinner(), add = TRUE)

  attach_wood_density(calculateDataFlora(), default_rho = 0.57)
})

### Wood density data (original input + wood density, downloadable)----
output$wd_data_table <- renderDT({

  req(flora_with_wd())

  datatable(flora_with_wd(), extensions = "Buttons", filter = "top",
            options = list(
              paging = TRUE,
              pageLength = 10,
              scrollX = TRUE,
              searching = TRUE,
              ordering = TRUE,
              dom = 'Bfrtip',
              buttons = dt_export_buttons(c("copy", "csv", "excel"))),
            escape = FALSE)
})

### Wood density distribution plot----
output$wd_dist_plot <- renderPlot({

  req(flora_with_wd())

  flora_with_wd() %>%
    ggplot(aes(x = wood_density, fill = wd_level)) +
    geom_histogram(binwidth = 0.05, color = "white", boundary = 0) +
    labs(
      title = "Wood Density Distribution",
      x = "Wood Density (g/cm3)",
      y = "Number of Trees",
      fill = "Resolved At"
    ) +
    scale_fill_tq() +
    theme_tq() +
    legend_theme
})

## Compare up to 5 allometric equations against each other (AGB vs DBH)----
flora_agb_compare <- reactive({

  req(flora_with_wd())
  req(input$allometric_compare)

  wd_data <- flora_with_wd() %>%
    dplyr::rename(DBH.G = Class)

  calc_AGB_compare(wd_data, methods = input$allometric_compare,
                    dbh_col = "DBH", h_col = "TT", rho_col = "wood_density")
})

### R² per method (best fit first)----
agb_r2_summary <- reactive({
  req(flora_agb_compare())

  flora_agb_compare() %>%
    dplyr::group_by(.method) %>%
    dplyr::summarise(R2 = round(summary(lm(AGB_kg ~ DBH))$r.squared, 3), .groups = "drop") %>%
    dplyr::rename(Method = .method) %>%
    dplyr::arrange(dplyr::desc(R2))
})

output$agb_r2_table <- renderDT({
  req(agb_r2_summary())

  datatable(agb_r2_summary(), extensions = "Buttons", filter = "top",
            options = list(
              paging = FALSE,
              searching = TRUE,
              ordering = FALSE,
              dom = 'Bfrtip',
              buttons = dt_export_buttons(c("copy", "csv"))),
            escape = FALSE)
})

### AGB vs DBH fit plot (GAM smooth, shaded CI) - method comparison----
output$agb_dbh_fit <- renderPlot({

  req(flora_agb_compare())
  req(agb_r2_summary())

  r2_tbl <- agb_r2_summary() %>%
    dplyr::mutate(label = paste0(Method, " (R²=", R2, ")"))

  data <- flora_agb_compare() %>%
    dplyr::left_join(r2_tbl %>% dplyr::select(Method, label), by = c(".method" = "Method")) %>%
    dplyr::mutate(label = factor(label, levels = r2_tbl$label))

  ggplot(data, aes(x = DBH, y = AGB_kg, color = label, fill = label)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE, alpha = 0.2, linewidth = 1) +
    labs(
      title = "AGB vs DBH — Method Comparison",
      subtitle = "GAM fit with 95% shaded CI - legend ordered by R² (highest first)",
      x = "DBH (cm)",
      y = "Tree Biomass (kg/tree)",
      color = "Method (R²)",
      fill = "Method (R²)"
    ) +
    theme_bw(base_size = 13) +
    theme(legend.position = "bottom") +
    legend_theme
})

## Stratum upload (Plot ID + Stratum columns)----
stratum_data <- reactive({
  req(input$stratum_csv_file)
  read.csv(input$stratum_csv_file$datapath, stringsAsFactors = FALSE)
})

observeEvent(input$stratum_csv_file, {
  req(stratum_data())

  updateSelectInput(session, "stratum_plotid_col", choices = names(stratum_data()))
  updateSelectInput(session, "stratum_stratum_col", choices = names(stratum_data()))
})

## Revised wood density re-upload (edited from Wood Density Data download)----
wd_revised_data <- reactive({
  req(input$wd_revised_csv)
  read.csv(input$wd_revised_csv$datapath, stringsAsFactors = FALSE)
})

## Confirmed dataset: chosen wood density source + stratum + final equation----
flora_confirmed <- eventReactive(input$carbonconfirm, {

  req(input$final_allometric_method)
  req(stratum_data())
  req(input$stratum_plotid_col)
  req(input$stratum_stratum_col)

  wd_source_data <- if (identical(input$wd_source, "upload")) {
    req(wd_revised_data())
    wd_revised_data()
  } else {
    req(flora_with_wd())
    flora_with_wd()
  }

  show_modal_spinner(spin = "cube-grid", text = "Calculating final carbon stock with confirmed stratum...")
  on.exit(remove_modal_spinner(), add = TRUE)

  strat <- stratum_data() %>%
    dplyr::transmute(
      Plot.ID = as.character(.data[[input$stratum_plotid_col]]),
      Stratum = as.character(.data[[input$stratum_stratum_col]])
    ) %>%
    dplyr::distinct(Plot.ID, .keep_all = TRUE)

  wd_data <- wd_source_data %>%
    dplyr::rename(DBH.G = Class) %>%
    dplyr::mutate(Plot.ID = as.character(Plot.ID))

  agb <- calc_AGB(wd_data, method = input$final_allometric_method,
                  dbh_col = "DBH", h_col = "TT", rho_col = "wood_density") %>%
    add_agb_tpha()

  agb %>%
    dplyr::left_join(strat, by = "Plot.ID")
})

### Plot-level summary: AGB/AGC (t/ha) and tree density (stems/ha), by Stratum----
flora_plot_confirmed <- reactive({
  req(flora_confirmed())

  flora_confirmed() %>%
    dplyr::mutate(density_factor = dplyr::case_when(
      DBH.G == "A" ~ 4,
      DBH.G == "B" ~ 25,
      DBH.G == "C" ~ 100,
      TRUE ~ NA_real_
    )) %>%
    dplyr::group_by(Plot.ID, Stratum) %>%
    dplyr::summarise(
      AGB_tpha_plot = sum(`AGB(ton/ha)`, na.rm = TRUE),
      Density_ha = sum(density_factor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(AGC_tpha = AGB_tpha_plot * 0.47)
})

### Normal Q-Q plot of plot-level AGB (t/ha)----
output$agb_qq_plot <- renderPlot({

  req(flora_plot_confirmed())

  data <- flora_plot_confirmed()
  agb_skew <- round(moments::skewness(data$AGB_tpha_plot, na.rm = TRUE), 3)

  ggplot(data, aes(sample = AGB_tpha_plot)) +
    qqplotr::stat_qq_point(size = 2, color = "black") +
    qqplotr::stat_qq_line(color = "forestgreen") +
    labs(
      title = "Normal Q-Q Plot of AGB (t/ha) per Plot",
      subtitle = paste("Equation used:", input$final_allometric_method),
      x = "Theoretical quantiles (Normal)",
      y = "Sample quantiles"
    ) +
    theme_bw() +
    annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 2.5,
             label = paste("Skewness =", agb_skew), color = "blue", size = 5)
})

### Boxplot: distribution of plot-level AGB (t/ha)----
output$agb_normality_plot <- renderPlot({

  req(flora_plot_confirmed())

  ggplot(flora_plot_confirmed(), aes(x = "", y = AGB_tpha_plot)) +
    geom_boxplot(fill = "forestgreen", alpha = 0.5, outlier.colour = "red") +
    labs(
      x = NULL, y = "AGB per Plot (t/ha)",
      title = "Distribution of AGB per Plot",
      subtitle = paste("Equation used:", input$final_allometric_method)
    ) +
    theme_bw()
})

## Indonesia FRL 2nd reference table (AGB/AGC mean per national stratum)----
output$frl_ref_table <- renderDT({

  datatable(frl_stratum_ref, extensions = "Buttons", filter = "top",
            options = list(
              paging = TRUE,
              pageLength = 10,
              scrollX = TRUE,
              searching = TRUE,
              ordering = TRUE,
              dom = 'Bfrtip',
              buttons = dt_export_buttons(c("copy", "csv"))),
            escape = FALSE)
})

## Distribution of AGC across Plot (editable: Plot.ID, AGC, Stratum)----
agc_plot_edited <- reactiveVal(NULL)

observeEvent(flora_plot_confirmed(), {
  agc_plot_edited(
    flora_plot_confirmed() %>%
      dplyr::transmute(Plot.ID, Stratum, AGC = round(AGC_tpha, 2)) %>%
      as.data.frame()
  )
})

output$agc_plot_data_table <- renderDT({

  req(agc_plot_edited())

  datatable(agc_plot_edited(), extensions = "Buttons", editable = "cell", filter = "top",
            options = list(
              paging = TRUE,
              pageLength = 10,
              scrollX = TRUE,
              searching = TRUE,
              ordering = TRUE,
              dom = 'Bfrtip',
              buttons = dt_export_buttons(c("copy", "csv", "excel"))),
            escape = FALSE)
})

observeEvent(input$agc_plot_data_table_cell_edit, {
  info <- input$agc_plot_data_table_cell_edit
  df <- agc_plot_edited()
  df[info$row, info$col] <- DT::coerceValue(info$value, df[info$row, info$col])
  agc_plot_edited(df)
})

### Plot-level dataset with any user revisions applied (Density kept from the confirmed calculation)----
flora_plot_final <- reactive({
  req(agc_plot_edited())
  req(flora_plot_confirmed())

  agc_plot_edited() %>%
    dplyr::rename(AGC_tpha = AGC) %>%
    dplyr::left_join(
      flora_plot_confirmed() %>% dplyr::select(Plot.ID, Density_ha),
      by = "Plot.ID"
    )
})

## AGC distribution across plots, by user-confirmed/revised Stratum----
output$agc_by_stratum_plot <- renderPlot({

  req(flora_plot_final())

  data <- flora_plot_final() %>%
    dplyr::mutate(Plot.ID = forcats::fct_reorder(Plot.ID, AGC_tpha))

  ggplot(data, aes(x = Plot.ID, y = AGC_tpha, fill = Stratum)) +
    geom_col(width = 0.9) +
    geom_hline(yintercept = 100, linetype = 2) +
    geom_hline(yintercept = 250, linetype = 4) +
    scale_fill_viridis_d() +
    labs(
      x = "Plot Name", y = "Above Ground Carbon (tC/ha)",
      title = "AGC across plots by stratum"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    legend_theme
})

## Shared stratum-level statistical summary (Skew/Mean/SD/N/DF/t/SE/CI/Precision)----
compute_stratum_stats <- function(df, value_col) {
  df %>%
    dplyr::group_by(Stratum) %>%
    dplyr::summarise(
      Skew = round(moments::skewness(.data[[value_col]], na.rm = TRUE), 1),
      Mean = round(mean(.data[[value_col]], na.rm = TRUE), 1),
      SD   = round(sd(.data[[value_col]], na.rm = TRUE), 1),
      N    = sum(!is.na(.data[[value_col]])),
      DF   = pmax(N - 1, 0),
      `t-statistic at 95%` = round(ifelse(N >= 2, qt(0.975, DF), NA_real_), 1),
      SE   = round(ifelse(N > 0, SD / sqrt(N), NA_real_), 1),
      `95% Confidence interval` = round(`t-statistic at 95%` * SE, 1),
      `Precision (%)` = round(ifelse(Mean != 0, (SE / Mean) * 100, NA_real_), 1),
      `Lower 95% Confidence interval` = round(Mean - `t-statistic at 95%` * SE, 1),
      `Upper 95% Confidence interval` = round(Mean + `t-statistic at 95%` * SE, 1),
      .groups = "drop"
    )
}

### Mean Tree Density Across Plot (stems/ha, per Stratum)----
output$tree_density_table <- renderDT({

  req(flora_plot_final())

  stats_tbl <- compute_stratum_stats(flora_plot_final(), "Density_ha")

  datatable(stats_tbl, extensions = "Buttons", filter = "top",
            options = list(
              paging = FALSE,
              searching = TRUE,
              ordering = TRUE,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons = dt_export_buttons(c("copy", "csv", "excel"))),
            escape = FALSE)
})

### Mean Tree Density by DBH size class (sanity check: A vs B vs C)----
flora_density_by_class <- reactive({
  req(flora_confirmed())
  req(flora_plot_final())

  strat_lookup <- flora_plot_final() %>% dplyr::select(Plot.ID, Stratum)

  flora_confirmed() %>%
    dplyr::mutate(density_factor = dplyr::case_when(
      DBH.G == "A" ~ 4,
      DBH.G == "B" ~ 25,
      DBH.G == "C" ~ 100,
      TRUE ~ NA_real_
    )) %>%
    dplyr::select(-Stratum) %>%
    dplyr::left_join(strat_lookup, by = "Plot.ID") %>%
    dplyr::group_by(Plot.ID, Stratum, DBH.G) %>%
    dplyr::summarise(Density_ha = sum(density_factor, na.rm = TRUE), .groups = "drop")
})

output$tree_density_by_class_table <- renderDT({

  req(flora_density_by_class())

  wide_tbl <- flora_density_by_class() %>%
    dplyr::group_by(Stratum, DBH.G) %>%
    dplyr::summarise(Mean = round(mean(Density_ha, na.rm = TRUE), 1), .groups = "drop") %>%
    tidyr::complete(Stratum, DBH.G, fill = list(Mean = 0)) %>%
    tidyr::pivot_wider(names_from = DBH.G, values_from = Mean) %>%
    dplyr::rename_with(~ paste0("Class ", ., " (trees/ha)"), -Stratum)

  datatable(wide_tbl, extensions = "Buttons", filter = "top",
            options = list(
              paging = FALSE,
              searching = TRUE,
              ordering = TRUE,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons = dt_export_buttons(c("copy", "csv", "excel"))),
            escape = FALSE)
})

### Mean Weighted Carbon Stock (AGC tC/ha, per Stratum)----
output$mean_weighted_carbon_table <- renderDT({

  req(flora_plot_final())

  stats_tbl <- compute_stratum_stats(flora_plot_final(), "AGC_tpha")

  datatable(stats_tbl, extensions = "Buttons", filter = "top",
            options = list(
              paging = FALSE,
              searching = TRUE,
              ordering = TRUE,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons = dt_export_buttons(c("copy", "csv", "excel"))),
            escape = FALSE)
})

## Flora conservation----
### Upload validated species list (export from Species Validation step)----
flo_validated_species_data <- reactive({
  req(input$flo_species_cs_csv)
  read.csv(input$flo_species_cs_csv$datapath, stringsAsFactors = FALSE)
})

### Retrieve conservation status----
flo_species_list <- eventReactive(input$flo_searchcs, {

  speciesCons <- req(flo_validated_species_data())

  show_modal_spinner(spin = "cube-grid", text = "Preparing species list...")
  on.exit(remove_modal_spinner(), add = TRUE)

  # Reframe dataset - Class, Order and Family come directly from the
  # species validation export (GBIF-derived) instead of being re-derived
  # from IUCN, since IUCN doesn't have an entry for every species
  Species_df <- speciesCons %>%
    distinct(Species = Suggested.Name, Class, Order, Family) %>%
    mutate(
      genus = word(Species, 1),
      species = word(Species, 2))

  # IUCN and CITES only accept up to 50 species per request, so split into
  # batches of 50, query each batch separately, then recombine
  batch_id <- ceiling(seq_len(nrow(Species_df)) / 50)
  batches <- split(Species_df, batch_id)
  n_batches <- length(batches)

  batch_results <- purrr::imap(batches, function(batch_df, idx) {

    # Retrieve IUCN data (drop its own Class/Order/Family - we keep the
    # ones carried over from the species validation export instead)
    update_modal_spinner(text = paste0("Batch ", idx, "/", n_batches, ": querying IUCN Red List..."))
    sp1 <- get_iucn_species_data(api, batch_df) %>%
      select(-any_of(c("Class", "Order", "Family")))

    # Retrieve CITES data - some species aren't CITES-listed at all, which
    # retrieve_CITES_data() already handles by returning NA rather than
    # failing, so a partial miss here never stops the batch
    update_modal_spinner(text = paste0("Batch ", idx, "/", n_batches, ": querying CITES..."))
    sp2 <- retrieve_CITES_data(batch_df$Species) %>%
      distinct(Species, .keep_all = TRUE)

    left_join(sp1, sp2, by = "Species")
  })

  combined <- bind_rows(batch_results)

  # Combine IUCN, CITES and PP106 with the taxonomy carried over from upload
  result <- Species_df %>%
    select(Species, Class, Order, Family) %>%
    left_join(combined, by = "Species") %>%
    left_join(db, by = "Species") %>%
    select(Class, Order, Family, Species, `Common name`, Status, CITES_Appendix, Protected, Endemic, Migratory) %>%
    mutate(across(c(Class, Order, Family), tolower)) %>%
    mutate(across(c(Class, Order, Family), tools::toTitleCase)) %>%
    mutate(CITES_Appendix = ifelse(is.na(CITES_Appendix), "Not identified", CITES_Appendix)) %>%
    rename(Appendix = CITES_Appendix) %>%
    arrange(Order, Family, Species)

  result # Return the result
})

### Table of taxon and conservation status----
output$flo_splistcs <- renderDT({

  req(flo_species_list())

  datatable(flo_species_list(), extensions = "Buttons", filter = "top",
    options = list(
      paging = TRUE,
      pageLength = 20,
      lengthMenu = c(20, 50, 100),
      scrollX = TRUE,
      searching = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = dt_export_buttons()
    ),
    escape = FALSE
  )
})

### Treemap----
output$flo_treemap <- renderPlot({

  req(flo_species_list())

  rearranged_species <- flo_species_list() %>%
    group_by(Order, Family) %>%
    filter(Order != "Na") %>%
    summarise(Num_Species = n(), .groups = 'drop')

  treemap(rearranged_species, index = c("Order", "Family"),
          vSize = "Num_Species", type = "index",
          title = "",
          fontsize.labels = c(16, 12),
          fontcolor.labels = c("white", "white"),
          fontface.labels = c(2, 2),
          bg.labels = c("transparent"),
          align.labels = list(
            c("center", "center"),
            c("center", "bottom")
          ),
          overlap.labels = 0.3,
          lowerbound.cex.labels = 0.6,
          inflate.labels = F
          )
})

}
