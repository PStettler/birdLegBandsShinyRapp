require(shiny)
require(readxl)
require(readr)
require(openxlsx)
require(ggplot2)
require(stringr)
require(gplots)
require(farver)
require(dplyr)
require(tidyr)
require(purrr)
require(bslib)
#require(rio)


# Load your reference dataset once at app startup
color_dict <- read.csv("farben_endefr.csv", sep=";") # Use relative path
color_aehn <- as.matrix(read.csv("farben_flagging.csv", sep=";"))


my_leg_band_function <- function(input_df, 
                                 num_combinations, 
                                 sex_combinations,
                                 extra_colors_vec = NULL,
                                 nonavailable_colors_vec= NULL,
                                 ref_data = NULL) {
  
  # remove accents and other special letters from all colors
  color_dict$Deutsch = stringi::stri_trans_general(color_dict$Deutsch, "Latin-ASCII")
  color_dict$Franzözisch = stringi::stri_trans_general(color_dict$Franzözisch, "Latin-ASCII")
  
  # prep ahenlichkeitsmatrix
  rownames(color_aehn) = colnames(color_aehn)
  
  # ------------------------------------------------------------------------------
  # 1. COLOR ENGLISH FINDER
  # ------------------------------------------------------------------------------
  get_english_colors <- function(color_vector, color_df = color_dict, english_col = "English") {
    # 1. Sicherheitsprüfung
    if (!english_col %in% colnames(color_df)) {
      stop(paste0("Fehler: Die Spalte '", english_col, 
                  "' existiert nicht im Dataframe. Verfügbar: ", 
                  paste(colnames(color_df), collapse = ", ")))
    }
    
    # 2. Englische Zielnamen als reinen Text-Vektor auslesen
    english_vec <- as.character(color_df[[english_col]])
    
    # 3. Zuordnungstabelle spaltenweise aufbauen
    lookup_keys <- c()
    lookup_vals <- c()
    
    for (col_name in colnames(color_df)) {
      vals <- tolower(trimws(as.character(color_df[[col_name]])))
      lookup_keys <- c(lookup_keys, vals)
      lookup_vals <- c(lookup_vals, english_vec)
    }
    
    lookup <- setNames(lookup_vals, lookup_keys)
    valid <- !is.na(names(lookup)) & names(lookup) != ""
    lookup <- lookup[valid]
    lookup <- lookup[!duplicated(names(lookup))]
    
    # 4. Ergebnis-Vektor vorbereiten & NA-Werte isolieren
    result <- rep(NA_character_, length(color_vector))
    non_na_mask <- !is.na(color_vector)
    non_na_input <- color_vector[non_na_mask]
    
    non_na_clean <- tolower(trimws(as.character(non_na_input)))
    
    # 5. Fehlerprüfung NUR für Nicht-NA-Farben
    missing <- non_na_input[!non_na_clean %in% names(lookup)]
    
    if (length(missing) > 0) {
      stop(paste("Fehler: Folgende Farbe(n) sind nicht im Dataframe enthalten:", 
                 paste(unique(missing), collapse = ", ")))
    }
    
    # 6. Werte an den exakten Positionen zuweisen
    result[non_na_mask] <- unname(lookup[non_na_clean])
    
    return(result)
  }
  
  
  
  # ------------------------------------------------------------------------------
  # 1. USER FILE Formatting
  # ------------------------------------------------------------------------------
  format_bird_file <- function(input_data) {
    # Ensure standard data frame and character types
    input_data_formatted <- as.data.frame(input_data)
    input_data_formatted[] <- lapply(input_data_formatted, as.character)
    return(input_data_formatted)
  }
  
  # ------------------------------------------------------------------------------
  # 2. MATCHING HELPER
  # ------------------------------------------------------------------------------
  find_matching_col <- function(df, pattern) {
    match_counts <- map_dbl(df, function(col_vec) {
      clean_vals <- col_vec[!is.na(col_vec) & col_vec != ""]
      if (length(clean_vals) == 0) return(0)
      mean(str_detect(clean_vals, pattern))
    })
    
    best_idx <- which.max(match_counts)
    if (length(best_idx) > 0 && match_counts[best_idx] > 0.25) {
      return(names(df)[best_idx])
    }
    return(NULL)
  }
  
# ------------------------------------------------------------------------------
  # 3. LEG BAND PARSER (Preserves original row count)
  # ------------------------------------------------------------------------------
  parse_leg_bands <- function(df) {
    n_rows <- nrow(df)
    col_names <- colnames(df)
    
    regex_left_hdr  <- "(?i)^(left|links|l|left_leg|leg_left|gauge|g)$"
    regex_right_hdr <- "(?i)^(right|rechts|r|right_leg|leg_right|droit|d)$"
    
    left_col  <- col_names[str_detect(col_names, regex_left_hdr)][1]
    right_col <- col_names[str_detect(col_names, regex_right_hdr)][1]
    
    
    clean_band_color <- function(color_vec) {
      
      # Regex for color names
      color_pattern <- "(?i)\\b[a-z]{3,}\\b" #simple search for any color word
      
      sapply(color_vec, function(val) {
        if (is.na(val) || str_trim(val) == "") return(NA_character_)
        
        # REMOVE ACCENTS / DIACRITICS (e.g., 'dunkelgrün' -> 'dunkelgrun', 'bleu foncé' -> 'bleu fonce')
        clean_val <- stringi::stri_trans_general(val, "Latin-ASCII")
        
        # Clean up leading 'L:', 'R:', 'links:', 'rechts:' residual prefixes if present
        clean_val <- str_remove(clean_val, "(?i)^\\b(left|right|links|rechts|gauge|droit|[lrgd])\\b[:\\s=-]*")
        
        # Case 1 & 2: Contains a recognizable color word (with or without index)
        extracted_color <- str_extract(clean_val, color_pattern)[1] #in case more than one match
        
        if (!is.na(extracted_color)) {
          return(tolower(extracted_color)) # Returns 'braun' from 'braun TaI'
        }
        
        # Case 3: Metal ring with code/numbers and no explicit color name
        # Checks if string contains numbers or alphanumeric code characters
        if (str_detect(clean_val, "[0-9A-Z]")) {
          return("gray")
        }
        
        return(clean_val) # Fallback for unknown entries
      }, USE.NAMES = FALSE)
    }
    
    
    # Case A: Two distinct leg columns exist
    if (!is.na(left_col) && !is.na(right_col)) {
      return(data.frame(
        Left  = clean_band_color(df[[left_col]]),
        Right = clean_band_color(df[[right_col]]),
        stringsAsFactors = FALSE
      ))
    }
    
    # Case B: Single column search
    scores <- map_dbl(df, function(col_vec) {
      clean_vals <- col_vec[!is.na(col_vec)]
      if (length(clean_vals) == 0) return(0)
      mean(str_detect(clean_vals, "(?i)(left|right|links|rechts|gauge|droit|[a-zA-Z]+\\s*[/,]\\s*[a-zA-Z]+|\\b[lrgd][:;-]?\\b)"))
    })
    
    best_col_idx <- which.max(scores)
    
    if (length(best_col_idx) == 0 || scores[best_col_idx] == 0) {
      return(data.frame(
        left_leg  = rep(NA_character_, n_rows),
        right_leg = rep(NA_character_, n_rows),
        stringsAsFactors = FALSE
      ))
    }
    
    # Extract single column as pure atomic vector
    raw_bands <- df[[best_col_idx]]
    
    regex_left  <- "(?i)\\b(?:left|links|gauge|[gl])[:;-]?\\b[:\\s=-]*([\\p{L}0-9\\s-]+)"
    regex_right <- "(?i)\\b(?:right|rechts|droit|[rd])[:;-]?\\b[:\\s=-]*([\\p{L}0-9\\s-]+)"
    
    valid_vals <- raw_bands[!is.na(raw_bands)]
    has_explicit_sides <- any(str_detect(valid_vals, "(?i)\\b(left|links|right|rechts|gauge|droit|[rlgd])[:;-]?\\b"))
    
    if (has_explicit_sides) {
      left_vals  <- str_match(raw_bands, regex_left)[, 2]
      right_vals <- str_match(raw_bands, regex_right)[, 2]
    } else {
      # Split single string by '/' or ',' while maintaining NAs
      split_bands <- str_split_fixed(raw_bands, "\\s*[/,]\\s*", 2)
      left_vals   <- split_bands[, 1]
      right_vals  <- split_bands[, 2]
      
      # Clean empty strings resulting from splits
      left_vals[left_vals == ""]   <- NA_character_
      right_vals[right_vals == ""] <- NA_character_
    }
    
    left_vals  <- clean_band_color(left_vals)
    right_vals <- clean_band_color(right_vals)
    
    return(data.frame(
      Left  = str_trim(left_vals),
      Right = str_trim(right_vals),
      stringsAsFactors = FALSE
    ))
  }

  
  # ------------------------------------------------------------------------------
  # 4. MAIN USER DATA PIPELINE
  # ------------------------------------------------------------------------------
  process_bird_data <- function(input_data) {
    raw_df <- format_bird_file(input_data)
    
    regex_id  <- "^(?=.*[A-Za-z])(?=.*[0-9])[A-Za-z0-9]+$"
    regex_sex <- "^(?i)(\\b[01]\\.[01](\\.[01])?|m|f|u|w|male|female|undetermined|männ.*|weib.*|unbe.*)$"
    
    id_col  <- find_matching_col(raw_df, regex_id)
    sex_col <- find_matching_col(raw_df, regex_sex)
    
    # Clean the sex_col to only contain Male / Female / Unde.
    if (!is.null(sex_col)) { 
        sex_map <- c(
          "male" = "Male", "männlich" = "Male", "männchen" = "Male", "mâle" = "Male", "m" = "Male", "1.0" = "Male", "1.0.0" = "Male",
          "female" = "Female", "weiblich" = "Female", "weibchen" = "Female", "femelle" = "Female", "f" = "Female", "0.1" = "Female", "0.1.0" = "Female"
        )
        
        sex = raw_df[[sex_col]]
        sex <- tolower(trimws(as.character(sex)))
        sex <- unname(sex_map[sex])
        
        # Fehlende Matches (NA) mit "U" auffüllen
        sex <- as.character(ifelse(is.na(sex), "Unde.", sex))
        } else {sex = rep(NA_character_, nrow(raw_df))}
    
    leg_df <- parse_leg_bands(raw_df)
    
    clean_df <- data.frame(
      id        = if (!is.null(id_col)) raw_df[[id_col]] else rep(NA_character_, nrow(raw_df)),
      sex       = sex,
      Left  = get_english_colors(leg_df$Left),
      Right = get_english_colors(leg_df$Right),
      stringsAsFactors = FALSE
    )
    
    return(clean_df)
  }
  
  
  # ------------------------------------------------------------------------------
  # 5. COLOR ANALYSIS AND COLOR COMBINATION GENERATION
  # ------------------------------------------------------------------------------
  color_combinations = function(user_data, additional_colors = extra_colors_vec,
                                non_available_colors = nonavailable_colors_vec,
                                aehnlichkeitsMatrix = color_aehn) {
    
    # 1.A FARBENLISTE DER RINGE ERSTELLEN
    farben = na.omit(unique(c(user_data$Left,user_data$Right)))
    
    # add additional user data
    if(!is.null(additional_colors) & length(additional_colors)>0) {
      farben = unique(c(farben, get_english_colors(tolower(stringi::stri_trans_general(additional_colors, "Latin-ASCII")))))}
    
    n_farben <- length(farben)
    
    # 1.B Definiere hier einfach die Farben, die aktuell physisch NICHT mehr auf Lager sind
    ausgegangene_farben <- unique(c("gray", get_english_colors(tolower(stringi::stri_trans_general(non_available_colors, "Latin-ASCII"))))) # Füge hier die Farbe(n) hinzu, die leer sind, 
    # grau ist standard (nicht zu unterscheiden von metallringen)
    
    # 2. ÄHNLICHKEITS-MATRIX loaded (from file)
    aehnlichkeit <- aehnlichkeitsMatrix
    
    # 3. ALLE THEORETISCH MÖGLICHEN KOMBINATIONEN GENERIEREN & HARTE FILTER
    # Für die neuen Kombinationen nutzen wir nur noch die verfügbaren Farben
    verfuegbare_farben_pool <- farben[!(farben %in% ausgegangene_farben)]
    
    # (Wir nutzen 'verfuegbare_farben_pool' statt 'farben'):
    alle_kombis <- expand.grid(Left = verfuegbare_farben_pool, Right = verfuegbare_farben_pool, stringsAsFactors = FALSE)
    alle_kombis <- as.matrix(alle_kombis)
    
    ist_erlaubt <- rep(FALSE, nrow(alle_kombis))
    for (i in 1:nrow(alle_kombis)) {
      if (aehnlichkeit[alle_kombis[i, "Left"], alle_kombis[i, "Right"]] == 0) {
        ist_erlaubt[i] <- TRUE
      }
    }
    erlaubte_kombis <- alle_kombis[ist_erlaubt, ]
    return(erlaubte_kombis)
    
  }
  
  
  # ------------------------------------------------------------------------------
  # 6. CALCULATION OF NEW COMBINATIONS
  # ------------------------------------------------------------------------------
  berechne_neue_ringe <- function(geschlecht_neu, anzahl_gesucht, ex_df, erl_kombis, aehnl_mat = color_aehn) {
    
    # --- LOGIK FÜR BESTANDSABGLEICH ---
    if (tolower(geschlecht_neu) == "any") {
      # Bei "Any" werden alle Zeilen übernommen
      ex_aktuell <- ex_df[, c("Left", "Right"), drop = FALSE]
    } else {
      # Filtert genau auf das neue Geschlecht ODER unbestimmte Tiere ("Unbe.")
      zeilen_index <- ex_df$sex %in% c(geschlecht_neu, "Unbe.")
      ex_aktuell <- ex_df[zeilen_index, c("Left", "Right"), drop = FALSE]
    }
    
    ex_aktuell <- as.matrix(ex_aktuell)
    
    # Schritt A: Bereits exakt vergebene Kombinationen löschen
    if (nrow(ex_aktuell) > 0) {
      bereits_vergeben <- rep(FALSE, nrow(erl_kombis))
      for (i in 1:nrow(erl_kombis)) {
        for (j in 1:nrow(ex_aktuell)) {
          
          # Wir holen uns die Werte des Bestandsvogels
          ex_l <- ex_aktuell[j, 1]
          ex_r <- ex_aktuell[j, 2]
          
          # KORREKTUR: Nur vergleichen, wenn der Bestandsvogel KEIN NA auf beiden Seiten hat.
          # Wenn er ein NA hat, ist es keine exakte 2-Ring-Übereinstimmung mit erl_kombis.
          if (!is.na(ex_l) && !is.na(ex_r)) {
            if (erl_kombis[i, "Left"] == ex_l && 
                erl_kombis[i, "Right"] == ex_r) {
              bereits_vergeben[i] <- TRUE
            }
          }
          
        }
      }
      verfuegbare_kombis <- erl_kombis[!bereits_vergeben, ]
    } else {
      verfuegbare_kombis <- erl_kombis
    }
    
    if (nrow(verfuegbare_kombis) < anzahl_gesucht) {
      stop(paste("Not enough combinations possible for sex: ", geschlecht_neu))
    }
    
    # Schritt B: Scoring (Strafpunktevorteilung gegen relevanten Bestand)
    scores <- rep(0, nrow(verfuegbare_kombis))
    
    if (nrow(ex_aktuell) > 0) {
      for (i in 1:nrow(verfuegbare_kombis)) {
        cand_l <- verfuegbare_kombis[i, "Left"]
        cand_r <- verfuegbare_kombis[i, "Right"]
        straf_score <- 0
        
        for (j in 1:nrow(ex_aktuell)) {
          ex_l <- ex_aktuell[j, 1]
          ex_r <- ex_aktuell[j, 2]
          
          # 1. Ein-Bein-Konflikt (Nur strafen, wenn der Bestandsring existiert und übereinstimmt)
          if (!is.na(ex_l) && cand_l == ex_l) straf_score <- straf_score + 3
          if (!is.na(ex_r) && cand_r == ex_r) straf_score <- straf_score + 3
          
          # 2. Ähnlichkeits-Konflikt (Nur prüfen, wenn der Bestandsring existiert)
          if (!is.na(ex_l)) straf_score <- straf_score + (aehnl_mat[cand_l, ex_l] * 2) 
          if (!is.na(ex_r)) straf_score <- straf_score + (aehnl_mat[cand_r, ex_r] * 2) 
          
          # 3. Spiegelungs-Konflikt (Nur prüfen, wenn BEIDE Bestandsringe existieren)
          if (!is.na(ex_l) && !is.na(ex_r)) {
            if (cand_l == ex_r && cand_r == ex_l) straf_score <- straf_score + 4
          }
        }
        scores[i] <- straf_score
      }
    }
    
    # Ergebnisse zusammenführen und sortieren
    ergebnis_df <- data.frame(verfuegbare_kombis, Score = scores, stringsAsFactors = FALSE)
    ergebnis_df <- ergebnis_df[order(ergebnis_df$Score), ]
    
    # Die besten N Kombinationen extrahieren
    beste_auswahl <- head(ergebnis_df, anzahl_gesucht)
    beste_auswahl$Sex <- geschlecht_neu
    
    return(beste_auswahl)
  }
  
  
  
  # ------------------------------------------------------------------------------
  # 7. RUN USER DATA THROUGH
  # ------------------------------------------------------------------------------
  
  #Process input bird data
  user_dat_processed = process_bird_data(input_df)
  
  #Generate allowed combinations
  color_combos_allowed = color_combinations(user_data = user_dat_processed)
  
  #Run new combinations
  out_df = berechne_neue_ringe(geschlecht_neu = sex_combinations, anzahl_gesucht = num_combinations,
                               ex_df = user_dat_processed, erl_kombis = color_combos_allowed)
  
  #out_df is final output file
  
  #Plot for visualization
  df_vis <- data.frame(
    vogel_id = rev(c(user_dat_processed$id, paste0("New ", 1:nrow(out_df)))),
    ring_links = rev(c(user_dat_processed$Left, out_df$Left)),
    ring_rechts = rev(c(user_dat_processed$Right, out_df$Right)),
    group = rev(c(user_dat_processed$sex, paste0("New (",out_df$Sex, ")")))
  )
  
  
  # Return BOTH the table and the recorded base plot
  return(list(table = out_df, 
              for_plot = df_vis,
              plot_width = length(table(df_vis$group))*100,
              plot_height = max(table(df_vis$group))*18+250))
  
  
}#function end

  draw_leg_band_plot <- function(df) {
    
    # empty plot as basis, ylim adapted to largest group size
    plot(NULL, 
         xlim=c(0,length(unique(df$group))*2), ylim=c(0, max(table(df$group))+3),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty="n")
    
    # identifier colors are added one-by-one. xpos and group_tick_po vectors 
    # define the x-axis position per group.
    # Groups and contained individuals as looped and stacked ontop of 
    # oneanother in the plot. Order is reversed from input for final
    # top-down stacking.
    xpos = 0
    for(group in unique(df$group)){
      subgroup = df[which(df$group==group),]
      n_subgroup = nrow(subgroup)
      xpos = xpos+1
      group_tick_pos = seq(1,length(unique(df$group))*2, by=2)[xpos]
      text(group_tick_pos, 0, as.character(group), font = 2, cex=1.4)
      for(i in 1:n_subgroup){
        lines(c(group_tick_pos-.5,group_tick_pos+.5),c(i,i),lwd=2)
        points(c(group_tick_pos-.5,group_tick_pos+.5),c(i,i),cex=3,
               pch = ifelse(is.na(subgroup[i,2:3]),NA,21) ,
               bg  = as.character(subgroup[i,2:3]),
               col = ifelse(is.na(subgroup[i,2:3]),"white",1))
        text(group_tick_pos, i+.4, labels = as.character(subgroup$vogel_id[i]))
      }
    }
  }

# ==============================================================================
# 1. USER INTERFACE (UI)
# ==============================================================================
ui <- fluidPage(
  titlePanel(
    tagList(
      "🦜 Zoo Bird Leg Band Organizer",
      actionLink(
        inputId = "show_info_btn",
        label = NULL,
        icon = icon("circle-info"),
        style = "font-size: 0.8em; color: #17a2b8; margin-left: 10px; cursor: pointer;",
        title = "Click for algorithm details"
      )
    )
  ),
  
  sidebarLayout(
    # LINKS: Eingabefelder & Downloads
    sidebarPanel(
      # File Upload (Excel or CSV)
      fileInput("bird_file", "1. Upload Bird Data File",
                accept = c(".xlsx", ".xls", ".csv", ".tsv", ".txt")),
      
      # Required Input 1: Number of new combinations
      numericInput("num_combos", 
                   "2. Number of new combinations *", 
                   value = 3, 
                   min = 1, 
                   step = 1),
      
      # Required Input 2: Sex of new combinations
      selectInput("sex_combos", 
                  "3. Sex of new combinations *", 
                  choices = c("Select option..." = "", "Male", "Female", "Any"),
                  selected = "Any"),
      
      # Optional Extra Colors Input Box
      textInput("extra_colors", 
                "4. Optional Additional Colors", 
                value = "", 
                placeholder = "e.g., Red, Greenyellow, Purple"),
      helpText("Separate multiple colors with commas."),
      
      # Optional Non-available Colors Input Box
      textInput("nonavailable_colors", 
                "5. Non-available Colors", 
                value = "", 
                placeholder = "e.g., Red, Greenyellow, Purple"),
      helpText("Separate multiple colors with commas."),
      
      br(),
      
      # Action Button
      actionButton("run_btn", "Generate Combinations", 
                   class = "btn-primary btn-lg", 
                   style = "width: 100%;"),
      
      hr(),
      
      # Download Section (Erscheint hier links, sobald Ergebnisse da sind)
      uiOutput("downloads_ui")
    ),
    
    # RECHTS: Zeigt entweder Info-Text ODER Ergebnisse (Tabelle & Plot)
    mainPanel(
      uiOutput("main_content")
    )
  )
)

# ==============================================================================
# 2. SERVER LOGIC
# ==============================================================================
  server <- function(input, output, session) {
    
    # 1. Reaktiver Speicher für die Ergebnisse
    calc_results <- reactiveVal(NULL)
    
    # 2. Track view state ("info" or "results")
    current_view <- reactiveVal("info")
    
    # 3. Switch to "info" view when info icon/button is clicked
    observeEvent(input$show_info_btn, {
      current_view("info")
    })
    
    # 4. Test data set
    observeEvent(input$demo_btn, {
      demo_file_path <- "example_bird_data.csv" # Ensure this file is in your app folder
      
      if (!file.exists(demo_file_path)) {
        showNotification("Demo file 'example_bird_data.csv' was not found in the app directory.", type = "error")
        return(NULL)
      }
      
      # 1. Read demo file
      ext <- tools::file_ext(demo_file_path)
      demo_df <- if (ext == "csv") {
        readr::read_csv2(demo_file_path, show_col_types = FALSE, 
                         locale = readr::locale(encoding = "Latin1"))
      } else {
        readxl::read_excel(demo_file_path)
      }
      
      # 2. Pre-fill sidebar inputs with demo defaults
      updateNumericInput(session, "num_combos", value = 3)
      updateSelectInput(session, "sex_combos", selected = "Any")
      
      # 3. Execute function
      res <- tryCatch({
        my_leg_band_function(
          input_df                   = demo_df, 
          num_combinations           = 3,
          sex_combinations           = "Any",
          extra_colors_vec           = NULL,
          nonavailable_colors_vec = NULL,
          ref_data                   = reference_data
        )
      }, error = function(e) {
        showNotification(paste("Demo Error:", e$message), type = "error", duration = 10)
        return(NULL)
      })
      
      # 4. Save results & switch view to "results"
      req(res)
      calc_results(res)
      current_view("results") # Switches from "info" to results tabsetPanel automatically!
      
      showNotification("Demo data calculated successfully!", type = "message")
    })
    
    
    # 5. Ausführung NUR beim Klick auf "Generate Combinations"
    observeEvent(input$run_btn, {
      req(input$bird_file)
      
      # Validierung der Eingaben
      if (is.null(input$num_combos) || is.na(input$num_combos) || input$num_combos < 1) {
        showNotification("Please specify a valid 'Number of new combinations' (1 or more).", type = "error")
        return()
      }
      
      if (!nzchar(input$sex_combos)) {
        showNotification("Please select a 'Sex of new combinations'.", type = "error")
        return()
      }
      
      # Datei einlesen (file_path definiert)
      file_path <- input$bird_file$datapath
      ext       <- tools::file_ext(input$bird_file$name)
      
      raw_df <- tryCatch({
        switch(ext,
               "csv" = {
                 # Try standard CSV (comma) first
                 df <- readr::read_csv(file_path, show_col_types = FALSE, progress = FALSE)
                 
                 # If European CSV (;), read_csv loads everything into 1 column
                 if (ncol(df) <= 1) {
                   df <- readr::read_csv2(
                     file_path, 
                     show_col_types = FALSE, 
                     locale = readr::locale(encoding = "Latin1")
                   )
                 }
                 df
               },
               "xlsx" = readxl::read_excel(file_path),
               "xls"  = readxl::read_excel(file_path),
               "tsv"  = readr::read_tsv(file_path, show_col_types = FALSE),
               "txt"  = data.table::fread(file_path, data.table = FALSE),
               stop("Invalid file type! Please upload a .csv, .tsv, or .xlsx file.")
        )
      }, error = function(e) {
        showNotification(paste("File Read Error:", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(raw_df)) return()
      
      # Optionale Farben verarbeiten
      extra_colors_vector <- NULL
      if (nzchar(trimws(input$extra_colors))) {
        extra_colors_vector <- unlist(strsplit(input$extra_colors, ","))
        extra_colors_vector <- trimws(extra_colors_vector)
        extra_colors_vector <- extra_colors_vector[extra_colors_vector != ""]
      }
      
      # Optionale non-available Farben verarbeiten
      nonavailable_colors_vector <- NULL
      if (nzchar(trimws(input$nonavailable_colors))) {
        nonavailable_colors_vector <- unlist(strsplit(input$nonavailable_colors, ","))
        nonavailable_colors_vector<- trimws(nonavailable_colors_vector)
        nonavailable_colors_vector <- nonavailable_colors_vector[nonavailable_colors_vector != ""]
      }
      
      # Hauptfunktion ausführen
      res <- tryCatch({
        my_leg_band_function(
          input_df                = raw_df, 
          num_combinations        = input$num_combos,
          sex_combinations        = input$sex_combos,
          extra_colors_vec        = extra_colors_vector,
          nonavailable_colors_vec = nonavailable_colors_vector,
          ref_data                = reference_data
        )
      }, error = function(e) {
        showNotification(
          ui       = paste("Validation Error:", e$message), 
          type     = "error", 
          duration = 10
        )
        return(NULL)
      })
      
      # Ergebnis in reactiveVal speichern
      calc_results(res)
      
      # Switch view to "results" on success
      if (!is.null(res)) {
        current_view("results")
      }
    })
    
    # 3. Dynamic main content output (Info vs Results)
    output$main_content <- renderUI({
      if (current_view() == "info") {
        # INFO VIEW
        wellPanel(
          style = "background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 20px;",
          h3("About the Leg Band Optimization Algorithm"),
          p("This application optimizes color-band combinations for birds in a zoo registry to minimize visual confusion and collision risk."),
          hr(),
          h4("How it works:"),
          tags$ul(
            tags$li(tags$b("Similarity Matrix:"), " Compares human color perception to identify pairs that look too similar."),
            tags$li(tags$b("Existing Registry Check:"), " Scans currently used combinations in your uploaded file to calculate as different as possible combinations."),
            tags$li(tags$b("Recommendations:"), " The program returns a defined number of new combinations. These combinations have been calculated by their color-collision risk. Lower scores represent safer color pairings. Important, chose only one ofthese combinations for the next bird to mark. Calculate new colors everytime for new birds.")
          ),
          p(em("Upload your file on the left panel, select wheter the combinations should be calculated for male, female, or any new bird. The program will assume all colors available have already been used on the current birds. If not, you can optionally add new colors, or name colors that should be excluded from the calculations. Then click 'Generate Combinations' to start.")),
          
          # ------------------------------------------------------------------------
          # Demo Data Button
          # ------------------------------------------------------------------------
          div(
            style = "margin-top: 15px; margin-bottom: 15px; text-align: center;",
            actionButton(
              "demo_btn", 
              "📋 Run Example Dataset", 
              class = "btn-info btn-lg", 
              style = "font-weight: bold; width: 60%;"
            )
          ),
          # ------------------------------------------------------------------------
          
          hr(),
          h5("Trouble shooting:"),
          tags$ul(
            tags$li(tags$b("Data loading error:"), " The program is designed to automatically detect the required coulmns in your data. The provided file must contain a column with animal IDs, one with animal sexes, and a least one column stating the band colors. As a rule of thumb, if the data in the file can be interpreted by a human, this programm should be able to so too. If the program fails anyways, try to provide the data in an 'easier' format (e.g. one column with left identifiers, one with right identifiers)"),
            tags$li(tags$b("Color not known:"), " The program runs on a databse of ~150 color names in english, german, and french language. Nevertheless, a color might be unknown to the program because of writing differences or other reasons. The unknown color name will be displayed in the erorr message. Spell this color differently in the provided dataset or replace it with a name of a similar color."),
            tags$li(tags$b("File input problems:"), " This program allows various input file types (.xslx, .xls, .csv, ...). However, errors may occour with some data file types, espiecially if the input data contains special characters and the encoding type does not match the expected data encoding type. This program is robust, but cannot do magic. It is commended to upload data from .xlsxs or .csv files to not risk encoding problems."),
          ),
          hr(),
          p("Created by Philip Stettler, Papiliorama Kerzers.")
            )
      } else {
        # 2. ERGEBNIS-ANSICHT (Schaltbare Tabs)
          tabsetPanel(
          tabPanel("Table Preview", br(), h4("Recommended leg band combinations:"), tableOutput("output_table")),
          tabPanel("Plot Preview", br(), h4("Recommended leg band combinations:"), plotOutput("output_plot"))
          ) 
      }
    })
    
    
    # 4. OUTPUTS (Müssen auf oberster Ebene der Server-Funktion stehen!)
    
    # Tabelle anzeigen
    output$output_table <- renderTable({
      req(calc_results())
      calc_results()$table
    })
    
    # Plot in der App anzeigen (Nutzt draw_leg_band_plot statt replayPlot)
    output$output_plot <- renderPlot({
      req(calc_results())
      draw_leg_band_plot(calc_results()$for_plot)
    }, height = function() {
      req(calc_results())
      calc_results()$plot_height
    })
    
    # Download-Buttons anzeigen
    output$downloads_ui <- renderUI({
      req(calc_results())
      tagList(
        h4("Download Results:"),
        downloadButton("download_excel", "Download Data (Excel)", 
                       class = "btn-success", style = "width: 100%; margin-bottom: 8px;"),
        downloadButton("download_plot", "Download Plot (PNG)", 
                       class = "btn-info", style = "width: 100%;")
      )
    })
    
    # Excel-Download Handler
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("Optimized_Leg_Bands_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        openxlsx::write.xlsx(calc_results()$table, file)
      }
    )
    
    # PNG-Download Handler (Sauberes Neuzeichnen ohne replayPlot)
    output$download_plot <- downloadHandler(
      filename = function() { paste0("Leg_Bands_Plot_", Sys.Date(), ".png") },
      content = function(file) {
        res <- calc_results()
        req(res)
        
        png(file, width = res$plot_width*6, height = res$plot_height*3.75, res = 260)
        draw_leg_band_plot(res$for_plot)
        dev.off()
      }
    )
  }

# Run the app locally
shinyApp(ui = ui, server = server)
