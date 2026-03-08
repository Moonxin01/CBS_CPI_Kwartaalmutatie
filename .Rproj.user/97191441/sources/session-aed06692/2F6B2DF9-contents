# Installeren en inladen
install.packages("cbsodataR")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("DBI")
install.packages("RSQLite")

library(cbsodataR)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)

# Data ophalen
haal_data_op <- function() {
  
  cbs_get_data(
    id                    = "83131NED",
    Bestedingscategorieen = c("CPI041100", "CPI072200"),
    Perioden              = has_substring("MM"),
    select                = c("Bestedingscategorieen", "Perioden", "CPI_1")
  ) |>
    cbs_add_label_columns()
}

# Opschonen en datum afleiden
schoon_data <- function(raw) {
  
  raw |>
    mutate(
      jaar     = as.integer(substr(Perioden, 1, 4)),
      maand    = as.integer(substr(Perioden, 7, 8)),
      kwartaal = ceiling(maand / 3),
      categorie_label = case_when(
        grepl("Huur",    Bestedingscategorieen_label, ignore.case = TRUE) ~ "Huur van woning",
        grepl("Benzine", Bestedingscategorieen_label, ignore.case = TRUE) ~ "Benzine",
        TRUE ~ Bestedingscategorieen_label
      )
    ) |>
    filter(!is.na(CPI_1))
}


# Filteren op verslagperiode
filter_periode <- function(data, verslagperiode) {
  
  if (is.null(verslagperiode)) return(data)
  
  if (nchar(verslagperiode) == 4) {
    data |> filter(jaar == as.integer(verslagperiode))
  } else {
    jaar_filter <- as.integer(substr(verslagperiode, 1, 4))
    kw_filter   <- as.integer(substr(verslagperiode, 7, 8))
    data |> filter(jaar > jaar_filter |
                     (jaar == jaar_filter & kwartaal >= kw_filter))
  }
}

# Kwartaalgemiddelde berekenen
bereken_kwartaalgemiddelde <- function(data) {
  
  data |>
    group_by(categorie_label, jaar, kwartaal) |>
    summarise(gem_index = mean(CPI_1, na.rm = TRUE),
              n_maanden = n(),
              .groups   = "drop") |>
    filter(n_maanden == 3) |>
    arrange(categorie_label, jaar, kwartaal)
}

# Kwartaalmutatie berekenen
bereken_mutatie <- function(kwartaal_data) {
  
  kwartaal_data |>
    group_by(categorie_label) |>
    mutate(
      kwartaalmutatie_pct = round((gem_index / lag(gem_index) - 1) * 100, 2),
      periode_label       = paste0(jaar, " K", kwartaal)
    ) |>
    ungroup() |>
    filter(!is.na(kwartaalmutatie_pct))
}

# Lijngrafiek
maak_lijngrafiek <- function(data) {
  
  unieke_labels <- unique(data$categorie_label)
  kleuren       <- setNames(c("#00519E", "#F07D00")[seq_along(unieke_labels)],
                            unieke_labels)
  
  recent <- data |>
    filter(jaar >= (max(jaar) - 4))
  
  ggplot(recent, aes(x = periode_label, y = kwartaalmutatie_pct,
                     color = categorie_label, group = categorie_label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = kleuren) +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(title    = "Kwartaalmutatie consumentenprijsindex",
         subtitle = "Huur van woning en Benzine (83131NED)",
         x = NULL, y = "Mutatie t.o.v. vorig kwartaal (%)",
         color = NULL, caption = paste("Berekend op:", Sys.Date())) +
    theme_minimal(base_size = 12) +
    theme(legend.position  = "bottom",
          plot.title       = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          axis.text.x      = element_text(size = 8))
}

# Staafdiagram
maak_staafdiagram <- function(data) {
  
  unieke_labels <- unique(data$categorie_label)
  kleuren       <- setNames(c("#00519E", "#F07D00")[seq_along(unieke_labels)],
                            unieke_labels)
  
  recent <- data |>
    group_by(categorie_label) |>
    slice_tail(n = 8) |>
    ungroup()
  
  ggplot(recent, aes(x = periode_label, y = kwartaalmutatie_pct,
                     fill = categorie_label)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_hline(yintercept = 0, color = "grey30") +
    scale_fill_manual(values = kleuren) +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(title    = "Kwartaalmutatie CPI: laatste 8 kwartalen",
         subtitle = "Huur van woning en Benzine (83131NED)",
         x = NULL, y = "Mutatie t.o.v. vorig kwartaal (%)",
         fill = NULL, caption = paste("Berekend op:", Sys.Date())) +
    theme_minimal(base_size = 12) +
    theme(legend.position  = "bottom",
          plot.title       = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          axis.text.x      = element_text(size = 9))
}

# Opslaan in SQLite database
sla_op_in_database <- function(data, db_pad = "output/cpi_resultaten.db") {
  
  con <- dbConnect(RSQLite::SQLite(), db_pad)
  on.exit(dbDisconnect(con))
  
  dbWriteTable(con, "cpi_kwartaalmutatie", data, overwrite = TRUE)
  
  dbWriteTable(con, "run_log", data.frame(
    run_timestamp = as.character(Sys.time()),
    n_rijen       = nrow(data),
    categorieen   = paste(unique(data$categorie_label), collapse = " | "),
    r_versie      = R.version$version.string
  ), append = TRUE)
}

# Hoofdproces
main <- function(verslagperiode = NULL) {
  
  dir.create("output", showWarnings = FALSE)
  
  resultaat <- haal_data_op() |>
    schoon_data() |>
    filter_periode(verslagperiode) |>
    bereken_kwartaalgemiddelde() |>
    bereken_mutatie()
  
  ggsave("output/grafiek1_lijngrafiek.png",
         maak_lijngrafiek(resultaat), width = 10, height = 5.5, dpi = 150)
  
  ggsave("output/grafiek2_staafdiagram.png",
         maak_staafdiagram(resultaat), width = 10, height = 5.5, dpi = 150)
  
  sla_op_in_database(resultaat)
  
  invisible(resultaat)
}

# Uitvoering
resultaat <- main()

# resultaat <- main(verslagperiode = "2024")
# resultaat <- main(verslagperiode = "2023KW01")