# Make sure required checkpoint is installed
if(tryCatch(packageVersion("checkpoint") <= '1.0.0', error = function(e) T)) {
  install.packages("checkpoint")}

# Checkpoint installs packages
checkpoint::create_checkpoint(
  "2022-02-13", checkpoint_location = Sys.getenv("USERPROFILE"),
  project_dir = "./data-raw")
checkpoint::use_checkpoint(
  "2022-02-13", checkpoint_location = Sys.getenv("USERPROFILE"))

# Load libraries
library(dplyr)
library(tidyr)
library(stringi)
library(devtools)
library(tibble)

# Load postinumbers data
fi_postnumbers_2016 <- vroom::vroom(
  "./data-raw/postnumbers_2016.txt", skip = 4,
  col_types = vroom::cols(.default = vroom::col_character()),
  .name_repair = janitor::make_clean_names
  ) %>% {
    # Shorten postnumbers to first three characters
    bind_rows(
      mutate(., postinumero = paste0(stri_sub(postinumeroalue, 1, 2), "0")) %>%
        distinct(postinumero, .keep_all = TRUE),
      mutate(., postinumero = stri_sub(postinumeroalue, 1, 3)))
    } %>%
  # Add new area variable by Maakunta
  mutate_at(vars(maakunta), list(alue = ~case_when(
    . %in% c("01", "05", "07", "08") ~ "Etelä-Suomi",
    . %in% c("02", "04", "06", "13", "14", "15", "16") ~ "Länsi-Suomi",
    . %in% c("09", "10", "11", "12") ~ "Itä-Suomi",
    . %in% c("17", "18", "19") ~ "Pohjois-Suomi",
    TRUE ~ "Tuntematon"))) %>%
  # Select
  select(
    "postinumero", "maakunta", "maakunnan_nimi", "kuntaryhma",
    "kuntaryhman_nimi", "seutukunta", "seutukunnan_nimi", "suuralue",
    "suuralueen_nimi", "alue"
    ) %>%
  distinct(postinumero, .keep_all = TRUE)

# Load postinumbers data
fi_postnumbers_2020 <- vroom::vroom(
  "./data-raw/postnumbers_2020.txt", skip = 4,
  col_types = vroom::cols(.default = vroom::col_character()),
  .name_repair = janitor::make_clean_names
  ) %>% {
  # Shorten postnumbers to first three characters
  bind_rows(
    mutate(., postinumero = paste0(stri_sub(postinumeroalue, 1, 2), "0")) %>%
      distinct(postinumero, .keep_all = TRUE),
    mutate(., postinumero = stri_sub(postinumeroalue, 1, 3)))
    } %>%
  # Add new area variable by Maakunta
  mutate_at(vars(maakunta), list(alue = ~case_when(
    . %in% c("01", "05", "07", "08") ~ "Etelä-Suomi",
    . %in% c("02", "04", "06", "13", "14", "15", "16") ~ "Länsi-Suomi",
    . %in% c("09", "10", "11", "12") ~ "Itä-Suomi",
    . %in% c("17", "18", "19") ~ "Pohjois-Suomi",
    TRUE ~ "Tuntematon"))) %>%
  # Select
  select(
    "postinumero", "maakunta", "maakunnan_nimi", "kuntaryhma",
    "kuntaryhman_nimi", "seutukunta", "seutukunnan_nimi", "suuralue",
    "suuralueen_nimi", "alue"
  ) %>%
  distinct(postinumero, .keep_all = TRUE)

# Load industries data
fi_industries_2008 <- vroom::vroom(
  "./data-raw/industries_2008.txt", skip = 3,
  .name_repair = janitor::make_clean_names) %>%
  filter(taso %in% c(1, 2)) %>%
  mutate(
    tol_1 = if_else(taso == 1, koodi, NA_character_),
    tol_1_name = if_else(taso == 1, nimike, NA_character_)) %>%
  fill(tol_1, tol_1_name) %>%
  filter(taso == 2) %>%
  rename(tol_2 = koodi, tol_2_name = nimike) %>%
  mutate(tol_0 = case_when(
      tol_1 %in% c("A", "B") ~ "A+B",
      tol_1 %in% c("D", "E") ~ "D+E",
      tol_1 %in% c("J", "L", "M", "N") ~ "J+L+M+N",
      tol_1 %in% c("K", "O", "P", "R", "S", "T") ~ "K+O+P+R+S+T",
      TRUE ~ tol_1)) %>%
  select(tol_0, tol_1, tol_1_name, tol_2, tol_2_name)

# Load people names data
fi_people_names <- vroom::vroom(
  "./data-raw/people_names.txt",
  .name_repair = janitor::make_clean_names
    ) %>%
  # Turn n from character to integer
  mutate_at(vars(n), ~(stri_replace_all_regex(.x, " ", "") %>% as.integer)) %>%
  arrange(desc(n))

# Create people names to remove
fi_remove_names <- enframe(
  c(NA_character_),
  name = NULL, value = "name")

# Create people names to leave
fi_leave_names <- c("En")

# Create filtered people names data
fi_filtered_people_names <- fi_people_names %>%
  filter(!name %in% fi_leave_names) %>%
  select(name) %>%
  bind_rows(fi_remove_names) %>%
  filter(!is.na(name)) %>%
  distinct()

# Create remove words data
fi_remove_words <- enframe(
  c("että", "kiitos", "moite", "ikä", "sitä", "sen", "palaute", "hei", "myös",
    "moi", "terveisin"),
  name = NULL, value = "word")

# Create company abbreviations data
fi_company_abbr <- enframe(
  c("ry", "RY", "Oy", "OY", "oy", "OyJ", "Oyj", "oyj", "OYJ", "Ky", "KY", "AB",
  "Ab", "ab", "säätiö", "Säätiö"),
  name = NULL, value = "abbr")

# Save data to use in the package
use_data(fi_postnumbers_2016, fi_postnumbers_2020, fi_industries_2008,
         fi_people_names, overwrite = TRUE)
use_data(fi_filtered_people_names, fi_remove_words, fi_company_abbr,
         internal = TRUE, overwrite = TRUE)
