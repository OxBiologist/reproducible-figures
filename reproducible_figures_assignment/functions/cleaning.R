shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

remove_columns <- function(penguins_raw) {
  penguins_raw %>%
    select(-starts_with("Delta")) %>% #remove columns starting with delta
    select(-Comments)#remove comments columns
  }

