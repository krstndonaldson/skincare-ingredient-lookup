library(tidyverse)

# Some cells in the CSV have a trailing space (e.g. "Niacinamide "), so trim every text column
ingredients <- read_csv("ingredients.csv", show_col_types = FALSE) |>
  mutate(across(where(is.character), str_trim))

# Look up one ingredient by name
lookup <- function(name) {
  ingredients |>
    # Keep rows where the search term appears anywhere in the ingredient name (case-insensitive)
    filter(str_detect(tolower(Name), tolower(name))) |>
    select(Name, `INCI name`, Function, `Irritation potential`, Notes)
}

# Search by function, e.g. lookup_function("acne") or lookup_function("hydrat")
lookup_function <- function(fn) {
  ingredients |>
    filter(str_detect(tolower(Function), tolower(fn))) |>
    select(Name, Function, `Irritation potential`, Notes)
}

# Examples
lookup("niacinamide")
lookup_function("acne")

# Chart: how many ingredients per function (category)
ingredients |>
  separate_rows(Function, sep = ", ") |>
  count(Function, sort = TRUE) |>
  ggplot(aes(x = reorder(Function, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Ingredients by function", x = NULL, y = "Count")

ggsave("ingredients_by_function.png", width = 7, height = 5)
