library(tidyverse)

# Some cells in the CSV have a trailing space (e.g. "Niacinamide "), so trim every text column
ingredients <- read_csv("ingredients.csv", show_col_types = FALSE) |>
  mutate(across(where(is.character), str_trim))

irritation_levels <- c("Low", "Medium", "High")

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

# Filter by irritation level. max_level = "Low" gives Low only,
# "Medium" gives Low + Medium, "High" gives everything that has a rating.
# Optionally narrow by function too, e.g. filter_irritation("Low", fn = "acne")
filter_irritation <- function(max_level, fn = NULL) {
  max_level <- str_to_title(max_level)
  if (!max_level %in% irritation_levels) {
    stop("max_level must be one of: ", paste(irritation_levels, collapse = ", "))
  }
  allowed <- irritation_levels[seq_len(match(max_level, irritation_levels))]

  result <- ingredients |>
    filter(`Irritation potential` %in% allowed)

  if (!is.null(fn)) {
    result <- result |>
      filter(str_detect(tolower(Function), tolower(fn)))
  }

  result |>
    select(Name, Function, `Irritation potential`, Notes) |>
    arrange(match(`Irritation potential`, irritation_levels), Name)
}

# Examples
lookup("niacinamide")
lookup_function("acne")
filter_irritation("Low")
filter_irritation("Low", fn = "acne")

# Chart: how many ingredients per function (category)
ingredients |>
  separate_rows(Function, sep = ", ") |>
  count(Function, sort = TRUE) |>
  ggplot(aes(x = reorder(Function, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Ingredients by function", x = NULL, y = "Count")

ggsave("ingredients_by_function.png", width = 7, height = 5)
