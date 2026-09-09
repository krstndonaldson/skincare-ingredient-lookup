library(tidyverse)

ingredients <- read_csv("ingredients.csv")

# Look up one ingredient
lookup <- function(name) {
  ingredients |>
    # Keep rows where the search term appears anywhere in the ingredient name (case-insensitive)
    filter(str_detect(tolower(Name), tolower(name))) |>
    select(Name, `INCI name`, Function, `Irritation potential`, Notes)
}

lookup("niacinamide")

# Chart: how many ingredients per function (category)
ingredients |>
  separate_rows(Function, sep = ", ") |>
  count(Function, sort = TRUE) |>
  ggplot(aes(x = reorder(Function, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Ingredients by function", x = NULL, y = "Count")

ggsave("ingredients_by_function.png", width = 7, height = 5)
