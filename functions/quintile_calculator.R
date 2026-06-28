# This script assigns bins based on separating the population into quintiles

# declare file location
here::i_am("functions/quintile_calculator.R")

quintile_year <- "2025"

# Load in and select for this year's data
quintile_df <- read.csv(here(
  paste0("cached_data/salary_data_", quintile_year, ".csv"))) |> 
  select(salary_range, count_all) |> 
  mutate(cum_sum = cumsum(count_all)) |> 
  filter(salary_range != "Total")

# get total and target 
total <- sum(quintile_df$count_all)
target <- total / 5

# Assign each group a bin
quintile_df <- quintile_df |>
  mutate(bin = findInterval(cum_sum - 1, seq(0, total, by = target)))

# get summary 
bin_summary <- quintile_df %>%
  group_by(bin) %>%
  summarise(
    from = first(salary_range),
    to   = dplyr::last(salary_range),
    count = sum(count_all)
  )

# Get names
# Extract the upper bound of each bin's "to" salary range
upper_bounds <- bin_summary$to |>
  str_extract("\\d{2,3},\\d{3}(?!.*\\d{2,3},\\d{3})") |>
  str_remove(",") |>
  as.numeric()

# Build the labels
n <- nrow(bin_summary)
labels <- character(n)
labels[1] <- paste("Under", format(upper_bounds[1], big.mark = ","))
for (i in 2:(n - 1)) {
  lower <- formatC(upper_bounds[i - 1] + 1, format = "d", big.mark = ",")
  upper <- formatC(upper_bounds[i], format = "d", big.mark = ",")
  labels[i] <- paste(lower, "to", upper)
}
labels[n] <- paste(format(upper_bounds[n - 1] + 1, big.mark = ","), "and over")

bin_summary <- bin_summary |> 
  mutate(bin = labels) |> 
  select

