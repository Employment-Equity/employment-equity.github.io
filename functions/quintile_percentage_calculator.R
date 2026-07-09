# This script assigns bins based on separating the population into quintiles

# declare file location
here::i_am("functions/quintile_percentage_calculator.R")

# year to calculate quintiles for
q_year <- "2025"

# Load in cached salary df and select for this year's data
count_df <- read.csv(here(
  paste0("cached_data/salary_data_", q_year, ".csv"))) |> 
  select(salary_range, count_all)   |> 
  filter(salary_range != "Total")
  
# Get total sum of counts
total <- sum(count_df$count_all)
  
# Load in quintile data
quintiles <- 
  read.csv(here("supplementary_tables/quintile_ranges.csv")) |>
            filter(quintile_year == q_year)
          
  
  
quintile_percentages <- count_df |> 
  left_join(quintiles, by = "salary_range") |> 
  group_by(quintile) |> 
  summarise(
    from = first(salary_range),
    to = dplyr::last(salary_range),
    count = sum(count_all)) |> 
  mutate(percent = count/total*100)
  

write.csv(quintile_percentages, paste0(here("supplementary_tables/quintile_percentages/"), "quintile_percentages_", q_year, ".csv"))
