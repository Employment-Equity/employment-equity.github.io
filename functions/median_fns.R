

# Create a function that calculates median for each group for a year
# median = lower limit of median group + ((half pop - cum freq up to median)/ pop of median group) * width of median group

est_median <- function(year, ee_group) {
  # pull salary data by year and change back to wide format
  median_df <-
    df |>
    filter(Year == year) |> # filter for selected year
    pivot_wider(names_from = "group", # change back to wide format
                values_from = c("count_cumsum", 
                                "total", 
                                "half_pop")) |>
    dplyr::select(-contains(c("total_cumsum", # remove unneeded cols
                              "total_count",
                              "half_pop_cumsum"))) 
  
  # rename cols
  median_colnames <- c("salary_range", 
                       "Year", 
                       "bin_width",
                       "count_all", 
                       "count_black", 
                       "count_vm", 
                       "count_indigenous", 
                       "count_pwd", 
                       "count_non_vm",
                       "bin_width_cumsum", 
                       "cumsum_all",
                       "cumsum_black",
                       "cumsum_vm",
                       "cumsum_indigenous",
                       "cumsum_pwd",
                       "cumsum_non_vm",
                       "half_pop_all",
                       "half_pop_black",
                       "half_pop_vm",
                       "half_pop_indigenous",
                       "half_pop_pwd",
                       "half_pop_non_vm")
  
  colnames(median_df) <- median_colnames
  half_pop <- as.numeric(first(median_df[paste("half_pop_", ee_group, sep = "")])[1])
  
  # find lower limit of median group
  median_group_ll <- max(median_df$bin_width_cumsum[median_df[paste("cumsum_", ee_group, sep = "")] < half_pop], na.rm=TRUE)
  
  # find the cumulative frequncy up the median
  cumsum_uptomedian <- max(median_df[paste("cumsum_", ee_group, sep = "")][median_df[paste("cumsum_", ee_group, sep = "")]< half_pop], na.rm=TRUE)
  
  # find the frequency of the median group
  freq_mediangroup <- median_df[paste("count_", ee_group, sep = "")][median_df[paste("cumsum_", ee_group, sep = "")] > half_pop][1]
  
  # find the width of the median group 
  width_mediangroup <- median_df$bin_width[median_df[paste("cumsum_", ee_group, sep = "")] > half_pop][1]
  
  # calculate median [finally (●'◡'●)]
  median <- median_group_ll + ((half_pop - cumsum_uptomedian ) / freq_mediangroup) * width_mediangroup  
  
  # Create output and place in global environment
  assign("median_table", 
         {data.table(med = median, 
                     med_year = year, 
                     group = ee_group)}, 
         envir=globalenv())
}