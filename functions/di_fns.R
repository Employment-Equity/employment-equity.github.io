# Calculate DI
get_di <- function(year){
  
  quintile_info_year <- quintile_ranges |> filter(quintile_year == paste(year))
  
  # Establish level order
  level_order <- quintile_info_year |> 
    pull(quintile) |>  
    unique(fromLast = TRUE) 
  
  di_df_year <- years_data_clean |> 
    filter(Year == year) |>
    left_join(quintile_info_year,
              by = "salary_range") |> 
  dplyr::group_by(quintile) |>
  dplyr::summarize( # sum accross quintile groups 
    count_all = sum(count_all, na.rm = TRUE),
    count_black = sum(count_black, na.rm = TRUE),
    count_vm = sum(count_vm, na.rm = TRUE),
    count_non_vm = count_all - count_vm,
    count_indigenous = sum(count_indigenous, na.rm = TRUE),
    count_pwd = sum(count_pwd, na.rm = TRUE),
    quintile = first(quintile),
    range_label = first(range_label),
    quintile_label = first(quintile_label)
  ) |>
  mutate(quintile = ifelse(is.na(quintile),
                           "All ranges",
                           quintile)) |> 
  ungroup() |> 
  mutate(quintile = factor(quintile,
         levels = level_order)) |> 
  arrange(quintile) |> 
    
  mutate(
    
    # create new column for percentage of representation within subgroup
    per_all = round(count_all / count_all[n()] * 100, 2),
    per_black = round(count_black / count_black[n()] * 100, 2),
    per_vm = round(count_vm / count_vm[n()] * 100, 2),
    per_non_vm = round(count_non_vm / count_non_vm[n()] * 100, 2),
    per_indigenous = round(count_indigenous / count_indigenous[n()] * 100, 2),
    per_pwd = round(count_pwd / count_pwd[n()] * 100, 2),
    
    # create new column for disproportionality index (DI)
    di_all = 1,
    di_black = round(per_black / per_all, 2),
    di_vm = round(per_vm / per_all, 2),
    di_non_vm = round(per_non_vm / per_all, 2),
    di_indigenous = round(per_indigenous / per_all, 2),
    di_pwd = round(per_pwd / per_all, 2)
  ) |>
  mutate(year_di = paste(year)) |> 
  select(year_di, 
         quintile,
         range_label,
         quintile_label,
         per_all,
         di_all,
         
         per_black,
         di_black,
         
         per_vm,
         di_vm,
         
         per_non_vm,
         di_non_vm,
         
         per_indigenous,
         di_indigenous,
         
         per_pwd,
         di_pwd) 
  
  assign("di_df_year",
         di_df_year,
         envir = globalenv())
  
}

# Make flextable from DI 
di_table_year <- function(year){
  
  # Set up quintile info
  quintile_info_year <- quintile_ranges |> filter(quintile_year == paste(year))
  
  # Create header labels for table
  table_df <- di_df |> 
    filter(year_di == paste(year)) |> 
    select(-c(year_di, range_label, quintile_label))
  
  # Establish level order
  level_order <- quintile_info_year |> 
    pull(quintile) |>  
    unique(fromLast = TRUE) 
  

  di_flextable_year <- flextable(table_df) |>
    set_header_labels(values = list(
      quintile = "Salary Range",
      per_all = "Percent",
      di_all = "DI",
      per_black = "Percent",
      di_black = "DI",
      per_vm = "Percent",
      di_vm = "DI",
      per_non_vm = "Percent",
      di_non_vm = "DI",
      per_indigenous = "Percent",
      di_indigenous = "DI",
      per_pwd = "Percent",
      di_pwd = "DI")) |> 
    add_header_row(colwidths = c(1, 2, 2, 2, 2, 2, 2),
                   values = c(" ",
                              "All Employees",
                              "Black Employees",
                              "All Visible Minority Employees",
                              "Non-Visible Minority Employees",
                              "Indigenous Employees",
                              "Employees with Disabilities")) |>
    width(j = 1, width = 3) |> # make fist column a little wider
    align(align = "center", part = "body") |>
    align(align = "center", part = "header") 
  
  di_flextable_year
  
}



# Make flextable from DI 
di_table_year_fr <- function(year){
  
  # Translate quintile labels
  di_table_labels_fr <- labels |> 
    filter(type == "quintile") |> 
    select(en, fr)
  
  # Grab data for the table
  table_df <- di_df |> 
    filter(year_di == paste(year)) |> 
    select(-c(year_di, range_label, quintile_label)) |> 
    left_join(di_table_labels_fr, by = c("quintile" = "en")) |> 
    select(-c(quintile)) |> 
    select(fr, everything()) |> 
    rename("échelle des salaires" = "fr")
    
  
  di_flextable_year_fr <- flextable(table_df) |>
    set_header_labels(
      values = list(
        range_group = "Fourchette des salaires",
        per_all = "Pourcentage",
        di_all = "ID",
        per_black = "Pourcentage",
        di_black = "ID",
        per_vm = "Pourcentage",
        di_vm = "ID",
        per_non_vm = "Pourcentage",
        di_non_vm = "ID",
        per_indigenous = "Pourcentage",
        di_indigenous = "ID",
        per_pwd = "Pourcentage",
        di_pwd = "ID"
      )) |>
    add_header_row(
      colwidths = c(1, 2, 2, 2, 2, 2, 2),
      values = c(
        " ",
        "Tous les employés",
        "Employés de race noire",
        "Employés Minorité visible",
        "Employés Non-Minorité visible",
        "Employés autochtones",
        "Employés handicapées")) |>
    width(j = 1, width = 3) |> # make fist column a little wider
    align(align = "center", part = "body") |>
    align(align = "center", part = "header")
  
  di_flextable_year_fr
  
}



