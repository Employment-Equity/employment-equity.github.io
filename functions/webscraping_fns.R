
# Functions used in webscraping for Visualizations.qmd

# Define root directory for TBS website with salary data
url <- "https://www.canada.ca/en/treasury-board-secretariat/services/innovation/human-resources-statistics/diversity-inclusion-statistics/distribution-public-service-canada-employees-designated-sub-group-salary-range-"



# List of years to get data for 
years <- c("2023", "2022", "2021", "2020", "2019", "2018", "2017")


# Function for removing extra characters
remove_bullish <- function(bullish_cols) {
  as.numeric(gsub(
    pattern = "[^0-9.-]",
    replacement = "",
    x = bullish_cols
    ))
  }

# Function for removing spaces and space-like characters 
remove_spaces <- function(spacey_cols) {
  gsub(pattern = "\\s+",
       replacement = " ",
       x = spacey_cols)
}

# Function for cleaning up tables 
# Clean up totals rows
table_cleanup <- function(messy_cols){
  function(.data){
    .data |> 
      
     # Clean up totals column
       mutate({{ messy_cols }} := ifelse(grepl("Total", {{ messy_cols }}),
                                   "Total",
                                   {{ messy_cols}})) |>
      
      # Remove rows with salary or table
      filter(!grepl("^Salary|^Table|^Footnote", {{messy_cols}})) |>
      
      
      mutate(across(where(is.character), ~na_if(., "Table 1 Footnote *"))) |>
      
      
      mutate_at(vars(matches("count")), remove_bullish) |> # remove extra characters from counts
      
      
      mutate_at(vars(matches("salary")), remove_spaces) # remove extra spaces from counts
  }
}


# Function to get salary data from website over years
get_salary_data <- function(Year,
                            cache_dir = here("cached_data"), # place for cached data
                            refresh = NULL){
  
  
  
  # Ask user if refresh is not specified and cache exists
  if (is.null(refresh) && file.exists(here(paste0("cached_data/salary_data_", Year, ".csv")))) {
    answer <- readline(paste0("Cached table for", Year, "found. Refresh cache? (y/n): "))
    refresh <- tolower(answer) == "y"
  } else if (is.null(refresh)) {
    refresh <- TRUE # No cache exists, must scrape
    stop(paste("Webscraping", Year, "data."))
  }
  
  
  
  
  # If cache exists and no refresh, load from cache
  if (file.exists(here(paste0("cached_data/salary_data_", Year, ".csv"))) && !refresh) {
    message("Loading tables from cache...")
    
    # read csv for file from cache and assign in global environment
    assign(paste0("salary_data_", Year),
           {read.csv(here(paste0("cached_data/salary_data_", Year, ".csv")))},
           envir = globalenv())
    
    
  }
  
  if(refresh == TRUE){
    
    # Match years to their index in the data
    year_dictionary <- data.frame(year = years,
                                  index = c(1:length(years)))
    # Index for year
    year_index <- as.numeric(year_dictionary$index[year_dictionary$year == Year])
    
    
    # Get vm and black df
    vm_black_df <- {html_nodes(read_html(paste0( 
      url,
      "members-visible-minorities")), 
      "table") |> 
        html_table(fill = TRUE)
    }[[year_index]][c(1:3, 25)] |> 
      dplyr::rename( # fix col names
        salary_range = "Salary range ($)",
        count_all = "All employees",
        count_black = "Black",
        count_vm = "Members of visible minorities") |> 
      
      table_cleanup(salary_range)()
    
    
    
    # get indigenous df
    indig_df <- {html_nodes(read_html(paste0(url,"indigenous-peoples")), "table") |>
        html_table(fill = TRUE)
    }[[year_index]][c(1, 11)] |> 
      dplyr::rename(salary_range = "Salary range ($)",
                    count_indigenous = "Indigenous Peoples")|> 
      
      table_cleanup(salary_range)()
    
    # get persons with disabilities df 
    pwd_df <-  {html_nodes(read_html(paste0(url, "persons-disabilities")), "table") |>
        html_table(fill = TRUE)
    }[[year_index]][c(1, 15)] |>
      dplyr::rename(salary_range = "Salary range ($)",
                    count_pwd = "Persons with disabilities") |> 
      
      table_cleanup(salary_range)()
    
    # Merge all 3 df's
    merged_df <- vm_black_df |> 
      full_join(indig_df, by = "salary_range") |> 
      full_join(pwd_df, by = "salary_range") 
    
    # Write to csv
    write.csv(merged_df, file = here(paste0("cached_data/salary_data_", Year, ".csv")), 
                row.names = FALSE)
    
    # Load tables to environment
    message("Loading tables from cache...")
    assign(paste0("salary_data_", Year),
           {merged_df},
           envir = globalenv())
   
    
   }
   
}

