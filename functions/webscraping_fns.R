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
  
  
  gsub(
    
    
    pattern = "\\s+",
    
    
    replacement = " ",
    
    
    x = spacey_cols
    
    
  )
  
  
}











# Gather and clean data ----


# Define root directory for TBS website with salary data


url <-
  
  
  "https://www.canada.ca/en/treasury-board-secretariat/services/innovation/human-resources-statistics/diversity-inclusion-statistics/distribution-public-service-canada-employees-designated-sub-group-salary-range-"





# List of years to get data for 


years <- c("2023", "2022", "2021", "2020", "2019", "2018", "2017")








# Create function to get salary data from website over 5 years





get_salary_data <- function(Year) {
  
  
  
  
  
  # Progress message 
  
  
  message(paste("Webscraping", Year, "data."))
  
  
  
  
  
  # Match years to their index in the data
  
  
  year_dictionary <- data.frame(year = years,
                                
                                
                                index = c(1:length(years)))
  
  
  
  
  
  # Index for year
  
  
  year_index <-
    
    
    as.numeric(year_dictionary$index[year_dictionary$year == Year])
  
  
  
  
  
  # Pull dataset with rvest
  
  
  
  
  
  assign(paste("salary_df", # Create df's for each year
               
               
               Year, 
               
               
               sep = "_"), {
                 
                 
                 merge(x = {
                   
                   
                   
                   
                   
                   ## VM and Black employees ----
                   
                   
                   {html_nodes(read_html(paste0( 
                     
                     
                     url,
                     
                     
                     "members-visible-minorities")), 
                     
                     
                     "table") |> 
                       
                       
                       html_table(fill = TRUE)
                     
                     
                   }[[year_index]][c(1:3, 25)] |> # remove unneeded cols
                     
                     
                     # retrieve all employees and total vm column
                     
                     
                     dplyr::rename( # fix col names
                       
                       
                       salary_range = "Salary range ($)",
                       
                       
                       count_all = "All employees",
                       
                       
                       count_black = "Black",
                       
                       
                       count_vm = "Members of visible minorities")},
                   
                   
                   
                   
                   
                   y = {
                     
                     
                     
                     
                     
                     # Indigenous employees----
                     
                     
                     {html_nodes(read_html(paste0(url, 
                                                  
                                                  
                                                  "indigenous-peoples")), 
                                 
                                 
                                 "table") |>
                         
                         
                         html_table(fill = TRUE)
                       
                       
                     }[[year_index]][c(1, 11)] |> # retrieve total indigenous column
                       
                       
                       dplyr::rename(salary_range = "Salary range ($)",
                                     
                                     
                                     count_indigenous = "Indigenous Peoples")   },
                   
                   
                   by = "salary_range",
                   
                   
                   all = TRUE) |> 
                   
                   
                   
                   
                   
                   merge(y = {
                     
                     
                     
                     
                     
                     ## Persons with disabilities ----
                     
                     
                     {html_nodes(read_html(paste0(url,
                                                  
                                                  
                                                  "persons-disabilities")), "table") |>
                         
                         
                         html_table(fill = TRUE)
                       
                       
                     }[[year_index]][c(1, 15)] |> # retrieve total pwd column
                       
                       
                       dplyr::rename(salary_range = "Salary range ($)",
                                     
                                     
                                     count_pwd = "Persons with disabilities")
                     
                     
                     
                     
                     
                   },
                   
                   
                   by = "salary_range",
                   
                   
                   all = TRUE) |>
                   
                   
                   
                   
                   
                   # Remove rows with salary or table 
                   
                   
                   filter(!grepl("^Salary|Table|Footnote", salary_range)) |> 
                   
                   
                   mutate(across(where(is.character), ~na_if(., "Table 1 Footnote *"))) |> 
                   
                   
                   mutate_at(vars(matches("count")), remove_bullish) |> # remove extra characters from counts
                   
                   
                   mutate_at(vars(matches("salary")), remove_spaces) # remove extra spaces from counts
                 
                 
               },
         
         
         envir = globalenv())
  
  
  
  
  
}