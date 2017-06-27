clean_ppm_data <- function(list) {
  
  # rbind all df in list together 
  df <- do.call("rbind", list)
  
  # add a column that is the name of the data frame
  df$df_name <- rep(names(list), sapply(list, nrow))
  
  # use tidyverse to: 
  # filter rows that do contain "TOTALS"
  # rename columns
  # convert year to factor
 df %>% 
    filter(grepl("TOTAL", `Cleanup Type`)) %>%
    rename(people = People,
           pounds = Pounds,
           miles = Miles,
           bags = `# of Bags`) %>% 
    mutate(df_name = gsub("PPM", "", df_name)) %>% 
    separate(df_name, into = c("location", "year"), sep="_") %>% 
    mutate(year = as.factor(year)) %>% 
    select(location, year, people, pounds, miles, bags)
} 
