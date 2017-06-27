clean_summary_data <- function(list) {
  
  # rbind all data frames in list together 
  df <- do.call("rbind", list)
  
  # add a column that is the name of the data frame
  df$df_name <- rep(names(list))
  
  # use tidyverse to:
  # select specific columns
  # remove all rows that have NAs
  # remove all rows that contain certain words
  # convert year to a factor
  # change column names while making categories column all lowercase
  # select specific rows
  md_0812 <- df %>% 
    filter(!grepl("Totals", `Categorized Items`),
           !grepl("TOTAL", `Categorized Items`),
           !grepl("REPORT", `Categorized Items`),
           !is.na(`Total Items`)) %>% 
    mutate(category = tolower(`Categorized Items`),
           df_name = gsub("Summary", "", df_name)) %>%
    separate(df_name, into = c("location", "year"), sep="_") %>% 
    mutate(year = as.factor(year)) %>% 
    rename(total = `Total Items`) %>% 
    select(location, year, category, total)
  
  # subset out 2013-2015 data and make it's own data frame
  md_1315 <- md_0812[md_0812$year %in% c(2013, 2014, 2015), ]
  
  # delete 2013-2015 from the orignial data 
  md_0812 <- md_0812[!md_0812$year %in% c(2013, 2014, 2015), ]
  
  # assign 2008-2012 data frame to global env
  assign("md_0812", md_0812, env = .GlobalEnv)
  
  # assign data 2013-2015 data frame to global env
  assign("md_1315", md_1315, env = .GlobalEnv)
}