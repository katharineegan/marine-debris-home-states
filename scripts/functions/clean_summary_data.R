clean_summary_data <- function(list) {
  
  # rbind all data frames in list together 
  df <- do.call("rbind", list)
  
  # add a column that is the name of the data frame
  df$df_name <- rep(names(list), sapply(list, nrow))
  
  # use tidyverse to:
  # select specific columns
  # remove all rows that have NAs
  # remove all rows that contain certain words
  # convert year to a character
  # change column names while making categories column all lowercase
  # select specific rows
  # make location upper case
  md <- df %>% 
    mutate(category = tolower(`Categorized Items`),
           df_name = gsub("Summary", "", df_name)) %>%
    rename(total = `Total Items`) %>% 
    separate(df_name, into = c("location", "year"), sep="_") %>% 
    filter(!grepl("totals", category),
           !grepl("total", category),
           !is.na(total)) %>% 
    mutate(year = as.character(year),
           location = toupper(location)) %>% 
    select(location, year, category, total)
  
  # assign all md to global environment
  assign("all_md", md, env = .GlobalEnv)
  
  # filter out 2008-2012 data
  md_0812 <- md %>% filter(year %in% c("2008", "2009", "2010", "2011", "2012"))
  
  # assign 2008-2012 data frame to global env
  assign("md_0812", md_0812, env = .GlobalEnv)
  
  # filter out 2008-2012 data
  md_1315 <- md %>% filter(year %in% c("2013", "2014", "2015"))
  
  # assign data 2013-2015 data frame to global env
  assign("md_1315", md_1315, env = .GlobalEnv)
}
