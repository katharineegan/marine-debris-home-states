clean_ppm_data <- function(list) {
  
  # rbind all df in list together 
  df <- do.call("rbind", list)
  
  # add a column that is the name of the data frame
  df$df_name <- rep(names(list), sapply(list, nrow))
  
  # use tidyverse to: 
  # filter rows that do contain "TOTALS"
  # rename columns
  # convert year to a character
  # make location column upper case
 ppm <- df %>% 
    filter(grepl("TOTAL", `Cleanup Type`)) %>%
    rename(people = People,
           pounds = Pounds,
           miles = Miles,
           bags = `# of Bags`) %>% 
    mutate(df_name = gsub("PPM", "", df_name)) %>% 
    separate(df_name, into = c("location", "year"), sep="_") %>% 
    mutate(year = as.character(year),
           location = toupper(location)) %>% 
    select(location, year, people, pounds, miles, bags)
 
 # assign all ppm to the globa env
 assign("all_ppm", ppm, env = .GlobalEnv)
 
 # filter out 2008-2012 data
 ppm_0812 <- ppm %>% filter(year %in% c("2008", "2009", "2010", "2011", "2012"))
 
 # assign 2008-2012 data frame to global env
 assign("ppm_0812", ppm_0812, env = .GlobalEnv)
 
 # filter out 2008-2012 data
 ppm_1315 <- ppm %>% filter(year %in% c("2013", "2014", "2015"))
 
 # assign data 2013-2015 data frame to global env
 assign("ppm_1315", ppm_1315, env = .GlobalEnv)
} 
