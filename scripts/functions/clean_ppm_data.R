clean_ppm_data <- function(list) {
  
  # rbind all df in list together 
  df <- do.call("rbind", list)
  
  # add a column that is the name of the data frame
  df$df_name <- rep(names(list), sapply(list, nrow))
  
  # use tidyverse to: 
  # filter rows that do contain "TOTALS"
  # rename columns
  # convert year to factor
 ppm_0812 <- df %>% 
    filter(grepl("TOTAL", `Cleanup Type`)) %>%
    rename(people = People,
           pounds = Pounds,
           miles = Miles,
           bags = `# of Bags`) %>% 
    mutate(df_name = gsub("PPM", "", df_name)) %>% 
    separate(df_name, into = c("location", "year"), sep="_") %>% 
    mutate(year = as.factor(year)) %>% 
    select(location, year, people, pounds, miles, bags)
 
 # subset out 2013-2015 data and make it's own data frame
 ppm_1315 <- ppm_0812[ppm_0812$year %in% c(2013, 2014, 2015), ]
 
 # delete 2013-2015 from the orignial data 
 ppm_0812 <- ppm_0812[!ppm_0812$year %in% c(2013, 2014, 2015), ]
 
 # assign 2008-2012 data frame to global env
 assign("ppm_0812", ppm_0812, env = .GlobalEnv)
 
 # assign data 2013-2015 data frame to global env
 assign("ppm_1315", ppm_1315, env = .GlobalEnv)
} 
