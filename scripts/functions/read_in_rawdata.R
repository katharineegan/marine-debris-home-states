read_in_rawdata <- function(rawdata_path, list_name) {
  
  # create list of all .xls files in folder
  files <- list.files(path=paste0(getwd(),"/raw_data/summary/"), pattern="*.xls", 
                      full.names = T)
  
  # create an empty list 
  df_list <- list()
  
  # read all excel files into list
  for (i in seq_along(files)) {
    df_list[[i]] <- read_excel(files[i])
  }
  
  # name all the files with the names of the excel sheets
  names(df_list) <- gsub("\\.xls$", "", files)
  
  # assign list to the global environment 
  assign(list_name, df_list, envir=.GlobalEnv)
  
}
