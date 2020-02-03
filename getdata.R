
### Function for reading data



get_data <- function(vect, vars){
  
  #### PReparing the environment
  
  path = "../data/"
  
  filelist <- list.files(path = path, full.names = TRUE, recursive = TRUE)
  
  
  pattern <- "P_ver|H_ver"
  
  filelist <- filelist[grepl(pattern,filelist)]
  
  
  data <- data.frame()
  
  ## data <- list()
  
  var <- c("PB010","PB020", "PB220A", "PB150","PB030","PB140","PE040","PL040","PY200G","HB010","HB020", "HB030", "HY010", "HY020", "HY022", "HY023", "HB080")
  
  pvar <<- var[grepl("P", var)]
  hvar <<- var[grepl("H", var)]
  
  pfiles <<- filelist[grepl("P_ver", filelist)]
  
  hfiles <<- filelist[grepl("H_ver", filelist)] 
  
  
  #### Reading the data
  
  for(i in seq_along(vect)){
    
    ### Getting all headers from files and checking for the variable 
    var_inc <- colnames(read.csv(vect[i], nrows = 1, header = T))
    
    
    ### Detecting the variables of interest
    cols <- which(var_inc %in% vars)
    
    ### Creating the classes variables that will be the column filter in the read.csv function
    classes <- rep("NULL", times=length(var_inc))
    
    classes[cols] <- c(NA)
    
    
    
    ### Read only colums 2 and 32 of the CSV file where the variable are stored
    data <- rbind(data, read.csv(vect[i], colClasses = classes))
    
    ### data[[i]] <- read.csv(vect[i], colClasses = classes)
    
  }  
  
  return(data)
  
}
