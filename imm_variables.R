library(dplyr)

imm_variables <- function(surveytype){
  
  path = paste("F:/EU-SILC/data/",surveytype, sep = "")
  setwd(path)
  
  # Preparing the environment and reading file list
  ## Reading directory content
  filelist <- list.files(recursive = TRUE, include.dirs = TRUE)
  filelist <- filelist[grepl(".csv", filelist)]

  files_P <- filelist[grepl(("_...P_"), filelist, ignore.case = TRUE)]
  files_R <- filelist[grepl(("_...R_"), filelist, ignore.case = TRUE)]
  
  
  if(tolower(surveytype) == "longitudinal") {
    
    years <- unique(gsub(".*L-(.+)/.*", "\\1" ,filelist))
    
  } else if(tolower(surveytype) == "cross-sectional"){
    
    years <-      unique(gsub(".*C-(.+)/.*", "\\1" ,filelist))

  } else{print("Kindly enter as longitudinal or cross-sectional as function's input")}

  
  varstat <- list()

  i=1
  
  ## while loop begins
  while (i <= length(files_P)){
    
    print(paste0("processing data of year ", years[i], " for both P & R variables"))
    
#   cols <- sapply(files_year, FUN = function(x) colnames(read.csv(file = filelist[i], nrows = 1)))
 
    cols <- colnames(read.csv(file = files_P[i], nrows = 1))
    cols2 <- colnames(read.csv(file = files_R[i], nrows = 1))
    

    ## Case starts
    temp1 <- case_when(c("PB020", "PB210", "PB220A", "PB220B") %in% cols 
                                     ~ c("PB020", "PB210", "PB220A", "PB220B"))
    
    temp1 <- c(temp1, case_when(c("RB020", "RB031") %in% cols2 
                       ~ c("RB020", "RB031")))
    
    varstat[[i]] <- temp1 
    
    rm(temp1)
    
        i <- i+1
    ## While end
  }
  
  filename = paste0("variables_", surveytype,".csv")
  
  ## Setting the dataframe columns name
  varstat <- data.frame(varstat)
  colnames(varstat) <- years

  reports_path = "F:/EU-SILC/reports/"
  
  write.csv(varstat, file = paste0(reports_path, filename))
  

  print(paste("Operation completed. Check file ",filename," in directory ", reports_path))
  
  return(data.frame(varstat))
}
