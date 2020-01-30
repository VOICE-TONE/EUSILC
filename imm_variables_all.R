library(dplyr)


## Function preparing the dataset

imm_prep <- function(){
  
  
  surveytype <- c("longitudinal", "cross-sectional")
  
  len <- 2
  
  k=1
  
    
  for(k in seq_along(len)){

    path = paste("F:/EU-SILC/data/",surveytype[k], sep = "")
    setwd(path)
    
    # Preparing the environment and reading file list
    ## Reading directory content
    filelist <- list.files(recursive = TRUE, include.dirs = TRUE)
    filelist <<- filelist[grepl(".csv", filelist)]  
    
    if(tolower(surveytype[k]) == "longitudinal") {
      
      years <<- unique(gsub(".*L-(.+)/.*", "\\1" ,filelist))
      
    } else if(tolower(surveytype[k]) == "cross-sectional"){
      
      years <<- unique(gsub(".*C-(.+)/.*", "\\1" ,filelist))
      
    } else{print("Kindly enter as longitudinal or cross-sectional as function's input")}

  }
  

}


## Function checking variable status

imm_check <- function(filelist){
  
  ## Getting the list of variables
  
  var_eusilc <- read.csv("C:/Users/Administrator/Documents/MOS/Thesis/EU_SILC_Selected_variables_shortlist.csv",header = TRUE)
  
  code <- levels(var_eusilc$Code)
  
  
  varstat <- list()
  
  i=1
  
  ## while loop begins
  while (i <= length(years)){
    
    print(paste0("processing data of year ", years[i]))
    
    files <- filelist[grepl(years[i], filelist, ignore.case = TRUE)]
    
    temp <- c()
    
    for(n in seq_along(length(files))){
    
      cols <- colnames(read.csv(file = files[n], nrows = 1))
      
      temp <- c(temp, 
                case_when(code %in% cols ~ code
                          )
                )
    }
    
    ## Putting the result of the test in the list of results
    varstat[[i]] <- temp 

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


imm_main <- function() {
  

  filelist <<- imm_prep()
  
  variables <- imm_check(filelist)
  
  return(variables)
}

