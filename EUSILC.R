library(dplyr)

## Functions

imm_ratio_part1 <- function(){

  ## Using data on disk
  setwd(paste("C:/Users/Administrator/Documents/MOS/Thesis/",surveytype, sep = ""))
 
  ## Using data on memory stick 
#  setwd(paste("F:/EU-SILC/data/",surveytype, sep = ""))

  countries <- c("AT","BE","BG","CY","CZ","EE","EL","ES","FI","FR","HU","IE","IS","IT","LT","LU","LV","MT","NL","NO","PL","PT","RO","RS","SE","SI","UK")
  
}


## Longitudinal data function
imm_ratio_long <- function(){

  # Preparing the environment and reading file list
  ## Reading directory content
  filelist <- list.files(recursive = TRUE, include.dirs = TRUE)
  filelist <- filelist[grepl(".csv", filelist)]
  
  ## Setting up environment for longitudinal study
  Pvar <- filelist[grepl("l..P",filelist, ignore.case = TRUE)]
  Pvar2 <- filelist[grepl("l..R",filelist, ignore.case = TRUE)]
  years <- gsub(".*L-(.+)/.*", "\\1" ,Pvar)
  
  ## Extracting the years from the file names
  
  years <- sapply(years, function(x) paste("Y_",x, sep = ""))
  
  years <-  levels(factor(years))
  

  ## Looping on all files and getting the ratio of foreign citizens by year migrated
  
  i=1
  ratio_P <- list()
  
  ## while loop begins
  while (i <= length(Pvar)){
    eusilc_data <- read.csv(file = Pvar[i])
    cols <- colnames(eusilc_data)
    
    ## Case starts
   if("PB020" %in% cols & "PB210" %in% cols){
        temp <- eusilc_data %>% group_by(PB020) %>% filter(PB210 == "OTH") %>% summarise(n()*100/nrow(eusilc_data)) %>% tbl_df()
        ratio_P[[i]] <- temp[,2]
    }else{
      ratio_P[[i]] <- paste(i,"PB210 is missing in year", years[i])
      print(ratio_P[[i]])
    } 
      
    i <- i+1
    rm(eusilc_data)
    rm(temp)
  
    ## While end
  }

## (Debugging) Printing i to ensure the logic works (To be deleted)
print(paste0("last recorded value of i is", i))
  
j=i
k=1
## Testing second leg of the script for R variables in Longitudinal studies
  while (j <= length(Pvar)+length(Pvar2)){
    eusilc_data <- read.csv(file = Pvar2[j])
    cols <- colnames(eusilc_data)
    
    ## Case starts
    if("RB020" %in% cols & "RB031" %in% cols){
      temp <- eusilc_data %>% group_by(RB020) %>% filter(RB031 == "OTH") %>% summarise(n()*100/nrow(eusilc_data)) %>% tbl_df()
      ratio_P[[j]] <- temp[,2]
    }else{
      ratio_P[[j]] <- paste(k,"RB031 is missing in year", years[k])
      print(ratio_P[[j]])
    } 
    
    j <- j+1
    k <- k+1
    rm(eusilc_data)
    rm(temp)
    
    ## While end
  }
 
  return(ratio_P)
  ## Function end
}


## Cross sectional data function using old method
imm_ratio_cross2 <- function(){

  # Preparing the environment and reading file list
  ## Reading directory content
  filelist <- list.files(recursive = TRUE, include.dirs = TRUE)
  filelist <- filelist[grepl(".csv", filelist)]
  
  ## Setting up environment for cross-sectional study
  Pvar <- filelist[grepl("c..R",filelist, ignore.case = TRUE)]
  years <- gsub(".*C-(.+)/.*", "\\1" ,Pvar)

  ## Looping on all files and getting the ratio of foreign citizens by year migrated
  
  i=1
  ratio_R <- list()

  ## while loop begins
  while (i <= length(Pvar)){
    eusilc_data <- read.csv(file = Pvar[i], nrows = 1)
    cols <- colnames(eusilc_data)
    
    ## Percentage and Head count of immigrants (blanks included)
    if("RB020" %in% cols & "RB031" %in% cols){
      
      eusilc_data <- read.csv(file = Pvar[i])
      temp <- eusilc_data %>% 
        select(RB020,RB030,RB031)%>% 
        group_by(RB020) %>% 
        filter(!is.na(RB031)) %>% 
        distinct(RB030) %>% 
        summarise(Forei_perc=n()*100/nrow(eusilc_data),Forei_n=n())%>% 
        arrange(desc(Forei_perc)) %>% 
        tbl_df()
      
## Uncomment later
      ratio_R[[i]] <- temp
      names(ratio_R)[[i]]<-years[i]
      print(paste0("Processing data of year:",{years[i]}))
    }else{
      
      ratio_R[[i]] <- paste(i,"RB031 is missing in year", years[i])
      print(ratio_R[[i]])
    }
    
    i <- i+1
    rm(eusilc_data)
#    rm(temp)
    
    ## While end
  }
## uncomment later
  return(ratio_R)
  ## Function end
}

## Cross sectional data function using new method

imm_ratio_cross <- function(){
  
  # Preparing the environment and reading file list
  ## Reading directory content
  filelist <- list.files(recursive = TRUE, include.dirs = TRUE)
  filelist <- filelist[grepl(".csv", filelist)]
  
  ## Setting up environment for cross-sectional study
  Pvar <- filelist[grepl("c..P",filelist, ignore.case = TRUE)]
  years <- gsub(".*C-(.+)/.*", "\\1" ,Pvar)
  
  ## Looping on all files and getting the ratio of foreign citizens by year migrated
  
  i=1
  country_stat <- list()
  eu_agg <- list()
  
  ## while loop begins
  while (i <= length(Pvar)){
    eusilc_data <- read.csv(file = Pvar[i], nrows = 1)
    cols <- colnames(eusilc_data)
    
    ## Percentage and Head count of immigrants (blanks included)
    if("PB020" %in% cols & "PB220A" %in% cols){
      
      eusilc_data <- read.csv(file = Pvar[i])
      

      ##### Using the country of nationality (PB220A) & The country of survey (PB020)
      
      data_PB220A <- eusilc_data %>% 
        select(PB220A, PB020) %>% 
        filter(PB220A %in% c("LOC", "EU", "OTH")) %>%
        group_by(PB220A,PB020) %>% 
        summarise(Counts = n())
      
      data2 <- recast(data = data_PB220A, PB020 ~ PB220A)
      
      data2 <- data2 %>% 
        arrange(desc(OTH)) %>% 
        select(country=PB020,LOC, EU, OTH)
      
      data2
      
      ##### EU Wide summary
      
      data2_eu_agg <-  data2 %>% 
        summarise(LOC=sum(na.omit(LOC)), 
                  EU=sum(na.omit(EU)), 
                  OTH=sum(na.omit(OTH))) %>% 
        select(LOC,EU,OTH)
      
      
      ## Storing the data frame into a list element
      country_stat[[i]] <- data2
      eu_agg[[i]] <- data2_eu_agg
      
      names(country_stat)[[i]]<-years[i]
      names(eu_agg)[[i]]<-years[i]
      print(paste0("Processing data of year:",{years[i]}))
    }else{
      
      country_stat[[i]] <- paste(i,"PB220A is missing in year", years[i])
      print(country_stat[[i]])
    }
    
    i <- i+1
    rm(eusilc_data)
    #    rm(temp)
    
    ## While end
  }
  ## uncomment later
  return(list(country_stat, eu_agg))
  ## Function end
}


## Function to get ratio of immigrants per country per year in the EUSILC dataset
imm_ratio <- function(type=c("longitudinal","cross-sectional")){
  
  surveytype <<- type
  
  imm_ratio_part1()
  
  if(tolower(type)=="longitudinal"){
    imm_ratio_long()
  }else if(tolower(type)=="cross-sectional"){
    imm_ratio_cross()
  }else{print("type is incorrect. Should be longituinal or cross-sectional")}

}

results <- imm_ratio("cross-sectional")

library(erer)

## Writing the list into a csv file for processing
write.list(results[[1]], file = 'F:/EU-SILC/data/results_countries.csv')
write.list(results[[2]], file = 'F:/EU-SILC/data/results_agg.csv')


###############################################################################
##### Second method to this using the PB210 (country of birth) variables ######
## Selecting only needed columns for performances, 
## Filtering only meaningful origin categories from PB210
## Grouping by country and by category to output stats


data_PB210 <- eudata %>% 
  select(PB210, PB020) %>% 
  filter(PB210 %in% c("LOC", "EU", "OTH")) %>% 
  group_by(PB210,PB020) %>% 
  summarise(Counts = n())

## Reshaping the dataset as pivot table 

data1 <- recast(data = data_PB210, PB020 ~ PB210)


##### Using the country of nationality (PB220A) & The country of survey (PB020)

data_PB220A <- eudata %>% 
  select(PB220A, PB020) %>% 
  filter(PB220A %in% c("LOC", "EU", "OTH")) %>%
  group_by(PB220A,PB020) %>% 
  summarise(Counts = n())

data2 <- recast(data = data_PB220A, PB020 ~ PB220A)

data2 <- data2 %>% 
  arrange(desc(OTH)) %>% 
  select(country=PB020,LOC, EU, OTH)

data2

##### EU Wide summary
data2 %>% 
  summarise(LOC=sum(na.omit(LOC)), 
            EU=sum(na.omit(EU)), 
            OTH=sum(na.omit(OTH))) %>% 
  select(LOC,EU,OTH)
###############################################################################