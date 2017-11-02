# ----------------------------------------------------------------------------
# Program: ecog_314_functions.R
# ----------------------------------------------------------------------------
# global variables
set_global_variables <- function() {
  if ( !exists("dataDir") & !exists("rdsDataDir")  ) {
    
    #   directory -- location of the CSV files (not my working directory)
    dataDir <<- "./data"
    rdsDataDir <<- paste(dataDir, "rds", sep="/")     #processed file directory
    
    #   define file
    countryList_raw      <<- "countryList.csv"
    directionOfTrade_raw <<- "DirectionOfTrade.txt"
    countryLongLat_raw   <<- "countryLongLat.csv"
  }
}

#--
#Read countryList.csv file and save as countryList.rds format

#   Why saveRDS() and readRDS() instead of save() and read() -- see
#   http://www.fromthebottomoftheheap.net/2012/04/01/saving-and-loading-r-objects/

readFILE <- function( dir="./data/raw", filename, seperator=";", fileheader=TRUE, rdsfilename=NULL)  {
  
  
  # -- if countryList.csv, rdsfilename = countryList.rds, etc
  if ( missing(rdsfilename) || is.null(rdsfilename) ) {
    # construct the rds filename using the filename
    rdsfilename <- gsub(".csv|.txt", ".rds", filename )
  }
  
  # --
  # Use easy to type aliases
  fName   <- paste(dir, filename, sep="/")
  rdsDir  <- gsub("/raw", "/rds", dir )
  rdsName <- paste(rdsDir, rdsfilename, sep="/")
  
  # --
  # As this point, we have: 
  #   readFILE(filename="abs.csv") => Filename: ./data/raw/abs.csv   |   RDS: ./data/rds/abs.rds 
  #cat( sprintf("\n Filename: %s   |   RDS: %s\n", fName, rdsName )  )
  
  #check to see if environment contains dataset name
  if (!file.exists(rdsName)) {
    cat( sprintf("\n @ Reading raw countryList csv file:  %s\n", fName ) )
    dsName <- read.csv(
      file   = fName,  
      header = fileheader, 
      stringsAsFactors = FALSE,
      sep=seperator
    )
    saveRDS(dsName,  rdsName)  #  save in an rds file: faster to read than the raw files
  } else {
    cat( sprintf("\n + Reading stored countryList rds file: %s\n", rdsName ) )
    dsName <- readRDS(rdsName)
  }
  #return(dsName)
}

#----
#Importing Raw Data files into R 
read_raw_data_files <- function() {
  
  #  large data files take time to read. If dataset is in memory, do not rebuild
  if ( !exists("countryList") ) {
    readFILE(filename=countryList_raw, seperator = ",")          #country long / lat file 
  }
  
  if ( !exists("directionOfTrade") ) {
    readFILE(filename=directionOfTrade_raw, fileheader=FALSE)    #DirectionOfTrade.txt file 
  }
  
  if ( !exists("countryLongLat") ) {
    readFILE(filename=countryLongLat_raw, seperator = "\t")      #country long/lat file
  }
  
}

#--
# drop_na_entries="any", "all", NULL=> do nothing
drop_incomplete_records <- function(mydf, drop_empty_entries="any", drop_na_entries=NULL) {
  
  blank_entry <- "^[[:space:]]*$"                     # space characters
  
  matched_records <- data.frame(lapply(mydf, function(x) grepl(blank_entry, x))) 
  
  
  #change <NA> to BLANK
  mydf[mydf=="<NA>"] = ""                             # What if we do not want to change the data???
  
  #BLANK
  if ( tolower(drop_empty_entries) == "any" ) {
    mydf <- mydf[!apply(matched_records, 1, any), ]   # remove rows with *any* column
  } else {
    mydf <- mydf[!apply(matched_records, 1, all), ]   # remove rows with `blank_entry` in every column 
  }
  
  #NA 
  if ( is.null(drop_na_entries) ) {
    return(mydf)
  } else if ( tolower(drop_na_entries) == "any" ) {
    return( mydf[complete.cases(mydf), ] )            # same as subset(mydf, complete.cases(mydf))  #remove rows with *any* NA entries
  } else {
    return( mydf[! rowSums(is.na(mydf)) == dim(df)[2], ] )   # subset(mydf, ! rowSums(is.na(mydf)) == dim(df)[2])  #remove rows with `NA` in every column
  }
}


#--
clean_raw_data_files <- function(rdsDataDir="./data/rds") { 
  
  #Missing data
  
  ### How to locate both (NAs and empty) records or rows?
  # 
  # "APEC";;"Goods, Value of Imports, Cost, Insurance, Freight (CIF), US Dollars";"Iran, Islamic Republic of";"IR"
  # "APEC"; ;"Goods, Value of Exports, Free on board (FOB), US Dollars";"Trinidad and Tobago";"TT"
  # "APEC";\t;"Goods, Value of Exports, Free on board (FOB), US Dollars";"French Territories: New Caledonia";"NC"
  # "APEC";\t\t;"Goods, Value of Imports, Cost, Insurance, Freight (CIF), US Dollars";"Turkey";"TR"
  # "APEC";NA;"Goods, Value of Exports, Free on board (FOB), US Dollars";"Trinidad and Tobago";"TT"
  # NA;NA;NA;NA
  # <NA>;<NA>;<NA>;<NA>
  #   
  #   1. __[[:space:]]__ (__?regex__) find element space characters: tab, newline, vertical tab, form feed, carriage return, space
  # 2. __stri_isempty__  (_stringi_) fastest way to find out whether the elements of a character vector are empty strings('').
  # 3. __complete.case__  (_stats_) return a logical vector indicating which cases are complete (no missing values -- NA).
  # 4. __is.na__ (_baseR_) 'Not Available' / Missing Value
  
  #--locatemissingData -----
  # Test case
  # df <- data.frame(A = c("a1", "", "\n", " ", " \t\t", "f1", "g1", NA),
  #                  B = c("a2", "b2", "\t", " ", "\t\t\t", "f2", "g2", NA),
  #                  D = c(1:4, "", NA, 7, NA),
  #                  C = c(1:4, "", 6, 7, NA))
  
  # df <- read.table(header=FALSE, sep=';',  as.is = TRUE, text="
  #                  APEC;;Goods, Value of Imports, Cost, Insurance, Freight (CIF), US Dollars;Iran, Islamic Republic of;IR;201301;33258614.94
  #                  APEC; ;Goods, Value of Exports, Free on board (FOB), US Dollars;Trinidad and Tobago;TT;201301;3325861494.17093
  #                  APEC;\t;Goods, Value of Exports, Free on board (FOB), US Dollars;French Territories: New Caledonia;NC;201301;26989110.45
  #                  APEC;\t\t;Goods, Value of Imports, Cost, Insurance, Freight (CIF), US Dollars;Turkey;TR;201301;3261861528.36
  #                  APEC;NA;Goods, Value of Exports, Free on board (FOB), US Dollars;Trinidad and Tobago;TT;201301;3571349884.00
  #                  NA;NA;NA;NA;NA;NA;NA
  #                  <NA>;<NA>;<NA>;<NA>;NA;NA;NA
  #                  ;;;;;;;
  #                  Afghanistan and Pakistan; ;Goods, Value of Exports, Free on board (FOB), US Dollars;Turkey;TR;201301;2925627494.23
  #                  Barbados;BB;Goods, Value of Imports, Cost, Insurance, Freight (CIF), US Dollars;Bolivia;BO;201301;2458601206.90
  #                  ") 
  #  
  # df %>% drop_incomplete_records(drop_empty_entries = "any", drop_na_entries = "any") %>% print
  # df %>% drop_incomplete_records(drop_empty_entries = "all", drop_na_entries = "all") %>% print
  
  
  ### Remove both (NAs and empty) records or rows?
  
  # Missing data can be a not so trivial problem when analysing a dataset and accounting for it is usually not so straightforward either.
  # 
  # __Note:__ If the amount of missing data is very small relatively to the size of the dataset, then leaving out the few samples with missing features may be the best strategy in order not to bias the analysis, however leaving out available datapoints deprives the data of some amount of information and depending on the situation you face, you may want to look for other fixes before wiping out potentially useful datapoints from your dataset.
  # 
  # __Assumption:__ We are going to leave _any record_ containing a missing data value
  
  # dropmissingData
  
  #  large data files take time to read. If dataset is in memory, do not rebuild
  if (!file.exists(paste(rdsDataDir, "countryList_clean.rds", sep="/"))) {
    if ( !exists("countryList_clean" ) ) {
      countryList_clean      <- countryList      %>% drop_incomplete_records(drop_empty_entries = "any")
      saveRDS(countryList_clean,  paste(rdsDataDir, "countryList_clean.rds", sep="/") )
    }
  }
  
  if (!file.exists(paste(rdsDataDir, "directionOfTrade_clean.rds", sep="/"))) {
    if ( !exists("directionOfTrade_clean") ) {
      directionOfTrade_clean <- directionOfTrade %>% drop_incomplete_records(drop_empty_entries = "any", drop_na_entries = NULL)
      saveRDS(directionOfTrade_clean,  paste(rdsDataDir, "directionOfTrade_clean.rds", sep="/") )
    }
  }
  
  if (!file.exists(paste(rdsDataDir, "countryLongLat_clean.rds", sep="/"))) {
    if ( !exists("countryLongLat_clean") ) {
      countryLongLat_clean   <- countryLongLat   %>% drop_incomplete_records(drop_empty_entries = "any")
      saveRDS(countryLongLat_clean,  paste(rdsDataDir, "countryLongLat_clean.rds", sep="/") )
    }
  }
}

#--

#Now we think we have clean data we can use for our project

fix_dates_add_trade_scale <- function(rdsDataDir="./data/rds") {
  
  #return if clean file exists
  if (file.exists(paste(rdsDataDir, "directionOfTrade_clean_rescale.rds", sep="/"))) {
    return()
  }
  
  # # __Note:__ Fix some of the data columns
  # # 1. Fix date field
  # # 2. Add Cirles field
  # 
  # 
  # # We can use any one of the following
  # # 1: Base R
  # as.Date(paste("201201","01",sep=""), "%Y%m%d")
  # 
  # # 2: Using zoo package
  # 
  # as.yearmon("200001", "%Y%m")
  # as.Date(as.yearmon("200001", "%Y%m"))
  
  #
  # head(directionOfTrade_clean) %>% 
  #   mutate(., date=as.yearmon(as.character(V6), "%Y%m"))   # fix date to 'Mon Year' format
  
  # 3	Bermuda	BM	Goods, Value of Exports, Free on board (FOB), US Dollars	Colombia	CO	201303	136720	Mar 2013
  # 4	Bermuda	BM	Goods, Value of Exports, Free on board (FOB), US Dollars	Colombia	CO	201304	     0  Apr 2013
  
  # So now we add the date column -- Best practive -- for large data we fix it inline
  
  #get the parent file
  load_rds_file("directionOfTrade_clean")
  
  directionOfTrade_clean_rescale <- mutate(directionOfTrade_clean, 
                                           date=as.yearmon(as.character(V6), "%Y%m"), 
                                           V8=round(rescale( V7, to=c(1, 4)), 2 ) ) 
  # tail(directionOfTrade_clean_rescale)
  
  
  # rename variables
  names(directionOfTrade_clean_rescale) <-  c("reporter_country", "reporter_ISO_2_code", "type_of_trade",
                                              "partner_country", "partner_ISO_2_code", "period", "trade_value", 
                                              "date", "scaled_trade_value_size")
  # head(directionOfTrade_clean_rescale)
  
  # check
  #subset(directionOfTrade_clean, scaled_trade_value_size>3)
  # we update the stored data
  if (!file.exists(paste(rdsDataDir, "directionOfTrade_clean_rescale.rds", sep="/"))) {
    saveRDS(directionOfTrade_clean_rescale,  paste(rdsDataDir, "directionOfTrade_clean_rescale.rds", sep="/") )
  }
  
}


#--
# function to load rds file
load_rds_file <- function(my_data, rdsDataDir="./data/rds") {
  
  if ( !exists(my_data) ) {
    return( readRDS( paste(rdsDataDir, paste0(my_data, ".rds"), sep="/") ) )
  } else {
    cat( sprintf(" ->  \"%s\" exists in your workspace\n", my_data ) )
    return(get(my_data))   #return the value of the variable
  }
}

#--

#Split data into _[import/export]

split_data <- function(rdsDataDir="./data/rds") {
  
  #do nothing if data exists in work space
  if (exists("exports_fob_desired") & exists("imports_fob_desired") & exists("imports_cif_desired")) {
    return()
  } 
  
  #---
  # ### V3 - Trade Flow (Type of trade) 
  # a. Goods, Value of Exports, Free on board (FOB), US Dollars (including or assuming delivery without charge to the buyer's named destination)
  # b. Goods, Value of Imports, Cost, Insurance, Freight (CIF), US Dollars, 
  # c. Goods, Value of Imports, Free on board (FOB), US Dollars)
  #                  
  
  if (! exists("directionOfTrade_clean_rescale")) {
    directionOfTrade_clean_rescale <- load_rds_file("directionOfTrade_clean_rescale")
  }
  
  # non dplyr example
  exports_fob <- directionOfTrade_clean_rescale[grep('Export.*FOB.*,', directionOfTrade_clean_rescale$type_of_trade), ]
  imports_fob <- directionOfTrade_clean_rescale[grep('Import.*FOB.*,', directionOfTrade_clean_rescale$type_of_trade), ]
  imports_cif <- directionOfTrade_clean_rescale[grep('Import.*CIF.*,', directionOfTrade_clean_rescale$type_of_trade), ]
  
  # #check
  # dim(exports_fob)[1] + dim(imports_fob)[1] + dim(imports_cif)[1] - dim(directionOfTrade_clean_rescale)[1]  # 0
  # 
  # # dplyr method
  # exports_fob_2 <- filter(directionOfTrade_clean_rescale, grepl("Export.*FOB.*,", type_of_trade))
  # imports_fob_2 <- filter(directionOfTrade_clean_rescale, grepl('Import.*FOB.*,', type_of_trade))
  # imports_cif_2 <- filter(directionOfTrade_clean_rescale, grepl('Import.*CIF.*,', type_of_trade))
  # 
  # #---
  # #check:  Note using regular R, rownames are the original row numbers, in dplyr they are renumbered
  # #             so we make them identical for a fair comparisom
  # 
  # rownames(exports_fob) <- 1:dim(exports_fob)[1]
  # identical(exports_fob, exports_fob_2)
  # 
  # rownames(imports_fob) <- 1:dim(imports_fob)[1]
  # identical(imports_fob, imports_fob_2)
  # 
  # rownames(imports_cif) <- 1:dim(imports_cif)[1]; 
  # identical(imports_cif, imports_cif_2)
  
  #save the splitted dataset
  if (!file.exists(paste(rdsDataDir, "directionOfTrade_clean_rescale.rds", sep="/"))) {
    saveRDS(exports_fob,  paste(rdsDataDir, "directionOfTrade_clean_rescale.rds", sep="/") )
  }
  if (!file.exists(paste(rdsDataDir, "imports_fob.rds", sep="/"))) {
    saveRDS(imports_fob,  paste(rdsDataDir, "imports_fob.rds", sep="/") )
  }
  if (!file.exists(paste(rdsDataDir, "imports_cif.rds", sep="/"))) {
    saveRDS(imports_cif,  paste(rdsDataDir, "imports_cif.rds", sep="/") )
  }
  
  
  #Subset data for valid countries only
  #Interested in data for valid countries only
  
  
  # list of continents
  
  if (! exists("countryList_clean")) {
    countryList_clean <- load_rds_file("countryList_clean")
  }
  
  continents <- unique( countryList_clean$Continent )
  
  # list of countries
  countrycodes <- unique( countryList_clean$ISO.2.Code )
  #countrycodes <- distinct(countryList_clean, ISO.2.Code)[1:3,1]        #Afghanistan -- AF
  
  # if you wish  to
  # Data for desired countries  -- no blank country codes
  exports_fob_desired <- exports_fob %>% filter(., reporter_ISO_2_code %in% countrycodes & partner_ISO_2_code %in% countrycodes)
  imports_fob_desired <- imports_fob %>% filter(., reporter_ISO_2_code %in% countrycodes & partner_ISO_2_code %in% countrycodes)
  imports_cif_desired <- imports_cif %>% filter(., reporter_ISO_2_code %in% countrycodes & partner_ISO_2_code %in% countrycodes)
  
  #check
  # exports_fob %>% filter(., !( reporter_ISO_2_code %in% countrycodes & partner_ISO_2_code %in% countrycodes)) %>% head
  # 
  # subset(countrycodes, grepl('^P', countrycodes))
  
  if (!file.exists(paste(rdsDataDir, "exports_fob_desired.rds", sep="/"))) {
    saveRDS(exports_fob_desired,  paste(rdsDataDir, "exports_fob_desired.rds", sep="/") )
  }
  if (!file.exists(paste(rdsDataDir, "imports_fob_desired.rds", sep="/"))) {
    saveRDS(imports_fob_desired,  paste(rdsDataDir, "imports_fob_desired.rds", sep="/") )
  }
  if (!file.exists(paste(rdsDataDir, "imports_cif_desired.rds", sep="/"))) {
    saveRDS(imports_cif_desired,  paste(rdsDataDir, "imports_cif_desired.rds", sep="/") )
  }
  
}


#--

### What are the top 5 import/export countries to the US from Jan 2013 to Dec 2015


get_top_n <- function(rdsDataDir="./data/rds", start_date="Jan 2013", end_date = "Dec 2015", top_n=15, reporter_partner_ISO_code="US") {
  
  #do nothing if data exists in work space
  if (exists("import_export_data_topN") ) {
    return()
  } 
  
  if (! exists("exports_fob_desired")) {
    exports_fob_desired <- load_rds_file("exports_fob_desired")
  }
  
  
  exports_fob_topN <- filter(exports_fob_desired, reporter_ISO_2_code == reporter_partner_ISO_code & date >= start_date & date < end_date )  %>%   
    group_by(. , reporter_country, reporter_ISO_2_code, partner_country, partner_ISO_2_code)  %>%
    summarize(., TotalExport = sum(trade_value, na.rm = T), AverageExport = mean(trade_value, na.rm = T))  %>% #create total and average
    filter(. , TotalExport > 0.0)    %>%                                                                       #get non-zero data values
    arrange(., desc(AverageExport))  %>%                                                                       #order by ISO code
    head(., n=top_n)
  
  #exports_fob_topN
  
  countries_topN <- exports_fob_topN$partner_ISO_2_code
  #countries_topN
  
  if (! exists("imports_fob_desired")) {
    imports_fob_desired <- load_rds_file("imports_fob_desired")
  }
  
  
  imports_fob_topN <- filter(imports_fob_desired, partner_ISO_2_code == reporter_partner_ISO_code & date >= start_date & date < end_date & reporter_ISO_2_code %in% countries_topN)  %>%   
    group_by(. , partner_country, partner_ISO_2_code, reporter_country, reporter_ISO_2_code)  %>%
    summarize(., TotalImport = sum(trade_value, na.rm = T), AverageImport = mean(trade_value, na.rm = T))  %>% #create total and average
    filter(. , TotalImport > 0.0)    %>%                                                                       #get non-zero data values
    arrange(., desc(AverageImport))  %>%                                                                       #order by ISO code
    head(., n=top_n)
  
  #imports_fob_topN
  
  
  if (! exists("imports_cif_desired")) {
    imports_cif_desired <- load_rds_file("imports_cif_desired")
  }
  
  imports_cif_topN <- filter(imports_cif_desired, partner_ISO_2_code == reporter_partner_ISO_code & date >= start_date & date < end_date & reporter_ISO_2_code %in% countries_topN)  %>%   
    group_by(. , partner_country, partner_ISO_2_code, reporter_country, reporter_ISO_2_code)  %>%
    summarize(., TotalImport = sum(trade_value, na.rm = T), AverageImport = mean(trade_value, na.rm = T))  %>% #create total and average
    filter(. , TotalImport > 0.0)    %>%                                                                       #get non-zero data values
    arrange(., desc(AverageImport))  %>%                                                                       #order by ISO code
    head(., n=top_n)
  
  #imports_cif_topN
  #---
  
  # start_date="Jan 2013"
  # end_date = "Dec 2015"
  # top_n <- 15
  # 
  # ( exports_fob_topN <- get_top_n(exports_fob_desired, start_date, end_date, top_n) )
  # ( imports_fob_topN <- get_top_n(imports_fob_desired, start_date, end_date, top_n) )
  # ( imports_cif_topN <- get_top_n(imports_cif_desired, start_date, end_date, top_n) )
  
  #--
  # Merge data to plot
  #--
  #Exclude redundant fields in y
  
  
  import_export_data_topN <- merge(exports_fob_topN, 
                                   subset( imports_fob_topN, select=c(reporter_country, reporter_ISO_2_code, TotalImport, AverageImport) ), 
                                   by.x="partner_ISO_2_code", by.y="reporter_ISO_2_code", all.x = TRUE)    %>% 
    merge( ., 
           subset( imports_cif_topN, select=c(reporter_country, reporter_ISO_2_code, TotalImport, AverageImport) ),
           by.x="partner_ISO_2_code", by.y="reporter_ISO_2_code", all.x = TRUE)    %>%
    subset(., select=c(partner_ISO_2_code, partner_country, TotalExport, AverageExport, TotalImport.x, AverageImport.x, TotalImport.y, AverageImport.y) ) 
  
  names(import_export_data_topN) <- c("partner_ISO_2_code", "partner_country", 
                                      "total_fob_exports", "average_fob_exports", 
                                      "total_fob_imports", "average_fob_imports",
                                      "total_cif_imports", "average_cif_imports")
  
  #--
  #replace NA values with 0
  #--
  if (! exists("countryList_clean")) {
    countryList_clean <- load_rds_file("countryList_clean")
  }
  
  # list of countries
  countrycodes <- unique( countryList_clean$ISO.2.Code )
  
  #--
  
  import_export_data_topN <- import_export_data_topN %>% 
    mutate( partner_ISO_2_code = factor(partner_ISO_2_code, levels=countrycodes, ordered=FALSE),
            total_fob_exports = ifelse(is.na(total_fob_exports), 0, total_fob_exports), 
            total_fob_imports = ifelse(is.na(total_fob_imports), 0, total_fob_imports),
            total_cif_imports = ifelse(is.na(total_cif_imports), 0, total_cif_imports),
            average_fob_exports = ifelse(is.na(average_fob_exports), 0, average_fob_exports), 
            average_fob_imports = ifelse(is.na(average_fob_imports), 0, average_fob_imports),
            average_cif_imports = ifelse(is.na(average_cif_imports), 0, average_cif_imports)
    ) %>% arrange(., desc(total_fob_exports)) 
  
  
  # Save data
  saveRDS(import_export_data_topN,  paste(rdsDataDir, "import_export_data_topN.rds", sep="/") )
}


### Plot Total US FOB/CIF imports/exports values

#isualize overall trade between US and the entire continent

prepare_map_data <- function(rdsDataDir="./data/rds",  reporter_partner_ISO_code="US") {
  
  #do nothing if data exists in work space
  if (exists("fobx") & exists("fobi") & exists("cifi") ) {
    return()
  } 
  
  if (! exists("exports_fob_desired")) {
    exports_fob_desired <- load_rds_file("exports_fob_desired")
  }
  
  if (! exists("imports_fob_desired")) {
    imports_fob_desired <- load_rds_file("imports_fob_desired")
  }
  
  
  if (! exists("imports_cif_desired")) {
    imports_cif_desired <- load_rds_file("imports_cif_desired")
  }
  
  if (! exists("countryLongLat_clean")) {
    countryLongLat_clean <- load_rds_file("countryLongLat_clean")
  }
  
  # US FOB exports
  fobx <- filter(exports_fob_desired, reporter_ISO_2_code == "US" & date >= start_date & date < end_date)  %>%   
    group_by(. , partner_country, partner_ISO_2_code) %>%
    summarize( ., TotalExport = sum(trade_value, na.rm = T), 
               AverageExport = mean(trade_value, na.rm = T), 
               mean_scaled_trade_value_size=mean(scaled_trade_value_size, na.rm = T) )  %>%      #create total and average
    filter(. , TotalExport > 0.0)    %>%                                                                            #get non-zero data values
    merge.data.frame(., countryLongLat_clean, all.x=TRUE, by.x="partner_ISO_2_code", by.y="country")             %>%       # Merge lds and loc
    subset(., select=c(partner_ISO_2_code, partner_country,  TotalExport, AverageExport, latitude, longitude, mean_scaled_trade_value_size ) ) 
  
  # dim(fobx)  #199   7
  
  
  #--
  # US FOB imports from other countries
  #  grep -i '"US";"Goods,' DirectionOfTrade.txt | grep '(CIF)' | grep '"CZ"' | head
  
  fobi <- filter(imports_fob_desired, reporter_ISO_2_code == "US" )  %>%   
    group_by(. , partner_country, partner_ISO_2_code)  %>%
    summarize( ., TotalImport = sum(trade_value, na.rm = T), 
               AverageImport = mean(trade_value, na.rm = T),
               mean_scaled_trade_value_size=mean(scaled_trade_value_size, na.rm = T) )  %>%      #create total and average
    filter(. , TotalImport > 0.0)    %>%                                                                            #get non-zero data values
    merge.data.frame(., countryLongLat_clean, all.x=TRUE, by.x="partner_ISO_2_code", by.y="country")             %>%       # Merge lds and loc
    subset(., select=c(partner_ISO_2_code, partner_country, TotalImport, AverageImport, latitude, longitude, mean_scaled_trade_value_size) ) 
  
  # dim(fobi)   #0 7
  
  #--
  # US CIF imports from other countries
  cifi <- filter(imports_cif_desired, reporter_ISO_2_code == "US" )  %>%   
    group_by(. , partner_country, partner_ISO_2_code)  %>%
    summarize( ., TotalImport = sum(trade_value, na.rm = T), 
               AverageImport = mean(trade_value, na.rm = T),
               mean_scaled_trade_value_size=mean(scaled_trade_value_size, na.rm = T) )  %>%      #create total and average
    filter(. , TotalImport > 0.0)    %>%                                                                            #get non-zero data values
    merge.data.frame(., countryLongLat_clean, all.x=TRUE, by.x="partner_ISO_2_code", by.y="country")             %>%       # Merge lds and loc
    subset(., select=c(partner_ISO_2_code, partner_country, TotalImport, AverageImport, latitude, longitude, mean_scaled_trade_value_size) ) 
  
  #dim(cifi)  #197   7
  #--
  # Save data
  saveRDS(fobx,  paste(rdsDataDir, "fobx.rds", sep="/") )
  saveRDS(fobi,  paste(rdsDataDir, "fobi.rds", sep="/") )
  saveRDS(cifi,  paste(rdsDataDir, "cifi.rds", sep="/") )
  
}

#--

#--


mapData <- function( mapfile="./png/abc.png", 
                     data=usfobx, title="US FOB Exports from Jan 2015 to Dec 2015", 
                     label="FOB - Contracts relieve the seller of responsibility once the goods are shipped") {
  #--
  #see if data file is empty
  if ( dim(data)[1] < 1 ) {
    cat(sprintf("\n    Warning: Datafile has no records.  Skipping %s ...", mapfile))
    
  } else {
    
    ## set the path and file name of the image file
    png(mapfile,
        width = 1920, 
        height = 1080)
    
    ## plot the world map, earthquakes, and time stamp
    map(database = "world",
        bg="slategray2",
        col="white",
        fill=T,
        xlim=c(-180, 180),
        ylim=c(-90, 90))
    
    map.axes()
    
    symbols(data$longitude, data$latitude, bg="red",
            fg="green", lwd = 1, circles = data$mean_scaled_trade_value_size, 
            inches = 0.175, add = TRUE)
    
    text(0, 88, labels = label, cex=1.8, col = "blue")
    
    title(main=list(title, 
                    cex=2.5, 
                    col="black", 
                    font=12))
    box()
    
    dev.off()
    
    cat(sprintf("\n %s saved ...", mapfile))
  }
  
}


#--


