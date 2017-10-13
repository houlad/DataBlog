library(readr)
library(stringi)
library(dplyr)
library(lubridate)

# ----------------------------------------------------------------------------------
# Function to filter raw data to remove characters that will trip up the geocoding. Then
# concatenating city and zip code with block address to form a full address for the
# geocoding service. Incidents with 'community at large|metro at large|pending location' location are 
# removed as geocoding is impossible

geocode_prep <- function(data){
  
   prepped_data <- data%>%
    # Remove block addresses that don't provide enough info to geocode
    filter(block_address != "community at large", block_address != "pending location",
            block_address != "metro at large")%>%
     # Remove entries with troublesome geocoding characters
    filter(stri_detect_regex(block_address, "&|#|@|\\.|\\(|\\)", negate = TRUE))%>%
     # Remove extraneous 'block' from addresses--doesn't add anything and occasionally 
     # messes up geocoding. After, remake the full address variable incorporating new info
    mutate(block_address = stri_replace_all_regex(block_address, "block ", ""),
           block_address = if_else(stri_detect_regex(block_address, "wf -"), "Waterfront Park", block_address),
           # Addresses with '/' intersections are transformed to just the first street
           # For a more complete analysis, some means of geocoding cross streets would
           # need to be developed
           block_address = stri_trim_both(stri_split_fixed(block_address, "/", simplify = TRUE)[,1], pattern = "\\P{Wspace}"),
           full_address = paste0(block_address, ", Louisville, KY, ", zip_code))
}

#----------------------------------------------------------------------------------
# Raw Data loaded and and various cleaning steps taken to prepare data for
# geocoding and for later manipulation

data_import_and_clean <- function(csvfile, save.raw.data = FALSE,
                                  create.date.vars = TRUE, label.crimes = TRUE, ...){
  csv_file_name <- csvfile
  # Read csv into r
  
  raw_data <- read_csv(csvfile)
  
  # Lowercase everything and trim whitespace from columns then convert to a tibble df
  names(raw_data) <- tolower(names(raw_data))
  raw_data <- data.frame(lapply(raw_data, tolower), stringsAsFactors = TRUE)
  raw_data$uor_desc <- stri_trim_both(raw_data$uor_desc, pattern = "\\P{Wspace}")
  raw_data$block_address <- stri_trim_both(raw_data$block_address, pattern = "\\P{Wspace}")
  raw_data <- as_tibble(raw_data)
  
  if(save.raw.data == TRUE){
    #Generates output filename based on the year of the original csv file
    saved_file_name <- paste0("raw_data_", stri_split_fixed(stri_split_fixed(csv_file_name, "_")[[1]][3], ".")[[1]][1])
    write_csv(raw_data, path = saved_file_name)
  }
  
  # Convert date variables to POSIX 
  # Entries with missing dates are removed
  raw_data <- raw_data%>%
    mutate(date_occured = as.POSIXct(raw_data$date_occured, format = "%Y-%m-%d %H:%M:%S"),
           date_reported = as.POSIXct(raw_data$date_reported, format = "%Y-%m-%d %H:%M:%S"))%>%
    filter_(~!is.na(date_occured), ~!is.na(date_reported))
  
  ## If create.date.vars == TRUE, create new variables for:
    # --year
    # --month
    # --hour
    # --day of week
  if(create.date.vars == TRUE){
    raw_data <- raw_data %>%
      mutate(year = year(raw_data$date_occured), 
             year_reported = year(raw_data$date_reported),
             month = month(raw_data$date_occured, label = TRUE), 
             month_reported = month(raw_data$date_reported, label = TRUE),
             day = day(raw_data$date_occured),
             hour = round(hour(raw_data$date_occured) + minute(raw_data$date_occured)/60, 0),
             day_of_week = wday(raw_data$date_occured, label = TRUE, abbr = FALSE),
             weekday = ifelse(day_of_week == "Saturday" | day_of_week == "Sunday",
                              "Weekend", "Weekday"),
             yday = yday(raw_data$date_occured))
  }
  
  ## add more specific crime labels based on the nibrs codes supplied
  
  if(label.crimes == TRUE){
    nibrs_offenses <- data.frame(nibrs_offenses = c("Arson", "Aggravated Assault", "Simple Assault", "Intimidation",
                                                    "Bribery", "Burglary/B&E", "Counterfeiting/Forgery", "Destruction/Damage/Vandalism of Property",
                                                    "Drug/Narcotic Violations", "Drug/Narcotic Equip. Violations",
                                                    "Embezzlement", "Extortion/Blackmail", "False Pretenses/Swindle/Confidence Games",
                                                    "Credit Card/Automatic Teller Machine Fraud", "Impersonation",
                                                    "Welfare Fraud", "Wire Fraud", "Betting/Wagering", "Operating/Promoting/Assisting Gambling",
                                                    "Gambling Equip. Violations", "Sports Tampering", "Murder/Non-Negligent Manslaughter",
                                                    "Negligent Manslaughter", "Justifiable Homicide", "Commercial Sex Acts",
                                                    "Involuntary Servitude", "Kidnapping/Abduction", "Pocket Picking",
                                                    "Purse Snatching", "Shoplifting", "Theft from Building", "Theft from Coin-Operated Machine or Device",
                                                    "Theft from Motor Vehicle", "Theft of Motor Vehicle Parts or Accessories",
                                                    "All Other Larceny"," Motor Vehicle Theft", "Pornography/Obscene Material",
                                                    "Prostitution", "Assisting or Promoting Prostitution", "Purchasing Prostitution",
                                                    "Robbery", "Rape", "Sodomy", "Sexual Assault with An Object", "Forcible Fondling",
                                                    "Incent", "Statutory Rape", "Stolen Property Offenses", "Weapon Law Violations", 
                                                    "Bad Checks", "Curfew/Loitering/Vagrancy Violations", "Disorderly Conduct",
                                                    "Driving Under the Influence", "Drunkenness", "Family Offenses, Non-Violent",
                                                    "Liquor Law Violations", "Peeping Tom", "Runaway", "Tresspassing", "All Other Offenses"),
                                 nibrs_code = c("200", "13A", "13B", "13C", "510", "220", 
                                                "250", "290", "35A", "35B", "270", "210", 
                                                "26A", "26B", "26C", "26D", "26E", "39A", 
                                                "39B", "39C", "39D", "09A", "09B", "09C",
                                                "64A", "64B", "100", "23A", "23B", "23C", 
                                                "23D", "23E", "23F", "23G", "23H", "240",
                                                "370", "40A", "40B", "40C", "120", "11A",
                                                "11B", "11C", "11D", "36A", "36B", "280",
                                                "520", "90A", "90B", "90C", "90D", "90E",
                                                "90F", "90G", "90H", "90I", "90J", "90Z"))
    nibrs_offenses$nibrs_code <- as.factor(tolower(nibrs_offenses$nibrs_code))
    # Different number of nibrs_codes in each df, so join the two together to include them all
    # and avoid any errors in the join
    combined_nibrs_codes <- sort(union(levels(nibrs_offenses$nibrs_code), levels(raw_data$nibrs_code)))
    raw_data <- left_join(mutate(raw_data, nibrs_code = factor(nibrs_code, levels = combined_nibrs_codes)),
                          mutate(nibrs_offenses, nibrs_code = factor(nibrs_code, levels = combined_nibrs_codes)))
  }
  
  # Removing rows with zip codes that are not Louisville zip codes.
  lou_zip <- c(40056, 40118, 40201, 40202, 40203, 40204, 40205, 40206,
               40207, 40208, 40209, 40210, 40211, 40212, 40213, 40214, 
               40215, 40216, 40217, 40218, 40219, 40220, 40221, 40222, 
               40223, 40224, 40225, 40228, 40229, 40231, 40232, 40233, 
               40241, 40242, 40243, 40245, 40250, 40251, 40252, 40253,
               40255, 40256, 40257, 40258, 40259, 40261, 40266, 40268, 
               40269, 40270, 40272, 40280, 40281, 40282, 40283, 40285, 
               40287, 40289, 40290, 40291, 40292, 40293, 40294, 40295,
               40296, 40297, 40298, 40299)
  lou_zip <- as.factor(lou_zip)
  raw_data <- raw_data%>%
    filter(zip_code %in% lou_zip == TRUE)
  
  #####################################################################################
  # Various data removed here after exploratory analysis
  
  # Reports with 'see accident module' as uor_despriction removed as it appears to be some preliminary
  # report (so possibly duplicates). They are all labeled as 'other' crimes too, which
  # doesn't yield useful info
  # 'Preliminary report number' descriptions also removed
  raw_data <- raw_data%>%
    filter_(~uor_desc != "see accident module", ~uor_desc != "preliminary report number",
            ~uor_desc != "report number not required", ~uor_desc != "any non criminal charge not covered by these codes",
            ~uor_desc != "injured person requiring police report", ~uor_desc != "voided report number",
            ~uor_desc != "cold case report number", ~uor_desc != "dv waiting on charge", ~uor_desc != "death investigation",
            ~uor_desc != "non-criminal death (natural causes)")%>%
    filter(stri_detect_fixed(uor_desc, 'pending', negate = TRUE))


  # selects rows to return, including creating a 'full_address' variable with zip/city/state
  raw_data %>%
    select(incident_number, date_reported, date_occured, uor_desc, crime_type, nibrs_code,
           att_comp, lmpd_division, lmpd_beat, premise_type, block_address,
           zip_code, year, year_reported, month, month_reported, day, hour,
           day_of_week, weekday, yday, nibrs_offenses)%>%
    mutate(full_address = paste0(block_address, ", Louisville, KY, ", zip_code))
  
}



#----------------------------------------------------------------------------------
# Loading old geocoding results to use to cut down on new geocoding needed.
# Then I select out just the lat/lng and the full address to use to join back with
# the full data set so I can see what addressess need geocoding
# After filtering distinct addresses, save the file so it can be imported in the geocoding
# script

addresses_to_geocode <- function(data){
  if(exists("already_geocoded")== FALSE) {
    already_geocoded <- as_tibble(read_csv("addresses_and_coords.csv"))
  }
  
  
  ag<- already_geocoded%>%
    filter(is.na(full_address) == FALSE, is.na(lat) == FALSE, is.na(lng) == FALSE)%>%
    mutate(lat = as.character(lat), lng = as.character(lng))

  
  # Join the geocoded addresses with the new data so we can filter out those 
  # addresses missing lat/lng
  coord_join <- left_join(data, ag, by = 'full_address')
  
  # # Filter out just the missing coords to be geocoded
  missing_coords <- coord_join%>%
    filter(is.na(lat))%>%
    distinct(full_address)
  
  # If there are addresses that need coords, write to a csv file to be used by the geocoding
  # script
  if(!nrow(missing_coords)){
    print("No new addresses to geocode.")
  }else{
    # write this dataframe of addresses to a file to be read by the geocoding script
    write_csv(missing_coords, "/home/adam/R/LouisvilleCrime/addresses_to_geocode.csv")
  }
  
}


############################################################################################
# write the resulting cleaned data frame to a csv file for use elsewhere
save_clean_data <- function(data){
  write_csv(data, path = "lou_crime_without_coords.csv")
}
# 
# #----------------------------------------------------------------------------------
# # Write this data set out to be used in other scripts
# write.csv(crime_lou, file = "crime_lou_with_geocoding.csv", row.names = FALSE)
