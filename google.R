#installs googleway api
install.packages('googleway')

#load linraries to be used
liby <- c("rstudioapi", "devtools", "googleway")
lapply(liby, require, character.only = TRUE)


api_key <- 'your-api-key-here' #google api key to be used

#set the latitude, longitude, and town name of where you want to search
lat <- 42.4084
long <- -71.0120
loc <- c(lat,long)
radius <- 7500 #radius in meters
town = 'Revere' 

#loop to compile place search data
places <- data.frame()
for (i in 1:20) {
  print(i)
  Sys.sleep(3) #Google doesn't like users spamming their API so we need to delay
  if (i == 1) {
    #on the first iteration don't use a next page token
    response <- google_places(location = loc, radius = radius, key = api_key)
  } else {
    #on the other passes, the next page token is used
    response <- google_places(location = loc, radius = radius, key = api_key, page_token = token)
  }
  token <- response$next_page_token
  raw_places <- response$results
  print(length(raw_places))
  if ((length(raw_places) == 0) | (length(token) < 0)) {
    #break if no more results are coming in
    break
  }
  #take only the columns we need
  new_places <- raw_places[,c('name','place_id','business_status')]
  
  #save the coordinates of the place
  new_places$lat <- raw_places$geometry$location$lat
  new_places$long <- raw_places$geometry$location$lng
  
  #row bind to running place dataframe
  places <- rbind(places,new_places)
  if (length(token) == 0 ) {
    #Google seems to only give three page tokens so once we don't get a new one, break
    break
  }
}



#assign columns to keep from place search
keep = c("name","formatted_address","formatted_phone_number","business_status","place_id","types", "url","website", "opening_hours", "lat", "long")  
#make an empty data frame
compiled <- data.frame()
for (row in 1:nrow(places)) {
  if (is.na(places[row, "business_status"])) {
    #places that are not business will not have operational statuses
    next
  }
  
  #extract specific place id from pleces list
  place_id <- places[row, "place_id"]
  
  #pause loop call to server for .1s to not have Google block us for too many calls
  Sys.sleep(.1)
  print(row)
  
  #call API go get specific place information
  current_place <- google_place_details(place_id = place_id, key = api_key)$result
  
  if (!(town %in% current_place$address_components$long_name)) {
    #if the target town isn't in the address, skip it as well
    next
  }
  
  
  
  #add latitude and longitude
  current_place$lat <- places$lat[row]
  current_place$long <- places$long[row]
  
  #check if any of the columns intended to be kept are missing
  for (category in keep) {
    if (!(category %in% names(current_place))) {
      #if so, add them
      current_place[[category]] <- NA
    }
  }

  
  #format the place information and append it to the table
  #collapsing necessary columns
  if (!is.na(current_place$opening_hours)) {
    #can only collapse if opening_hours is present
    current_place$opening_hours <- paste(current_place$opening_hours$weekday_text,collapse=', ')
  }
  #types should always be present
  current_place$types <- paste(current_place$types,collapse=', ')
  
  
  #append to running list
  compiled <- rbind(compiled,current_place[keep])
}


#write to file, eliminating the row names
filename <- paste(town, '-google.csv', sep = '')
write.csv(compiled, filename, row.names=FALSE)

  
  
  