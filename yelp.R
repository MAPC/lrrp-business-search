
# Install required libraries
install.packages('devtools')

# Install yelpr library directly from its github repository
devtools::install_github("OmaymaS/yelpr", force = TRUE)

# Load required libraries
liby <- c("rstudioapi", "devtools", "yelpr")
lapply(liby, require, character.only = TRUE)



###########################################################################################################

key<- 'your-api-key-here' # insert yelp API key


town <- "Revere" #town search is to be performed in
state <- "MA" #state, always MA for purposes of LRRP
loc <- paste(town, state, sep = ', ')
limit <- 50 #the number of results on one page you would like to see. The max for one page is 50

# Write a loop for 1000 results


clean_data <- data.frame() #create an empty data fraame to add 
for(offset in seq(0, 1000, 50)) {
  #offset by 50 to get get the next 50 results from yelp everytime
  #probably won't go through all 50 iterations
  #temporary storage container for our call to the server
  temp <- business_search(api_key = key, location = loc, limit = limit, offset = offset)
  #use if statement to execute the next part if temp$businesses is not empty
  if(length(temp$businesses) != 0) {
    
    #store results of call in a dataframe called temp1
    temp1 <- temp$businesses
    temp1 <- temp1[temp1$location$city == town,] #filter out any location not in target town


    
    #retrieve coordianates and address
    geom <- temp1$coordinates
    add <- temp1$location
    
    #select columns we want
    temp1 <- temp1[, c("name", "display_phone", "is_closed", "categories")]
    
    #bind columns together
    merge <- cbind(temp1, geom, add)

    #append rows generated by the loop to the clean_data data frame
    clean_data <- rbind(clean_data, merge)
    
  } else {
  #loop to input data into loop_yelp is completed when you get this far 
    break
  }
}  

#cleans the categories list that is returned from yelp
#parsing the json response returns imbedded data Frames which can't be saved to csv
for (row in 1:nrow(clean_data)) {
  clean_data$categories_list[row] = toString(clean_data$categories[[row]][,1])
}
clean_data <- subset (clean_data, select = -categories)


final_data <- apply(clean_data,2,as.character) #fixes some saving issue that I don't know
filename <- paste(town, '-yelp.csv', sep = '')
write.csv(final_data, file = filename, row.names=FALSE)


