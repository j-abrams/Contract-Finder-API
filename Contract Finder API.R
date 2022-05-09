
# Contract Finder API 

# pull data from Contracts Finder using this API

# https://www.contractsfinder.service.gov.uk/Search
# https://www.contractsfinder.service.gov.uk/apidocumentation/V2
# https://www.contractsfinder.service.gov.uk/apidocumentation/

# developing into something which automatically runs every day or week, 
# retrieving data, cleaning it and populating it into our own database.


#### Pacman packages routine ---- 
# Install and load all required packages at once
#install.packages("pacman")  # This command only needs to be called once to make sure pacman is pre-installed
require(pacman)
pacman::p_load(httr, jsonlite, tidyverse)

# Packages required
#install.packages(c("httr", "jsonlite", "tidyverse"))
#library(httr)
#library(jsonlite)
#library(tidyverse)

#### Defining local Times / Dates ----

print(paste("Start:", Sys.time()))

# Variable contains the current date / week
# Incorporating a new run each week / day to explore newly listed contracts
today         <- Sys.Date()
last_week     <- Sys.Date() - 7
yesterday     <- Sys.Date() - 1
last_2_years  <- Sys.Date() - (365 * 2)
last_4_years  <- Sys.Date() - (365 * 4)
#day_before_yesterday <- Sys.Date() - 2

# First entry: 2000-01-01

# Defining end_date
end_date <- yesterday

# Defining start_date (Picking up from where the last response left off)
# This routine will ensure in future the response runs the optimum number of times
if(file.exists("last_run.txt")){
  start_date <- paste(readLines("last_run.txt"), collapse=" ")
} else {
  start_date <- end_date
}


#### Query for the Response ----

url <- "https://www.contractsfinder.service.gov.uk/Search/Results"

# Have to run path here first to determine maxPage variable.
# Commenting out the right line here is vital. response_func() will fall over if maxPage is incorrect

#path  <- paste0("Published/Notices/OCDS/Search")
# Searching To / From a specified date (today)

path_1  <- paste0("Published/Notices/OCDS/Search?publishedFrom=", 
               last_4_years, "&publishedTo=", last_2_years)

path_2 <- paste0("Published/Notices/OCDS/Search?publishedFrom=", 
                last_2_years, "&publishedTo=", end_date)

path_3 <- paste0("Published/Notices/OCDS/Search?publishedFrom=", 
                start_date, "&publishedTo=", today)


# Defining a function to extract the max number of pages.
# This is vital to deine how many iterations the loops will require.
# Stored in a function for reproducability.
page_function <- function(path) {
  # httr:GET() takes two arguments, url and path
  response <- GET(url = url, path = path)
  # View the response in the console
  print(response)
  
  # jsonlie::fromJSON converts the reponse to a format we can work with and clean
  test <- fromJSON(rawToChar(response$content), flatten = TRUE)
  # Define "maxPage" to determine the final iteration of our for loop
  maxPage  <- test$maxPage
  return(maxPage)
}

# Calling page_function()
maxPage_1 <- page_function(path_1)
maxPage_2 <- page_function(path_2)
maxPage_3 <- page_function(path_3)




#### Extracting data from the Response ----

# Iterate with a for loop to generate one page at a time before pulling all the reusults together
# Define empty dataframe ready to be populated within the loop.

# response_func: function wrapper
response_func <- function(pages = maxPage, from = last_week, to = end_date,
                          url = "https://www.contractsfinder.service.gov.uk/Search/Results") {
  data <- data.frame()
  for (i in 1:pages) {    #pages = #maxPage
    
    path <- paste0("Published/Notices/OCDS/Search?size=100&page=", i,
                   "&publishedFrom=", from, 
                   "&publishedTo=", to)
    #path <- paste0("Published/Notices/OCDS/Search?size=100&page=", i, "&publishedFrom=", from)
    response <- GET(url = url, path = path)
    
    temp <- fromJSON(rawToChar(response$content), flatten = TRUE)$results$releases
    
    # Routine incorporated to extract nested lists from  as dataframe.
    new <- temp %>% 
      map(unlist) %>% 
      map(t) %>% 
      map(as_tibble) %>% 
      bind_rows()
    
    # Keep only columns which intersect the old and new sets.
    # Conditional fixes an error risen on the first iteration only
    if (i == 1) {
      data <- rbind(data, new)
      # rbind() will not operate successfully without this clause because certain responses have varying numbers of columns. 
    } else {
      cols <- intersect(colnames(data), colnames(new))
      data <- rbind(data[,cols], new[,cols])
    }
    # Alternative solution to the rbind() problem, "smartbind"
    #install.packages("gtools")
    #library(gtools)
    #pop_dat <- smartbind(pop_dat, new2)
    
  }
  return(data)
}

# Running the function to generate what we need
data_1  <- response_func(pages = maxPage_1, from = last_4_years, to = last_2_years)

data_2 <- response_func(pages = maxPage_2, from = last_2_years, to = end_date)

data_3 <- response_func(pages = maxPage_3, from = start_date, to = today)


#### Date / Time Stamp ----

# Save a date and time stamp on the time of last completed run
# This will store the final day included in our previous run,
# informing the user which day to start from next time (publishedFrom for next iteration)
fileConn <- file("last_run.txt")
writeLines(as.character(end_date), fileConn)
close(fileConn)

print(paste("End:", Sys.time()))









####
#### Archive ----
####
#
# publishedFrom - Lower limit on publication date; ISO 8601 format
# publishedTo - Upper limit on publication date
# stage - Procurement Stage filter, Values = planning, tender, award, implementation
#
# Additional commands from the documentation to experiment with
#
#path <- "Postcode/Postcodes/SG5"   # Testing the postcode generator
#path <- "api/rest/2/search_notices"   # Testing v2... couldnt get this one to work.
#
# Investigating the response. Unsure how the flatten argument influences the process 
#
#names(response)
#response$status_code
#test  <-  fromJSON(rawToChar(response$content), flatten = FALSE)
#test2 <-  fromJSON(rawToChar(response$content), flatten = TRUE)
#test$results
#test$results$releases
#
# TODO: Keep only the fields we are interested in please: DONE
# TODO: Incorporate this response routine into the loop: DONE
# TODO: Experiment with publishedFrom and publishedTo: DONE
#
#
# Examining the data ---- 
#
#names(data)
#table(data$tag)
#table(data$tender.status)
#table(data$tender.xClassifications.ojeuContractType)
#table(data$buyer.address.locality)

