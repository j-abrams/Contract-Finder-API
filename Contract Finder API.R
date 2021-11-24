
# Contract Finder API ----

# pull data from Contracts Finder using this API

# https://www.contractsfinder.service.gov.uk/apidocumentation/V2

# developing into something which automatically runs every day or week, 
# retrieving data, cleaning it and populating it into our own database.

# Packages required
install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)
library(tidyverse)


# publishedFrom - Lower limit on publication date; ISO 8601 format
# publishedTo - Upper limit on publication date
# stage - Procurement Stage filter, Values = planning, tender, award, implementation


# Variable contains the current date / week
# Incorporating a new run each week / day to explore newly listed contracts
today <- Sys.Date()
last_week <- Sys.Date() - 7

url <- "https://www.contractsfinder.service.gov.uk/Search/Results"
#path <- "Published/Notices/OCDS/Search"
path <- paste0("Published/Notices/OCDS/Search?publishedFrom=", last_week)   # Searching To / From a specified date (today)

#path <- "Postcode/Postcodes/SG5"   # Testing the postcode generator
#path <- "api/rest/2/search_notices"   # Testing v2... couldnt get this one to work.


# httr:GET() takes two arguments, url and path
response <- GET(url = url, path = path)
# View the response in the console
response
# jsonlie::fromJSON converts the reponse to a format we can work with and clean
test <- fromJSON(rawToChar(response$content), flatten = TRUE)
maxPage <- test$maxPage

# Investigating the response. Unsure how the flatten argument influences the process 

#names(response)
#response$status_code
#test  <-  fromJSON(rawToChar(response$content), flatten = FALSE)
#test2 <-  fromJSON(rawToChar(response$content), flatten = TRUE)
#test$results
#test$results$releases


# TODO: Keep only the fields we are interested in please: DONE
# TODO: Incorporate this response routine into the loop: DONE
# TODO: Experiment with publishedFrom and publishedTo


# Iterate with a for loop to generate one page at a time before pulling all the reusults together
# Define empty dataframe ready to be populated within the loop.
data <- data.frame()
for (i in 1:5) {    #maxPage
  
  path <- paste0("Published/Notices/OCDS/Search?size=100&page=", i)
  #path <- paste0("Published/Notices/OCDS/Search?size=100&page=", i, "&publishedFrom=", last_week)
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



#TODO: Confirm with Tom which fields to keep and which to remove.
# Cpv codes  (tender.items.classification.id, )
# Suitable for sme  (awards.suppliers.sme, tender.xClassifications.isSuitableForSme,)
# OJEU procedure type  (tender.xClassifications.ojeuContractType)
# Requesting by date range

data_clean <- data %>%
  #select(-c(82:ncol(.))) %>%
  select(c(date, 
           tag, 
           awards.date, 
           awards.suppliers.sme, 
           awards.suppliers.name,
           awards.suppliers.address.streetAddress,
           awards.value.amount, 
           awards.contractPeriod.startDate, 
           awards.contractPeriod.endDate,
           tender.id, 
           tender.title,
           tender.description,
           tender.status,
           tender.items.classification.id, 
           tender.items.classification.description,
           tender.procurementMethodDetails,
           tender.milestones.dueDate1, 
           tender.milestones.dueDate2,
           tender.minValue.amount,
           tender.value.amount,
           tender.tenderPeriod.endDate,
           tender.xClassifications.isSuitableForSme,
           tender.xClassifications.ojeuContractType,
           buyer.name,
           buyer.address.streetAddress,
           buyer.address.locality,
           buyer.address.postalCode, 
           buyer.contactPoint.name, 
           buyer.contactPoint.email)
  ) %>%
  # filter to find only those contracts which have already been awarded.
  filter(tag %in% c("award", "awardUpdate")) %>%
  # filter to keep only rows which contain the string pattern "analyic"
  filter_all(any_vars(grepl("analytic|Analytic", .)))


names(data)
table(data$tag)
table(data$tender.status)
table(data$tender.xClassifications.ojeuContractType)
table(data$buyer.address.locality)


# Write dataframe to a SQL database object with help from RSQLite and DBI packages 

install.packages("RSQLite")
install.packages("DBI")
library(DBI)
library(RSQLite)


con <- dbConnect(SQLite())
dbWriteTable(con, "data_clean", mtcars)
dbReadTable(con, "mtcars")


