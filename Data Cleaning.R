

#TODO: Confirm with Tom which fields to keep and which to remove.
# Cpv codes  (tender.items.classification.id, )
# Suitable for sme  (awards.suppliers.sme, tender.xClassifications.isSuitableForSme,)
# OJEU procedure type  (tender.xClassifications.ojeuContractType)
# Requesting by date range

#### Data Cleaning ----

response_clean <- function(data) {
  data_clean <- data %>%
    #select(-c(82:ncol(.))) %>%
    # select only variables we are interested in
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
    # (using the combination of grepl, and regex)
    filter_all(any_vars(grepl("analytic|Analytic", .)))
  return(data_clean)
}    
  
# Call response_clean() once for each response
data_clean <- response_clean(data)
data_clean2 <- response_clean(data_2)

# Call "anti_join()" to maintain only the contents of data_clean2 not overlapping with data_clean
# This will cover items found in the most recent run not contained in previous iterations
data_clean3 <- anti_join(data_clean2, data_clean)

# Preserve original and most recent responses in one final data frame.
data_final <- rbind(data_clean, data_clean3)


#### Database Export ----


# Write dataframe to a SQL database object with help from RSQLite and DBI packages 

# Install and Load RODBC package using pacman::p_load()
pacman::p_load(RODBC)

# Reference the location of the existing database - in the specified shared area
#C:\Users\JamesAbrams\Hartley McMaster Ltd\Reference - Files\Training
db <- "C:/Users/JamesAbrams/Hartley McMaster Ltd/Reference - Files/Training/Contract Finder.accdb"

# Establish the connection with the "odbcConnectAccess2007()" function
con <- RODBC::odbcConnectAccess2007(db)

# Save the new dataframe as a table in the sql database.
sqlSave(con, data_final)





