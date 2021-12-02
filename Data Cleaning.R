

source("Contract Finder API.R")

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

# Cleaning the environment
rm(data_clean, data_clean2, data_clean3)
# rm(data_final_new, data_final_new_2, data_final_new_3)

#### Exploring the Data ----

df <- data_final
head(df)
dim(df)
summary(df)


# Ensure number format is applied correctly
typeof(df$awards.value.amount)

df[, c("awards.value.amount", "tender.items.classification.id",
       "tender.minValue.amount", "tender.value.amount")] <- 
  sapply(df[, c("awards.value.amount", "tender.items.classification.id",
                "tender.minValue.amount", "tender.value.amount")], as.numeric)

# TODO: Fix date format columns.

typeof(df$date)

df <- df %>%
  # Fixing date formats 
  mutate(time = substr(date , start = 12 , stop = 19)) %>%
  mutate(date = as.Date(substr(date , start = 1 , stop = 10))) %>%
  mutate(awards.date = as.Date(substr(awards.date , start = 1, stop = 10))) %>%
  mutate(awards.contractPeriod.startDate = as.Date(substr(awards.contractPeriod.startDate, start = 1 , stop = 10))) %>%
  mutate(awards.contractPeriod.endDate   = as.Date(substr(awards.contractPeriod.endDate, start = 1 , stop = 10))) %>%
  mutate(tender.milestones.dueDate1 = as.Date(substr(tender.milestones.dueDate1, start = 1 , stop = 10))) %>%
  mutate(tender.milestones.dueDate2 = as.Date(substr(tender.milestones.dueDate2, start = 1 , stop = 10))) %>%
  mutate(tender.tenderPeriod.endDate = as.Date(substr(tender.tenderPeriod.endDate, start = 1 , stop = 10))) %>%
  # Specifying order so that date and time appear side by side
  select(date, time, everything(.))




library(openxlsx)
# Excluding decimal readings in our formatting
options("openxlsx.numFmt" = "0") # 2 decimal cases formating
openxlsx::write.xlsx(df, 
                     #file = "C:/Users/JamesAbrams/Desktop/Contract Finder API.xlsx", 
                     file = "Contract Finder API.xlsx",
                     asTable = TRUE, 
                     sheetName = "Test")

# Emulating pivot tables in R with dplyr::summarize() and dplyr::group_by()

df_test <- df %>%
  group_by(buyer.name, awards.suppliers.name) %>%
  summarise(sum = sum(awards.value.amount),
            awards.value.amount = n()) %>%
  arrange(desc(sum))

