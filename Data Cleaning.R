

source("Contract Finder API.R")

#TODO: Confirm with Tom which fields to keep and which to remove.
# Cpv codes  (tender.items.classification.id, )
# Suitable for sme  (awards.suppliers.sme, tender.xClassifications.isSuitableForSme,)
# OJEU procedure type  (tender.xClassifications.ojeuContractType)
# Requesting by date range

#### Data Cleaning ----

# Primary purpose of function - to reduce the number of variables stored in our database.
# Original entry has over 100 fields, most of which are redundant for our purposes.
#- data argument: which response to clean
#- string argument: filters to be applied
response_clean <- function(data, string = "analytic|Analytic") {
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
    #filter(grepl(string, awards.suppliers.name))
    filter_all(any_vars(grepl(string, .)))
    #filter_all(any_vars(grepl("analytic|Analytic", .)))
  return(data_clean)
}    
  


# Call response_clean() once for each response
# Use "string" argument to specify clien we are interested in. Note this is case sensitive (for now)
data_clean_1 <- response_clean(data_1, "analytic|Analytic")
data_clean_2 <- response_clean(data_2, "analytic|Analytic")
data_clean_3 <- response_clean(data_3, "analytic|Analytic")

# Call "anti_join()" to maintain only the contents of data_clean2 not overlapping with data_clean
# This will cover items found in the most recent run not contained in previous iterations
data_clean_4     <- anti_join(data_clean_2, data_clean_1)
data_clean_final <- anti_join(data_clean_3, data_clean_4)

# Preserve original and most recent responses in one final data frame.
data_final <- rbind(data_clean_4, data_clean_final)


# Cleaning the environment
rm(data_clean_1, data_clean_2, data_clean_3, data_clean_4, data_clean_final)
# rm(data_final_new, data_final_new_2, data_final_new_3)


#### Exploring the Data ----

# Investigating Head, dimensions, summary for brief overview of our database
df <- data_final
head(df)
dim(df)
summary(df)


# Ensure number format is applied correctly (before exporting to excel)
# Numbers are stored as text by default
typeof(df$awards.value.amount)

df[, c("awards.value.amount", "tender.items.classification.id",
       "tender.minValue.amount", "tender.value.amount")] <- 
  sapply(df[, c("awards.value.amount", "tender.items.classification.id",
                "tender.minValue.amount", "tender.value.amount")], as.numeric)

# Ensure date format is applied correctly (before exporting to excel)
# Dates are stored as text by default
typeof(df$date)

df <- df %>%
  # Fixing date formats using substr() to extract only the first 10 characters of the date each time
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


#### Competitor Intel ----

# Emulating pivot tables in R with dplyr::summarize() and dplyr::group_by()
df_test <- df %>%
  group_by(buyer.name, awards.suppliers.name) %>%
  summarise(sum = sum(awards.value.amount),
            awards.value.amount = n()) %>%
  arrange(desc(sum))


#### Percentile & Clientele ----

# This is included to deliver an estimation of scale of competitors.
# Using df_test as a starting point

# Return the 10%, 50% and 90% percentile
percentile <- as.data.frame(quantile(df_test$sum, probs = c(0.1, 0.5, 0.9)))
names(percentile) <- "contract_value"
percentile <- t(percentile)

# Return info on the suppliers top public sector client found in the database.
client <- df_test %>%
  mutate(`temp` = 1) %>%
  group_by(temp) %>%
  mutate(total = sum(sum)) %>%
  ungroup() %>%
  mutate(percentage = paste0(as.character(round(sum / total * 100, 1)), "%")) %>%
  select(buyer.name, percentage) %>%
  head(5) %>%
  rename("Top 5 Clients" = "buyer.name")



