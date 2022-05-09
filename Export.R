

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




# Load openxlsx package
pacman::p_load(openxlsx)

# Excluding decimal readings in our formatting
options("openxlsx.numFmt" = "0") # 2 decimal cases formating
openxlsx::write.xlsx(df, 
                     #file = "C:/Users/JamesAbrams/Desktop/Contract Finder API.xlsx", 
                     file = "Contract Finder API.xlsx",
                     asTable = TRUE, 
                     sheetName = "Test")