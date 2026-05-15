library(tidyverse)
library(readxl)
library(vroom)
library(janitor)

# Read and combine population data ---------------------------------------------
# List all detailed table paths
file_paths <- list.files("C:/Users/connachan.cara/OneDrive - IS/Research - Detailed Tables/",
                         full.names = TRUE) 
file_paths <- file_paths[file_paths != "C:/Users/connachan.cara/OneDrive - IS/Research - Detailed Tables/desktop.ini"]

# Function for reading and cleaning
read_and_clean <- function(path) {
  
  # Extract council name from file  path
  council_name <- path %>%
    str_extract(., ".+(?=\\s-\\sDetailed\\sTables\\.xlsx)") %>%
    str_replace(., "C:/Users/connachan.cara/OneDrive - IS/Research - Detailed Tables/", "")
  
  data <- read_xlsx(path, sheet = "Table 1") %>%
    row_to_names(row_number = 1) %>%
    # Add council name as column
    mutate("Council.Name" = council_name) %>%
    mutate("Area Type" = case_when(`Area Type` != "Council" ~ "Small Area",
                                   .default = "Council")) %>%
    mutate_at(c("All Ages", "90 and over"), as.numeric) %>%
    pivot_longer(cols = "All Ages":"90 and over", 
                 names_to = "Age",
                 values_to = "Population") %>%
    filter(Age != "All Ages") %>%
    mutate(Age = case_when(Age == "90 and over" ~ "90",
                           .default = Age)) %>%
    mutate(Age = as.numeric(Age)) %>%
    select("Council.Name", 
           "Level" = "Area Type",
           "Area.Name" = "Area Name",
           "Year" = "Year to 30 June",
           "Sex",
           "Age",
           "Population")
}

# Run function for all detailed tables
total_population_data <- file_paths %>% 
  # map applys the read and clean function to each of the files passed from the list
  map(., read_and_clean) %>% 
  # list_rbind appends the rows of the different files read in 
  list_rbind()

# Add Scotland level data 
population_data_with_scot <- total_population_data %>%
  filter(Level == "Council") %>%
  group_by(Year, Sex, Age) %>% 
  summarise(Population = sum(Population)) %>%
  mutate(Council.Name = "Scotland") %>%
  mutate(Level = "Scotland") %>%
  mutate(Area.Name = "Scotland") %>%
  select(Council.Name, Level, Area.Name, Year, Sex, Age, Population) %>%
  rbind(., total_population_data)

# Collate data for national summary doc -----------------------------------------

# Function for reading and cleaning
read_and_clean_national_summ <- function(path) {
  
  # Extract council name from file  path
  council_name <- path %>%
    str_extract(., ".+(?=\\s-\\sDetailed\\sTables\\.xlsx)") %>%
    str_replace(., "C:/Users/connachan.cara/OneDrive - IS/Research - Detailed Tables/", "")
  
  data <- read_xlsx(path, sheet = "Table 1") %>%
    row_to_names(row_number = 1) %>%
    rename("Small Area Name" = "Area Name") %>%
    # Add council name as column
    mutate("Area Name" = council_name) %>%
    mutate_at("All Ages", as.numeric) %>%
    filter(Sex == "Persons") %>%
    select("Area Name", 
           "Small Area Name", 
           "Area Type",
           "Year to 30 June",
           "All Ages")
  
}

# Run function for all detailed tables
national_summary_data <- file_paths %>% 
  # map applys the read and clean function to each of the files passed from the list
  map(., read_and_clean_national_summ) %>% 
  # list_rbind appends the rows of the different files read in 
  list_rbind()

# Add Scotland level data 
national_summary_data_with_scot <- national_summary_data %>%
  filter(`Area Type` == "Council") %>%
  group_by(`Year to 30 June`) %>% 
  summarise(`All Ages` = sum(`All Ages`)) %>%
  mutate(`Area Name` = "Scotland") %>%
  mutate(`Area Type` = "Scotland") %>%
  mutate(`Small Area Name` = "Scotland") %>%
  select("Area Name", 
         "Small Area Name", 
         "Area Type",
         "Year to 30 June",
         "All Ages") %>%
  rbind(., national_summary_data) %>%
  pivot_wider(names_from = `Year to 30 June`, values_from = `All Ages`)

# Read and combine other measures data -----------------------------------------

# Create vector of file paths
# Accessed from personal one drive but also saved in same file structure on sharepoint
file_paths_other_measures <- c(
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Aberdeen_MMW_2011_out/comp_Continuity_no special pop.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Aberdeenshire_2011_Place_out/comp_Continuity_SCAP_4.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Angus_MMW_2011_out/comp_Continuity SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Argyll_Bute_HMA_2011_out/comp_Continuity SCAP_Mig7.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/City_of_Edinburgh_MMW_2011_out/comp_Continuity_SCAP_no_special_pop_mig3.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Clackmannanshire_MMW_2011_out/comp_Continuity_SCAP_no_spec_pop.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Dumfries_Galloway_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Dundee_City_MMW_2011_out/comp_Continuity_SCAP_no_special_pop.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/East_Ayrshire_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/East_Dunbartonshire_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/East_Lothian_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/East_Renfrewshire_HMA_2011_out/comp_Continuity SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Falkirk_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Fife_MMW_2011_out/comp_Continuity_SCAP_Mig9.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Glasgow_City_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Highland_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Inverclyde_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Midlothian_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Moray_MMW_2011_out/comp_Continuity_SCAP_7.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Eilean_Siar_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/North_Ayrshire_CPP_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/North_Lanarkshire_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Orkney_Islands_MMW_2011_out/comp_Continuinty_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Perth_Kinross_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Renfrewshire_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Scottish_Borders_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Shetland_Islands_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/South_Ayrshire_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/South_Lanarkshire_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/Stirling_MMW_2011_out/comp_Continuity_SCAP_no_special_pop.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/West_Dunbartonshire_MMW_2011_out/comp_Continuity_SCAP.xls",
"C:/Users/connachan.cara/Documents/Population Projections 2026/Forecast/1. POPGROUP V4.1/West_Lothian_MMW_2011_out/comp_Continuity_SCAP.xls"
)

# Create vector of Council names for other measures data 
council_names <- unique(total_population_data$Council.Name)

# Function to read and clean the sheets in the other measures data
read_sheets <- function(sheets, path, council) {
  
  data <- read_xls(path = path,
                   sheet = sheets,
                   range = "A5:AB90",
                   na = "*") %>%
    mutate(Council.Name = council) %>%
    mutate(Area.Name = sheets) %>%
    filter(`...1` %in% c("Natural change", 
                         "Net migration", 
                         "Total",
                         "0-15 and 65+ / 16-64")) %>%
    pivot_longer(cols = 3:28, names_to = "Year", values_to = "Value") %>%
    select("Council.Name",
           "Area.Name", 
           "Year",
           "Measure" = "...1",
           "Value") %>%
    mutate(Year = gsub("[0-9]{2}-", "", Year)) %>%
    filter(Year %in% c(2022:2037)) %>%
    mutate(Measure = case_when(Measure == "Natural change" ~ "Natural Change",
                               Measure == "Net migration" ~ "Net Migration",
                               Measure == "Total" ~ "Total Population",
                               Measure == "0-15 and 65+ / 16-64" ~ "Dependency Ratio",
                               .default = Measure)) %>%
    # Converts dependency ratio value to % value
    mutate(Value = case_when(Measure == "Dependency Ratio" ~ Value * 100,
                             .default = Value)) %>%
    mutate(Value = round(Value, 0))
    
}

# Function for reading each sheet in the other measures data
read_files <- function(file_list, council_list) {
  
  sheet_names <- excel_sheets(path = file_list)
  sheet_names <- sheet_names[! sheet_names == "Notes"]
  
  # Runs the read sheets function and combines the sheets into 1 data frame
  data <- map(sheet_names, 
              read_sheets,
              path = file_list,
              council = council_list) %>%
    list_rbind()
  
}

# The read_files function needs to then be run within map2 to allow it
# to loop through multiple lists - list of councils and list of file paths
# list_rbind will then combine the results into a single data frame
measures_data <- map2(file_paths_other_measures,
                      council_names,
                      read_files) %>%
  list_rbind()

# Write data files -------------------------------------------------------------

# Need to use write_excel_csv to write apostrophe's correctly
write_excel_csv(population_data_with_scot, 
                "Data files/2022 SCAP - Population Projections With Aggregations.csv")

# Need to use write_excel_csv to write apostrophe's correctly
write_excel_csv(national_summary_data_with_scot,
                "Data files/2022 SCAP - National Summary Data.csv")

write.csv(measures_data, 
          "Data files/2022 SCAP - Other measures data.csv", 
          row.names = FALSE)
