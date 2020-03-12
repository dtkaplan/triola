# Read the Triola data and save them in the data/ folder

library(readr)
library(dplyr)

#----------------------#

TDS01_Body_Data <- read_delim("inst/raw-data/01 - Body Data.txt",
                            "\t", escape_double = FALSE, trim_ws = TRUE)
names(TDS01_Body_Data)[c(2,14)] <- c("GENDER",  "ARM_CIRC")
TDS01_Body_Data$GENDER <- ifelse(TDS01_Body_Data$GENDER == 1, "male", "female")

save(TDS01_Body_Data, file = "data/TDS01_Body_Data.rda")

#----------------------#

TDS02_Foot_and_Height <- read_delim("inst/raw-data/02 - Foot and Height.txt",
                             "\t", escape_double = FALSE, trim_ws = TRUE)
names(TDS02_Foot_and_Height)[3:5] <- c("FOOT_LENGTH", "SHOE_PRINT", "SHOE_SIZE")
save(TDS02_Foot_and_Height, file =  "data/TDS02_Foot_and_Height.rda")

#----------------------#

TDS03_Body_Temperatures <- read_delim("inst/raw-data/03 - Body Temperatures.txt",
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
names(TDS03_Body_Temperatures)[3:6] <- c("DAY_1_8AM", "DAY_1_12AM", "DAY_2_8AM", "DAY_2_12AM" )
save(TDS03_Body_Temperatures, file = "data/TDS03_Body_Temperatures.rda")

#----------------------#

TDS04_Births <- read_delim("inst/raw-data/04 - Births.txt",
                                     "\t", escape_double = FALSE, trim_ws = TRUE)
names(TDS04_Births)[c(3,  4, 7, 8)] <- c("GENDER", "LENGTH_OF_STAY", "BIRTH_WEIGHT", "TOTAL_CHARGES")
TDS04_Births$GENDER <- ifelse(TDS04_Births$GENDER == 1, "male", "female")
save(TDS04_Births, file = "data/TDS04_Births.rda")

#-----------------------#

TDS05_Family_Heights <- read_delim("inst/raw-data/05 - Family Heights.txt",
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
names(TDS05_Family_Heights) <- c("father", "mother", "son1", "daughter1")
save(TDS05_Family_Heights, file = "data/TDS05_Family_Heights.rda")

#-----------------------#

TDS06_Freshman_15 <- read_delim("inst/raw-data/06 - Freshman 15.txt",
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
names(TDS06_Freshman_15) <- c("sex", "wt_sept", "wt_april",
                                "bmi_sept", "bmi_april")
save(TDS06_Freshman_15, file = "data/TDS06_Freshman_15.rda")

#-----------------------#

TDS07_IQ_and_lead <- read_delim("inst/raw-data/07 - IQ and Lead.txt",
                               "\t", escape_double = FALSE, trim_ws = TRUE)
names(TDS07_IQ_and_lead) <- c("lead", "age", "sex", "year1", "year2",
                             "IQ_verb", "IQ_perf", "IQ_full")
save(TDS07_IQ_and_lead, file = "data/TDS07_IQ_and_lead.rda")

#-----------------------#

TDS08_IQ_and_Brain_Size <- read_delim("inst/raw-data/08 - IQ and Brain Size.txt",
                                     "\t", escape_double = FALSE, trim_ws = TRUE)
names(TDS08_IQ_and_Brain_Size) <- tolower(names(TDS08_IQ_and_Brain_Size))
names(TDS08_IQ_and_Brain_Size)[2] <- "sex"
TDS08_IQ_and_Brain_Size$sex <- save(TDS08_IQ_and_Brain_Size, file = "data/TDS08_IQ_and_Brain_Size.rda")

#-----------------------#

# Read the raw data, apply fixes in the form of a
# purpose-written function, and save the result in data/

create_df <- function(root_name, fix_fun = I) {
  Tmp <-
    read.delim(paste0("inst/raw-data/",root_name, ".txt")) %>%
    fix_fun(.)

  df_name <- gsub("^([0-9]{2}) - ", "TDS\\1_", root_name) %>%
    gsub(" ", "_", .) %>%
    gsub("&", "and", .)

  assign(df_name, Tmp, envir = .GlobalEnv)
  save(list = df_name, file = paste0("data/", df_name, ".rda"))
}

fix_general <- function(data) {
  names(data) <-
    tolower(names(data)) %>%
    gsub("\\.$", "", .) %>%
    gsub("\\.{2,}", "_", .)

  data
}

#--------
bear_fix <- function(data) {
  names(data) <- tolower(names(data))
  names(data)[3] <- "sex"
  data$sex <- ifelse(data$sex==1, "M", "F")
  data
}
create_df("09 - Bear Measurements", bear_fix)

#-------
create_df("10 - Manatee Deaths", fix_general)

#-------

create_df("11 - Alcohol and Tobacco in Movies", fix_general)

#-------
create_df("12 - Passive and Active Smoke", fix_general)

#-------
create_df("13 - Cigarette Contents", fix_general)

#-------
create_df("14 - Oscar Winner Age", fix_general)

#-------
create_df("15 - Presidents", fix_general)

#-------
create_df("16 - Nobel Laureates and Chocolate", fix_general)

#-------
create_df("17 - Course Evaluations", fix_general)

#-------
create_df("18 - Speed Dating", fix_general)

#-------
create_df("19 - Car Crash Tests", fix_general)

#-------
create_df("20 - Car Measurements", fix_general)

#-------
create_df("21 - Earthquakes", fix_general)

#-------
create_df("22 - Tornadoes", fix_general)

#-------
create_df("23 - Old Faithful", fix_general)

#-------
create_df("24 - Word Counts", fix_general)

#-------
create_df("25 - Fast Food", fix_general)
TDS25_narrow <- TDS25_Fast_Food %>%
  tidyr::gather(., "where_when", "wait") %>%
  mutate(where_when = gsub("\\.([^\\.]+)$", "_\\1", where_when)) %>%
  tidyr::separate(., where_when, into=c("chain", "meal"), sep = "_",
                  remove = FALSE)
save(TDS25_narrow, file = "data/TDS25_narrow.rda")
#-------
create_df("26 - Cola Weights and Volumes", fix_general)
TDS26_narrow <- TDS26_Cola_Weights_and_Volumes %>%
  mutate(id = row_number()) %>%
  tidyr::gather(., "what", "amount", - id) %>%
  mutate(what = gsub("\\.([^\\.]+)$", "_\\1", what)) %>%
  tidyr::separate(., what, into=c("type", "quantity"), sep = "_",
                  remove = TRUE) %>%
  tidyr::separate(., type, into=c("brand", "diet"), sep = "\\.",
                  remove = FALSE) %>%
  tidyr::spread(key = quantity, value = amount) %>%
  select(- id)
save(TDS26_narrow, file = "data/TDS26_narrow.rda")
#-------
create_df("27 - M&M Weights", fix_general)
TDS27_narrow <- TDS27_MandM_Weights %>%
  tidyr::gather(key = color, value = weight, na.rm = TRUE)
save(TDS27_narrow, file = "data/TDS27_narrow.rda")
#-------
create_df("28 - Chocolate Chip Cookies", fix_general)
TDS28_narrow <- TDS28_Chocolate_Chip_Cookies %>%
  tidyr::gather(key = brand, value = nchips, na.rm = TRUE)
save(TDS28_narrow, file = "data/TDS28_narrow.rda")
#-------
create_df("29 - Coin Weights")
Convert <- tibble::tribble(
  ~ amount, ~ value,
  "PENNIES", "penny",
  "QUARTERS", "quarter",
  "COINS", "dollar",
)
TDS29_narrow <- TDS29_Coin_Weights %>%
  tidyr::gather(., "what", "weight", na.rm = TRUE) %>%
  mutate(what = gsub("\\.([^\\.]+)$", "_\\1", what)) %>%
  tidyr::separate(., what, into=c("coin", "amount"), sep = "_",
                  remove = FALSE) %>%
  left_join(Convert) %>%
  select(-amount)
save(TDS29_narrow, file = "data/TDS29_narrow.rda")

#-------
create_df("30 - Aluminum Cans")
TDS30_narrow <- TDS30_Aluminum_Cans %>%
  tidyr::gather(key = thickness, value = max_load, na.rm = TRUE) %>%
  mutate(thickness = as.numeric(gsub("CANS", "", thickness))/10)
save(TDS30_narrow, file = "data/TDS30_narrow.rda")
#-------
create_df("31 - Garbage Weight", fix_general)

#-------
create_df("32 - Airport Data Speeds", fix_general)
TDS32_narrow <- TDS32_Airport_Data_Speeds %>%
  tidyr::gather(key = carrier, value = mbps, - airport.code) %>%
  mutate(carrier = gsub("at\\.t", "att", carrier))
save(TDS32_narrow, file = "data/TDS32_narrow.rda")

#-------
create_df("13 - Draft Lottery")

#-------
create_df("14 - Aircraft Altimeter Errors")

#-------
create_df("14 - Energy Consumption")

#-------
create_df("14 - Weights of Minted Quarters")


