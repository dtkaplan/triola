# Read the Triola data and save them in the data/ folder

library(readr)

#----------------------#

ch01_Body_Data <- read_delim("inst/raw-data/01 - Body Data.txt",
                            "\t", escape_double = FALSE, trim_ws = TRUE)
names(ch01_Body_Data)[c(2,14)] <- c("GENDER",  "ARM_CIRC")
ch01_Body_Data$GENDER <- ifelse(ch01_Body_Data$GENDER == 1, "male", "female")

save(ch01_Body_Data, file = "data/ch01_Body_Data.rda")

#----------------------#

ch02_Foot_and_Height <- read_delim("inst/raw-data/02 - Foot and Height.txt",
                             "\t", escape_double = FALSE, trim_ws = TRUE)
names(ch02_Foot_and_Height)[3:5] <- c("FOOT_LENGTH", "SHOE_PRINT", "SHOE_SIZE")
save(ch02_Foot_and_Height, file =  "data/ch02_Foot_and_Height.rda")

#----------------------#

ch03_Body_Temperatures <- read_delim("inst/raw-data/03 - Body Temperatures.txt",
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
names(ch03_Body_Temperatures)[3:6] <- c("DAY_1_8AM", "DAY_1_12AM", "DAY_2_8AM", "DAY_2_12AM" )
save(ch03_Body_Temperatures, file = "data/ch03_Body_Temperatures.rda")

#----------------------#

ch04_Births <- read_delim("inst/raw-data/04 - Births.txt",
                                     "\t", escape_double = FALSE, trim_ws = TRUE)
names(ch04_Births)[c(3,  4, 7, 8)] <- c("GENDER", "LENGTH_OF_STAY", "BIRTH_WEIGHT", "TOTAL_CHARGES")
ch04_Births$GENDER <- ifelse(ch04_Births$GENDER == 1, "male", "female")
save(ch04_Births, file = "data/ch04_Births.rda")
