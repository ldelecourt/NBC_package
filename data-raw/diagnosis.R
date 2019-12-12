## code to prepare `DATASET` dataset goes here

library(readxl)
colnam <- c("Temperature", "Nausea", "Lumbar", "Urine", "Micturition", "Burning",
            "Inflammation", "Nephritis")
diagnosis <- read_excel("~/Cours/SISE_M2/Data_Sets/diagnosis.xlsx", col_names=colnam)

diagnosis <- as.data.frame(diagnosis)

usethis::use_data(diagnosis)
