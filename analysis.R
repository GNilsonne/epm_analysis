# Script to analyse data from EPM 
# GN 2023-02-20

# Require packages
library(readxl)
library(survival)

# Read data
data_2019 <- read_excel("C:/Users/gusta/Downloads/Arsrapport_aranden_2019_2022-09-22[63].xlsx")
data_2020 <- read_excel("C:/Users/gusta/Downloads/Arsrapport_aranden_2020_2022-09-22.xlsx")
data_2021 <- read_excel("C:/Users/gusta/Downloads/Arsrapport_aranden_2021_2022-09-22.xlsx")
data_2022 <- read_excel("C:/Users/gusta/Downloads/Arsrapport_aranden_2022_2022-09-22.xlsx")

# Clean data
data_2020 <- data_2020[-1787, ] # Misregistered application

# Convert columns to dates and calculate processing times
data_2020$inkom <- as.Date(data_2020$`Inkom datum`)
data_2020$beslut <- as.Date(data_2020$`Slutligt beslut datum`)
data_2020$handltid <- data_2020$beslut - data_2020$inkom

# Plot histograms
hist(as.numeric(data_2020$handltid))
hist(as.numeric(data_2020$handltid[data_2020$Avgiftskategori == "Ändringsansökan, 2000"]))
hist(as.numeric(data_2020$handltid[data_2020$Avgiftskategori == "En forskningshuvudman, 5000"]))

# Plot Kaplan-Meier curves
andr_2020_fit <- survfit(Surv(data_2020$handltid[data_2020$Avgiftskategori == "Ändringsansökan, 2000"]) ~1)
plot(andr_2020_fit, xlab="Days", main = '2020 Ändringsansökningar')

enhuvudm_2020_fit <- survfit(Surv(data_2020$handltid[data_2020$Avgiftskategori == "En forskningshuvudman, 5000"]) ~1)
plot(enhuvudm_2020_fit, xlab="Days", main = '2020 En forskningshuvudman')
abline(h = 0.5)

# Plot by month
# There must be a better way to do this
dayinyear <- as.numeric(data_2020$inkom) - 18262
hist(dayinyear, breaks = c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365))


# TODO: 
# - clean data, remove names, sync datafile and replace file location strings
# - analyse data for more years and application types
