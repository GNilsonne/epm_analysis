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

# Merge data
data_2019$year <- 2019
data_2020$year <- 2020
data_2021$year <- 2021
data_2022$year <- 2022

data_allyears <- rbind(data_2019, data_2020, data_2021, data_2022)
data_allyears <- data_allyears[, -9] # Drop names of main applicants

# Convert columns to dates and calculate processing times
data_allyears$inkom <- as.Date(data_allyears$`Inkom datum`)
data_allyears$beslut <- as.Date(data_allyears$`Slutligt beslut datum`)
data_allyears$handltid <- data_allyears$beslut - data_allyears$inkom

# Plot histograms
hist(as.numeric(data_allyears$handltid))
hist(as.numeric(data_allyears$handltid[data_allyears$Avgiftskategori == "Ändringsansökan, 2000"]))
hist(as.numeric(data_allyears$handltid[data_allyears$Avgiftskategori == "En forskningshuvudman, 5000"]))

### Note: about 50 applications have negative processing times. Must be misregistrations.
which(data_allyears$handltid < 0)

# Plot Kaplan-Meier curves
andr_2020_fit <- survfit(Surv(data_allyears$handltid[data_allyears$Avgiftskategori == "Ändringsansökan, 2000" & data_allyears$year == 2020]) ~1)
plot(andr_2020_fit, xlab="Days", main = '2020 Ändringsansökningar')

enhuvudm_2019_fit <- survfit(Surv(data_allyears$handltid[data_allyears$Avgiftskategori == "En forskningshuvudman, 5000" & data_allyears$year == 2019]) ~1)
enhuvudm_2020_fit <- survfit(Surv(data_allyears$handltid[data_allyears$Avgiftskategori == "En forskningshuvudman, 5000" & data_allyears$year == 2020]) ~1)
enhuvudm_2021_fit <- survfit(Surv(data_allyears$handltid[data_allyears$Avgiftskategori == "En forskningshuvudman, 5000" & data_allyears$year == 2021]) ~1)
enhuvudm_2022_fit <- survfit(Surv(data_allyears$handltid[data_allyears$Avgiftskategori == "En forskningshuvudman, 5000" & data_allyears$year == 2022]) ~1)

plot(1-enhuvudm_2019_fit$surv ~ enhuvudm_2019_fit$time, xlab= "Days", ylab = "proportion with decision", main = 'Cat A: Single applicant', type = "l", lwd = 2, lty = 2, frame.plot = F, xlim = c(0, 365), col = "lightgreen")
lines(1-enhuvudm_2020_fit$surv ~ enhuvudm_2020_fit$time, lwd = 2, lty = 3, col = "green")
lines(1-enhuvudm_2021_fit$surv ~ enhuvudm_2021_fit$time, lwd = 2, lty = 4, col = "forestgreen")
lines(1-enhuvudm_2022_fit$surv ~ enhuvudm_2022_fit$time, lwd = 2, lty = 1, col = "darkgreen")
legend("bottomright", lwd = 2, lty = c(2, 3, 4, 1), legend = c("2019, n = 1813", "2020, n = 1784", "2021, n = 1889", "2022 (Jan-Sep), n = 1053"), col = c("lightgreen", "green", "forestgreen", "darkgreen"))

# Calculate means for comparison to EPM annual reports
means_2020 <- aggregate(handltid ~ Avgiftskategori, data = data_2020, FUN = "mean")
medians_2020 <- aggregate(handltid ~ Avgiftskategori, data = data_2020, FUN = "median")


# Plot by month
# There must be a better way to do this
dayinyear <- as.numeric(data_2020$inkom) - 18262
hist(dayinyear, breaks = c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365))


# TODO: 
# - contact EPM to find out more about misregistrations with negative processing times and why the means are different in their annual report
# - analyse data for more application types
