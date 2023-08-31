# Script to analyse data from regional animal ethics review boards 
# GN 2023-08-29

# Require packages

library(survival)

# Read data
data <- read.csv("C:/Users/gusta/Box Sync/Gustavs_arbete/Arkiverat/ethics_data/animal_ethics_coding_sheet_2023-08-29.csv")

# Clean data

# Convert columns to dates and calculate processing times
data$date_received_2 <- as.Date(data$date_received)
data$date_decided_2 <- as.Date(data$date_decided)
data$date_informed_2 <- as.Date(data$date_informed)

data$time_to_decision <- data$date_decided_2 - data$date_received_2
data$time_to_notification <- data$date_informed_2 - data$date_received_2

# Plot histograms
hist(as.numeric(data$time_to_decision))
#hist(as.numeric(data_allyears$handltid[data_allyears$Avgiftskategori == "Ändringsansökan, 2000"]))
#hist(as.numeric(data_allyears$handltid[data_allyears$Avgiftskategori == "En forskningshuvudman, 5000"]))

# Plot Kaplan-Meier curves
fit1 <- survfit(Surv(data$time_to_decision) ~1)
plot(fit1, xlab="Days", main = 'All')

fit_gothenburg_2018 <- survfit(Surv(data$time_to_decision[data$committee == "Gothenburg"]) ~1)
fit_linkoping_2018 <- survfit(Surv(data$time_to_decision[data$committee == "Linköping"]) ~1)

fit_gothenburg_2018_notified <- survfit(Surv(data$time_to_notification[data$committee == "Gothenburg"]) ~1)
fit_linkoping_2018_notified <- survfit(Surv(data$time_to_notification[data$committee == "Linköping"]) ~1)


plot(1-fit_gothenburg_2018$surv ~ fit_gothenburg_2018$time, xlab= "Days", ylab = "proportion complete", main = 'Gothenburg 2018', type = "l", lwd = 2, lty = 1, 
     frame.plot = F, xlim = c(0, 365), col = "lightgreen", xaxt = "n")
axis(1, at= c(0, 40, 100, 200, 300, 365), labels= c(0, 40, 100, 200, 300, 365))
abline(v = 40, lty = 2)
lines(1-fit_gothenburg_2018_notified$surv ~ fit_gothenburg_2018_notified$time, lwd = 2, lty = 1, col = "darkgreen")
legend("bottomright", lwd = 2, lty = 1, legend = c("decided", "notified"), col = c("lightgreen", "darkgreen"))

plot(1-fit_linkoping_2018$surv ~ fit_linkoping_2018$time, xlab= "Days", ylab = "proportion complete", main = 'Linköping 2018', type = "l", lwd = 2, lty = 1, 
     frame.plot = F, xlim = c(0, 365), col = "lightgreen", xaxt = "n")
axis(1, at= c(0, 40, 100, 200, 300, 365), labels= c(0, 40, 100, 200, 300, 365))
abline(v = 40, lty = 2)
lines(1-fit_linkoping_2018_notified$surv ~ fit_linkoping_2018_notified$time, lwd = 2, lty = 1, col = "darkgreen")
legend("bottomright", lwd = 2, lty = c(1, 0), legend = c("decided", ""), col = c("lightgreen"))


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

