library(data.table)
library(ggplot2)
library(rdrobust)
library(modelsummary)
library(gt)
install.packages("rdrobust")

####Statistics pre-post August 25, 2022

##crimes against females
data <- read.csv("Data/data.csv")
setDT(data)


filtered <- data 
cutoff_date <- "25-AUG-2022"
cutoff_date <- as.IDate("25-AUG-2022", format = "%d-%b-%Y")
roe_date <- "24-JUN-2022"
roe_date <- as.IDate("24-JUN-2022", format = "%d-%b-%Y")
filtered[, INCDATE := as.IDate(INCDATE, format = "%d-%b-%Y")]
judgment_date <- "28-JUL-2022"
judgment_date <- as.IDate("28-JUL-2022", format = "%d-%b-%Y")

filtered[, female_victim := fifelse(
  victim_sex_1 == 0 | victim_sex_2 == 0 | victim_sex_3 == 0, 1, 0)]
num_fem_vic <- filtered[female_victim == 1, .N, by = INCDATE]

##Graph for female victims

events <- data.frame(
  event_date = as.Date(c("2022-06-24", "2022-07-28", "2022-08-25")),
  event_label = c("Roe v. Wade", "Formal Judgment", "Ban")
)

plot_fem_vic <- ggplot(num_fem_vic, aes(x = INCDATE, y = N)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  geom_vline(data = events, aes(xintercept = event_date, color = event_label),
             linetype = "dashed", show.legend = TRUE) +

  scale_color_manual(
    name = "Key Events",
    values = c(
      "Roe v. Wade" = "darkgreen",
      "Ban" = "red",
      "Formal Judgment" = "purple"
    )
  ) +
  labs(
    title = "Number of Incidents with Female Victims by Date",
    x = "Date",
    y = "Number of Incidents"
  ) +
  theme_minimal()


ggsave("Figures/female_victim_scatterplot.png", plot = plot_fem_vic,
       width = 8, height = 5, dpi = 300)

###Crimes committed by female offenders

filtered[, female_offender := fifelse(
  offender_sex_1 == 0 | offender_sex_2 == 0 | offender_sex_3 == 0, 1, 0)]

num_fem_off <- filtered[female_offender == 1, .N, by = INCDATE]

##Graph for female offender

plot_fem_off <- ggplot(num_fem_off, aes(x = INCDATE, y = N)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  geom_vline(data = events, aes(xintercept = event_date, color = event_label),
             linetype = "dashed", show.legend = TRUE) +

  scale_color_manual(
    name = "Key Events",
    values = c(
      "Roe v. Wade" = "darkgreen",
      "Ban" = "red",
      "Formal Judgment" = "purple"
    )
  ) +
  labs(
    title = "Number of Incidents with Female Offenders by Date",
    x = "Date",
    y = "Number of Incidents"
  ) +
  theme_minimal()

plot_fem_off

ggsave("Figures/female_offender_scatterplot.png", plot = plot_fem_off,
       width = 8, height = 5, dpi = 300)



###Domestic violence homiside comitted against women

filtered[, dom_homicide := fifelse(
  homicide_circum_1 == 6 | homicide_circum_1 == 6 | homicide_circum_1 == 6, 1, 0)]

num_dom_violence <- filtered[female_victim == 1 & dom_homicide == 1, .N, by = INCDATE]

##Graph for domestic violence homicides against women 

plot_fem_dom_violence <- ggplot(num_dom_violence, aes(x = INCDATE, y = N)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  geom_vline(data = events, aes(xintercept = event_date, color = event_label),
             linetype = "dashed", show.legend = TRUE) +

  scale_color_manual(
    name = "Key Events",
    values = c(
      "Roe v. Wade" = "darkgreen",
      "Ban" = "red",
      "Formal Judgment" = "purple"
    )
  ) +
  labs(title = "Number of Domestic Violence Homicides with Female Victims by Date",
       x = "Date",
       y = "Number of Incidents") +
  theme_minimal()

plot_fem_dom_violence

ggsave("Figures/plot_fem_dom_violence.png", plot = plot_fem_dom_violence,
       width = 8, height = 5, dpi = 300)



### Total crimes

num_total_crimes <- filtered[, .N, by = INCDATE]

##Graph for total crimes

plot_total_crimes <- ggplot(num_total_crimes, aes(x = INCDATE, y = N)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  geom_vline(data = events, aes(xintercept = event_date, color = event_label),
             linetype = "dashed", show.legend = TRUE) +

  scale_color_manual(
    name = "Key Events",
    values = c(
      "Roe v. Wade" = "darkgreen",
      "Ban" = "red",
      "Formal Judgment" = "purple"
    )
  ) +
  labs(title = "Total number of incidents by Date",
       x = "Date",
       y = "Number of Incidents") +
  theme_minimal()

plot_total_crimes

ggsave("Figures/plot_total_crimes.png", plot = plot_total_crimes,
       width = 8, height = 5, dpi = 300)





#####Regressions####

###1 month bandwidth 


##Victims
num_fem_vic[, INCDATE := as.Date(INCDATE)]
cutoff_date <- as.Date("2022-08-25")

num_fem_vic[, running_var := as.numeric(INCDATE - cutoff_date)]
num_fem_vic[, treat := as.integer(INCDATE >= cutoff_date)]
rdd_data <- num_fem_vic[abs(running_var) <= 30 & !is.na(N)]
Female_Victims <- lm(N ~ treat + running_var, data = rdd_data)

##Offenders

num_fem_off[, INCDATE := as.Date(INCDATE)]
cutoff_date <- as.Date("2022-08-25")

num_fem_off[, running_var := as.numeric(INCDATE - cutoff_date)]
num_fem_off[, treat := as.integer(INCDATE >= cutoff_date)]
rdd_data <- num_fem_off[abs(running_var) <= 30 & !is.na(N)]
Female_Offenders <- lm(N ~ treat + running_var, data = rdd_data)

##Domestic violence

num_dom_violence[, INCDATE := as.Date(INCDATE)]
cutoff_date <- as.Date("2022-08-25")

num_dom_violence[, running_var := as.numeric(INCDATE - cutoff_date)]
num_dom_violence[, treat := as.integer(INCDATE >= cutoff_date)]
rdd_data <- num_dom_violence[abs(running_var) <= 30 & !is.na(N)]
Domestic_Violence <- lm(N ~ treat + running_var, data = rdd_data)


##Total crimes

num_total_crimes[, INCDATE := as.Date(INCDATE)]
cutoff_date <- as.Date("2022-08-25")

num_total_crimes[, running_var := as.numeric(INCDATE - cutoff_date)]
num_total_crimes[, treat := as.integer(INCDATE >= cutoff_date)]

rdd_data <- num_total_crimes[abs(running_var) <= 30 & !is.na(N)]
Total_Crimes <- lm(N ~ treat + running_var, data = rdd_data)
 

model_list <- list("Female Victims" = Female_Victims, "Female Offenders" = Female_Offenders,  "Domestic Homicides" = Domestic_Violence,  "Total Crimes" = Total_Crimes)

modelsummary(model_list, stars = TRUE, title = "Regression Results: Household Income Determinants")

#Save

modelsummary(model_list, output = "Tables/model_summary.tex")
table_gt <- modelsummary(model_list, output = "gt")
gtsave(table_gt, "Tables/model_summary.pdf")


###2,5 months bandwidth 

##Victims
num_fem_vic[, INCDATE := as.Date(INCDATE)]
cutoff_date <- as.Date("2022-08-25")

num_fem_vic[, running_var := as.numeric(INCDATE - cutoff_date)]
num_fem_vic[, treat := as.integer(INCDATE >= cutoff_date)]
rdd_data <- num_fem_vic[abs(running_var) <= 75 & !is.na(N)]
Female_Victims <- lm(N ~ treat + running_var, data = rdd_data)

##Offenders

num_fem_off[, INCDATE := as.Date(INCDATE)]
cutoff_date <- as.Date("2022-08-25")

num_fem_off[, running_var := as.numeric(INCDATE - cutoff_date)]
num_fem_off[, treat := as.integer(INCDATE >= cutoff_date)]
rdd_data <- num_fem_off[abs(running_var) <= 75 & !is.na(N)]
Female_Offenders <- lm(N ~ treat + running_var, data = rdd_data)

##Domestic violence

num_dom_violence[, INCDATE := as.Date(INCDATE)]
cutoff_date <- as.Date("2022-08-25")

num_dom_violence[, running_var := as.numeric(INCDATE - cutoff_date)]
num_dom_violence[, treat := as.integer(INCDATE >= cutoff_date)]
rdd_data <- num_dom_violence[abs(running_var) <= 75 & !is.na(N)]
Domestic_Violence <- lm(N ~ treat + running_var, data = rdd_data)


##Total crimes

num_total_crimes[, INCDATE := as.Date(INCDATE)]
cutoff_date <- as.Date("2022-08-25")

num_total_crimes[, running_var := as.numeric(INCDATE - cutoff_date)]
num_total_crimes[, treat := as.integer(INCDATE >= cutoff_date)]

rdd_data <- num_total_crimes[abs(running_var) <= 75 & !is.na(N)]
Total_Crimes <- lm(N ~ treat + running_var, data = rdd_data)
 

model_list <- list("Female Victims" = Female_Victims, "Female Offenders" = Female_Offenders,  "Domestic Homicides" = Domestic_Violence,  "Total Crimes" = Total_Crimes)

modelsummary(model_list, stars = TRUE, title = "Regression Results: Household Income Determinants")


#Save

modelsummary(model_list, output = "Tables/model_summary_2.tex")
table_gt <- modelsummary(model_list, output = "gt")
gtsave(table_gt, "Tables/model_summary_2.pdf")


