library(ggplot2)
# for pivot/reshape data
library(tidyr)
library(dplyr)
# for multiple plots in one figure (ggarrange)
library(ggpubr)
theme_set(theme_pubr())

# COVID data for proportion by race
COVIDrace_data <- read.csv("~/schoolwork/DataVisualization/FinalProject/COVID_CasesDeathsRatios_byRaceState.csv")

# Household income by race
incomeData <- read.csv("~/schoolwork/DataVisualization/FinalProject/HouseIncomebyRace2019.csv")

COVIDrace_data$State

# Need to pivot table/reshape data
reshape_cases <- COVIDrace_data %>%
  select(case_White, case_Black, case_Asian, case_Hispanic, case_Indian) %>%
  pivot_longer(., cols = c(case_White, case_Black, case_Asian, case_Hispanic, case_Indian),
               names_to="Var", values_to='Val')


reshape_deaths <- COVIDrace_data %>%
  select(death_White, death_Black, death_Asian, death_Hispanic, death_Indian) %>%
  drop_na() %>%
  
  #drop outliers for American Indians to bring same scale with other ethnicities
  filter(between(death_Indian,
                 quantile(death_Indian, 0.25) - 1.5*IQR(death_Indian),
                 quantile(death_Indian, 0.75) + 0.75*IQR(death_Indian))) %>%
  
  pivot_longer(., cols = c(death_White, death_Black, death_Asian, death_Hispanic, death_Indian),
               names_to="Var", values_to='Val')


# plot ratio of cases
cases_plot <- ggplot(reshape_cases, aes(x=Var, y=Val, fill=Var)) +
  geom_boxplot() +
  labs(x='Race',
       y='Proportion',
       title='COVID19 Cases/Population of Race (in US)') +
  theme(axis.title.x = element_text(colour = "red"), 
        axis.title.y = element_text(colour = "red"),
        axis.text=element_text(color="blue"))

#to make smaller fond
theme_set(theme_minimal()) 

# Plot ratio of death
deaths_plot <- ggplot(reshape_deaths, aes(x=Var, y=Val, fill=Var)) +
  geom_boxplot() +
  labs(x='Race',
       y='Proportion',
       title='COVID19 Deaths/Population of Race (in US)') +
  theme(axis.title.x = element_text(colour = "red"), 
        axis.title.y = element_text(colour = "red"),
        axis.text=element_text(color="blue"))


# plot household income by race
income_plot <- ggplot(incomeData, aes(x=Race, y=Household_Income, fill=Race)) +
  geom_col() +
  labs(title="Household Income by Race in US 2019") +
  theme(axis.title.x = element_text(colour = "red"), 
        axis.title.y = element_text(colour = "red"),
        axis.text=element_text(color="blue"))


# combine two plot in one figure
figure <- ggarrange(cases_plot, deaths_plot, income_plot,
                    labels = c("A", "B", "C"),
                    ncol = 2, nrow = 2,
                    legend="none")
figure


# Save visualization to a file
ggsave("~/schoolwork/DataVisualization/FinalProject/COVID19byRace.png", width=10, height=8)


