#Data exploration
data_address <- "C:/Users/luzve/Documents/INSA 5eme annee/Visualisation des donnees/IEFIC_2018.csv"
data <- read.csv(data_address, header = TRUE, sep = ";", stringsAsFactors = FALSE)

# Clean data
# We'll use just the two first parts of the survey
start_col <- which(names(data) == "P2483")
data <- data[, 1:(start_col - 1)]
head(data)
summary(data)
dim(data)

# Rename some columns to make the code more legible
colnames(data)[colnames(data) == "P2439"] <- "home_owner"
colnames(data)[colnames(data) == "P2447"] <- "year_bought"
colnames(data)[colnames(data) == "P2461"] <- "min_sale_price"
colnames(data)[colnames(data) == "P2462"] <- "subsidy_used"
colnames(data)[colnames(data) == "P2464"] <- "subsidy_value"
colnames(data)[colnames(data) == "P2466"] <- "mortgage_used"
colnames(data)[colnames(data) == "P2168"] <- "mortgage_value"
colnames(data)[colnames(data) == "P2469"] <- "mortgage_terms_known"
colnames(data)[colnames(data) == "P2470"] <- "mortgage_active"
colnames(data)[colnames(data) == "P2478"] <- "monthly_expenses"
colnames(data)[colnames(data) == "P2478_1"] <- "food_expense"
colnames(data)[colnames(data) == "P2478_2"] <- "clothing_expense"
colnames(data)[colnames(data) == "P2478_3"] <- "water_expense"
colnames(data)[colnames(data) == "P2478_4"] <- "electricity_expense"
colnames(data)[colnames(data) == "P2478_5"] <- "gas_expense"
colnames(data)[colnames(data) == "P2478_6"] <- "cellphone_expense"
colnames(data)[colnames(data) == "P2478_7"] <- "domestic_service_expense"
colnames(data)[colnames(data) == "P2478_8"] <- "recreation_expense"
colnames(data)[colnames(data) == "P2478_9"] <- "health_expense"
colnames(data)[colnames(data) == "P2478_10"] <- "internet_expense"
colnames(data)[colnames(data) == "P2478_11"] <- "school_transport_expense"
colnames(data)[colnames(data) == "P2478_12"] <- "pension_expense"
colnames(data)[colnames(data) == "P2479"] <- "income_vs_expenses"

# First graphic
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Filtrar y agrupar los datos para obtener los conteos necesarios
data_summary <- data %>%
  filter(home_owner %in% c(1, 2) & income_vs_expenses %in% c(1, 2, 3)) %>%
  mutate(
    home_owner_label = ifelse(home_owner == 1, "Homeowner", "Non-homeowner"),
    income_vs_expenses_label = case_when(
      income_vs_expenses == 1 ~ "Expenses > Income",
      income_vs_expenses == 2 ~ "Expenses = Income",
      income_vs_expenses == 3 ~ "Expenses < Income"
    )
  ) %>%
  group_by(home_owner_label, income_vs_expenses_label) %>%
  summarise(count = n(), .groups = "drop")

# Crear la gráfica
ggplot(data_summary, aes(x = income_vs_expenses_label, y = count, fill = home_owner_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Income vs Expenses by Homeownership Status",
    x = "Income vs Expenses",
    y = "Number of Households",
    fill = "Homeownership"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


