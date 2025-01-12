#Data exploration
data_address <- "C:/Users/luzve/Documents/INSA 5eme annee/Visualisation des donnees/R-report/IEFIC_2018.csv"
data <- read.csv(data_address, header = TRUE, sep = ";", stringsAsFactors = FALSE)

# Clean data
# We'll use just the two first parts of the survey
start_col <- which(names(data) == "P2483")
data <- data[, 1:(start_col - 1)]

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

head(data)
summary(data)
dim(data)

# Graphic 1: Is income enough related to be a homeowner or not (paying or not rent) ?
library(dplyr)
library(ggplot2)

# Filter and group data to count
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
  geom_bar(stat = "identity", position = "stack") +
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


# Graphic 2 : Treemap of expenses by category
library(treemapify)

gastos_cols <- c(
  "food_expense", "clothing_expense", "water_expense", "electricity_expense",
  "gas_expense", "cellphone_expense", "domestic_service_expense",
  "recreation_expense", "health_expense", "internet_expense",
  "school_transport_expense"
)
gastos_data <- data %>%
  select(all_of(gastos_cols)) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "amount")

# Crear un treemap
ggplot(gastos_data, aes(area = amount, fill = category, label = paste(category, "\n$", round(amount, 2)))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
  labs(
    title = "Distribución de los gastos por categoría",
    fill = "Categorías"
  ) +
  theme_minimal()

# Graphic 3: Boxplot expenses by group of income vs expenses
# Cargar librerías
library(ggplot2)
library(dplyr)

# Definir las columnas de gasto
gastos_cols <- c(
  "food_expense", "clothing_expense", "water_expense", "electricity_expense",
  "gas_expense", "cellphone_expense", "domestic_service_expense",
  "recreation_expense", "health_expense", "internet_expense",
  "school_transport_expense", "pension_expense"
)

# Crear el dataset con las columnas de gastos y income_vs_expenses
gastos_data <- data %>%
  select(all_of(gastos_cols), income_vs_expenses) %>%
  mutate(income_vs_expenses_label = case_when(
    income_vs_expenses == 1 ~ "Expenses > Income",
    income_vs_expenses == 2 ~ "Expenses = Income",
    income_vs_expenses == 3 ~ "Expenses < Income"
  ))

# Limpiar los datos (eliminar valores NA o gastos negativos)
gastos_clean <- gastos_data %>%
  filter(if_all(all_of(gastos_cols), ~ !is.na(.) & . > 0))  # Filtrar NA y valores negativos

# Convertir los montos de COP a USD
exchange_rate <- 4500  # 1 USD = 4500 COP
gastos_clean[gastos_cols] <- gastos_clean[gastos_cols] / exchange_rate  # Convertir todas las columnas de gasto

# Crear un boxplot para cada categoría de gasto, diferenciando por income_vs_expenses_label
gastos_clean %>%
  pivot_longer(cols = all_of(gastos_cols), names_to = "category", values_to = "amount") %>%
  ggplot(aes(x = category, y = amount, fill = income_vs_expenses_label)) +
  geom_boxplot() +
  ylim(0, 600) +
  labs(
    title = "Distribución de los Gastos por Categoría y Estado de Ingresos vs Gastos",
    x = "Categoría de Gasto",
    y = "Monto de Gasto (USD)",
    fill = "Estado de Ingresos vs Gastos"
  ) +
  scale_fill_manual(values = c("skyblue", "orange", "red")) +  # Colores personalizados
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar las etiquetas de las categorías
    legend.title = element_text(size = 10),  # Mejorar la legibilidad del título de la leyenda
    legend.text = element_text(size = 8)  # Mejorar la legibilidad de los textos de la leyenda
  )





