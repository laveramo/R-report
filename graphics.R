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
      income_vs_expenses == 2 ~ "Expenses < Income",
      income_vs_expenses == 3 ~ "Expenses = Income"
    )
  ) %>%
  group_by(home_owner_label, income_vs_expenses_label) %>%
  summarise(count = n(), .groups = "drop")

# Create the graphic
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
library(tidyr)

# COP to USD in 2018 (1USD=3900COP)
usd_conversion_rate <- 0.00034  

# Prepare the data
expenses_cols <- c(
  "food_expense", "clothing_expense", "water_expense", "electricity_expense",
  "gas_expense", "cellphone_expense", "domestic_service_expense",
  "recreation_expense", "health_expense", "internet_expense",
  "school_transport_expense"
)

expenses_data <- data %>%
  # Seleccionar solo las columnas de gastos
  select(all_of(expenses_cols)) %>%
  # Filtrar valores NA o negativos
  filter(if_all(all_of(expenses_cols), ~ !is.na(.) & . > 0)) %>%
  # Calcular total y promedio para cada categoría
  summarise(across(everything(), list(
    total = ~sum(.x, na.rm = TRUE),
    avg = ~mean(.x, na.rm = TRUE)
  ))) %>%
  # Transformar a formato largo
  pivot_longer(cols = everything(), names_to = "category_stat", values_to = "amount") %>%
  # Separar nombre de la categoría y tipo de estadística (total o avg)
  separate(category_stat, into = c("category", "stat"), sep = "_(?=[^_]+$)") %>%
  # Volver a formato ancho
  pivot_wider(names_from = "stat", values_from = "amount") %>%
  # Convertir total y promedio a USD y eliminar "_expense"
  mutate(
    category = gsub("_expense", "", category),  # Remove "_expense" suffix
    total_usd = round(total * usd_conversion_rate),   # Convert total to USD
    avg_usd = round(avg * usd_conversion_rate)        # Convert average to USD
  )

# Create treemap
ggplot(expenses_data, aes(
  area = total_usd,
  fill = category,
  label = paste0(
    category, "\n",
    "Total: $", round(total_usd, 2), "\n",
    "Avg: $", round(avg_usd, 2)
  )
)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
  labs(
    title = "Expenses by Category (in USD)",
    fill = "Category"
  ) +
  theme_minimal()

# Graphic 3: Boxplot expenses by group of income vs expenses

# Create dataset with expense column and income_vs_expenses
expenses_data <- data %>%
  select(all_of(expenses_cols), income_vs_expenses) %>%
  mutate(income_vs_expenses_label = case_when(
    income_vs_expenses == 1 ~ "Expenses > Income",
    income_vs_expenses == 2 ~ "Expenses < Income",
    income_vs_expenses == 3 ~ "Expenses = Income"
  ))

# Clean data (delete NA or negative values)
expenses_clean <- expenses_data %>%
  filter(if_all(all_of(expenses_cols), ~ !is.na(.) & . > 0))  # Filtrar NA y valores negativos

# Convert COP to USD
expenses_clean[expenses_cols] <- expenses_clean[expenses_cols] * usd_conversion_rate  

expenses_long <- expenses_clean %>%
  pivot_longer(cols = all_of(expenses_cols), names_to = "category", values_to = "amount") %>%
  mutate(category = gsub("_expense", "", category))  # Remove "_expense" from category names

# Create boxplot
ggplot(expenses_long, aes(x = category, y = amount, fill = income_vs_expenses_label)) +
  geom_boxplot() +
  ylim(0, 600) +
  labs(
    title = "Expenses by Category",
    x = "Expense Category",
    y = "Expense Amount (USD)",
    fill = "Expenses vs Income"
  ) +
  scale_fill_manual(values = c("skyblue", "orange", "red")) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate labels
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8)  
  )




