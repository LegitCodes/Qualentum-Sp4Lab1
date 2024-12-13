# Paso 1: Configuración inicial

# Vectores de ejemplo
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))  # 10 valores para renovables y 10 para no renovables
consumo <- c(10, 12, NA, 14, 15, 11, 13, 12, 10, NA, 16, 18, 14, 12, 17, 16, NA, 13, 14, 12, 15)  # Consumo en kWh
costo_kwh <- c(0.12, 0.15, 0.12, 0.15, 0.12, 0.14, 0.13, 0.12, 0.14, 0.15, 0.10, 0.12, 0.10, 0.13, 0.10, 0.12, 0.14, 0.13, 0.10, 0.12)  # Costo por kWh

# Paso 2: Limpieza de datos

# Reemplazar los valores NA con la mediana del consumo para cada tipo de energía
# Primero, separamos los datos por tipo de energía
consumo_renovable <- consumo[energia == "Renovable"]
consumo_no_renovable <- consumo[energia == "No Renovable"]

# Calcular las medianas
mediana_renovable <- median(consumo_renovable, na.rm = TRUE)
mediana_no_renovable <- median(consumo_no_renovable, na.rm = TRUE)

# Reemplazar los NA en cada tipo de energía con la mediana correspondiente
consumo[energia == "Renovable" & is.na(consumo)] <- mediana_renovable
consumo[energia == "No Renovable" & is.na(consumo)] <- mediana_no_renovable

# Paso 3: Creación del dataframe

# Crear el dataframe con la información sobre tipo de energía, consumo y costo
df_consumo <- data.frame(
  energia = energia,
  consumo_diario = consumo,
  costo_kwh = costo_kwh
)

# Paso 4: Cálculos

# Agregar una columna de costo_total que calcule el costo diario
df_consumo$costo_total <- df_consumo$consumo_diario * df_consumo$costo_kwh

# Calcular el total de consumo y el costo total por tipo de energía
total_consumo <- tapply(df_consumo$consumo_diario, df_consumo$energia, sum)
total_costo <- tapply(df_consumo$costo_total, df_consumo$energia, sum)

# Calcular la media del consumo diario para cada tipo de energía
media_consumo <- tapply(df_consumo$consumo_diario, df_consumo$energia, mean)

# Agregar una columna de ganancia (aumento del 10%)
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: Resumen

# Ordenar el dataframe por la columna costo_total en orden descendente
df_consumo_ordenado <- df_consumo[order(-df_consumo$costo_total), ]

# Calcular el total de consumo energético por tipo de energía (renovable y no renovable)
total_consumo_renovable <- sum(df_consumo$consumo_diario[df_consumo$energia == "Renovable"])
total_consumo_no_renovable <- sum(df_consumo$consumo_diario[df_consumo$energia == "No Renovable"])

# Calcular el costo total por cada tipo de energía (renovable y no renovable)
total_costo_renovable <- sum(df_consumo$costo_total[df_consumo$energia == "Renovable"])
total_costo_no_renovable <- sum(df_consumo$costo_total[df_consumo$energia == "No Renovable"])

# Extraer las tres filas con el mayor costo_total
top_3_costos <- head(df_consumo_ordenado, 3)

# Crear la lista resumen_energia
resumen_energia <- list(
  df_consumo_ordenado = df_consumo_ordenado,
  total_consumo_renovable = total_consumo_renovable,
  total_consumo_no_renovable = total_consumo_no_renovable,
  total_costo_renovable = total_costo_renovable,
  total_costo_no_renovable = total_costo_no_renovable,
  top_3_costos = top_3_costos
)

# Mostrar la lista resumen_energia
print(resumen_energia)
