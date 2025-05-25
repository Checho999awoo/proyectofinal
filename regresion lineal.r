# Instalar paquetes si no están (solo la primera vez)
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")

# Cargar paquetes
library(readr)
library(dplyr)
library(ggplot2)

# 1. Leer archivo original
fact <- read_delim("C:/Users/sergi/Downloads/Facturacion FEBRERO 2025.csv", delim = ";")

# 2. Limpiar y convertir variables clave
fact <- fact %>%
  mutate(
    VR_TOTAL = as.numeric(VR_TOTAL),
    CONSUMO = as.numeric(CONSUMO),
    ESTRATO = as.numeric(ESTRATO),
    CARGO_FIJO_ACUE = as.numeric(CARGO_FIJO_ACUE),
    FINANCIACION = as.numeric(FINANCIACION),
    CLASE_USO = as.factor(CLASE_USO)
  )

# 3. Filtrar registros válidos para el modelo
fact_model_simple <- fact %>%
  filter(
    !is.na(VR_TOTAL),
    !is.na(CONSUMO),
    !is.na(ESTRATO),
    !is.na(CARGO_FIJO_ACUE),
    !is.na(FINANCIACION),
    !is.na(CLASE_USO)
  )

# 4. Ajustar el modelo reducido
modelo_simple <- lm(
  VR_TOTAL ~ CONSUMO + ESTRATO + CLASE_USO + CARGO_FIJO_ACUE + FINANCIACION,
  data = fact_model_simple
)

# 5. Ver resumen del modelo
summary(modelo_simple)

# 6. Agregar predicciones al dataset
fact_model_simple <- fact_model_simple %>%
  mutate(PREDICHO_SIMPLE = predict(modelo_simple))

# 7. Gráfico: valores reales vs predichos
ggplot(fact_model_simple, aes(x = PREDICHO_SIMPLE, y = VR_TOTAL)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(
    title = "VR_TOTAL: Real vs Predicho (modelo simple)",
    x = "Valor Predicho",
    y = "Valor Real"
  )

# 8. Gráfico: residuos del modelo simple
ggplot(fact_model_simple, aes(x = PREDICHO_SIMPLE, y = VR_TOTAL - PREDICHO_SIMPLE)) +
  geom_point(alpha = 0.4, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos del Modelo (simple)",
    x = "Valor Predicho",
    y = "Error (Residuo)"
  )

# 9. Exportar CSV con predicciones del modelo simple
write_csv(fact_model_simple, "C:/Users/sergi/Downloads/Facturacion_PREDICCIONES_SIMPLE.csv")
