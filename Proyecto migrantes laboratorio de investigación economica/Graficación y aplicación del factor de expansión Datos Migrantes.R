# ============================================================
# 0. CARGAR BASE LIMPIA Y LIBRERÍAS
# ============================================================
paquetes <- c("dplyr", "ggplot2", "srvyr", "survey", "scales", "tidyr")
for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(dplyr)
library(ggplot2)
library(srvyr)
library(survey)
library(scales)
library(tidyr)

# Cargar base limpia
migrantes <- readRDS("C:/Users/jerem/OneDrive/Escritorio/migrantes_AMCO_2025_limpio.rds")
cat("Base cargada:", nrow(migrantes), "observaciones\n")

# ============================================================
# 1. APLICAR FACTOR DE EXPANSIÓN
# ============================================================
disenio <- migrantes %>%
  as_survey_design(
    ids     = 1,        # sin conglomerados declarados
    weights = FEX_C18   # factor de expansión GEIH
  )

cat("Diseño muestral creado\n")
cat("   Población estimada total:",
    round(sum(migrantes$FEX_C18, na.rm = TRUE)), "personas\n")

# ============================================================
# TEMA BASE PARA TODAS LAS GRÁFICAS
# ============================================================
tema <- theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle   = element_text(hjust = 0.5, color = "gray50", size = 11),
    plot.caption    = element_text(color = "gray60", size = 9, hjust = 1),
    axis.text       = element_text(color = "gray30"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

paleta <- c("#2E86AB", "#E84855", "#F9C74F", "#6A994E", "#A44A3F", "#7B2D8B")

# ============================================================
# GRÁFICA 1: Condición de ocupación (EXPANDIDA)
# ============================================================
ocup_exp <- disenio %>%
  filter(!is.na(ocupado)) %>%
  group_by(ocupado) %>%
  summarise(
    total = survey_total(vartype = "ci"),
    pct   = survey_prop(vartype = "ci") * 100
  )

ggplot(ocup_exp, aes(x = ocupado, y = pct, fill = ocupado)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = pct_low, ymax = pct_upp), width = 0.2, color = "gray40") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -1.8, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("Ocupado" = "#2E86AB",
                               "Desocupado" = "#E84855",
                               "Inactivo" = "#F9C74F")) +
  scale_y_continuous(limits = c(0, 70), labels = label_percent(scale = 1)) +
  labs(
    title    = "Condición de ocupación de migrantes internacionales",
    subtitle = "Estimaciones expandidas a la población — AMCO 2025",
    caption  = "Fuente: GEIH 2025. Factor de expansión FEX_C18 aplicado.",
    x = NULL, y = "Porcentaje (%)"
  ) +
  tema + theme(legend.position = "none")

ggsave("C:/Users/jerem/OneDrive/Escritorio/g1_ocupacion_expandida.png",
       width = 7, height = 5, dpi = 300)

# ============================================================
# GRÁFICA 2: Ingreso laboral promedio por condición de ocupación
# ============================================================

mediana_val <- ing_ocup$mediana
etiqueta_mediana <- paste0("Mediana: $", 
                           formatC(round(mediana_val), 
                                   format = "f", 
                                   digits = 0, 
                                   big.mark = "."))

cat("Etiqueta que se mostrará:", etiqueta_mediana, "\n") 

# Gráfica
migrantes %>%
  filter(ocupado == "Ocupado", !is.na(ingreso_laboral), ingreso_laboral > 0) %>%
  ggplot(aes(x = ingreso_laboral)) +
  geom_histogram(fill = "#2E86AB", color = "white", bins = 30, alpha = 0.85) +
  geom_vline(xintercept = mediana_val, color = "#E84855",
             linetype = "dashed", linewidth = 1.2) +
  annotate("text",
           x     = mediana_val * 1.15,
           y     = Inf,
           vjust = 2,
           label = etiqueta_mediana,   # ← variable ya formateada, no calculada dentro
           color = "#E84855", fontface = "bold", size = 4.5) +
  scale_x_continuous(
    limits = c(0, 10000000),
    breaks = seq(0, 10000000, by = 1000000),
    labels = function(x) paste0("$", formatC(x/1000000, format="f", digits=1), "M")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Distribución del ingreso laboral",
    subtitle = "Migrantes internacionales ocupados — AMCO 2025",
    caption  = "Fuente: GEIH 2025. Mediana estimada con factor de expansión FEX_C18.\nValores superiores a $10M omitidos para mayor legibilidad.",
    x = "Ingreso laboral mensual (COP)",
    y = "Frecuencia"
  ) +
  tema +
  theme(
    axis.text.x = element_text(size = 10, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20"),
    axis.title  = element_text(size = 11)
  )

ggsave("C:/Users/jerem/OneDrive/Escritorio/g2_ingreso_expandido.png",
       width = 10, height = 6, dpi = 300)
# ============================================================
# GRÁFICA 3: Tipo de empleo (EXPANDIDO)
# ============================================================
empleo_exp <- disenio %>%
  filter(!is.na(tipo_empleo), tipo_empleo %in% c(1,2,3,4,5,6)) %>%
  mutate(tipo_empleo_label = case_when(
    tipo_empleo == 1 ~ "Empresa privada",
    tipo_empleo == 2 ~ "Gobierno",
    tipo_empleo == 3 ~ "Empleador",
    tipo_empleo == 4 ~ "Cuenta propia",
    tipo_empleo == 5 ~ "Trabajador familiar",
    tipo_empleo == 6 ~ "Trabajador doméstico"
  )) %>%
  group_by(tipo_empleo_label) %>%
  summarise(
    total = survey_total(vartype = "ci"),
    pct   = survey_prop(vartype = "ci") * 100
  ) %>%
  arrange(desc(pct))

# Graficar
ggplot(empleo_exp, aes(x = reorder(tipo_empleo_label, pct), y = pct, fill = pct)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = pct_low, ymax = pct_upp), width = 0.25, color = "gray40") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.25, fontface = "bold", size = 4, color = "gray20") +
  scale_fill_gradient(low = "#AED9E0", high = "#1F4E79") +
  scale_y_continuous(limits = c(0, 65), labels = label_percent(scale = 1)) +
  coord_flip() +
  labs(
    title    = "Tipo de empleo de migrantes internacionales",
    subtitle = "Estimaciones expandidas a la población — AMCO 2025",
    caption  = "Fuente: GEIH 2025. Factor de expansión FEX_C18 aplicado.",
    x = NULL, y = "Porcentaje (%)"
  ) +
  tema +
  theme(
    legend.position = "none",
    axis.text.y     = element_text(size = 12, color = "gray20"),
    axis.text.x     = element_text(size = 10),
    plot.margin     = margin(10, 40, 10, 10)
  )

ggsave("C:/Users/jerem/OneDrive/Escritorio/g3_tipo_empleo_expandido.png",
       width = 10, height = 6, dpi = 300)
# ============================================================
# GRÁFICA 4 CORREGIDA: escala Y más legible
# ============================================================
migrantes %>%
  filter(!is.na(edad), !is.na(ingreso_laboral), ingreso_laboral > 0,
         ocupado == "Ocupado") %>%
  mutate(
    grupo_edad = cut(edad,
                     breaks = c(14, 24, 34, 44, 54, 64, 100),
                     labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                     right  = TRUE)
  ) %>%
  filter(!is.na(grupo_edad)) %>%
  ggplot(aes(x = grupo_edad, y = ingreso_laboral, fill = grupo_edad)) +
  geom_boxplot(alpha = 0.85, outlier.color = "gray60",
               outlier.size = 1.5, width = 0.6) +
  geom_hline(yintercept = median(migrantes$ingreso_laboral, na.rm = TRUE),
             linetype = "dashed", color = "#E84855", linewidth = 1) +
  annotate("text", x = 0.6, y = median(migrantes$ingreso_laboral, na.rm = TRUE),
           vjust = -0.8, label = "Mediana general",
           color = "#E84855", fontface = "bold", size = 3.8) +
  scale_fill_manual(values = c(
    "15-24" = "#AED9E0", "25-34" = "#2E86AB", "35-44" = "#1F4E79",
    "45-54" = "#6A994E", "55-64" = "#F9C74F", "65+"   = "#E84855"
  )) +
  scale_y_continuous(
    limits = c(0, 6000000),               # techo en 6 millones
    breaks = seq(0, 6000000, by = 500000), # marcas cada 500 mil
    labels = function(x) paste0("$", format(x/1000000, big.mark=","), "M")
  ) +
  labs(
    title    = "Distribución del ingreso laboral por grupo de edad",
    subtitle = "Migrantes internacionales ocupados — AMCO 2025",
    caption  = "Fuente: GEIH 2025. La línea roja indica la mediana general.\nValores superiores a $6M omitidos para mejorar legibilidad.",
    x = "Grupo de edad",
    y = "Ingreso laboral mensual (COP)"
  ) +
  tema +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(size = 12, color = "gray20"),
    axis.text.y     = element_text(size = 10, color = "gray20"),
    axis.title      = element_text(size = 11),
    plot.margin     = margin(10, 30, 10, 10)
  )

ggsave("C:/Users/jerem/OneDrive/Escritorio/g4_ingreso_por_edad.png",
       width = 10, height = 6, dpi = 300)
cat("Gráfica 4 corregida.\n")
# ============================================================
# GRÁFICA 5: Ingreso promedio por tipo de empleo (EXPANDIDO)
# ============================================================
ing_empleo <- disenio %>%
  filter(
    !is.na(ingreso_laboral), ingreso_laboral > 0,
    tipo_empleo %in% c(1, 2, 3, 4, 5, 6)
  ) %>%
  mutate(tipo_empleo_label = case_when(
    tipo_empleo == 1 ~ "Empresa privada",
    tipo_empleo == 2 ~ "Gobierno",
    tipo_empleo == 3 ~ "Empleador",
    tipo_empleo == 4 ~ "Cuenta propia",
    tipo_empleo == 5 ~ "Trabajador familiar",
    tipo_empleo == 6 ~ "Trabajador doméstico"
  )) %>%
  group_by(tipo_empleo_label) %>%
  summarise(
    ingreso_medio     = survey_mean(ingreso_laboral, vartype = "ci")
  ) %>%
  arrange(desc(ingreso_medio))

ggplot(ing_empleo, aes(x = reorder(tipo_empleo_label, ingreso_medio),
                       y = ingreso_medio, fill = ingreso_medio)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = ingreso_medio_low, ymax = ingreso_medio_upp),
                width = 0.25, color = "gray40") +
  geom_text(
    aes(label = paste0("$", format(round(ingreso_medio / 1000), big.mark = ","), "k")),
    hjust = -0.25, fontface = "bold", size = 4, color = "gray20"
  ) +
  scale_fill_gradient(low = "#AED9E0", high = "#1F4E79") +
  scale_y_continuous(
    labels = dollar_format(prefix = "$", big.mark = ","),
    limits = c(0, max(ing_empleo$ingreso_medio_upp, na.rm = TRUE) * 1.3)
  ) +
  coord_flip() +
  labs(
    title    = "Ingreso laboral promedio por tipo de empleo",
    subtitle = "Migrantes internacionales ocupados — AMCO 2025",
    caption  = "Fuente: GEIH 2025. Factor de expansión FEX_C18 aplicado.",
    x = NULL,
    y = "Ingreso promedio mensual (COP)"
  ) +
  tema +
  theme(
    legend.position = "none",
    axis.text.y     = element_text(size = 12, color = "gray20"),
    axis.text.x     = element_text(size = 10),
    plot.margin     = margin(10, 50, 10, 10)
  )

ggsave("C:/Users/jerem/OneDrive/Escritorio/g5_ingreso_por_empleo.png",
       width = 10, height = 6, dpi = 300)
cat("Gráfica 5 guardada.\n")

# ============================================================
# REPORTE FINAL EXPANDIDO EN CONSOLA
# ============================================================
cat("\n========== ESTADÍSTICAS EXPANDIDAS ==========\n")

cat("\n Población migrante estimada:\n")
cat("  ", round(sum(migrantes$FEX_C18, na.rm = TRUE)), "personas\n")

cat("\n Ocupación (% poblacional):\n")
print(as.data.frame(ocup_exp[, c("ocupado", "pct", "pct_low", "pct_upp")]))

cat("\n Ingreso laboral (expandido):\n")
cat("   Media:  $", format(round(ing_ocup$media), big.mark = ","), "\n")
cat("   Mediana: $", format(round(ing_ocup$mediana), big.mark = ","), "\n")

cat("\n 5 gráficas guardadas en el Escritorio.\n")
names(migrantes)[grepl("empleo|P6430", names(migrantes), ignore.case = TRUE)]

names(migrantes)[grepl("sex|P6020|genero|género", names(migrantes), ignore.case = TRUE)]
names(migrantes)
