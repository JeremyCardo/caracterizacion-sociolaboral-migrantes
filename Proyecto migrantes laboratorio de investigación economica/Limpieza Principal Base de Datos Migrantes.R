install.packages("haven")
library(haven)

geih <- read_dta("C:/Users/jerem/OneDrive/Escritorio/TOTAL_AMCO_2025.dta")
geih <- zap_labels(geih)

# ============================================================
# LIMPIEZA FINAL - MIGRANTES INTERNACIONALES AMCO 2025
# ============================================================
library(haven)
library(dplyr)

# PASO 1: Eliminar etiquetas Stata
geih <- zap_labels(geih)

# PASO 2: Filtro correcto de migrantes internacionales
migrantes <- geih %>%
  filter(P7500S1 == 9 | P7510S1 == 9 | P7500S2 == 2)
cat("Total migrantes:", nrow(migrantes), "\n")

# PASO 3: Eliminar columnas con más del 90% de NAs
umbral_na <- 0.90
cols_vacias <- colMeans(is.na(migrantes)) > umbral_na
migrantes <- migrantes[, !cols_vacias]
cat(" Columnas tras limpiar NAs:", ncol(migrantes), "\n")

# PASO 4: Eliminar duplicados
antes <- nrow(migrantes)
migrantes <- migrantes %>%
  distinct(DIRECTORIO, SECUENCIA_P, ORDEN, .keep_all = TRUE)
cat(" Duplicados eliminados:", antes - nrow(migrantes), "\n")

# PASO 5: Seleccionar solo variables que existen en tu base
vars_quiero <- c(
  "DIRECTORIO","SECUENCIA_P","ORDEN","FEX_C18",
  "DPTO","AREA","CLASE",
  "P6020","P6040","P6070","P6090",
  "P6160","P6210","P6210S1",
  "P7500S1","P7500S2","P7510S1","P7510S3",
  "P750S1","P750S2","P760",
  "P6240","P6430","P6450","P6500","P6510","INGLABO"
)
vars_ok <- vars_quiero[vars_quiero %in% names(migrantes)]
cat("No encontradas:", paste(vars_quiero[!vars_quiero %in% names(migrantes)], collapse=", "), "\n")
migrantes <- migrantes %>% select(all_of(vars_ok))

# PASO 6: Renombrar
migrantes <- migrantes %>%
  rename_with(~ dplyr::recode(.x,
                              P6020    = "sexo",
                              P6040    = "edad",
                              P6070    = "estado_civil",
                              P6090    = "seguridad_social",
                              P6160    = "sabe_leer",
                              P6210    = "nivel_educ",
                              P6210S1  = "ultimo_anio_educ",
                              P7500S1  = "lugar_nacimiento",
                              P7500S2  = "pais_nacimiento_tipo",
                              P7510S1  = "lugar_hace_5anios",
                              P7510S3  = "pais_hace_5anios_cod",
                              P750S1   = "lugar_hace_12meses",
                              P750S2   = "pais_hace_12meses_tipo",
                              P760     = "pais_cod",
                              P6240    = "actividad",
                              P6430    = "tipo_empleo",
                              P6450    = "tiene_contrato",
                              P6500    = "salario",
                              P6510    = "horas_trabajo",
                              INGLABO  = "ingreso_laboral"
  ))

# PASO 7: Recodificar — solo variables que existen
migrantes <- migrantes %>%
  mutate(
    # Sexo (solo si existe)
    sexo = if ("sexo" %in% names(.))
      factor(sexo, levels=c(1,2), labels=c("Hombre","Mujer")) else NULL,
    
    # Tipo de migrante
    tipo_migrante = case_when(
      lugar_nacimiento == 9 & lugar_hace_5anios == 9 ~ "Nació y vivía afuera",
      lugar_nacimiento == 9                           ~ "Nació afuera (establecido)",
      lugar_hace_5anios == 9                          ~ "Vivía afuera hace 5 años",
      pais_nacimiento_tipo == 2                       ~ "País nacimiento extranjero",
      TRUE                                            ~ "Otro"
    ),
    
    # País de origen usando P760
    origen = case_when(
      pais_cod == 862  ~ "Venezuela",
      pais_cod == 604  ~ "Perú",
      pais_cod == 218  ~ "Ecuador",
      pais_cod == 840  ~ "EEUU",
      pais_cod == 484  ~ "México",
      pais_cod == 724  ~ "España",
      pais_cod == 76   ~ "Brasil",
      pais_cod == 152  ~ "Chile",
      pais_cod == 32   ~ "Argentina",
      pais_cod == 0    ~ "Sin código/Venezuela",  # revisar
      !is.na(pais_cod) ~ "Otro país",
      TRUE             ~ NA_character_
    ),
    
    # Nivel educativo
    nivel_educ_agrup = if ("nivel_educ" %in% names(.)) case_when(
      nivel_educ %in% c(1,2)   ~ "Ninguno/Preescolar",
      nivel_educ %in% c(3,4)   ~ "Primaria",
      nivel_educ %in% c(5,6)   ~ "Secundaria",
      nivel_educ == 7           ~ "Media",
      nivel_educ %in% c(8,9)   ~ "Técnico/Tecnológico",
      nivel_educ %in% c(10,11) ~ "Universitario",
      nivel_educ == 12          ~ "Posgrado",
      TRUE                      ~ NA_character_
    ) else NA_character_,
    nivel_educ_agrup = factor(nivel_educ_agrup,
                              levels=c("Ninguno/Preescolar","Primaria","Secundaria","Media",
                                       "Técnico/Tecnológico","Universitario","Posgrado")),
    
    # Ocupación
    ocupado = if ("actividad" %in% names(.)) case_when(
      actividad == 1           ~ "Ocupado",
      actividad == 2           ~ "Desocupado",
      actividad %in% c(3,4,5) ~ "Inactivo",
      TRUE                     ~ NA_character_
    ) else NA_character_,
    ocupado = factor(ocupado, levels=c("Ocupado","Desocupado","Inactivo"))
  )

# PASO 8: Winsorizar outliers
winsorizar <- function(x, p1=0.01, p2=0.99) {
  q <- quantile(x, probs=c(p1,p2), na.rm=TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  x
}
if ("edad" %in% names(migrantes))
  migrantes$edad <- ifelse(migrantes$edad < 0 | migrantes$edad > 100, NA, migrantes$edad)
if ("ingreso_laboral" %in% names(migrantes))
  migrantes$ingreso_laboral <- winsorizar(migrantes$ingreso_laboral)
if ("salario" %in% names(migrantes))
  migrantes$salario <- winsorizar(migrantes$salario)
if ("horas_trabajo" %in% names(migrantes))
  migrantes$horas_trabajo <- ifelse(migrantes$horas_trabajo < 0 | migrantes$horas_trabajo > 98, NA, migrantes$horas_trabajo)

# PASO 9: Reporte final
cat("\n========== RESUMEN FINAL ==========\n")
cat("Observaciones:", nrow(migrantes), "\n")
cat("Variables:    ", ncol(migrantes), "\n")
cat("\n País de origen:\n")
print(sort(table(migrantes$origen, useNA="always"), decreasing=TRUE))
cat("\n Tipo de migrante:\n")
print(table(migrantes$tipo_migrante, useNA="always"))
cat("\n Ocupación:\n")
print(table(migrantes$ocupado, useNA="always"))
cat("\n Nivel educativo:\n")
print(table(migrantes$nivel_educ_agrup, useNA="always"))

# PASO 10: Guardar
saveRDS(migrantes, "C:/Users/jerem/OneDrive/Escritorio/migrantes_AMCO_2025_limpio.rds")
cat("\n Base guardada exitosamente.\n")

# CORRECCIÓN 1: Eliminar nivel_educ_agrup (no tenemos la variable fuente)
migrantes <- migrantes %>% select(-nivel_educ_agrup)

# CORRECCIÓN 2: Reclasificar pais_cod == 0 como "Sin identificar"
migrantes <- migrantes %>%
  mutate(origen = case_when(
    pais_cod == 862  ~ "Venezuela",
    pais_cod == 604  ~ "Perú",
    pais_cod == 218  ~ "Ecuador",
    pais_cod == 840  ~ "EEUU",
    pais_cod == 484  ~ "México",
    pais_cod == 724  ~ "España",
    pais_cod == 76   ~ "Brasil",
    pais_cod == 152  ~ "Chile",
    pais_cod == 32   ~ "Argentina",
    pais_cod == 0    ~ "Sin identificar",
    !is.na(pais_cod) ~ "Otro país",
    # Los 514 NAs en pais_cod pero con pais_nacimiento_tipo==2
    pais_nacimiento_tipo == 2 ~ "Sin identificar",
    TRUE ~ NA_character_
  ))

# Verificar
print(sort(table(migrantes$origen, useNA="always"), decreasing=TRUE))

# Guardar de nuevo
saveRDS(migrantes, "C:/Users/jerem/OneDrive/Escritorio/migrantes_AMCO_2025_limpio.rds")
cat("Base corregida y guardada.\n")

