library(tidyverse)
library(srvyr)
library(kableExtra)
# Aumentar el timeout para permitir descargas grandes
options(timeout = 600)
# Ajustar la varianza cuando hay algún estrato con una sola PSU
options(survey.lonely.psu = "adjust")

#########################
# Funciones y variables #
#########################

# Etiquetas para estados
codigo_a_nombre <- c(
  "01" = "Aguascalientes", "02" = "Baja California", "03" = "Baja California Sur",
  "04" = "Campeche", "05" = "Coahuila de Zaragoza", "06" = "Colima", "07" = "Chiapas",
  "08" = "Chihuahua", "09" = "Ciudad de México", "10" = "Durango",
  "11" = "Guanajuato", "12" = "Guerrero", "13" = "Hidalgo", "14" = "Jalisco",
  "15" = "Estado de México", "16" = "Michoacán de Ocampo", "17" = "Morelos", "18" = "Nayarit",
  "19" = "Nuevo León", "20" = "Oaxaca", "21" = "Puebla", "22" = "Querétaro",
  "23" = "Quintana Roo", "24" = "San Luis Potosí", "25" = "Sinaloa", "26" = "Sonora",
  "27" = "Tabasco", "28" = "Tamaulipas", "29" = "Tlaxcala", "30" = "Veracruz de Ignacio de la Llave",
  "31" = "Yucatán", "32" = "Zacatecas"
)

# Función para preparar la base
preparar_base_enigh <- function(enigh_df, periodo) {
  # Seleccionar y procesar variables en una sola operación
  base <- enigh_df %>%
    transmute(
      # Variables para el diseño muestral
      upm,           # Unidad Primaria de Muestreo
      est_dis,       # Estrato de diseño muestral
      factor,        # Factor de expansión
      
      # Clave para identificar al hogar
      clave_hogar = paste(ubica_geo, upm, est_dis, folioviv, foliohog, sep = "_"),
      
      # Estado y periodo
      id_edo = substr(ubica_geo, 1, 2),
      edo = codigo_a_nombre[id_edo],
      periodo = periodo,
      
      # Variables de ingreso y gasto
      tot_integ,  # No. de integrantes del hogar
      percep_ing, # Integrantes perceptores de ingreso
      ing_cor,    # Ingreso corriente trimestral original
      gasto_mon,  # Gasto corriente trimestral original
      ing_cor_men = ing_cor/3, # Ingreso corriente mensual
      gasto_mon = gasto_mon/3, # Gasto monetario mensual
      
      # Calcular indicadores mensuales
      ing_prom_int = ing_cor_men/tot_integ,        # Ingreso mensual por integrante
      ing_prom_int_percep = ing_cor_men/percep_ing # Ingreso mensual por perceptor
    )
  
  # Crear diseño muestral y calcular decil
  base <- base %>%
    as_survey_design(id = upm, strata = est_dis, weights = factor, nest = TRUE) %>%
    mutate(decil = ntile(ing_cor, 10)) %>%
    as_tibble()
  
  return(base)
}

# Función para generar tablas de análisis con formato de miles y decimales
tabla_analisis <- function(diseno, variables_agrupacion, var_interes, titulo) {
  # Primero obtenemos los resultados sin formato
  resultados <- diseno %>%
    group_by(across(all_of(variables_agrupacion))) %>%
    summarise(
      Promedio = survey_mean({{var_interes}}, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(across(all_of(variables_agrupacion)))
  
  # Luego convertimos a character y formateamos
  resultados <- resultados %>%
    transmute(Entidad = edo,
           Promedio = formatC(as.numeric(Promedio), 
                                 format = "f", big.mark = ",", digits = 2),
           'Err. Std.' = formatC(as.numeric(Promedio_se), 
                               format = "f", big.mark = ",", digits = 2))
  
  # Finalmente presentamos en formato tabla
  resultados %>%
    kable(caption = titulo, align = c('l', 'r')) %>%
    kable_styling(full_width = FALSE)
}


#############
# Ejecución #
#############

# Descargar ENIGH 2022 (install.packages("importinegi") o importarla manualmente)
enigh2022 <- importinegi::enigh(2022, "concentradohogar", formato = "sav")

# Preparar bases
base_2022 <- preparar_base_enigh(enigh2022, 2022)

# Crear diseño muestral complejo
diseno_enigh <- base_2022 %>%
  as_survey_design(
    id = upm,
    strata = est_dis,
    weights = factor,
    nest = TRUE
  )


#######################
# Limpieza de la base #
#######################

# Verificamos ceros en columnas de interés

# Para ver las filas con cero en ing_cor
filas_con_cero <- diseno_enigh$variables %>%
  filter(ing_cor == 0) %>%
  select(edo, ing_cor, ing_prom_int_percep, percep_ing ) %>% 
  head(10)

print(filas_con_cero)

# Para ver las filas donde percep_ing es cero
filas_con_cero <- diseno_enigh$variables %>%
  filter(percep_ing == 0) %>%
  select(edo, ing_cor, ing_prom_int_percep, percep_ing ) %>% 
  nrow()

print(filas_con_cero)

# Para contabilizar cuántas filas tienen cero en la columna ing_cor por estado
conteo_por_estado <- diseno_enigh$variables %>%
  group_by(edo) %>%
  summarise(
    total_filas = n(),
    filas_con_cero = sum(ing_cor == 0, na.rm = TRUE),
    porcentaje = round(filas_con_cero / total_filas * 100, 2)
  )

print(conteo_por_estado, n = 32)

# Para contabilizar cuántas filas tienen cero en la columna ing_cor por estado
conteo_por_estado <- diseno_enigh$variables %>%
  group_by(edo) %>%
  summarise(
    total_filas = n(),
    filas_con_cero = sum(percep_ing == 0, na.rm = TRUE),
    porcentaje = round(filas_con_cero / total_filas * 100, 2)
  )

print(conteo_por_estado, n = 32)

# Filtramos: solo hogares con ingresos y perceptores
base_2022 <- base_2022 %>%
  filter(ing_cor != 0, percep_ing != 0)

# Recrear diseño muestral complejo
diseno_enigh <- base_2022 %>%
  as_survey_design(
    id = upm,
    strata = est_dis,
    weights = factor,
    nest = TRUE
  )

# Liberar memoria
rm(codigo_a_nombre, conteo_por_estado, filas_con_cero)
gc() # Forzar garbage collection


##########
# Tablas #
##########

# Función para generar tablas de análisis:
# tabla_analisis(tabla de diseño muestral, 
#                variables de agrupacion, 
#                variable de interés, 
#                título de la tabla) 

# names(diseno_enigh$variables) # para ver el nombre de las variables

# Ej 1: Por  Entidad Federativa, por integrante perceptor
tabla_analisis(
  diseno_enigh, 
  c("edo"), 
  ing_prom_int_percep, 
  "ENIGH (2022): Ingreso Corriente Mensual Promedio por Integrante Perceptor de Ingreso"
)

# Ej 2: Por Entidad Federativa, por hogar
tabla_analisis(
  diseno_enigh, 
  c("edo"), 
  ing_cor_men, 
  "ENIGH (2022): Ingreso Corriente Mensual Promedio por Hogar"
)

# Ej 3: Por Entidad Federativa, por hogar
diseno_enigh %>%
  group_by(edo, decil) %>%
  summarise(
    Promedio = survey_mean(ing_cor_men, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(edo) %>%
  transmute(Entidad = edo,
            Decil = decil,
            Promedio = formatC(as.numeric(Promedio), 
                               format = "f", big.mark = ",", digits = 2),
            'Err. Std.' = formatC(as.numeric(Promedio_se), 
                                  format = "f", big.mark = ",", digits = 2)) %>%
  kable(caption = "ENIGH (2022): Ingreso Corriente Mensual Promedio por Hogar",
        align = c('l', 'r')) %>%
  kable_styling(full_width = FALSE)


############
# GRÁFICAS #
############

library(ggplot2)
library(patchwork)

# Cálculo de medias ponderadas por decil y estado
resumen_entidad <- diseno_enigh %>%
  group_by(edo) %>%
  summarise(
    ing_prom_int_percep = survey_mean(ing_prom_int_percep, na.rm = TRUE),
    ing_cor_men = survey_mean(ing_cor_men, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(-ing_prom_int_percep_se, -ing_cor_men_se)

resumen_decil <- diseno_enigh %>%
  group_by(edo, decil) %>%
  summarise(
    ing_prom_int_percep = survey_mean(ing_prom_int_percep, na.rm = TRUE),
    ing_cor_men = survey_mean(ing_cor_men, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(-ing_prom_int_percep_se, -ing_cor_men_se)

# --- Por Entidad Federativa ---

# Promedio por Integrante
ggplot(resumen_entidad,
       aes(x = ing_prom_int_percep, y = reorder(edo, ing_prom_int_percep))) + 
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(ing_prom_int_percep, accuracy = 1)),
            hjust = -0.1, size = 3) +
  labs(
    title = "Ingreso Corriente Mensual Promedio por Integrante Perceptor",
    subtitle = "México - Entidad Federativa",
    x = "Pesos corrientes",
    y = "Entidad federativa",
    caption = "Fuente: Elaboración propia con datos de la ENIGH 2022, INEGI."
  ) +  
  theme_minimal(base_size = 11) +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic")
  ) +
  xlim(0, max(resumen_entidad$ing_prom_int_percep) * 1.1)


# Promedio por Hogar
ggplot(resumen_entidad,
       aes(x = ing_cor_men, y = reorder(edo, ing_cor_men))) + 
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(ing_cor_men, accuracy = 1)),
            hjust = -0.1, size = 3) +
  labs(
    title = "Ingreso Corriente Mensual Promedio por Hogar",
    subtitle = "México - Entidad Federativa",
    x = "Pesos corrientes",
    y = "Entidad federativa",
    caption = "Fuente: Elaboración propia con datos de la ENIGH 2022, INEGI."
  ) +  
  theme_minimal(base_size = 11) +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic")
  ) +
  xlim(0, max(resumen_entidad$ing_cor_men) * 1.1)


# --- Por decil ---

# Promedio por Hogar
ggplot(resumen_decil %>% 
         filter(edo == "Ciudad de México") %>%
         mutate(Decil = as.character(decil)),
       aes(x = ing_cor_men, y = reorder(Decil, ing_cor_men))) + 
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(ing_cor_men, accuracy = 1)),
            hjust = -0.1, size = 3) +
  labs(
    title = "Ingreso Corriente Mensual Promedio por Hogar",
    subtitle = "Ciudad de México por Decil de Ingreso",
    x = "Pesos corrientes",
    y = "Decil de Ingreso",
    caption = "Fuente: Elaboración propia con datos de la ENIGH 2022, INEGI."
  ) +  
  theme_minimal(base_size = 11) +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic")
  ) +
  xlim(0, max(resumen_decil$ing_cor_men) * 1.1)
