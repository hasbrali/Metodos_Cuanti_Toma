#===========================================================================================
#Proyecto: Percepción de la Toma de Dintilhac
#Objetivo de este script: Analisis Descriptivo
#Fecha: 14-11-2026
#==========================================================================================


#1. Carga de librerias--------------------------------------------------------
library(rio)
library(tidyverse)
renv::snapshot()

df <- import("D:/Metodos_Cuanti_Toma/datos/procesados/DINTILHAC_procesada.rds")

# 1. PERFIL DEMOGRÁFICO
# Memo:omitiendo NA
tabla_demografica <- df %>%
  select(demografica_genero, demografica_escala, demografica_especialidad, filtro_ciclo) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Categoria") %>%
  filter(!is.na(Categoria)) %>%
  count(Variable, Categoria) %>%
  group_by(Variable) %>%
  mutate(Porcentaje = round((n / sum(n)) * 100, 1)) %>%
  ungroup()

export(tabla_demografica, "D:/Metodos_Cuanti_Toma/outputs01_perfil_demografico.xlsx")

# 2. ANÁLISIS UNIVARIADO

#Memo: Los porcentajes se calculan sobre los casos válidos (omitiendo NA).
vars_univariado <- c("nivel_involucramiento", "b1", "b3", "C3", "d1", "d2", "e2", "f1")

tabla_univariado <- df %>%
  select(all_of(vars_univariado)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Categoria") %>%
  filter(!is.na(Categoria)) %>% # Omitimos NA
  count(Variable, Categoria) %>%
  group_by(Variable) %>%
  mutate(Porcentaje = round((n / sum(n)) * 100, 1)) %>%
  ungroup()

export(tabla_univariado, "D:/Metodos_Cuanti_Toma/outputs/02_univariado.xlsx")

# Estadísticos descriptivos del índice
tabla_indice <- df %>%
  summarise(
    Media = round(mean(ind_apoyo_mov, na.rm = TRUE), 2),
    Desv_Est = round(sd(ind_apoyo_mov, na.rm = TRUE), 2),
    Minimo = min(ind_apoyo_mov, na.rm = TRUE),
    Maximo = max(ind_apoyo_mov, na.rm = TRUE)
  )

export(tabla_indice, "D:/Metodos_Cuanti_Toma/outputs/03_estadisticos_indice.xlsx")

# 3. ANÁLISIS BIVARIADO

# MEMO: Los porcentajes se calculan por fila (sobre el total de cada categoría de la variable X).

# Cruce 1: Género x Nivel de involucramiento
cruce1 <- df %>%
  filter(!is.na(demografica_genero), !is.na(nivel_involucramiento)) %>%
  count(demografica_genero, nivel_involucramiento) %>%
  group_by(demografica_genero) %>%
  mutate(Porcentaje_fila = round((n / sum(n)) * 100, 1)) %>%
  ungroup()

# Cruce 2: Ciclo x Participación (b1)
cruce2 <- df %>%
  filter(!is.na(filtro_ciclo), !is.na(b1)) %>%
  # Como el ciclo tiene 10 categorías, lo agrupamos en rangos para que la tabla y el gráfico se lean mejor
  mutate(Ciclo_Agrupado = case_when(
    filtro_ciclo %in% c("2do ciclo", "3er ciclo", "4to ciclo") ~ "2do-4to ciclo",
    filtro_ciclo %in% c("5to ciclo", "6to ciclo", "7mo ciclo") ~ "5to-7mo ciclo",
    filtro_ciclo %in% c("8vo ciclo", "9no ciclo", "10mo ciclo o +") ~ "8vo-10mo ciclo"
  )) %>%
  count(Ciclo_Agrupado, b1) %>%
  group_by(Ciclo_Agrupado) %>%
  mutate(Porcentaje_fila = round((n / sum(n)) * 100, 1)) %>%
  ungroup()

# Cruce 3: Escala de pago x Calificación de manifestaciones (b3)
cruce3 <- df %>%
  filter(!is.na(demografica_escala), !is.na(b3)) %>%
  count(demografica_escala, b3) %>%
  group_by(demografica_escala) %>%
  mutate(Porcentaje_fila = round((n / sum(n)) * 100, 1)) %>%
  ungroup()

# Cruce 4: Género x Índice de apoyo (categórico)
cruce4 <- df %>%
  filter(!is.na(demografica_genero), !is.na(ind_apoyo_cat)) %>%
  count(demografica_genero, ind_apoyo_cat) %>%
  group_by(demografica_genero) %>%
  mutate(Porcentaje_fila = round((n / sum(n)) * 100, 1)) %>%
  ungroup()

# Exportamos los cruces en un solo archivo de Excel
export(list("Cruce1_Gen_Invol" = cruce1, 
            "Cruce2_Ciclo_Particip" = cruce2, 
            "Cruce3_Escala_Manif" = cruce3, 
            "Cruce4_Gen_Indice" = cruce4), 
       "D:/Metodos_Cuanti_Toma/outputs/04_bivariado.xlsx")
