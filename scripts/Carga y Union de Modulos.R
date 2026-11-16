#===========================================================================================
#Proyecto: Percepción de la Toma de Dintilhac
#Objetivo de este script: Carga y unión de modulos
#Fecha: 14-11-2026
#==========================================================================================


#1. Carga de librerias--------------------------------------------------------
library(rio)
library(tidyverse)
library(janitor)
renv::snapshot()


#2. Importar datos------------------------------------------------------------

mod1 <- import("D:/Metodos_Cuanti_Toma/datos/crudos/test_survey.sav")
mod2 <- import("D:/Metodos_Cuanti_Toma/datos/crudos/listaB1.sav")
mod3 <- import("D:/Metodos_Cuanti_Toma/datos/crudos/percepcion_movimiento.sav")
mod4 <- import("D:/Metodos_Cuanti_Toma/datos/crudos/percepcion_sobre_actores.sav")
mod5 <- import("D:/Metodos_Cuanti_Toma/datos/crudos/e1.sav")

#3. Procesamiento de Módulos-------------------------------------------------
#Pasar a largo, de filas a columnas.
# 3.1 Emociones (mod5): 1 = Sí, 2 = No (se convierte a 1 y 0)
mod5_wide <- mod5 %>%
  mutate(emocion_dummy = ifelse(e1_1 == 1, 1, 0)) %>%
  select(interview__key, e1__id, emocion_dummy) %>%
  pivot_wider(names_from = e1__id, values_from = emocion_dummy,
              names_prefix = "emocion_", values_fill = 0) %>%
  distinct(interview__key, .keep_all = TRUE)

# 3.2 Percepción del movimiento (mod3): Escala 1-5
mod3_wide <- mod3 %>%
  select(interview__key, percepcion_movimiento__id, p1) %>%
  pivot_wider(names_from = percepcion_movimiento__id, values_from = p1,
              names_prefix = "per_mov_") %>%
  distinct(interview__key, .keep_all = TRUE)

# 3.3 Percepción de actores (mod4): Escala 1-5
mod4_wide <- mod4 %>%
  select(interview__key, percepcion_sobre_actores__id, c1) %>%
  pivot_wider(names_from = percepcion_sobre_actores__id, values_from = c1,
              names_prefix = "actores_") %>%
  distinct(interview__key, .keep_all = TRUE)

# 3.4 Acciones (mod2): Escala 1-5
mod2_wide <- mod2 %>%
  select(interview__key, listaB1__id, b2) %>%
  pivot_wider(names_from = listaB1__id, values_from = b2,
              names_prefix = "accion_") %>%
  distinct(interview__key, .keep_all = TRUE)


#4. Union de Bases-------------------------------------------------------

df_final <- mod1 %>%
  left_join(mod5_wide, by = "interview__key") %>%
  left_join(mod3_wide, by = "interview__key") %>%
  left_join(mod4_wide, by = "interview__key") %>%
  left_join(mod2_wide, by = "interview__key") %>%
  filter(filtro_participacion == 1 & 
           filtro_carrera == 1 & 
           filtro_conocimiento == 1 & 
           filtro_ciclo >= 2)
