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

#5. Recodificación-------------------------------------------------
# Convertimos "No sabe/No opina" a NA (d2=99, f1=9, escala=10)
df_final <- df_final %>%
  mutate(
    d2 = na_if(d2, 99),
    f1 = na_if(f1, 9),
    demografica_escala = na_if(demografica_escala, 10)
  )

# Factorizamos variables categóricas
df_final <- df_final %>%
  mutate(
    demografica_genero = factor(demografica_genero, levels = c(1, 2, 3, 4), 
                                labels = c("Masculino", "Femenino", "No binario", "Otro")),
    demografica_escala = factor(demografica_escala, levels = c(1:9), 
                                labels = c("G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9")),
    demografica_especialidad = factor(demografica_especialidad, levels = c(1:15),
                                      labels = c("Física", "Matemáticas", "Química", "Ingeniería Ambiental", 
                                                 "Ingeniería Biomédica", "Ingeniería Civil", "Ingeniería de Telecom", 
                                                 "Ingeniería de Minas", "Ingeniería Electrónica", "Ingeniería Geológica", 
                                                 "Ingeniería Industrial", "Ingeniería Informática", "Ingeniería Mecánica", 
                                                 "Ingeniería Mecatrónica", "Ingeniería Química")),
    filtro_ciclo = factor(filtro_ciclo, levels = c(1:10),
                          labels = c("1er ciclo", "2do ciclo", "3er ciclo", "4to ciclo", "5to ciclo", 
                                     "6to ciclo", "7mo ciclo", "8vo ciclo", "9no ciclo", "10mo ciclo")),
    nivel_involucramiento = factor(nivel_involucramiento, levels = c(2, 3, 4), 
                                   labels = c("Comentarios superficiales", "Revisó comunicados", "Siguió activamente")),
    b1 = factor(b1, levels = c(1:5), 
                labels = c("Espectador", "Compartió en redes", "Asambleas", "Marchas/plantones", "Rechazo público")),
    b3 = factor(b3, levels = c(1:4), 
                labels = c("Sin enfrentamientos", "Tensión verbal", "Agresiones/Daños", "Prefiere no opinar")),
    # OJO: En tu str() la variable C3 está en mayúscula
    C3 = factor(C3, levels = c(1:5), 
                labels = c("Ninguna", "Poca", "Moderada", "Bastante", "Total")),
    d1 = factor(d1, levels = c(1:5), 
                labels = c("Muy eficaz", "Eficaz", "Neutral", "Poco eficaz", "Nada eficaz")),
    d2 = factor(d2, levels = c(1:5), 
                labels = c("Ningún diálogo", "Poco diálogo", "Ni mucho ni poco", "Diálogo regular", "Mucho diálogo")),
    e2 = factor(e2, levels = c(1:5), 
                labels = c("Ninguno", "Bajo", "Moderado", "Alto", "Muy alto")),
    f1 = factor(f1, levels = c(1:8), 
                labels = c("Presión institucional", "Denuncias en redes", "Movilizaciones internas", 
                           "Plantones", "Tomas de pista", "Toma de edificios", "Toma de la universidad", "Ninguna"))
  ) %>% droplevels()

#6. PROTO INDICADOR
#INDICE DE APOYO A LA MOVILIZACIÓN
# Promediamos 3 ítems. Como 1=Muy de acuerdo y 5=Muy en desacuerdo, invertimos (6-x) 
# para que un mayor puntaje signifique mayor apoyo.
df_final <- df_final %>%
  mutate(
    inv_per_mov_1 = 6 - as.numeric(per_mov_1),
    inv_per_mov_2 = 6 - as.numeric(per_mov_2),
    inv_actores_4 = 6 - as.numeric(actores_4)
  ) %>%
  mutate(
    ind_apoyo_mov = rowMeans(select(., inv_per_mov_1, inv_per_mov_2, inv_actores_4), na.rm = TRUE),
    ind_apoyo_cat = cut(ind_apoyo_mov, breaks = c(-Inf, 2.4, 3.6, Inf), 
                        labels = c("En desacuerdo", "Neutral", "De acuerdo"))
  )

# Agregamos etiquetas a las nuevas variables
attr(df_final$ind_apoyo_mov, "label") <- "Índice de apoyo a la movilización (1-5)"
attr(df_final$ind_apoyo_cat, "label") <- "Nivel de apoyo a la movilización"

#7. Exportar
export(df_final, "D:/Metodos_Cuanti_Toma/datos/procesados/DINTILHAC_procesada.rds")

