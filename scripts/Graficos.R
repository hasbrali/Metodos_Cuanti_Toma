#===========================================================================================
#Proyecto: Percepción de la Toma de Dintilhac
#Objetivo de este script: Graficos
#Fecha: 14-11-2026
#==========================================================================================

library(tidyverse)
library(rio)
library(ggplot2)

df <- import("D:/Metodos_Cuanti_Toma/datos/procesados/DINTILHAC_procesada.rds")

#1. GRÁFICOS UNIVARIADOS


# 1.1 Distribución de Género
p1 <- df %>%
  filter(!is.na(demografica_genero)) %>%
  count(demografica_genero) %>%
  mutate(Porcentaje = (n / sum(n)) * 100) %>%
  ggplot(aes(x = demografica_genero, y = Porcentaje)) +
  geom_col(fill = "#0072B2", width = 0.6) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5, size = 3.5) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = "Distribución de la población por género",
       x = "Género", y = "Porcentaje (%)",
       caption = "Fuente: Encuesta DINTILHAC 2026") +
  theme_minimal()

ggsave("D:/Metodos_Cuanti_Toma/outputs/01_barras_genero.png", plot = p1, width = 6, height = 4, bg = "white")

# 1.2 Nivel de involucramiento
p2 <- df %>%
  filter(!is.na(nivel_involucramiento)) %>%
  count(nivel_involucramiento) %>%
  mutate(Porcentaje = (n / sum(n)) * 100) %>%
  ggplot(aes(x = reorder(nivel_involucramiento, -Porcentaje), y = Porcentaje)) +
  geom_col(fill = "#009E73", width = 0.6) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5, size = 3.5) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = "Nivel de información sobre el conflicto de escalas",
       x = "Nivel de involucramiento", y = "Porcentaje (%)",
       caption = "Fuente: Encuesta DINTILHAC 2026") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("D:/Metodos_Cuanti_Toma/outputs/02_barras_involucramiento.png", plot = p2, width = 7, height = 4, bg = "white")

# 1.3 Histograma del Índice de apoyo
p3 <- df %>%
  filter(!is.na(ind_apoyo_mov)) %>%
  ggplot(aes(x = ind_apoyo_mov)) +
  geom_histogram(bins = 10, fill = "#D55E00", color = "white") +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(title = "Distribución del Índice de apoyo a la movilización (1-5)",
       x = "Índice (Mayor valor = Mayor apoyo)", y = "Frecuencia (n)",
       caption = "Fuente: Encuesta DINTILHAC 2026") +
  theme_minimal()

ggsave("D:/Metodos_Cuanti_Toma/outputs/03_histograma_indice.png", plot = p3, width = 6, height = 4, bg = "white")

#2. GRÁFICOS BIVARIADOS


# 2.1 Género x Nivel de apoyo (Barras apiladas al 100%)

p4 <- df %>%
  filter(!is.na(demografica_genero), !is.na(ind_apoyo_cat)) %>%
  count(demografica_genero, ind_apoyo_cat) %>%
  group_by(demografica_genero) %>%
  mutate(Porcentaje = (n / sum(n))) %>%
  ungroup() %>%
  ggplot(aes(x = demografica_genero, y = Porcentaje, fill = ind_apoyo_cat)) +
  geom_col(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("En desacuerdo" = "tomato", 
                               "Neutral" = "gold", 
                               "De acuerdo" = "forestgreen")) +
  labs(title = "Nivel de apoyo a la movilización según género",
       x = "Género", y = "Porcentaje (%)", fill = "Nivel de apoyo",
       caption = "Fuente: Encuesta DINTILHAC 2026") +
  theme_minimal()

ggsave("D:/Metodos_Cuanti_Toma/outputs/04_bivariado_genero_apoyo.png", plot = p4, width = 7, height = 4, bg = "white")

# 2.2 Ciclo x Nivel de participación (Barras agrupadas)
# Usamos la paleta "Set1" que trae colores vivos (rojo, azul, verde)
p5 <- df %>%
  filter(!is.na(filtro_ciclo), !is.na(b1)) %>%
  mutate(Ciclo_Agrupado = case_when(
    filtro_ciclo %in% c("2do ciclo", "3er ciclo", "4to ciclo") ~ "2do-4to ciclo",
    filtro_ciclo %in% c("5to ciclo", "6to ciclo", "7mo ciclo") ~ "5to-7mo ciclo",
    filtro_ciclo %in% c("8vo ciclo", "9no ciclo", "10mo ciclo o +") ~ "8vo-10mo ciclo"
  )) %>%
  count(Ciclo_Agrupado, b1) %>%
  group_by(Ciclo_Agrupado) %>%
  mutate(Porcentaje = (n / sum(n)) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = b1, y = Porcentaje, fill = Ciclo_Agrupado)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_fill_brewer(palette = "Set1") + # Paleta clásica de colores vivos
  labs(title = "Participación en movilizaciones según ciclo de estudios",
       x = "Nivel de participación", y = "Porcentaje (%)", fill = "Ciclo",
       caption = "Fuente: Encuesta DINTILHAC 2026") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("D:/Metodos_Cuanti_Toma/outputs/05_bivariado_ciclo_participacion.png", plot = p5, width = 8, height = 4, bg = "white")
