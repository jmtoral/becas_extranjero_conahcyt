
# Bibliotecas -------------------------------------------------------------


library(tidyverse)
library(ggbump)
library(showtext)
library(rcartocolor)

# Temas -------------------------------------------------------------------

showtext_auto()
font_add_google("Anton", family = "Anton")
font_add_google("Mulish", family = "Mulish")


tema <- theme(
  plot.title = element_text(size = 24, face = "bold", family = "Anton", color = "grey15"),
  plot.subtitle = element_text(size = 20, colour = "grey30", family = "Mulish"),
  text = element_text(family = "Mulish", color = "grey20"),
  plot.title.position = 'plot',
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(size = 0.1),
  axis.title.y = element_text(angle = 0, vjust = 0.5),
  axis.title = element_text(family = "Mulish", color = "grey50", size = 18),
  axis.text = element_text(family = "Mulish", size = 14),
  legend.text = element_text(family = "Mulish", size = 14),
  legend.title = element_text(family = "Mulish", size = 16),
  legend.position="bottom"
)



# Datos -----------------------------------------------------------------

noimp <- read_csv("clean_data/padron_sin_importe.csv") |> 
  mutate(nivel_estudios = case_when(
    str_detect(nivel_estudios, "DOC") ~ "DOCTORADO",
    str_detect(nivel_estudios, "MAE") ~ "MAESTRIA",
    str_detect(nivel_estudios, "ESP$") ~ "ESPECIALIDAD",
    str_detect(nivel_estudios, "EST TEC$") ~ "ESTANCIA TECNICA",
    TRUE ~ nivel_estudios
  ))

noimp |> 
  count(filename)

noimp |> filter(is.na(inicio_beca))->x


noimp |> 
  count(filename, nivel_estudios) |> 
  ggplot(aes(filename, n/1000, color = nivel_estudios)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  labs(title = "Número de personas beneficiarias de becas al extranjero",
         subtitle = "por año y nivel educativo",
         y = "Miles de  \nbeneficiarixs  ",
         x = "\n",
       caption = "Fuente: Padrón de beneficiarios CONAHCYT S190 Becas de Posgrado y Apoyos a la Calidad\n@jmtoralc") +
  theme_minimal() +
  tema +
  theme(panel.grid.major = element_line(linetype = "dotted", size = 0.5)) +
  scale_x_continuous(labels = seq(2012,2023,1),
                     breaks = seq(2012,2023,1)) +
  scale_color_manual(name = "Nivel",
                     breaks = rev(c("S/D", "LICENCIATURA","ESPECIALIDAD",
                                "ESTANCIA TECNICA","MAESTRIA", "DOCTORADO")),
                     labels= rev(c("S/D", "Licenciatura","Especialidad",
                                    "Estancia técnica","Maestría", "Doctorado")),
                     values = carto_pal(6, "Vivid"))

ggsave("graphs/beneficiarixs.png", width = 8, height = 6, dpi = 120)

# País --------------------------------------------------------------------



df <- noimp |> 
  mutate(pais = case_when(
    pais == "HOLANDA" ~ "PAISES BAJOS",
    pais == "ESTADOS UNIDOS" ~ "EEUU",
    pais == "REINO UNIDO" ~ "RU",
    TRUE ~ pais
  )) |> 
  count(filename, pais, filename) |> 
  group_by(filename) |> 
  arrange(filename, desc(n)) |>
  top_n(10) |> 
  mutate(rank = rank(n)) |> 
  ungroup()
  
ggplot(df, aes(x = filename, y = rank, color = pais)) +
  geom_bump(size = 1) +
  geom_point(aes(size = n)) +
  geom_text(data = df %>% filter(filename == min(filename)),
            aes(x = filename - 1, label = pais),
            size = 6, hjust = 0.5) +
  geom_text(data = df %>% filter(filename == max(filename)),
            aes(x = filename + 1, label = pais),
            size = 6, hjust = 0.5) +
  theme_minimal() +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(2010,2025),
                     breaks=seq(2012, 2023, 1),
                     labels=seq(2012, 2023, 1))+
  guides(color = "none") +
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
        plot.title = element_text(size = 32, face = "bold", family = "Anton", color = "grey15"),
        plot.subtitle = element_text(size = 28, colour = "grey30", family = "Mulish")
        ) +
  scale_color_manual(values = c(carto_pal(12, "Vivid"), "navyblue", "darkred", "red")) +
  scale_size(range = c(1,20), name = "Beneficiarios")+
  labs(title = "Top 10 países destino de personas beneficiarias de becas al extranjero",
       subtitle = "por año",
       y = "",
       x = "Año",
       caption = "Fuente: Padrón de beneficiarios CONAHCYT S190 Becas de Posgrado y Apoyos a la Calidad\n@jmtoralc")

ggsave("graphs/rank_pais.png", width = 12, height = 10, dpi = 120)

# Universidad -------------------------------------------------------------

df <- noimp |> 
  mutate(institucion = str_to_title(institucion)) |> 
  mutate(institucion = str_replace_all(institucion, " Of ", " of ")) |> 
  mutate(institucion = str_replace_all(institucion, " De ", " de ")) |> 
  mutate(institucion = str_replace_all(institucion, "University|Universidad|Universite", "U.")) |> 
  mutate(institucion = str_replace_all(institucion, "Technische Universitat|Technische Universiteit", "TU.")) |> 
  count(filename, institucion, pais) |> 
  filter(filename >= 2012 & filename <= 2018) |> 
  group_by(filename) |> 
  arrange(filename, desc(n)) |>
  top_n(20) |> 
  mutate(rank = rank(n, ties.method = "first")) |> 
  ungroup() |> 
  mutate(institucion = str_wrap(institucion, 20))

ggplot(df, aes(x = filename, y = rank, color = institucion, group = institucion)) +
  geom_bump(size = 1) +
  geom_point(aes(size = n)) +
  geom_text(data = df %>% filter(filename == min(filename)),
            aes(x = filename - 1, label = institucion),
            size = 6, hjust = 0.5, lineheight = 0.5) +
  geom_text(data = df %>% filter(filename == max(filename)),
            aes(x = filename + 1, label = institucion),
            size = 6, hjust = 0.5, lineheight = 0.5) +
  theme_minimal() +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(2010,2025),
                     breaks=seq(2012, 2018, 1),
                     labels=seq(2012, 2018, 1))+
  guides(color = "none") +
  theme(plot.margin = margin(1,1,1.5,1.2, "cm")) +
  # scale_color_manual(values = c(carto_pal(12, "Vivid"),
  #                               carto_pal(12, "Prism"),
  #                               carto_pal(12, "Pastel"),
  #                               carto_pal(12, "Safe"))) +
  scale_size(range = c(1,10), name = "Beneficiarios")+
  labs(title = "Top 20 instituciones destino de personas beneficiarias de becas al extranjero",
       subtitle = "por año",
       y = "",
       x = "Año",
       caption = "Fuente: Padrón de beneficiarios CONAHCYT S190 Becas de Posgrado y Apoyos a la Calidad\n@jmtoralc")

ggsave("graphs/rank_instituciones_1.png", width = 12, height = 12, dpi = 180)

df <- noimp |> 
  mutate(institucion = str_to_title(institucion)) |> 
  mutate(institucion = str_replace_all(institucion, " Of ", " of ")) |> 
  mutate(institucion = str_replace_all(institucion, " De ", " de ")) |> 
  mutate(institucion = str_replace_all(institucion, "University|Universidad|Universite", "U.")) |> 
  mutate(institucion = str_replace_all(institucion, "Technische Universitat|Technische Universiteit", "TU.")) |> 
  mutate(institucion = str_replace_all(institucion, 
                                       "Galilee International Management Institute", 
                                       "Galilee Intl. Mgmnt. Inst.")) |> 
  mutate(institucion = case_when(
    str_detect(institucion, "Organizacion Iberoamericana") ~ "Organizacion Iberoamericana de Seguridad Social",
    TRUE ~ institucion
  )) |> 
  count(filename, institucion, pais) |> 
  filter(filename >= 2018) |> 
  group_by(filename) |> 
  arrange(filename, desc(n)) |>
  top_n(20) |> 
  mutate(rank = rank(n, ties.method = "first")) |> 
  ungroup() |> 
  mutate(institucion = str_wrap(institucion, 15))

ggplot(df, aes(x = filename, y = rank, color = institucion, group = institucion)) +
  geom_bump(size = 1) +
  geom_point(aes(size = n)) +
  geom_text(data = df %>% filter(filename == min(filename)),
            aes(x = filename - 1, label = institucion),
            size = 6, hjust = 0.2, lineheight = 0.5) +
  geom_text(data = df %>% filter(filename == max(filename)),
            aes(x = filename + 1, label = institucion),
            size = 6, hjust = 0.8, lineheight = 0.5) +
  theme_minimal() +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(2017,2024),
                     breaks=seq(2018, 2023, 1),
                     labels=seq(2018, 2023, 1))+
  guides(color = "none") +
  #theme(plot.margin = margin(1,1,1.5,1.2, "cm")) +
   scale_color_manual(values = c("red", "red",carto_pal(12, "Vivid"),
                                 carto_pal(12, "Prism"),
                                 carto_pal(12, "Safe"))) +
  scale_size(range = c(1,10), name = "Beneficiarios")+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
        plot.title = element_text(size = 32, face = "bold", family = "Anton", color = "grey15"),
        plot.subtitle = element_text(size = 28, colour = "grey30", family = "Mulish")
  ) +
  labs(title = "Top 20 instituciones destino de personas beneficiarias de becas al extranjero",
       subtitle = "por año",
       y = "",
       x = "Año",
       caption = "Fuente: Padrón de beneficiarios CONAHCYT S190 Becas de Posgrado y Apoyos a la Calidad\n@jmtoralc")

ggsave("graphs/rank_instituciones_2.png", width = 12, height = 12, dpi = 120)


# Áreas del conocimiento --------------------------------------------------


noimp |> 
  filter(!area %in% c("VI. CIENCIAS SOCIALES", "-")) |> 
  count(filename, area)|> 
  ggplot(aes(filename, n/1000, color = area)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  labs(title = "Número de personas beneficiarias de becas al extranjero",
       subtitle = "por año y área del conocimiento",
       y = "Miles de  \nbeneficiarixs  ",
       x = "\n",
       caption = "Fuente: Padrón de beneficiarios CONAHCYT S190 Becas de Posgrado y Apoyos a la Calidad\n@jmtoralc") +
  theme_minimal() +
  tema +
  theme(panel.grid.major = element_line(linetype = "dotted", size = 0.5)) +
  scale_x_continuous(labels = seq(2012,2023,1),
                     breaks = seq(2012,2023,1)) +
  scale_color_manual(name = "Área",
                     values = carto_pal(7, "Bold"))+
  guides(color = guide_legend(ncol=2))

ggsave("graphs/areas.png", width = 8, height = 6, dpi = 120)



# Programas ---------------------------------------------------------------



df <- noimp |> 
  mutate(programa = case_when(
    str_detect(programa, "BUSINESS ADMIN|MBA") ~ "MBA",
    str_detect(programa, "EPIDEMIO") ~ "EPIDEMIOLOGÍA",
    str_detect(programa, "DIRECCION Y GESTION|DIRECCIÓN Y GESTION|DIRECCIÓN Y GESTIÓN|GESTIÓN EN SISTEMAS DE SALUD|GESTION EN SISTEMAS DE SALUD") ~ "Gestión de Servicios de Salud",
    TRUE ~ programa
  )) |> 
  mutate(programa = str_to_title(programa)) |> 
  mutate(programa = str_replace(programa, "Mba", "MBA")) |> 
  count(filename, programa) |> 
  filter(filename >= 2018) |> 
  group_by(filename) |> 
  arrange(filename, desc(n)) |>
  top_n(20) |> 
  mutate(rank = rank(n, ties.method = "first")) |> 
  ungroup() |> 
  mutate(programa = str_wrap(programa, 15))

ggplot(df, aes(x = filename, y = rank, color = programa)) +
  geom_bump(size = 1) +
  geom_point(aes(size = n)) +
  geom_text(data = df %>% filter(filename == min(filename)),
            aes(x = filename - 1, label = programa),
            size = 6, hjust = 0.2, lineheight = 0.5) +
  geom_text(data = df %>% filter(filename == max(filename)),
            aes(x = filename + 1, label = programa),
            size = 6, hjust = 0.8, lineheight = 0.5) +
  theme_minimal() +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(2017,2024),
                     breaks=seq(2018, 2023, 1),
                     labels=seq(2018, 2023, 1))+
  guides(color = "none") +
  #theme(plot.margin = margin(1,1,1.5,1.2, "cm")) +
  scale_color_manual(values = c("red", "orange",carto_pal(12, "Vivid"),
                                carto_pal(12, "Prism"),
                                carto_pal(12, "Safe"),
                                carto_pal(12, "Pastel"),
                                "blue", "darkred", "gold")) +
  scale_size(range = c(1,10), name = "Beneficiarios")+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
        plot.title = element_text(size = 32, face = "bold", family = "Anton", color = "grey15"),
        plot.subtitle = element_text(size = 28, colour = "grey30", family = "Mulish")
  ) +
  labs(title = "Top 10 programas de estudio de personas beneficiarias de becas al extranjero",
       subtitle = "por año",
       y = "",
       x = "Año",
       caption = "Fuente: Padrón de beneficiarios CONAHCYT S190 Becas de Posgrado y Apoyos a la Calidad\n@jmtoralc")

ggsave("graphs/rank_programas.png", width = 12, height = 13, dpi = 120)

