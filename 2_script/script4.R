### Paquetes ----
pacman::p_load(tidyverse, scales, ggrepel)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 


### Data----
latam <- readxl::read_excel("1_data/latam.xlsx")

latam2 <- readxl::read_excel("1_data/latam.xlsx", 
                    sheet = "Hoja2")


### Manipulation----

glimpse(latam2)

latam3 <- latam2 %>% 
  group_by(country) %>% 
  summarise(cobertura_as = sum(house_coverage, na.rm = T),
            pib_as = sum(pib, na.rm = T)) 

b <- left_join(latam, latam3, by = "country")


### Viz----

# Seguridad social
b %>%
  mutate_at(vars(c(si, other_si)), ~replace(., is.na(.), 0)) %>% 
  mutate(si = (si + other_si)) %>%
  ggplot(aes(x = fct_reorder(country, -si), y = si, fill = cluster)) + 
  geom_col() + 
  coord_flip() + 
  scale_fill_brewer(type = "qual", palette = "Set3") + 
  geom_text(aes(label = si), size = 2.5, hjust = -.1) + 
  labs(x = "País",
       y = "% población con seguridad social",
       fill = "Cluster") + 
  theme_minimal()

ggsave("3_viz/27.1.png",
       width = 6,
       height = 4,
       dpi = 600)

glimpse(b)

#  Sin prtección social
b %>%
  ggplot(aes(x = fct_reorder(country, -not_receiving), y = not_receiving, fill = cluster)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = not_receiving), size = 2.5, hjust = -.1) + 
  scale_fill_brewer(type = "qual", palette = "Set3") + 
  
  labs(x = "País",
       y = "% población sin protección social",
       fill = "Cluster") + 
  theme_minimal()

ggsave("3_viz/27.2.png",
       width = 6,
       height = 4,
       dpi = 600)


# Asistencia social (WB)
b %>%
  ggplot(aes(x = fct_reorder(country, -sa), y = sa, fill = cluster)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = sa), size = 2.5, hjust = -.1) + 
  scale_fill_brewer(type = "qual", palette = "Set3") + 
  
  labs(x = "País",
       y = "% población con asistencia social (Banco Mundial)",
       fill = "Cluster") + 
  theme_minimal()

ggsave("3_viz/27.3.png",
       width = 6,
       height = 4,
       dpi = 600)


# Asistencia social (Yo)
b %>%
  ggplot(aes(x = fct_reorder(country, -cobertura_as), y = cobertura_as, fill = cluster)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = cobertura_as), size = 2.5, hjust = -.1) +   scale_fill_brewer(type = "qual", palette = "Set3") + 

  labs(x = "País",
       y = "% población con asistencia social monetaria (CEPAL)",
       fill = "Cluster") + 
  theme_minimal()

ggsave("3_viz/27.4.png",
       width = 6,
       height = 4,
       dpi = 600)


# Asistencia social (Yo)
b %>%
  ggplot(aes(x = fct_reorder(country, -pib_as), y = pib_as, fill = cluster)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = pib_as), size = 2.5, hjust = -.1) +   scale_fill_brewer(type = "qual", palette = "Set3") + 
  
  labs(x = "País",
       y = "Gasto en asistencia social monetaria\ncomo % del PIB (CEPAL)",
       fill = "Cluster") + 
  theme_minimal()

ggsave("3_viz/27.4.1.png",
       width = 6,
       height = 4,
       dpi = 600)

### Segunda parte----

##
b %>%
  mutate_at(vars(c(si, other_si)), ~replace(., is.na(.), 0)) %>% 
  mutate(si = (si + other_si)) %>%
  ggplot(aes(x = si, y = cobertura_as, color = cluster, group = 1)) +
  geom_point(size = 2.5,
             alpha = 0.9) +
  geom_text_repel(aes(label = acronym, color = cluster),
                  size = 2.5,
                  hjust = -0.5) +
  theme_minimal() +
  scale_color_brewer(type = "qual", palette = "Set2", direction = -1) + 
  labs(title = "Protección social en regímenes de bienestar, Martínez (2008)",
       x = "Cobertura de seguridad social (%), Banco Mundial",
       y = "Cobertura de sistencia social monetaria (%), CEPAL",
       color = "Conglomerado")

ggsave("3_viz/27.5.png",
       width = 6,
       height = 4,
       dpi = 600)

##
b %>%
  mutate_at(vars(c(si, other_si)), ~replace(., is.na(.), 0)) %>% 
  mutate(si = (si + other_si)) %>%
  ggplot(aes(x = si, y = cobertura_as,group = 1)) +
  geom_point(aes(color = cluster),size = 2.5,
             alpha = 0.8) +
  geom_text_repel(aes(label = acronym),
                  size = 2.5,
                  hjust = -0.5,
                  color = "grey40",
                  segment.alpha = 0.4) +
  theme_minimal() +
  facet_wrap(~cluster, ncol = 2) +
  theme_light() +
  scale_color_brewer(type = "qual", palette = "Dark2") + 
  labs(title = "Protección social en regímenes de bienestar, Martínez (2008)",
       x = "Cobertura de seguridad social (%), Banco Mundial",
       y = "Cobertura de sistencia social monetaria (%), CEPAL",
       color = "Conglomerado")

ggsave("3_viz/27.6.png",
       width = 6,
       height = 4,
       dpi = 600)


##
b %>%
  mutate_at(vars(c(si, other_si)), ~replace(., is.na(.), 0)) %>% 
  mutate(si = (si + other_si)) %>%
  ggplot(aes(x = si, y = cobertura_as, color = cluster, group = 1)) +
  geom_point(aes(color = cluster),size = 2.5,
             alpha = 0.8) +
  geom_text_repel(aes(label = acronym),
                  size = 2.5,
                  hjust = -0.5,
                  color = "grey40",
                  segment.alpha = 0.4) +
  theme_minimal() +
  facet_wrap(~cluster, ncol = 2, scales = "free") +
  theme_light() +
  scale_color_brewer(type = "qual", palette = "Dark2") + 
  labs(title = "Protección social en regímenes de bienestar, Martínez (2008)",
       x = "Cobertura de seguridad social (%), Banco Mundial",
       y = "Cobertura de sistencia social monetaria (%), CEPAL",
       color = "Conglomerado")

ggsave("3_viz/27.7.png",
       width = 6,
       height = 4,
       dpi = 600)



###

# Asistencia social (Yo)
b %>%
  ggplot(aes(x = fct_reorder(country, -cobertura_as), y = cobertura_as, fill = cluster)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = cobertura_as), size = 2.5, hjust = -.1) +   scale_fill_brewer(type = "qual", palette = "Set3") + 
  
  labs(x = "País",
       y = "% población con asistencia social monetaria (CEPAL)",
       fill = "Cluster") + 
  theme_minimal()

ggsave("3_viz/27.4.png",
       width = 6,
       height = 4,
       dpi = 600)

glimpse(b)
### Gráficas 31 agosto ----
# Asistencia social (Yo)
b %>%
  ggplot(aes(x = fct_reorder(country, -cobertura_as), y = cobertura_as, fill = region)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = cobertura_as), size = 2.5, hjust = -.1) +   scale_fill_brewer(type = "qual", palette = "Set3",
                                                                                labels = c("Caribe", "Centroamérica", "Sudamérica")) + 
  labs(x = "País",
       y = "% población que vive en algún hogar que\nrecibe asistencia social monetaria (CEPAL)",
       fill = "Región") + 
  scale_y_continuous(limits = c(0, 65)) +
  theme_minimal()

ggsave("3_viz/27.8.png",
       width = 6,
       height = 4,
       dpi = 600)

## 

b %>%
  ggplot(aes(x = fct_reorder(country, -informality), y = informality, fill = region)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = informality), size = 2.5, hjust = -.1) +   scale_fill_brewer(type = "qual", palette = "Set3") + 
  
  labs(x = "País",
       y = "% Informalidad. Asalariados sin derecho a seguridad social (CEDLAS)",
       fill = "Región") + 
  scale_fill_brewer(type = "qual", palette = "Set3",
                    labels = c("Caribe", "Centroamérica", "Sudamérica")) + 
  theme_minimal()

ggsave("3_viz/27.9.png",
       width = 6,
       height = 4,
       dpi = 600)

## 

b %>%
  ggplot(aes(x = fct_reorder(country, -pensions), y = pensions, fill = region)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = pensions), size = 2.5, hjust = -.1) +   scale_fill_brewer(type = "qual", palette = "Set3") + 
  
  labs(x = "País",
       y = "% Informalidad. Asalariados sin derecho a seguridad social (CEDLAS)",
       fill = "Región") + 
  scale_fill_brewer(type = "qual", palette = "Set3",
                    labels = c("Caribe", "Centroamérica", "Sudamérica")) + 
  theme_minimal()

ggsave("3_viz/27.9.1.png",
       width = 6,
       height = 4,
       dpi = 600)


## 
b %>%
  ggplot(aes(x = fct_reorder(country, -pensions), y = pensions, fill = region)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = pensions), size = 2.5, hjust = -.1) +  scale_fill_brewer(type = "qual", palette = "Set3",
                                                                                 labels = c("Caribe", "Centroamérica", "Sudamérica")) + 
  
  labs(x = "País",
       y = "% asalariados con derecho a pensión cuando se retiren (CEDLAS)",
       fill = "Región") + 
  theme_minimal()

ggsave("3_viz/27.9.1.png",
       width = 6,
       height = 4,
       dpi = 600)

## Buena 1----

b %>%
  filter()
  ggplot(aes(x = fct_reorder(country, -health), y = health, fill = region)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = health), size = 2.5, hjust = -.1) +   scale_fill_brewer(type = "qual", palette = "Set3",
                                                                                labels = c("Caribe", "Centroamérica", "Sudamérica")) + 
  
  labs(x = "País",
       y = "% asalariados con derecho a salud\nligado a su trabajo (CEDLAS)",
       fill = "Región") + 
  theme_minimal()

ggsave("3_viz/27.10.png",
       width = 6,
       height = 4,
       dpi = 600)

## 

b %>%
  filter(region == "caribe") %>% 
  ggplot(aes(x = fct_reorder(country, -cobertura_ss_caribe_ciss), y = cobertura_ss_caribe_ciss)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = cobertura_ss_caribe_ciss), size = 2.5, hjust = -.1) +
  scale_fill_brewer(type = "qual", palette = "Set3",
                                                                                                labels = c("Caribe", "Centroamérica", "Sudamérica")) +  
  
  labs(x = "País",
       y = "% cobertura de seguridad social en el Caribe (CISS)",
       fill = "Región") + 
  theme_minimal()

ggsave("3_viz/27.11.png",
       width = 6,
       height = 4,
       dpi = 600)

## 

b %>%
  ggplot(aes(x = fct_reorder(country, -receiving_pensions_ilo_above_age), y = receiving_pensions_ilo_above_age, fill = region)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = receiving_pensions_ilo_above_age), size = 2.5, hjust = -.1) + scale_fill_brewer(type = "qual", palette = "Set3",
                                                                                                        labels = c("Caribe", "Centroamérica", "Sudamérica")) +  
  scale_y_continuous(limits = c(0, 110)) +
  labs(x = "País",
       y = "% de personas por encima de la edad de retiro que reciben pensión (OIT)",
       fill = "Región") + 
  theme_minimal()

ggsave("3_viz/27.12.png",
       width = 6,
       height = 4,
       dpi = 600)

## 

b %>%
  ggplot(aes(x = fct_reorder(country, -cepal_sp_pib), y = cepal_sp_pib, fill = region)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = cepal_sp_pib), size = 2.5, hjust = -.1) +   scale_fill_brewer(type = "qual", palette = "Set3",
                                                                                      labels = c("Caribe", "Centroamérica", "Sudamérica")) + 
  labs(x = "País",
       y = "Gasto del gobierno central en protección social como % del PIB (CEPAL)",
       fill = "Región") + 
  scale_y_continuous(limits = c(0, 14)) +
  theme_minimal()

ggsave("3_viz/27.13.png",
       width = 6,
       height = 4,
       dpi = 600)


### Segunda parte----

##
b %>%
  mutate_at(vars(c(si, other_si)), ~replace(., is.na(.), 0)) %>% 
  mutate(si = (si + other_si)) %>%
  ggplot(aes(x = si, y = cobertura_as, color = cluster, group = 1)) +
  geom_point(size = 2.5,
             alpha = 0.9) +
  geom_text_repel(aes(label = acronym, color = cluster),
                  size = 2.5,
                  hjust = -0.5) +
  theme_minimal() +
  scale_color_brewer(type = "qual", palette = "Set2", direction = -1) + 
  labs(title = "Protección social en regímenes de bienestar, Martínez (2008)",
       x = "Cobertura de seguridad social (%), Banco Mundial",
       y = "Cobertura de sistencia social monetaria (%), CEPAL",
       color = "Conglomerado")

ggsave("3_viz/27.5.png",
       width = 6,
       height = 4,
       dpi = 600)



##
b %>%
  mutate_at(vars(c(si, other_si)), ~replace(., is.na(.), 0)) %>% 
  mutate(si = (si + other_si)) %>%
  ggplot(aes(x = si, y = cobertura_as, color = cluster, group = 1)) +
  geom_point(size = 2.5,
             alpha = 0.9) +
  geom_text_repel(aes(label = acronym, color = cluster),
                  size = 2.5,
                  hjust = -0.5) +
  theme_minimal() +
  scale_color_brewer(type = "qual", palette = "Set2", direction = -1) + 
  labs(title = "Protección social en regímenes de bienestar, Martínez (2008)",
       x = "Cobertura de seguridad social (%), Banco Mundial",
       y = "Cobertura de sistencia social monetaria (%), CEPAL",
       color = "Conglomerado")

ggsave("3_viz/27.5.png",
       width = 6,
       height = 4,
       dpi = 600)



## 

b %>%
  ggplot(aes(x = fct_reorder(country, -ciss_asterisco), y = ciss_asterisco, fill = region)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = ciss_asterisco), size = 2.5, hjust = -.1) +   scale_fill_brewer(type = "qual", palette = "Set3",
                                                                                      labels = c("Caribe", "Centroamérica", "Sudamérica")) + 
  labs(x = "País",
       y = "% cobertura (CISS)",
       fill = "Región") + 
  # scale_y_continuous(limits = c(0, 14)) +
  theme_minimal()

ggsave("3_viz/27.14.png",
       width = 6,
       height = 4,
       dpi = 600)

