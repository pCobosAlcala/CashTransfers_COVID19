### Paquetes ----
pacman::p_load(tidyverse, scales, ggalluvial, ggrepel)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 


### Data----
base <- readxl::read_excel("1_data/base.xlsx")

latam <- readxl::read_excel("1_data/latam.xlsx")

latam2 <- readxl::read_excel("1_data/latam.xlsx", 
                             sheet = "Hoja2")

latam3 <- latam2 %>% 
  group_by(country) %>% 
  summarise(cobertura_as = sum(house_coverage, na.rm = T),
            pib_as = sum(pib, na.rm = T)) 

b <- left_join(latam, latam3, by = "country")


### Cálculos ----

base %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  nrow()

base %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  count(new_program)

base %>% 
  left_join(latam, by = "country") %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  filter(region == "central america") %>% 
  count(new_program, new_registration, new_delivery)

### Gráfica 1----

## Promedios
b %>%
  filter(health != is.na(health)) %>% 
  summarise(mean = mean(health))

b %>%
  filter(health != is.na(health)) %>% 
  filter(region == "south america") %>% 
  summarise(mean = mean(health))

b %>%
  filter(health != is.na(health)) %>% 
  filter(region == "central america") %>% 
  summarise(mean = mean(health))

## Viz
b %>%
  filter(health != is.na(health)) %>% 
  mutate(signo = "%",
         porc = str_c(health, signo, sep = "")) %>% 
ggplot(aes(x = fct_reorder(country, health), y = health/100, fill = region)) + 
  geom_col(alpha = 0.9) + 
  coord_flip() + 
  geom_hline(yintercept = .571, color = "grey40", size = .5,
             linetype = "dashed") +
  geom_hline(yintercept = .606, color = "steelblue", size = .5,
             linetype = "dashed") +
  geom_hline(yintercept = .517, color = "salmon", size = .5,
             linetype = "dashed") +
  geom_text(aes(label = porc), size = 3, hjust = -.1) + 
  scale_x_discrete (labels = c("Ecuador", "Guatemala", "El Salvador", "México", "Perú", "Colombia","Argentina", "Costa Rica", "Brasil", "Uruguay")) + 
  scale_fill_manual(values = c("salmon", "steelblue"),
                                                                                labels = c("Centroamérica\ny México", "Sudamérica")) + 
  scale_y_continuous(breaks = c(0,.20,.4,.6,.8,1),
                     label = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(x = "",
       y = "\nProporción de personas asalariadas que tienen\nderecho a un seguro médico vinculado a su trabajo\n",
       fill = "Región") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(0.80, 0.2),
        panel.grid.minor.y = element_blank())


ggsave("3_viz/1.png",
       width = 6,
       height = 4,
       dpi = 600)

### Gráfica 2----

## Cálculos
b %>%
  filter(cobertura_as != is.na(cobertura_as)) %>% 
  summarise(mean = mean(cobertura_as))

b %>%
  filter(cobertura_as != is.na(cobertura_as)) %>% 
  filter(region == "central america") %>% 
  summarise(mean = mean(cobertura_as))

b %>%
  filter(cobertura_as != is.na(cobertura_as)) %>% 
  filter(region == "south america") %>% 
  summarise(mean = mean(cobertura_as))

b %>%
  filter(cobertura_as != is.na(cobertura_as)) %>% 
  filter(region == "caribe") %>% 
  summarise(mean = mean(cobertura_as))

## Viz
b %>%
  filter(cobertura_as != is.na(cobertura_as)) %>% 
  mutate(cobertura_as2 = cobertura_as/100,
         cobertura_as = round(cobertura_as, 2),
         signo = "%",
         porc = str_c(cobertura_as, signo, sep = "")) %>% 
  ggplot(aes(x = fct_reorder(country, cobertura_as2), y = cobertura_as2, fill = region)) + 
  geom_col(alpha = 0.9) + 
  coord_flip() + 
  geom_hline(yintercept = .0285, color = "khaki2", size = .5,
             linetype = "dashed") +
  geom_hline(yintercept = .229, color = "steelblue", size = .5,
             linetype = "dashed") +
  geom_hline(yintercept = .145, color = "grey40", size = .5,
             linetype = "dashed") +
  geom_hline(yintercept = .149, color = "salmon", size = .5,
             linetype = "dashed") +
  scale_x_discrete (labels = c("San Cristóbal y Nieves",
                               "Barbados",
                               "Bahamas",
                               "Venezuela",
                               "Guyana",
                               "Cuba",
                               "Haití",
                               "Belice",
                               "Guatemala",
                               "Trinidad y Tobago",
                               "El Salvador",
                               "Panamá",
                               "Perú",
                               "Ecuador",
                               "Paraguay",
                               "Jamaica",
                               "Honduras",
                               "Chile",
                               "Costa Rica",
                               "Argentina",
                               "Brasil",
                               "Colombia",
                               "México",
                               "Uruguay",
                               "República Dominicana",
                               "Bolivia")) +
  geom_text(aes(label = porc), size = 2.5, hjust = -.1,
            fontface = "bold") +
  scale_fill_manual(values = c("khaki2", "salmon", "steelblue"),
                                                                                            labels = c("Caribe", "Centroamérica y México", "Sudamérica")) +
  labs(x = "",
       y = "Cobertura de asistencia social monetaria",
       fill = "Región") + 
  scale_y_continuous(breaks = c(0,.20,.4,.6,.8,1),
                     label = percent_format(accuracy = 1),
                     limits = c(0, .65)) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(0.75, 0.2),
        panel.grid.minor.y = element_blank())

ggsave("3_viz/2.png",
       width = 6,
       height = 5,
       dpi = 600)


### Gráfica 3----

## Cálculos
base %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  filter(new_program == "yes") %>% 
  count(new_registration)


base %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  filter(new_program == "yes") %>% 
  count(periodicity, duration_months)

base %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  filter(new_program == "yes") %>% 
  count(duration_months)

## Viz
l <- latam %>% 
  select(country, region)

base %>% 
  left_join(l, by = "country") %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  count(region, new_program, new_registration, new_delivery) %>%
  rename("Novedad del programa" = new_program,
         "Uso de información previa" = new_registration,
         "Uso de mecanismos de entrega previos" = new_delivery) %>% 
  mutate(`Novedad del programa` = str_replace_all(`Novedad del programa`,
                                                  c("adaptation" = "Adaptación",
                                                    "vertical/horizontal" = "Vertical y horizontal",
                                                    "vertical" = "Exp. vertical",
                                                    "horizontal" = "Exp. horizontal",
                                                    " Horizontal" = " horizontal",
                                                    "yes" = "Nuevo programa")),
         `Uso de mecanismos de entrega previos` = str_replace_all(`Uso de mecanismos de entrega previos`,
                                                                  c("No" = "Sí",
                                                                    "Yes" = "No")),
         `Uso de información previa` = str_replace_all(`Uso de información previa`,
                                                       c("No" = "Sí",
                                                         "Yes" = "No"
                                                       )),
         region = str_replace_all(region,
                                                       c("caribbean" = "Caribe",
                                                         "central america" = "Centroamérica\ny México",
                                                         "south america" = "Sudamérica"
                                                       ))) %>% 
  ggplot(aes(axis1 = region,
             axis2 = `Uso de información previa`,
             axis3 = `Uso de mecanismos de entrega previos`,
             y = n)) + 
  geom_alluvium(aes(fill = `Novedad del programa`), aes.bind = "alluvia", alpha = 0.6) +
  geom_stratum(color = "grey0",
               fill = "grey40",
               size = 1, 
               alpha = .3) +
  geom_label(stat = "stratum",
             label.strata = T,
             infer.label = T,
             size = 3,
             alpha = 0.8,
             check_overlap = T) +
  scale_x_continuous(breaks = 1:3, labels = c("Región\n",
                                              "Uso de información\n previa para el\nregistro o selección",
                                              "Uso de\nmecanismos de\npago previos"),
                     position = "top") +
  theme_minimal() +
  labs(y = "",
       fill = "Novedad del\nprograma") + 
  scale_fill_brewer(type = "qual", palette = "Set1") + 
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")

ggsave("3_viz/3.png",
       width = 8.5,
       height = 5,
       dpi = 600)


### Gráfica 4----
g5 <- base %>% 
  filter(new_program != "adaptation") %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  mutate_at(vars(c(coverage_precovid, coverage_postcovid, amount_individual_postcovid, amount_individual_precovid)), ~replace(., is.na(.), 0)) %>% 
  mutate(coverage_postcovid = coverage_postcovid - coverage_precovid) %>% 
    mutate(diferencia = (amount_individual_postcovid - amount_individual_precovid)) %>% 
  filter(diferencia != 0) %>% 
  filter(duration_months != 99,
         duration_months != 0) %>% 
  mutate(apoyo = diferencia*duration_months,
         costo = apoyo*coverage_postcovid) %>%
  group_by(country) %>% 
  summarise(costo = sum(costo)) %>% 
  ungroup()


glimpse(b)

b %>% 
  select(country, acronym, population, region, rs_cob:pib_as) %>% 
  left_join(g5, by = "country") %>% 
  mutate(info = str_c(registro, padron, interoperabilidad, sep = ""))  %>%
  mutate(covid = costo/population) %>% 
  select(-rs_cob, -pob_rs, -registro, -padron, -interoperabilidad, -costo) %>% 
  mutate_at(vars(c(covid, rs_cob_porc)), ~replace(., is.na(.), 0)) %>% 
  filter(acronym != "ATG",
         acronym != "BHS",
         acronym != "BLZ",
         acronym != "ATG",
         acronym != "BRB",
         acronym != "CUB",
         acronym != "DMA",
         acronym != "GRD",
         acronym != "SUR",
         acronym != "GUY",
         acronym != "JAM",
         acronym != "NIC",
         acronym != "LCA",
         acronym != "VCT",
         acronym != "VEN") %>% 
  ggplot(aes(x = cobertura_as/100,
             y = covid,
             color = info)) +
  geom_point(aes(size = rs_cob_porc * 100,
                 shape = factor(info_covid)),
             alpha = 0.7) + 
  geom_text_repel(aes(label = acronym),
                  size = 2.5,
                  vjust = .5,
                  hjust = -.5,
                  segment.color = "grey40",
                  segment.alpha = 0.5,
                  show.legend = F,
                  fontface = "bold") +
  scale_color_brewer(type  = "qual",
                     palette = "Set2",
                     labels = c("\nNinguno\n",
                                "\nSólo registro social\n",
                                "\nRegistro social y\npadrón único\n",
                                "\nRegistro social,\npadrón único\ne interoperabilidad")) +
  scale_x_continuous(label = percent_format(accuracy = 2)) +
  scale_shape_discrete(label = c("No", "Sí")) + 
  theme_minimal() +
  labs(y = "Presupuesto per cápita en asistencia social\n monetaria como respuesta a COVID-19 (USD)",
       x = "Proporción de personas que viven en un\nhogar que recibe asistencia social monetaria",
       col = "",
       size = "Cobertura de\nregistro social (%)",
       shape = "Uso del sistema\npor COVID-19") +
  guides(colour = guide_legend(override.aes = list(size = 4)),
         shape = guide_legend(override.aes = list(size = 4)))

ggsave("3_viz/5.png",
       width = 7,
       height = 5.6,
       dpi = 700)