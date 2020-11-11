### Paquetes ----
pacman::p_load(tidyverse, scales, ggalluvial, ggrepel)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Data----
base <- readxl::read_excel("1_data/db.xlsx", sheet = "responses_covid")

latam <- readxl::read_excel("1_data/db.xlsx", sheet = "countries")

latam2 <- readxl::read_excel("1_data/db.xlsx", sheet = "social_assistance_precovid")


latam3 <- latam2 %>% 
  group_by(country) %>% 
  summarise(cobertura_as = sum(house_coverage, na.rm = T),
            pib_as = sum(pib, na.rm = T)) 

b <- left_join(latam, latam3, by = "country")


### Calculations ----

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

base %>% 
  left_join(latam, by = "country") %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  group_by(new_program, new_registration, new_delivery) %>% 
  mutate(a = n_distinct(country)) %>% 
  ungroup() %>% 
  count(new_program, new_registration, new_delivery, a)


### Graph 1: Social assistance coverage - ECLAC----

## Calcutations
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

## Visualization in Spanish

b %>%
  filter(cobertura_as != is.na(cobertura_as)) %>% 
  mutate(cobertura_as2 = cobertura_as/100,
         cobertura_as = round(cobertura_as, 1),
         signo = "%",
         porc = str_c(cobertura_as, signo, sep = "")) %>% 
  ggplot(aes(x = fct_reorder(country, cobertura_as2), y = cobertura_as2, fill = region)) + 
  geom_col(alpha = 0.9) + 
  coord_flip() + 
  geom_hline(yintercept = .0285, color = "khaki2", size = .7,
             linetype = "dashed") +
  geom_hline(yintercept = .229, color = "steelblue4", size = .7,
             linetype = "dashed") +
  geom_hline(yintercept = .145, color = "grey40", size = .7,
             linetype = "dashed") +
  geom_hline(yintercept = .149, color = "salmon3", size = .7,
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
  scale_fill_manual(values = c("khaki2", "salmon3", "steelblue4"),
                                                                                            labels = c("Caribe", "Centroamérica y México", "Sudamérica")) +
  labs(x = "",
       y = "Proporción de personas que viven en un hogar\nque recibe un programa de transferencia monetaria",
       fill = "Región") + 
  scale_y_continuous(breaks = c(0,.20,.4,.6,.8,1),
                     label = percent_format(accuracy = 1),
                     limits = c(0, .65)) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(0.75, 0.2),
        panel.grid.minor.y = element_blank())

ggsave("3_viz/graph1.esp.png",
       width = 160,
       height = 100,
       units = "mm",
       dpi = 600)

ggsave("3_viz/graph1.esp.jpg",
       width = 160,
       height = 100,
      units = "mm",  
       dpi = 600)

## Visualization in English
b %>%
  filter(cobertura_as != is.na(cobertura_as)) %>% 
  mutate(cobertura_as2 = cobertura_as/100,
         cobertura_as = round(cobertura_as, 1),
         signo = "%",
         porc = str_c(cobertura_as, signo, sep = "")) %>% 
  ggplot(aes(x = fct_reorder(country, cobertura_as2),
             y = cobertura_as2,
             fill = region)) + 
  geom_col(alpha = 0.9) + 
  coord_flip() + 
  geom_hline(yintercept = .0285, color = "khaki2", size = .6,
             linetype = "dashed") +
  geom_hline(yintercept = .229, color = "steelblue4", size = .6,
             linetype = "dashed") +
  geom_hline(yintercept = .145, color = "grey40", size = .6,
             linetype = "dashed") +
  geom_hline(yintercept = .149, color = "salmon3", size = .6,
             linetype = "dashed") +
  geom_text(aes(label = porc), size = 2.5, hjust = -.1,
            fontface = "bold") +
  scale_fill_manual(values = c("khaki2", "salmon3", "steelblue4"),
                    labels = c("The Caribbean", "Central America & Mexico", "South America")) +
  labs(x = "",
       y = "Proportion of people living in a household\nthat receives a cash transfer program",
       fill = "Region") + 
  scale_y_continuous(breaks = c(0,.20,.4,.6,.8,1),
                     label = percent_format(accuracy = 1),
                     limits = c(0, .65)) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(0.75, 0.2),
        panel.grid.minor.y = element_blank())

ggsave("3_viz/graph1.eng.png",
       width = 160,
       height = 100,
       units = "mm",  
       dpi = 600)

ggsave("3_viz/graph1.eng.jpg",
       width = 160,
       height = 100,
       units = "mm",  
       dpi = 600)


### Table 3: Number of programs to COVID----

## Visualization in Spanish
base %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  count(country,
        new_program) %>% 
  pivot_wider(names_from = "new_program",
              values_from = "n") %>% 
  mutate_at(vars(vertical, yes, horizontal, adaptation), ~replace(., is.na(.),0)) %>% 
  pivot_longer(vertical:adaptation,
               names_to = "new_program",
               values_to = "n") %>% 
  mutate(country = str_replace_all(country, c("Trinidad and Tobago" = "Trinidad y Tobago",
                                              "Suriname" = "Surinam",
                                              "Saint Christopher and Nevis" = "San Cristóbal y Nieves",
                                              "Peru" = "Perú",
                                              "Panama" = "Panamá", 
                                              "Mexico" = "México",
                                              "Haiti" = "Haití",
                                              "Dominican Republic" = "República Dominicana",
                                              "Brazil" = "Brasil",
                                              "Belize" = "Belice"))) %>% 
  
  ggplot(aes(y = fct_reorder(country, desc(country)),
             x = factor(new_program,
                        order = T,
                        levels = c("yes",
                                   "vertical",
                                   "horizontal",
                                   "adaptation")))) + 
  geom_tile(aes(fill = n)) +
  geom_text(aes(label = n),
            size = 3) +
  scale_fill_gradient2(low = "white",
                       high = "steelblue") +
  labs(fill = "Número de\nprogramas") +
  scale_x_discrete(labels = c("Programa\nnuevo",
                              "Expansión\nvertical",
                              "Expansión\nhorizontal",
                              "Adaptación\n"),
                   position = "top") + 
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust = .5,
                                   vjust = .5,
                                   angle = 0))

ggsave("3_viz/table3.esp.png",
       width = 160,
       height = 100,
       units = "mm",  
       dpi = 600)

ggsave("3_viz/table3.esp.jpg",
       width = 160,
       height = 100,
       units = "mm",  
       dpi = 600)

## Visualization in English
base %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  count(country,
        new_program) %>% 
  pivot_wider(names_from = "new_program",
              values_from = "n") %>% 
  mutate_at(vars(vertical, yes, horizontal, adaptation), ~replace(., is.na(.),0)) %>% 
  pivot_longer(vertical:adaptation,
               names_to = "new_program",
               values_to = "n") %>% 
  ggplot(aes(y = fct_reorder(country, desc(country)),
             x = factor(new_program,
                        order = T,
                        levels = c("yes",
                                   "vertical",
                                   "horizontal",
                                   "adaptation")))) + 
  geom_tile(aes(fill = n)) +
  geom_text(aes(label = n),
            size = 3) +
  scale_fill_gradient2(low = "white",
                       high = "steelblue") +
  labs(fill = "Número de\nprogramas") +
  scale_x_discrete(labels = c("New\nprogram",
                              "Vertical\nexpansion",
                              "Horizontal\nexpansion",
                              "Adaptation\n"),
                   position = "top") + 
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust = .5,
                                   vjust = .5,
                                   angle = 0))

ggsave("3_viz/table3.eng.png",
       width = 160,
       height = 100,
       units = "mm",  
       dpi = 600)


ggsave("3_viz/table3.eng.jpg",
       width = 160,
       height = 100,
       units = "mm",  
       dpi = 600)


### Graph 2----

## Visualization in Spanish

left_join(base, latam) %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  mutate_at(vars(c(duration_months, coverage_precovid, coverage_postcovid, amount_individual_postcovid, amount_individual_precovid)), ~replace(., is.na(.), 0)) %>% 
  mutate_at(vars(c(duration_months)), ~replace(., duration_months == 99, 3)) %>% 
  mutate(resta = amount_individual_postcovid - amount_individual_precovid,
         mult = resta * duration_months,
         mw = mult / min_wage,
         gdp = (mult / min_wage) * 12,
         cov1 = coverage_postcovid - coverage_precovid,
         cov = cov1/population) %>% 
  select(acronym, program_name,  country, mult, mw, gdp, cov, new_program) %>% 
  filter(cov != is.na(cov)) %>% 
  filter(mw != is.na(mw)) %>% 
  ggplot(aes(x = cov,
             y = mw,
             color = new_program)) + 
  geom_point(alpha = 0.7, aes(shape=new_program)) + 
  geom_text_repel(aes(label = acronym,),
                  size = 2.5,
                  vjust = .5,
                  hjust = -.5,
                  segment.color = "grey40",
                  segment.alpha = 0.4,
                  show.legend = F) +
  scale_y_continuous(breaks = c(0,1,2,3),
                     label = percent_format(accuracy = 1),
                     limits = c(0,3.2)) +
  scale_x_continuous(breaks = c(0,.1,.2,.3,.4),
                     label = percent_format(accuracy = 1), limits = c(0,.4)) +
  scale_color_brewer(type = "qual", palette = "Dark2",
                     labels = c("Exp. horizontal","Exp. vertical","Programa nuevo")) +
  scale_shape_discrete(labels = c("Exp. horizontal","Exp. vertical","Programa nuevo")) +
  labs(y = "Monto adicional total entre salario mínimo mensual", x = "Cobertura\n(beneficiarios directos entre población total)",
       shape = "Tipo de innovación",
       color = "Tipo de innovación") + 
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  guides(colour = guide_legend(override.aes = list(size = 4))) + 
  theme(legend.position = "bottom")

ggsave("3_viz/graph2.esp.png",
       width = 160,
       height = 120,
       units = "mm",  
       dpi = 600)

ggsave("3_viz/graph2.esp.jpg",
       width = 160,
       height = 120,
       units = "mm",  
       dpi = 600)


## Visualization in English

left_join(base, latam) %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  mutate_at(vars(c(duration_months, coverage_precovid, coverage_postcovid, amount_individual_postcovid, amount_individual_precovid)), ~replace(., is.na(.), 0)) %>% 
  mutate_at(vars(c(duration_months)), ~replace(., duration_months == 99, 3)) %>% 
  mutate(resta = amount_individual_postcovid - amount_individual_precovid,
         mult = resta * duration_months,
         mw = mult / min_wage,
         gdp = (mult / min_wage) * 12,
         cov1 = coverage_postcovid - coverage_precovid,
         cov = cov1/population) %>% 
  select(acronym, program_name,  country, mult, mw, gdp, cov, new_program) %>% 
  filter(cov != is.na(cov)) %>% 
  filter(mw != is.na(mw)) %>% 
  ggplot(aes(x = cov,
             y = mw,
             color = new_program)) + 
  geom_point(alpha = 0.7, aes(shape = new_program)) + 
  geom_text_repel(aes(label = acronym,),
                  size = 2.5,
                  vjust = .5,
                  hjust = -.5,
                  segment.color = "grey40",
                  segment.alpha = 0.4,
                  show.legend = F) +
  scale_y_continuous(breaks = c(0,1,2,3),
                     label = percent_format(accuracy = 1),
                     limits = c(0,3.2)) +
  scale_x_continuous(breaks = c(0,.1,.2,.3,.4),
                     label = percent_format(accuracy = 1), limits = c(0,.4)) +
  scale_color_brewer(type = "qual", palette = "Dark2",
                     labels = c("Horizontal exp.","Vertical exp.","New program")) +
  labs(y = "Total additional social benefit\ndivided by monthly minimum wage", x = "Coverage\n(direct beneficiaries divided by total population)",
       color = "Type of innovation", shape = "Type of innovation") + 
  scale_shape_discrete(labels = c("Horizontal exp.","Vertical exp.","New program")) +
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  guides(colour = guide_legend(override.aes = list(size = 4))) + 
  theme(legend.position = "bottom")

ggsave("3_viz/graph2.eng.png",
       width = 160,
       height = 120,
       units = "mm",  
       dpi = 600)

ggsave("3_viz/graph2.eng.jpg",
       width = 160,
       height = 120,
       units = "mm",  
       dpi = 600)


### Graph 3----

## Visualization in Spanish
left_join(base, latam) %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  mutate_at(vars(c(duration_months, coverage_precovid, coverage_postcovid, amount_individual_postcovid, amount_individual_precovid)), ~replace(., is.na(.), 0)) %>% 
  mutate_at(vars(c(duration_months)), ~replace(., duration_months == 99, 3)) %>% 
  mutate(resta = amount_individual_postcovid - amount_individual_precovid,
         mult = resta * duration_months,
         mw = mult / min_wage,
         gdp = (mult / gdp_percap) * 12,
         cov1 = coverage_postcovid - coverage_precovid,
         cov = cov1/population) %>% 
  select(acronym, program_name,  country, mult, mw, gdp, cov, new_program) %>% 
  filter(cov != 0) %>% 
  filter(gdp != 0) %>% 
  ggplot(aes(x = cov,
             y = gdp,
             color = new_program)) + 
    geom_point(alpha = 0.6) + 
  geom_text_repel(aes(label = acronym,),
                  size = 2.5,
                  vjust = .5,
                  hjust = -.5,
                  segment.color = "grey40",
                  segment.alpha = 0.5,
                  show.legend = F) +
  scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1,1.2,1.4),
                     label = percent_format(accuracy = 1)) + 
  scale_x_continuous(breaks = c(0,.1,.2,.3,.4),
                     label = percent_format(accuracy = 1), limits = c(0,.4)) +
  scale_color_brewer(type = "qual", palette = "Dark2",
                     labels = c("Exp. horizontal","Exp. vertical","Programa nuevo")) +
  labs(y = "Monto adicional total entre PIB per cápita", x = "Cobertura\n(beneficiarios directos entre población total)",
       color = "Tipo de innovación") + 
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  theme(legend.position = "bottom")

ggsave("3_viz/graph3.esp.png",
       width = 160,
       height = 120,
       units = "mm",  
       dpi = 600)

ggsave("3_viz/graph3.esp.jpg",
       width = 160,
       height = 120,
       units = "mm",  
       dpi = 600)


## Visualization in English
left_join(base, latam) %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  mutate_at(vars(c(duration_months, coverage_precovid, coverage_postcovid, amount_individual_postcovid, amount_individual_precovid)), ~replace(., is.na(.), 0)) %>% 
  mutate_at(vars(c(duration_months)), ~replace(., duration_months == 99, 3)) %>% 
  mutate(resta = amount_individual_postcovid - amount_individual_precovid,
         mult = resta * duration_months,
         mw = mult / min_wage,
         gdp = (mult / gdp_percap) * 12,
         cov1 = coverage_postcovid - coverage_precovid,
         cov = cov1/population) %>% 
  select(acronym, program_name,  country, mult, mw, gdp, cov, new_program) %>% 
  filter(cov != 0) %>% 
  filter(gdp != 0) %>% 
  ggplot(aes(x = cov,
             y = gdp,
             color = new_program)) + 
  geom_point(alpha = 0.6) + 
  geom_text_repel(aes(label = acronym,),
                  size = 2.5,
                  vjust = .5,
                  hjust = -.5,
                  segment.color = "grey40",
                  segment.alpha = 0.5,
                  show.legend = F) +
  scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1,1.2,1.4),
                     label = percent_format(accuracy = 1)) + 
  scale_x_continuous(breaks = c(0,.1,.2,.3,.4),
                     label = percent_format(accuracy = 1), limits = c(0,.4)) +
  scale_color_brewer(type = "qual", palette = "Dark2",
                     labels = c("Exp. horizontal","Exp. vertical","New program")) +
  labs(y = "Total additional social benefit\ndivided by monthly GDP per capita", x = "Coverage\n(direct beneficiaries divided by total population)",
       color = "Type of innovation") + 
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  theme(legend.position = "bottom")

ggsave("3_viz/graph3.eng.png",
       width = 160,
       height = 120,
       units = "mm",  
       dpi = 600)

ggsave("3_viz/graph3.eng.jpg",
       width = 160,
       height = 120,
       units = "mm",  
       dpi = 600)


### Graph 4: Type of modification - Alluvium----

## Calculations
base %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  # filter(new_program == "yes") %>% 
  count(new_registration)


base %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  count(new_delivery)

base %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  count(new_program)

base %>%
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>%
  filter(new_program == "yes") %>%
  count(new_registration)

base %>%
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>%
  filter(new_program == "yes") %>%
  count(new_delivery)

## Visualization in Spanish
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
                                  c("caribe" = "Caribe",
                                    "central america" = "Centroamérica\ny México",
                                    "south america" = "Sudamérica"
                                  ))) %>% 
  ggplot(aes(axis1 = `Novedad del programa`,
             axis2 = `Uso de información previa`,
             axis3 = `Uso de mecanismos de entrega previos`,
             y = n)) + 
  geom_alluvium(aes(fill = `Novedad del programa`), aes.bind = "alluvia", alpha = 0.7) +
  geom_stratum(color = "grey0",
               fill = "grey40",
               size = 1, 
               alpha = .3) +
  geom_label(stat = "stratum",
             label.strata = T,
             infer.label = T,
             size = 2.4,
             alpha = 0.5,
             check_overlap = T) +
  scale_x_continuous(breaks = 1:3, labels = c("Tipo de\ninnovación",
                                              "Uso de información\n previa para el\nregistro o selección",
                                              "Uso de\nmétodos de\npago previos"),
                     position = "top") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  theme_minimal() +
  labs(y = "",
       fill = "") + 
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

ggsave("3_viz/graph4.esp.png",
       width = 170,
       height = 120,
       units = "mm",  
       dpi = 600)

ggsave("3_viz/graph4.esp.jpg",
       width = 170,
       height = 120,
       units = "mm",  
       dpi = 600)


## Visualization in Spanish
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
                                                  c("adaptation" = "Adaptation",
                                                    "vertical/horizontal" = "Vertical y horizontal",
                                                    "vertical" = "Vertical exp.",
                                                    "horizontal" = "Horizontal exp.",                                                    "yes" = "New program")),
         `Uso de mecanismos de entrega previos` = str_replace_all(`Uso de mecanismos de entrega previos`,
                                                                  c("Yes" = "Sí",                                            "No" = "Yes")),
         `Uso de información previa` = str_replace_all(`Uso de información previa`,
                                                       c( "Yes" = "Sí",
                                                          "No" = "Yes"
                                                       )),
         `Uso de mecanismos de entrega previos` = str_replace_all(`Uso de mecanismos de entrega previos`,
                                                                  c("Sí" = "No")),
         `Uso de información previa` = str_replace_all(`Uso de información previa`,
                                                       c( "Sí" = "No"
                                                       )),
         region = str_replace_all(region,
                                  c("caribe" = "Caribe",
                                    "central america" = "Centroamérica\ny México",
                                    "south america" = "Sudamérica"
                                  ))) %>% 
  ggplot(aes(axis1 = `Novedad del programa`,
             axis2 = `Uso de información previa`,
             axis3 = `Uso de mecanismos de entrega previos`,
             y = n)) + 
  geom_alluvium(aes(fill = `Novedad del programa`), aes.bind = "alluvia", alpha = .7) +
  geom_stratum(color = "grey0",
               fill = "grey40",
               size = 1, 
               alpha = .3) +
  geom_label(stat = "stratum",
             label.strata = T,
             infer.label = T,
             size = 2.4,
             alpha = 0.8,
             check_overlap = T) +
  scale_x_continuous(breaks = 1:3, labels = c("Type of\ninnovation",
                                              "Use of existing\ninformation for\nregistration or selection",
                                              "Use of existing\npayment\nmethods"),
                     position = "top") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  theme_minimal() +
  labs(y = "",
       fill = "") + 
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

ggsave("3_viz/graph4.eng.png",
       width = 170,
       height = 120,
       units = "mm",  
       dpi = 600)

ggsave("3_viz/graph4.eng.jpg",
       width = 170,
       height = 120,
       units = "mm",  
       dpi = 600)



### Graph 5----

g5 <- base %>% 
  filter(new_program != "adaptation") %>% 
  filter(type_of_program != "Food",
         type_of_program != "Platform") %>% 
  mutate_at(vars(c(coverage_precovid, coverage_postcovid, amount_individual_postcovid, amount_individual_precovid)), ~replace(., is.na(.), 0)) %>% 
  mutate(coverage_postcovid = coverage_postcovid - coverage_precovid) %>% 
  mutate(diferencia = (amount_individual_postcovid - amount_individual_precovid)) %>% 
  filter(diferencia != 0) %>% 
  mutate_at(vars(c(duration_months)), ~replace(., duration_months == 99, 3)) %>% 
  filter(duration_months != 99,
         duration_months != 0) %>% 
  mutate(apoyo = diferencia*duration_months,
         costo = apoyo*coverage_postcovid) %>%
  group_by(country) %>% 
  summarise(cov = sum(coverage_postcovid)) %>% 
  ungroup()


## Visualization in Spanish
b %>% 
  select(country, acronym, population, region, rs_cob:pib_as) %>% 
  left_join(g5, by = "country") %>% 
  mutate(info = str_c(registro, padron, interoperabilidad, sep = ""))  %>%
  mutate(covid = cov/population) %>%
  select(-rs_cob, -pob_rs, -registro, -padron, -interoperabilidad) %>% 
  mutate_at(vars(c(covid, rs_cob_porc)), ~replace(., is.na(.), 0)) %>% 
  filter(acronym != "ATG",
         acronym != "BHS",
         acronym != "DMA",
         acronym != "CUB",
         acronym != "GRD",
         acronym != "GUY",
         acronym != "NIC",
         acronym != "LCA",
         acronym != "VCT",
         acronym != "VEN") %>% 
  ggplot(aes(y = covid,
             x = rs_cob_porc,
             color = info)) +
  geom_vline(xintercept = 0.303, color = "grey40", size = .5,
             linetype = "dashed") +
  geom_hline(yintercept = .183, color = "grey40", size = .5,
             linetype = "dashed") +
  geom_point(aes(shape = factor(info_covid)),
             alpha = 0.5, size = 4) + 
  geom_text_repel(aes(label = acronym),
                  size = 2.5,
                  vjust = -.7,
                  hjust = .9,
                  segment.color = "grey40",
                  segment.alpha = 0.5,
                  show.legend = F,
                  fontface = "bold") +
  scale_color_brewer(type  = "qual",
                     palette = "Dark2",
                     labels = c("\nNinguno\n",
                                "\nSólo registro social\n",
                                "\nRegistro social y\npadrón único\n",
                                "\nRegistro social,\npadrón único\ne interoperabilidad")) +
  scale_shape_discrete(labels = c("No","Sí")) +
  scale_x_continuous(label = percent_format(accuracy = 1), limits = c(-.1, 1)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  theme_minimal() +
  labs(x = "\nProporción de población en el registro social",
       y = "Beneficiarios directos entre población\n",
       col = "Sistema de información\nde protección social",
       shape = "Uso del sistema\npor COVID-19") + guides(colour = guide_legend(override.aes = list(size = 4)),
                                                         shape = guide_legend(override.aes = list(size = 4)))

ggsave("3_viz/graph5.esp.png",
       width = 160,
       height = 120,
       units = "mm",  
       dpi = 600)

ggsave("3_viz/graph5.esp.jpg",
       width = 160,
       height = 120,
       units = "mm",  
       dpi = 600)


## Visualization in English
b %>% 
  select(country, acronym, population, region, rs_cob:pib_as) %>% 
  left_join(g5, by = "country") %>% 
  mutate(info = str_c(registro, padron, interoperabilidad, sep = ""))  %>%
  mutate(covid = cov/population) %>%
  select(-rs_cob, -pob_rs, -registro, -padron, -interoperabilidad) %>% 
  mutate_at(vars(c(covid, rs_cob_porc)), ~replace(., is.na(.), 0)) %>% 
  filter(acronym != "ATG",
         acronym != "BHS",
         acronym != "DMA",
         acronym != "CUB",
         acronym != "GRD",
         acronym != "GUY",
         acronym != "NIC",
         acronym != "LCA",
         acronym != "VCT",
         acronym != "VEN") %>% 
  ggplot(aes(y = covid,
             x = rs_cob_porc,
             color = info)) +
  geom_vline(xintercept = 0.303, color = "grey40", size = .5,
             linetype = "dashed") +
  geom_hline(yintercept = .183, color = "grey40", size = .5,
             linetype = "dashed") +
  geom_point(aes(shape = factor(info_covid)),
             alpha = 0.5, size = 4) + 
  geom_text_repel(aes(label = acronym),
                  size = 2.5,
                  vjust = -.7,
                  hjust = .9,
                  segment.color = "grey40",
                  segment.alpha = 0.5,
                  show.legend = F,
                  fontface = "bold") +
  scale_color_brewer(type  = "qual",
                     palette = "Dark2",
                     labels = c("\nNone\n",
                                "\nOnly social registry\n",
                                "\nSocial registry &\nsingle beneficiary registry\n",
                                "\nSocial registry,\nsingle beneficiary registry,\n& interoperability")) +
  scale_x_continuous(label = percent_format(accuracy = 1), limits = c(-.1, 1)) +
  scale_shape_discrete(labels = c("No","Yes")) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  theme_minimal() +
  labs(x = "\nProportion of the population in the social registry",
       y = "Direct beneficiaries divided by total population\n",
       col = "Social protection\ninformation system",
       shape = "Use of the system\nfor COVID-19") + 
  guides(colour = guide_legend(override.aes = list(size = 4)),
         shape = guide_legend(override.aes = list(size = 4)))

ggsave("3_viz/graph5.eng.png",
       width = 160,
       height = 120,
       units = "mm",  
       dpi = 600)

ggsave("3_viz/graph5.eng.jpg",
       width = 160,
       height = 120,
       units = "mm",  
       dpi = 600)

