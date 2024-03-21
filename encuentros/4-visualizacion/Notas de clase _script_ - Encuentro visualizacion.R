
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        Notas de clase (script) - Encuentro 5, visualización de datos     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Cargo librería
library(tidyverse)

### Cargo base de datos
df_puna <- read.csv("datos/puna_base_agregada.csv", encoding = "latin1")


### Filtro CON delta (por la positiva)
df_puna_con_delta <- df_puna %>% 
  filter(ruta_natural %in% c("Delta", "Llanuras y Costa Atlantica"))

unique(df_puna_con_delta$ruta_natural)

### Filtro SIN delta (por la negativa)
df_puna_sin_delta <- df_puna %>% 
  filter(!ruta_natural %in% c("Delta", "Llanuras y Costa Atlantica"))

df_puna <- df_puna %>% 
  mutate(tipo = case_when(tipo == "Otros colectivos" ~ NA_character_,
                          .default = tipo))


#                  Tratamiento de valores NA                  ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
edad <- c(22, 23, 45, NA)

!is.na(edad)

df_puna_sin_na <- df_puna %>% 
  filter(!is.na(tipo))



### Recodificación
df_puna_regiones <- df_puna %>% 
  mutate(region = case_when(provincia_nombre %in% c("Neuquén", "Chubut",
                                                    "Santa Cruz", "Tierra del Fuego") ~ "Patagonia",
                            provincia_nombre %in% c("Buenos Aires", "La Pampa", "Córdoba") ~ "Pampeana"))


prov <- c("nqn", "chubut", "bsas")
region <- c(NA, NA, NA)

base <- data.frame(prov, region)

base <- base %>% 
  mutate(region = case_when(prov %in% c("nqn", "chubut") ~ "Patagonia",
                            prov == "bsas" ~ "Pampeana"))

table(base$prov, 
      base$region)



#              Visualziación de datos con ggplot2             ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Armo datos para visualizar
tabla <- df_puna %>%
  filter(indice_tiempo == 2022) %>%
  group_by(tipo) %>%
  summarise(cant_habitantes = sum(habitaciones, na.rm = TRUE)) %>%
  mutate(porc_habitantes = round(cant_habitantes / sum(cant_habitantes) * 100, 1))

### Armo gráfico
ggplot(data = tabla, 
       aes(y = cant_habitantes, x = fct_reorder(tipo, -cant_habitantes))) +
  geom_col(aes(fill = tipo)) +
  # geom_text(aes(label = format(cant_habitantes, big.mark = ".")),
  #           vjust = "inward") +
  geom_text(aes(label = paste0(format(cant_habitantes,
                                      big.mark = "."), "(",porc_habitantes, "%)")),
            vjust = "inward") +
  geom_hline(yintercept = 0, color = "grey") + 
  scale_fill_brewer(type = "qual", 
                    palette = 3) +
  theme_classic() +
  theme(legend.position = "none")



### Guardo tabla en Excel
library(writexl)

write_xlsx(tabla, "datos/outputs/Tabla tipo de alojamiento_2022.xlsx")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Consultas extras                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)

df_puna <- read_csv("datos/puna_base_agregada.csv")

### Filtro CON delta (por la positiva)
df_puna_con_delta <- df_puna %>% 
  filter(ruta_natural %in% c("Delta", "Llanuras y Costa Atlantica"))

unique(df_puna_con_delta$ruta_natural)

### Filtro SIN delta (por la negativa)
df_puna_sin_delta <- df_puna %>% 
  filter(!ruta_natural %in% c("Delta", "Llanuras y Costa Atlantica"))

df_puna <- df_puna %>% 
  mutate(tipo = case_when(tipo == "Otros colectivos" ~ NA_character_,
                          .default = tipo))


edad <- c(22, 23, 45, NA)

!is.na(edad)

df_puna_sin_na <- df_puna %>% 
  filter(!is.na(tipo))



### Recodificación
df_puna_regiones <- df_puna %>% 
  mutate(region = case_when(provincia_nombre %in% c("Neuquén", "Chubut",
                                                    "Santa Cruz", "Tierra del Fuego") ~ "Patagonia",
                            provincia_nombre %in% c("Buenos Aires", "La Pampa", "Córdoba") ~ "Pampeana"))

