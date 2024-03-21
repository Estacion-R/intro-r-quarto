
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    Notas (script) de clase - Encuentro 5                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Cargo librería
library(tidyverse)

df_puna <- read_csv("datos/puna_base_agregada.csv")


### Selecciono columnas y creo nueva variable
df_lugar_tot <- df_puna |> 
  select(localidad, habitaciones, plazas) |> 
  mutate(lugar_disponible = habitaciones + plazas)


### Creo variable con la función paste() de ayuda
df_lugar_ej1 <- df_puna %>% 
  select(localidad, region, ruta_natural) %>% 
  mutate(region_ruta = paste(region, ruta_natural, sep = "-"))

### Ejemplo de use paste()
anio <- c(2020, 2020, 2100, 3000)
mes <- c("enero", "enero", "diciembre", "Abril")

base <- data.frame(anio, mes)

base_nueva_var <- base %>% 
  mutate(periodo = paste(anio, mes, sep = "-"))


### Recodificación
df_puna_agrup <- df_puna |> 
  select(tipo, plazas) |> 
  mutate(tipo_agrupado = case_when(tipo == "Hoteleros" ~ "Hoteleros",
                                   .default = "Otros"))

### Calculo frecuencia
df_puna_agrup %>% 
  count(tipo_agrupado)


#  Recodificación --> Uso del .default = para "todo el resto" ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_puna_agrup_hotel <- df_puna |> 
  select(clasificacion_minturdep, plazas) |> 
  mutate(clasif_agrupado = case_when(
    clasificacion_minturdep %in% c("Hotel 1 estrella", 
                                   "Hotel 2 estrellas",
                                   "Hotel 3 estrellas") ~ "Hotel hasta 3 estrellas",
    .default = "Otros"))



df_diego <- df_puna %>% 
  select(localidad, clasificacion_minturdep) %>%  
  mutate(nueva_clasificacion = case_when(clasificacion_minturdep == "Camping" ~ "Camping",
                                         .default = "Otros"))

unique(df_diego$nueva_clasificacion)





#                        Flujo completo                       ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Armo base
unique(df_puna$clasificacion_minturdep)
df_jimena <- df_puna %>% 
  filter(indice_tiempo == 2020) %>% 
  select(localidad,clasificacion_minturdep) %>% 
  mutate (nueva_clasif = case_when(clasificacion_minturdep=="Hotel 1 estrella" ~"Hotel 1 estrella",
                                   .default= "Otros"))


## Calculo n y porcentaje
tabla_hoteles_1_estr <- df_jimena %>% 
  count(nueva_clasif) %>% 
  mutate(porcentaje = n / sum(n) * 100)


#                Grabar o escribir datos en csv               ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_csv(tabla, "datos/tabla_camping.csv")



#.........Dar formato a valores con la función 'format().........
tabla_fin_mundo_prov <- df_puna |>
  filter(ruta_natural == "Desiertos y Volcanes") %>% 
  group_by(provincia_nombre, indice_tiempo) %>% 
  summarise(cant_plazas = sum(plazas),
            prom_plazas = format(mean(plazas), 
                                 big.mark = ".", 
                                 decimal.mark = ",", 
                                 digits = 3),
            min_plazas  = min(plazas),
            max_plazas  = max(plazas)) %>% 
  mutate(porcentaje = round(cant_plazas / sum(cant_plazas) * 100, digits = 2)) %>% 
  filter(provincia_nombre == "Catamarca") %>% 
  select(-min_plazas)


sum(tabla_fin_mundo_prov$prom_plazas)


#          Armado de tablas con el paquete {janitor}          ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# install.packages("janitor")
library(janitor)

tabla_nueva <- df_puna %>% 
  mutate(anio_agrup = case_when(indice_tiempo %in% c(2020, 2021) ~ 1,
                                indice_tiempo == 2022 ~ 2))


tabla_nueva %>% 
  count(anio_agrup) %>% 
  adorn_totals()

tabla_nueva %>%
  tabyl(anio_agrup) %>%
  adorn_totals("row") %>% 
  adorn_pct_formatting()




