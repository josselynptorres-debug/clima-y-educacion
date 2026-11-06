library(tidyverse)

validar_columnas <- function(datos, requeridas, archivo) {
  faltantes <- setdiff(requeridas, names(datos))

  if (length(faltantes) > 0) {
    stop(
      sprintf(
        "Faltan estas columnas en %s: %s",
        archivo,
        paste(faltantes, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

# INEVAL usa punto y coma como separador y coma como separador decimal.
ineval <- read_delim(
  "ineval_serestudiante2024_2025_2025diciembre.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

validar_columnas(
  ineval,
  c("inev", "imat", "id_prov", "nm_regi", "grado"),
  "ineval_serestudiante2024_2025_2025diciembre.csv"
)

# En el diccionario INEVAL, grado == "3" corresponde al Nivel Bachillerato.
# Aplicar este filtro antes de cualquier cruce con los datos climáticos.
ineval <- ineval %>%
  filter(grado == "3")

variables_control <- intersect(
  c("tp_sexo", "tp_area", "sostenimiento", "tp_sostenimiento"),
  names(ineval)
)

ineval <- ineval %>%
  mutate(across(
    everything(),
    ~ replace(.x, .x %in% c("999999", "999998"), NA_character_)
  )) %>%
  mutate(
    id_prov = parse_integer(id_prov),
    inev = parse_number(inev, locale = locale(decimal_mark = ",")),
    imat = parse_number(imat, locale = locale(decimal_mark = ",")),
    mes_evaluacion = case_when(
      nm_regi == "1" ~ 11L,
      nm_regi == "2" ~ 5L,
      TRUE ~ NA_integer_
    ),
    anio_evaluacion = case_when(
      nm_regi == "1" ~ 2024L,
      nm_regi == "2" ~ 2025L,
      TRUE ~ NA_integer_
    ),
    regimen = case_when(
      nm_regi == "1" ~ "Costa",
      nm_regi == "2" ~ "Sierra",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(mes_evaluacion)) %>%
  filter(!is.na(inev), !is.na(imat), inev <= 1000, imat <= 1000) %>%
  select(
    id_prov,
    inev,
    imat,
    regimen,
    mes_evaluacion,
    anio_evaluacion,
    all_of(variables_control)
  ) %>%
  # Etiquetas según el diccionario INEVAL incluido con los datos.
  mutate(
    across(
      any_of("tp_sexo"),
      ~ factor(.x, levels = c("1", "2"), labels = c("Mujer", "Hombre"))
    ),
    across(
      any_of("tp_area"),
      ~ factor(.x, levels = c("1", "2"), labels = c("Rural", "Urbana"))
    ),
    across(
      any_of(c("sostenimiento", "tp_sostenimiento")),
      ~ factor(
        .x,
        levels = c("1", "2", "3", "4"),
        labels = c("Particular", "Municipal", "Fiscomisional", "Fiscal")
      )
    )
  )

# Coordenadas aproximadas de referencia para ubicar cada provincia en la
# cuadrícula climática. Las coordenadas están en grados decimales.
centroides_provincias <- tribble(
  ~id_prov, ~provincia, ~LAT, ~LON,
  1L, "Azuay", -2.9001, -79.0059,
  2L, "Bolívar", -1.5926, -79.0009,
  3L, "Cañar", -2.7397, -78.8486,
  4L, "Carchi", 0.8119, -77.7173,
  5L, "Cotopaxi", -0.9346, -78.6156,
  6L, "Chimborazo", -1.6636, -78.6546,
  7L, "El Oro", -3.2581, -79.9554,
  8L, "Esmeraldas", 0.9682, -79.6517,
  9L, "Guayas", -2.1709, -79.9224,
  10L, "Imbabura", 0.3517, -78.1223,
  11L, "Loja", -3.9931, -79.2042,
  12L, "Los Ríos", -1.8022, -79.5344,
  13L, "Manabí", -1.0546, -80.4545,
  14L, "Morona Santiago", -2.3094, -78.1114,
  15L, "Napo", -0.9938, -77.8129,
  16L, "Pastaza", -1.4924, -78.0011,
  17L, "Pichincha", -0.1807, -78.4678,
  18L, "Tungurahua", -1.2543, -78.6229,
  19L, "Zamora Chinchipe", -4.0692, -78.9567,
  20L, "Galápagos", -0.7400, -90.3100,
  21L, "Sucumbíos", 0.0862, -76.8897,
  22L, "Orellana", -0.4628, -76.9877,
  23L, "Santo Domingo de los Tsáchilas", -0.2522, -79.1754,
  24L, "Santa Elena", -2.2267, -80.8587
)

if (anyDuplicated(centroides_provincias$id_prov) ||
    !setequal(centroides_provincias$id_prov, 1:24)) {
  stop("La tabla debe contener exactamente una fila para cada id_prov del 1 al 24.", call. = FALSE)
}

write_csv(centroides_provincias, "centroides_provincias.csv")

archivos_climaticos <- list.files(
  path = ".",
  pattern = "^POWER_Regional_Daily_[0-9]{8}_[0-9]{8}\\.csv$",
  full.names = TRUE
)

if (length(archivos_climaticos) == 0) {
  stop("No se encontraron archivos diarios POWER con el patrón esperado.", call. = FALSE)
}

leer_power_diario <- function(archivo) {
  # El encabezado está en la línea 10; se declaran los nombres al omitirla.
  clima <- read_csv(
    archivo,
    skip = 10,
    col_names = c("LAT", "LON", "YEAR", "DOY", "T2M_MAX"),
    col_types = cols(
      LAT = col_double(),
      LON = col_double(),
      YEAR = col_integer(),
      DOY = col_integer(),
      T2M_MAX = col_double()
    ),
    show_col_types = FALSE
  )

  validar_columnas(
    clima,
    c("LAT", "LON", "YEAR", "DOY", "T2M_MAX"),
    basename(archivo)
  )

  clima
}

clima_diario <- archivos_climaticos %>%
  map(leer_power_diario) %>%
  bind_rows()

if (anyDuplicated(clima_diario[c("LAT", "LON", "YEAR", "DOY")])) {
  stop("Los archivos diarios POWER contienen observaciones duplicadas por ubicación y día.", call. = FALSE)
}

ubicaciones_power <- clima_diario %>%
  distinct(LAT, LON) %>%
  rename(lat_power = LAT, lon_power = LON)

# Asigna a cada provincia la celda NASA POWER geográficamente más cercana.
ubicacion_provincial <- crossing(
  centroides_provincias %>%
    transmute(id_prov, lat_provincia = LAT, lon_provincia = LON),
  ubicaciones_power
) %>%
  mutate(
    delta_lat = (lat_power - lat_provincia) * pi / 180,
    delta_lon = (lon_power - lon_provincia) * pi / 180,
    haversine = sin(delta_lat / 2)^2 +
      cos(lat_provincia * pi / 180) * cos(lat_power * pi / 180) *
        sin(delta_lon / 2)^2,
    distancia_km = 6371 * 2 * atan2(
      sqrt(haversine),
      sqrt(pmax(0, 1 - haversine))
    )
  ) %>%
  group_by(id_prov) %>%
  slice_min(order_by = distancia_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(id_prov, lat_provincia, lon_provincia, lat_power, lon_power)

# Convierte el año y el día del año de NASA POWER en una fecha calendario,
# y conserva exclusivamente las dos ventanas solicitadas (ambas inclusivas).
clima_provincial <- clima_diario %>%
  mutate(
    fecha = make_date(YEAR, 1L, 1L) + days(DOY - 1L),
    regimen = case_when(
      fecha >= ymd("2024-10-01") & fecha <= ymd("2024-11-19") ~ "Costa",
      fecha >= ymd("2025-04-01") & fecha <= ymd("2025-05-22") ~ "Sierra",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(regimen), !is.na(T2M_MAX), T2M_MAX != -999) %>%
  inner_join(
    ubicacion_provincial %>% select(id_prov, lat_power, lon_power),
    by = c("LAT" = "lat_power", "LON" = "lon_power"),
    relationship = "many-to-many"
  ) %>%
  group_by(id_prov, regimen) %>%
  summarise(
    dias_calor_extremo = sum(T2M_MAX > 30),
    .groups = "drop"
  )

data <- ineval %>%
  left_join(ubicacion_provincial, by = "id_prov") %>%
  left_join(clima_provincial, by = c("id_prov", "regimen"))

saveRDS(data, "base_ce_corregida.rds")
