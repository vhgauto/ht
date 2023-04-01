
# paquetes ----------------------------------------------------------------

library(spotifyr)
library(TMDb)
library(rvest)
library(tidyverse)
library(here)
library(glue)

# credenciales ------------------------------------------------------------

key <- read_csv(here("key.txt"))

# spotify -----------------------------------------------------------------

Sys.setenv(SPOTIFY_CLIENT_ID = key$key[1])
Sys.setenv(SPOTIFY_CLIENT_SECRET = key$key[2])
 
access_token <- get_spotify_access_token()
 
id_HT <- "6C4MdNWQSPhmzBlIVau30e"

# función p/descargar todos los datos de HT
f_ep <- function(x) {
  e <- get_show_episodes(id = id_HT,
                         limit = 50,
                         include_meta_info = FALSE,
                         offset = x) |> as_tibble()

  return(e)
}
 
# descargo todos los datos, CON las URL de las imágenes
map(.x = seq(0, 250, 50), ~ f_ep(x = .x)) |>
  list_rbind() |>
  unnest(images) |>
  write_tsv(here("data/spotify_datos_url.tsv"))
 
# descargo todos los datos, SIN las URL de las imágenes
map(.x = seq(0, 250, 50), ~ f_ep(x = .x)) |>
  list_rbind() |>
  unnest(images) |>
  filter(width == 640) |>
  write_tsv(here("data/spotify_datos.tsv"))

# TMDB --------------------------------------------------------------------

# credencial de acceso
api_key <- key$key[3]

# link a la lista de películas en LETTERBOXD
link_lista_letterboxd <- "https://letterboxd.com/matiasec/list/hoy-trasnoche-con-capitulo/detail/"

# cantidad de páginas que contienen la lista de películas
n_lista <- read_html(link_lista_letterboxd) |> 
  html_elements(xpath = "/html/body/div[1]/div/div/section/div[3]/div[3]/ul/li/a") |> 
  html_text() |> 
  as.numeric() |> 
  max()

# link base para la iteración con 'map()'
url_base <- "https://letterboxd.com/matiasec/list/hoy-trasnoche-con-capitulo/detail/page/"

# vector con todas las URL
url_vector <- glue("{url_base}{1:{n_lista}}")

# función p/obtener el título de la película y el año de estreno
f_letterboxd <- function(x) {
  
  # título y año
  ti_an <- read_html(x) |> 
    html_elements(xpath = "/html/body/div[1]/div/div/section/ul/li/div[2]/h2") |> 
    html_text()
  
  # armo un tibble
  v <- tibble(peli = ti_an) |> 
    # separo en título y año
    separate(peli, c("titulo", "año"), sep = " (?=\\d{4}$)")
  
  return(v)
}

# tibble con los títulos de las películas y el año de estreno
pelis_titulo_año <- map(.x = url_vector, ~ f_letterboxd(x = .x)) |>
  list_rbind() |>
  mutate(año = as.numeric(año)) |> 
  # casos especiales (MANUAL)
  mutate(titulo = case_match(titulo,
                             "I Wanna Dance with Somebody" ~ "Whitney Houston: I Wanna Dance with Somebody",
                             "Glass Onion" ~ "Glass Onion: A Knives Out Mystery",
                             .default = titulo))

# leo los datos actuales
tmdb_datos <- read_tsv(here("data/tmdb_datos.tsv"))

# verifico películas nuevas
# NO considero Leaving Neverland & The Viewing
no_peli <- c("Leaving Neverland", "The Viewing")
nueva_peli <- pelis_titulo_año |>
  anti_join(tmdb_datos |> distinct(titulo, año),
            by = join_by(titulo, año)) |>
  filter(!titulo %in% no_peli)

f_id <- function(x, y) {
  id <- search_multi(api_key = api_key, query = x) |> 
    as_tibble() |> 
    unnest(everything()) |> 
    filter(title == x) |> 
    mutate(d = abs(year(ymd(release_date)) - y)) |>
    slice_min(order_by = d, n = 1) |> 
    slice_max(order_by = popularity, n = 1) |> 
    pull(id)
  
  return(id)
  
}

# averiguo el 'id' de TMDB, ¡¡¡sólamente de las películas nuevas!!!
pelis_id <- nueva_peli |>
  mutate(id = map2(.x = titulo, .y = año,
                   ~ f_id(x = .x, y = .y))) |>
  unnest(id)

# obtengo toda la info 'crew' (list)
f_crew <- function(x) {
  movie_credits(api_key = api_key, id = x)$crew
}

# obtengo toda la info 'cast' (list)
f_cast <- function(x) {
  movie_credits(api_key = api_key, id = x)$cast
}

# obtengo el elenco a partir de 'cast' (data.frame)
f_elenco <- function(x) {
  x$name[1:5]
}

# obtengo la dirección a partir de 'crew' (data.frame)
f_direccion <- function(x) {
  x |> 
    as_tibble() |> 
    filter(job == "Director") |> 
    pull(name)
}

# obtengo el guión a partir de 'crew' (data.frame)
f_guion <- function(x) {
  x |> 
    as_tibble() |> 
    filter(job %in% c("Screenplay", "Writer", "Story")) |> 
    distinct(name) |> 
    pull(name)
}

# obtengo el género a partir de 'id' (data.frame)
f_genero <- function(x) {
  movie(api_key = api_key, id = x)$genre$name
}

# tibble con columnas p/crew y cast
# PUEDE TARDAR VARIOS MINUTOS
pelis_crew_cast <- pelis_id |>
  mutate(crew = map(.x = id, ~ f_crew(x = .x))) |>
  mutate(cast = map(.x = id, ~ f_cast(x = .x)))

# obtengo elenco, dirección, guión y género
# PUEDE TARDAR VARIOS MINUTOS
tmdb_datos1 <- pelis_crew_cast |>
  mutate(elenco = map(.x = cast, ~ f_elenco(x = .x))) |>
  mutate(direccion = map(.x = crew, ~ f_direccion(x = .x))) |>
  mutate(guion = map(.x = crew, ~ f_guion(x = .x))) |>
  mutate(genero = map(.x = id, ~ f_genero(x = .x))) |>
  select(-id, -crew, -cast) |>
  unnest(elenco) |>
  unnest(direccion) |>
  unnest(guion) |>
  unnest(genero)

# combino ambas bases de datos
tmdb_datos <- bind_rows(tmdb_datos, tmdb_datos1)

# guardo
write_tsv(tmdb_datos, here("data/tmdb_datos.tsv"))
