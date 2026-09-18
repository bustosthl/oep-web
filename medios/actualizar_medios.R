library(rvest)
library(yaml)
library(purrr)

# 1. Leer el Google Sheet desde el link CSV público
url_csv <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSjlvDoxf6g9gtYZouVChpZ1RZmAh8J39-fLaOF1S1Mn6m_my-QjQG5WPS6aCR4r9ZwlLWsPyVZ2eit/pub?output=csv"
datos_sheet <- read.csv(url_csv)

# 2. Función para scrapear cualquier portal de noticias
extraer_meta <- function(link) {
  html <- tryCatch(read_html(link), error = function(e) return(NULL))
  
  if(is.null(html)) {
    return(list(title = "Portal no disponible", image = "", date = as.character(Sys.Date()), medio = "Otro"))
  }
  
  # Buscar etiquetas universales Open Graph
  title <- html %>% html_element('meta[property="og:title"]') %>% html_attr("content")
  image <- html %>% html_element('meta[property="og:image"]') %>% html_attr("content")
  date <- html %>% html_element('meta[property="article:published_time"]') %>% html_attr("content")
  
  # Limpieza de fecha (quedarse solo con YYYY-MM-DD)
  if(is.na(date) || is.null(date)) date <- as.character(Sys.Date()) else date <- substr(date, 1, 10)
  
  # Identificar el medio según la URL para la categoría
  medio <- case_when(
    grepl("eldestape", link) ~ "* El Destape",
    grepl("cohete", link) ~ "* El Cohete a la Luna",
    grepl("tecla", link) ~ "* La Tecl@ Eñe",
    TRUE ~ "* Otro Medio"
  )
  
  list(title = title, image = image, date = date, medio = medio)
}

# 3. Procesar fila por fila y armar la estructura YAML
medios_lista <- map2(datos_sheet$URL, datos_sheet$Autor, function(url, autor) {
  meta <- extraer_meta(url)
  list(
    title = meta$title,
    author = autor,
    date = meta$date,
    image = meta$image,
    path = url,
    categories = list(meta$medio, autor)
  )
})

# 4. Sobrescribir el archivo de Quarto
write_yaml(medios_lista, "medios/medios.yml")