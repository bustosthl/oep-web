library(rvest)
library(yaml)
library(purrr)
library(dplyr)
library(httr) # Usamos httr para controlar la descarga

# 1. Leer el Google Sheet
url_csv <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSjlvDoxf6g9gtYZouVChpZ1RZmAh8J39-fLaOF1S1Mn6m_my-QjQG5WPS6aCR4r9ZwlLWsPyVZ2eit/pub?output=csv"
datos_sheet <- read.csv(url_csv, encoding = "UTF-8") %>% 
  distinct(URL, .keep_all = TRUE)

# 2. Función para scrapear a prueba de codificación
extraer_meta <- function(link) {
  # Descargar la página cruda
  respuesta <- tryCatch(GET(link), error = function(e) return(NULL))
  
  if(is.null(respuesta) || status_code(respuesta) != 200) {
    return(list(title = "Portal no disponible", image = "", date = as.character(Sys.Date()), medio = "Otro"))
  }
  
  # Forzar la lectura como texto UTF-8 ANTES de procesar el HTML
  texto_html <- content(respuesta, "text", encoding = "UTF-8")
  html <- read_html(texto_html)
  
  # Buscar etiquetas universales Open Graph
  title <- html %>% html_element('meta[property="og:title"]') %>% html_attr("content")
  image <- html %>% html_element('meta[property="og:image"]') %>% html_attr("content")
  date <- html %>% html_element('meta[property="article:published_time"]') %>% html_attr("content")
  
  # Limpieza de fecha
  if(is.na(date) || is.null(date)) date <- as.character(Sys.Date()) else date <- substr(date, 1, 10)
  
  # Identificar el medio
  medio <- case_when(
    grepl("eldestape", link) ~ "* El Destape",
    grepl("cohete", link) ~ "* El Cohete a la Luna",
    grepl("tecla", link) ~ "* La Tecl@ Eñe",
    grepl("pagina12", link) ~ "* Página/12",
    TRUE ~ "* Otro Medio"
  )
  
  list(title = title, image = image, date = date, medio = medio)
}

# 3. Procesar fila por fila
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

# 4. Convertir a YAML y guardar como bytes para evitar que Windows lo rompa
texto_yaml <- as.yaml(medios_lista)
writeLines(enc2utf8(texto_yaml), "medios/medios.yml", useBytes = TRUE)