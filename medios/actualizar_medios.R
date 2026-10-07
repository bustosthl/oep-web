library(rvest)
library(yaml)
library(purrr)
library(dplyr)
library(httr)

# 1. Leer el Google Sheet
url_csv <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSjlvDoxf6g9gtYZouVChpZ1RZmAh8J39-fLaOF1S1Mn6m_my-QjQG5WPS6aCR4r9ZwlLWsPyVZ2eit/pub?output=csv"
datos_sheet <- read.csv(url_csv, encoding = "UTF-8") %>% 
  distinct(URL, .keep_all = TRUE)

# 2. Función para scrapear a prueba de codificación
extraer_meta <- function(link) {
  respuesta <- tryCatch(GET(link, user_agent("Mozilla/5.0")), error = function(e) return(NULL))
  
  if(is.null(respuesta) || status_code(respuesta) != 200) {
    return(list(title = "Portal no disponible", image = "", date = as.character(Sys.Date()), medio = "* Otro Medio"))
  }
  
  texto_html <- content(respuesta, "text", encoding = "UTF-8")
  html <- read_html(texto_html)
  
  # Título e Imagen
  title <- html %>% html_element('meta[property="og:title"]') %>% html_attr("content")
  image <- html %>% html_element('meta[property="og:image"]') %>% html_attr("content")
  
  # Búsqueda encadenada de Fecha
  date <- html %>% html_element('meta[property="article:published_time"]') %>% html_attr("content")
  
  if(is.na(date) || is.null(date) || date == "") {
    date <- html %>% html_element('meta[name="parsely-pub-date"]') %>% html_attr("content")
  }
  if(is.na(date) || is.null(date) || date == "") {
    date <- html %>% html_element('meta[name="publish-date"]') %>% html_attr("content")
  }
  if(is.na(date) || is.null(date) || date == "") {
    date <- html %>% html_element('time') %>% html_attr("datetime")
  }
  
  # Extracción de fecha por Regex de la URL (si falla el HTML)
  if(is.na(date) || is.null(date) || date == "") {
    match_url <- regmatches(link, regexpr("20[0-9]{2}[0-1][0-9][0-3][0-9]", link))
    if(length(match_url) > 0) {
      date <- sprintf("%s-%s-%s", 
                      substr(match_url, 1, 4), 
                      substr(match_url, 5, 6), 
                      substr(match_url, 7, 8))
    }
  }
  
  # Formatear la fecha
  if(is.na(date) || is.null(date) || date == "") {
    date <- as.character(Sys.Date())
  } else {
    date <- substr(date, 1, 10)
  }
  
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

# 4. Convertir a YAML y guardar
texto_yaml <- as.yaml(medios_lista)
writeLines(enc2utf8(texto_yaml), "medios/medios.yml", useBytes = TRUE)