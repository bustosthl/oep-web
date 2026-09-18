library(tidyRSS)
library(dplyr)
library(yaml)

# Reemplazá con el ID real de tu canal
url_rss <- "https://www.youtube.com/feeds/videos.xml?channel_id=UCzA-TZmbC1dZ9BceMFOEahQ"
feed <- tidyfeed(url_rss)

# Buscamos cómo se llama la columna de descripción en este momento
columnas_posibles <- c("media_description", "entry_content", "entry_summary")
columna_desc <- intersect(colnames(feed), columnas_posibles)

videos_nuevos <- feed %>%
  mutate(
    # Extrae la descripción si existe; si no, pone un texto genérico
    description_segura = if(length(columna_desc) > 0) feed[[columna_desc[1]]] else "Conversatorio del Observatorio."
  ) %>%
  select(
    title = entry_title,
    date = entry_published,
    path = entry_link,
    description = description_segura
  ) %>%
  mutate(
    date = as.character(as.Date(date)),
    author = "Observatorio de Economía Política",
    id_video = gsub(".*v=", "", path),
    image = paste0("https://i.ytimg.com/vi/", id_video, "/hqdefault.jpg"),
    # Envolvemos las categorías en una lista para que el YAML quede con guiones (-)
    categories = list(c("Automático", "Coyuntura"))
  ) %>%
  select(-id_video)

# Convertir a lista y guardar
videos_lista <- apply(videos_nuevos, 1, as.list)
write_yaml(videos_lista, "charlas/videos.yml")