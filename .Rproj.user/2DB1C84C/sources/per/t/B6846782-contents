
# Bibliotecas -------------------------------------------------------------


library(tidyverse)


# Consumo -----------------------------------------------------------------



files <- list.files("data/", full.names = TRUE)

files.list <- lapply(seq_along(files), function(i) {
  df <- readxl::read_excel(files[i], skip = 2)
  file_name <- basename(files[i]) %>% str_remove("\\.xlsx")
  df <- df %>% mutate(FileName = file_name)
  return(df)
})

names(files.list) <- files |> 
  str_extract("[0-9]{4}")


sapply(files.list, names)



# Sin importe -------------------------------------------------------------

f2020 <- read_csv("Becas_al_extranjero_de_Enero_a_Diciembre_2020.csv", skip = 2) |> 
  mutate(`INICIO DE BECA` = as.Date(`INICIO DE BECA`, "%m/%d/%Y"))|> 
  mutate(`FIN DE BECA` = as.Date(`FIN DE BECA`, "%m/%d/%Y")) |> 
  select(1:10, last_col()) |> 
  setNames(c("id", "nombre", "inicio_beca", "fin_beca",
             "nivel_estudios", "institucion","pais", "programa", "area",
             "convocatoria", "filename")) |> 
  mutate(convocatoria = as.character(convocatoria))

convert_dates <- function(df) {
  if (class(df$inicio_beca)[1] == "character") {
    return(
      df %>% 
        mutate(inicio_beca = as.Date(as.numeric(inicio_beca), 
                                     origin = "1899-12-30")) |> 
        mutate(fin_beca = as.Date(as.numeric(fin_beca), 
                                     origin = "1899-12-30")) 
    )
  } else {
    return(df %>% select(1:10, last_col()))%>% 
      mutate(inicio_beca = as.Date(inicio_beca, "%Y-%m-%d")) |> 
      mutate(fin_beca = as.Date(fin_beca, "%Y-%m-%d")) 
  }
}


noimp <- lapply(files.list, function(df){
  df |> 
    select(1:10, last_col()) |> 
    setNames(c("id", "nombre", "inicio_beca", "fin_beca",
               "nivel_estudios", "institucion","pais", "programa", "area",
               "convocatoria", "filename")) |> 
    mutate(convocatoria = as.character(convocatoria)) |> 
    convert_dates() 
}) |> 
  bind_rows() |> 
  mutate(filename = str_extract(filename, "[0-9]{4}")) |> 
  mutate(convocatoria = case_when(
    filename %in% c("2015", "2016", "2017", "2018") ~ NA_character_,
    TRUE ~ convocatoria
  )) |> 
  filter(!is.na(id)) |> 
  filter(!is.na(programa)) |> 
  filter(programa != "N/A") |> 
  filter(area != "N/A") |> 
  bind_rows(f2020)


write.csv(noimp, "clean_data/padron_sin_importe.csv", row.names = F)


# Con importe -------------------------------------------------------------


