#####                        :::::::::::::::                                ####
#####                          DEPENDENCIAS                                 ####
importarInstrumentosJuridicos <- function() {
  
  ## Ruta del informe listado total
  listado_path <- file.path(raizPath, 'fondos/data/cargaMasiva/CoFFEE/Desembolsos - Operaciones.xlsx')
  
  ## Ruta del informe listado total
  resultsPath <- file.path(raizPath, 'fondos/results')


  ## * Lectura                                                                ####
  
  # Leer los datos desde el archivo Excel, omitiendo las primeras 2 filas
  datos <- read_excel(listado_path, skip = 2)
  
  #normalizar los nombres con la nomenclatura Camel Case
  datosNormalizados <- aCamelCase(datos)
  
  # Deduplicamos o verificamos al menos
  datos <- deduplicarDataFrame(datosNormalizados)
  
  ## * Tratamiento                                                                ####
  ### Creamos columna nuevo con suma de importes de if por iniciativa

  ### Creamos columna para agrupar observaciones como texto para c/instrumento jurídico

  datos <- datos %>%
    mutate(
      observacionesIj = paste0(
        "Código único de IJ: ", codigoUnicoIj, "\n",
        "Tipo: ", tipoOperacion, "\n",
        "Nombre: ", denominacionOperacion, "\n",
        "Importe neto: ", numeroAeuros(importeOperacionesSinIva), "\n",
        "Importe bruto: ", numeroAeuros(importeTotalOperaciones), "\n",
        "Entidad ejecutora: ", entidadEjecutora, "\n",
        "Observaciones: ", observaciones, "\r\n"
      )
    )
  
  ### Agrupamos todas las observaciones relacionadas con IJ por iniciativa  
  
  resultado <- datos %>%
    group_by(codigoIniciativa) %>%
    summarise(
      sumaImportesBrutosIjs = sum(importeOperacionesSinIva, na.rm = TRUE),
      sumaImportesNetosIjs = sum(importeTotalOperaciones, na.rm = TRUE),
      observacionesConcatenadas = paste(
        observacionesIj,
        collapse = "\n "
      )
    ) %>%
    mutate(
      observacionesConcatenadas = ifelse(
        nchar(observacionesConcatenadas) > 500,
        paste0(
          substr(observacionesConcatenadas, 1, 500),
          "***** --->\r\n *****CONSTAN MÁS OBSERVACIONES. Ver informe de instrumentos jurídicos"
        ),
        observacionesConcatenadas
      )
    )
  
  ### Función devuelve suma de importes y observaciones de IJs.  
  
  ## * Exportación                                                            ####
  
  exportarComoExcel("instrumentosJuridicosCoFFEE", datos,resultsPath)
  
  return(resultado)
}

importarPresupuestosCoFFEE <- function() {
  
  #Obtenemos ruta del directorio de trabajo
  raizPath <- getwd()
  
  ## Ruta presupuestos previstos anualizados cargados en CoFFEE
  presupuestosPA_path <- file.path(raizPath, 'fondos/data/cargaMasiva/CoFFEE/Desembolsos - Histórico de modificaciones de presupuesto previsto anualizado.xlsx')
  
  # Leer los datos desde el archivo Excel, omitiendo las primeras 2 filas
  datos <- read_excel(presupuestosPA_path, skip = 2)
  
  # Convertir "Fecha creación" al tipo POSIXct (incluye fecha y hora)
  datos <- datos %>%
    mutate(`Fecha creación` = as.POSIXct(`Fecha creación`, format = "%d/%m/%Y %H:%M"))
  
  # Convertir columnas numéricas a formato estándar (sin notación científica)
  datos <- datos %>%
    mutate(across(where(is.numeric), ~ as.numeric(.)))
  
  # Filtrar los últimos reportes por "Código iniciativa" y "Año" considerando fecha y hora
  datos_filtrados <- datos %>%
    group_by(`Código iniciativa`, Año) %>%
    filter(`Fecha creación` == max(`Fecha creación`)) %>%
    ungroup()
  
  # Seleccionar las columnas relevantes
  datos_filtrados <- datos_filtrados %>%
    select(`Código iniciativa`, Año, `Presupuesto Previsto (sin IVA)`)
  
  datos_transformados <- datos_filtrados
  
  # Transformar los datos en formato ancho
  datos_transformados <- datos_transformados %>%
    pivot_wider(names_from = Año, values_from = `Presupuesto Previsto (sin IVA)`) %>%
    relocate(`Código iniciativa`)
  
  #Normalizamos nombres
  datos_transformados <- datos_transformados %>%
    rename(
      codigoIniciativa = `Código iniciativa`,
      ppn2020 = `2020`,
      ppn2021 = `2021`,
      ppn2022 = `2022`,
      ppn2023 = `2023`,
      ppn2024 = `2024`,
      ppn2025 = `2025`,
      ppn2026 = `2026`
    )
  
  # Retornar los datos transformados
  return(datos_transformados)
}

importarListadoTotal <- function() {
  
  #Obtenemos ruta del directorio de trabajo
  raizPath <- getwd()
  
  ## Ruta del informe listado total
  listado_path <- file.path(raizPath, 'fondos/data/cargaMasiva/CoFFEE/Desembolsos - Listado Total - Relación de Proyectos, Subproyectos, Subproyectos Instrumentales y Actuaciones.xlsx')
  
  # Leer los datos desde el archivo Excel, omitiendo las primeras 2 filas
  datos <- read_excel(listado_path, skip = 2)
  
  
  
  #Normalizamos nombres
  datos <- aCamelCase(datos)
  
  
  # Retornar los datos transformados
  return(datos)
}

importarRelacionActuaciones<- function() {
  
  #Obtenemos ruta del directorio de trabajo
  raizPath <- getwd()
  
  ## Ruta del informe listado total
  listado_path <- file.path(raizPath, 'fondos/data/cargaMasiva/CoFFEE/Desembolsos - Relación de Actuaciones.xlsx')
  
  # Leer los datos desde el archivo Excel, omitiendo las primeras 2 filas
  datos <- read_excel(listado_path, skip = 2)
  
  # Normalizo nombres
  datos <- aCamelCase(datos)
  
  # Cambiar explicitamente el nombre de la columna del codigo iniciativa para po
  # der combinarlo fuera con el listado total de iniciativas
  datos <- datos %>%
    rename(codigoIniciativa = codigoActuacion)
  
  datos <- datos %>%
    select(codigoIniciativa, claseActuacion, tipoIniciativaPadre, noAuxDefinicion, noAuxPlanificacion)
  
  # Retornar los datos transformados
  return(datos)
}



#* Submódulos                                                               ####  

leerInformesHos2 <- function() {
  
  cargarYtransformar <- function(rutaArchivo) {
    rutaArchivo <- file.path(raizPath, rutaArchivo)
    data <- read_excel(rutaArchivo, skip = 2)
    data <- aCamelCase(data)
    return(data)
  }
  

  
  # Definición de rutas relativas para los archivos
  archivos <- list(
    indicadores = 'fondos/data/cargaMasiva/CoFFEE/HyO - Definición.xlsx',
    progresoIndicadores = 'fondos/data/cargaMasiva/CoFFEE/HyO - Progreso Indicadores.xlsx',
    datosEjecucion = 'fondos/data/cargaMasiva/CoFFEE/HyO - Datos de Ejecución.xlsx',
    hosOrganismos = 'fondos/data/cargaMasiva/CoFFEE/HyO - Organismos HyO CID.xlsx',
    detalleDesgloseVo = 'fondos/data/cargaMasiva/CoFFEE/Definición y planificación - Detalle desglose VO.xlsx',
    indicadoresRelacionadosConIniciativas = 'fondos/data/cargaMasiva/CoFFEE/Desembolsos - HyO con sus Indicadores.xlsx',
    atributosIndicadores = 'fondos/data/cargaMasiva/CoFFEE/HyO - Indicadores.xlsx'
  )
  
  # Leer y transformar cada archivo
  informes <- lapply(archivos, cargarYtransformar)

  # Retornar lista de dataframes nombrados
  return(informes)
}

procesarInformesIndicadores <- function(informes) {
  
  # Acceso a cada dataframe
  indicadores <- informes$indicadores
  progresoIndicadores <- informes$progresoIndicadores
  datosEjecucion <- informes$datosEjecucion
  hosOrganismos <- informes$hosOrganismos
  detalleDesgloseVo <- informes$detalleDesgloseVo
  indicadoresRelacionadosConIniciativas <- informes$indicadoresRelacionadosConIniciativas

  
  # Quedarse con el registro más reciente por cada 'codigoHyo'
  progresoIndicadores <- progresoIndicadores %>%
    group_by(codigoHyo) %>%
    filter(fechaRegistro == max(fechaRegistro)) %>%
    ungroup()
  
  # Unir progreso indicadores con indicadores
  indicadores <- indicadores %>%
    left_join(progresoIndicadores, by = c("codigoHyo" = "codigoHyo"))
  
  # Agregar columna iniciativa a partir del código de hito
  indicadores <- indicadores %>%
    mutate(iniciativaRelacionada = sub("\\.[^.]*$", "", codigoHyo))
  
  # Procesamos detalleVo para identificar a que iniciativa se refiere la info
  # agrupamos columnas de detallle desglose Vo
  cols <-c('nivel1', 'nivel2', 'nivel3', 'nivel4', 'nivel5', 'nivel6')   # nombres de tus columnas
  detalleDesgloseVo$unida <- apply(detalleDesgloseVo[ , cols], 1, paste, collapse = ",")
  
  detalleDesgloseVo$primero_no_na <- apply(detalleDesgloseVo[ , cols], 1, function(x) {
    # x es un vector de longitud 4 con los valores de la fila
    # rev(x):  invierte el orden → derecha a izquierda
    # na.omit: elimina NA
    # [1]:     primer elemento tras limpiar NAs  (que en realidad era el más a la derecha)
    na.omit(rev(x))[1]
  })
  
  # Agregar columna iniciativa a partir del código de hito
  detalleDesgloseVo <- detalleDesgloseVo %>%
    mutate(codigoIniciativa = sub("\\.[^.]*$", "", primero_no_na))
  
  # Cambio nombre de primero_no_na
  detalleDesgloseVo <- detalleDesgloseVo %>%
    rename(codigoHyO = primero_no_na)
  
  # Elimino columnas que no quiero
  detalleDesgloseVo <- detalleDesgloseVo %>%
    select(indicador, valorObjetivoVo,sumaDeValorObjetivoHijos,difVo,codigoIniciativa)
  
  
  # --- 1. Si tu data frame se llama 'detalleDesgloseVo', parte de él ------------
  # (ajusta el nombre si tu objeto tiene otro nombre)
  df_resumen <- detalleDesgloseVo %>% 
    ## a) Crea, para cada fila, la cadena con los 4 campos entre paréntesis
    mutate(cadena_fila = paste0(
      "(", indicador, "; ", valorObjetivoVo, "; ",
      sumaDeValorObjetivoHijos, "; ", difVo, ")"
    )) %>% 
    
    ## b) Agrupa por 'iniciativaRelacionada' y concatena las cadenas
    group_by(codigoIniciativa) %>% 
    summarise(
      detalleIndicadores = paste(cadena_fila, collapse = "* "),   # une con "; "
      .groups = "drop"
    )
  

  

  # Retornar dataframes procesados como una lista
  return(df_resumen)
}


#* Módulo                                                                   ####  

importarResumenIndicadores <- function(){
  ### * Leemos y procesamos (unimos, deduplicamos...) informes de Hos         ####                                            
  
  informes <- leerInformesHos2()
 

  informes <- procesarInformesIndicadores(informes)
  

  ### * Organizo indicadores: uno por columna y alineados para esto         ####   
  # modifico la columna detalleIndicadores para separarla por *
  
  datos_wide <- informes %>% 
    mutate(fila = row_number()) %>%                         # identificador opcional
    separate_rows(detalleIndicadores,                      # romper en bloques
                  sep = "\\)\\s*\\*\\s*\\(") %>% 
    mutate(
      detalleIndicadores = str_remove_all(detalleIndicadores, "^\\(|\\)$"),
      codigo  = str_extract(detalleIndicadores, "^[^;]+") |> str_trim()
    ) %>% 
    pivot_wider(
      id_cols    = codigoIniciativa,   # <- identificador único
      names_from = codigo,
      values_from = detalleIndicadores,
      values_fn   = dplyr::first
    )
  
  # Quitaremos el nombre del indicador de la celda
  df_limpio <- datos_wide %>%                       # ← tu data frame
    mutate(
      across(                                 # aplica a varias columnas…
        -1,                                   # …todas menos la 1.ª  (ó -codigoIniciativa)
        ~ str_remove(.x, "^[^;]+;\\s*")       # quita 1er elemento + “; ”
      )
    )
  
  
  

  
  # # Acceso a cada dataframe
  # indicadores <- informes$indicadores
  # progresoIndicadores <- informes$progresoIndicadores
  # datosEjecucion <- informes$datosEjecucion
  # hosOrganismos <- informes$hosOrganismos
  # resumenIndicadores <- informes$resumenIndicadores

  return(df_limpio)
  
}







importarTodosLosIndicadores <- function(){
  
  
  informes <- leerInformesHos2()
  
  datos <- informes$indicadoresRelacionadosConIniciativas
  
  
  
  # Creamos nueva columna 'iniciativaRelacionada' eliminando el último punto y lo que le sigue en 'codigoHyo'
  # Eliminamos también columnas innecesarias: 'profundidadHyo' y 'desembolso'  
  datos <- datos %>% 
    mutate(
      iniciativaRelacionada = str_remove(codigoHyo, "\\.[^.]*$")  
    )%>% 
    select(-profundidadHyo, -desembolso)
  
  
  atributosIndicadores <- informes$atributosIndicadores
  
  # Añado información sobre atributos indicadores, disponible sólo para los criticos en CoFFEE a 14/6/25
  datos <- left_join(
    datos,
    atributosIndicadores,
    by = c("codigoIndicador" = "indicador")
  )

  
  # Reemplazamos valores de 'claseHyo' con etiquetas más cortas 
  datos <- datos %>%
    mutate(claseHyo = case_when(
      claseHyo == "Auxiliar de planificación" ~ "HAP",
      claseHyo == "Auxiliar de Definición"    ~ "HAD",
      claseHyo == "CID"                       ~ "CID",
      claseHyo == "Gestión crítico"           ~ "HGC",
      claseHyo == "OA"                        ~ "OA",
      claseHyo == "Gestión no crítico"        ~ "HGNC",
      TRUE ~ claseHyo  # En caso de que haya otros valores no previstos
    ))
  
  # Creamos una columna 'titulo' combinando código del indicador, tipo Hyo y la denominación
  datos$titulo <- paste(datos$codigoCeHyo, datos$claseHyo, datos$codigoIndicador, datos$unidadMedida, datos$denominacionIndicador, sep=" - ")

  
  # Seleccionamos columnas
  datos <- datos %>%
    select(codigoIndicador, iniciativaRelacionada, titulo, contribucionDirecta, valorObjetivo)
  
  # Formateamos la columna 'valorObjetivo' para que no use notación científica (e.g., 1e+05)
  datos$valorObjetivo <- format(datos$valorObjetivo, scientific = FALSE)
  
  
  
  
  # Inicializamos un nuevo data frame con una columna: 'iniciativaRelacionada'
  resultado <- data.frame(iniciativaRelacionada = datos$iniciativaRelacionada, stringsAsFactors = FALSE)
  
  # Recorrer cada fila del data frame original
  for (i in seq_len(nrow(datos))) {
    cod <- datos$codigoIndicador[i]
    
    # Construye los nombres de columnas personalizadas para ese indicador
    col_titulo <- paste0(cod, "titulo")
    col_contrib <- paste0(cod, "contribucionDirecta")
    col_valor <- paste0(cod, "valorObjetivo")
    
    # Si esas columnas no existen aún en 'resultado', las crea con NA
    if (!(col_titulo %in% names(resultado))) resultado[[col_titulo]] <- NA
    if (!(col_contrib %in% names(resultado))) resultado[[col_contrib]] <- NA
    if (!(col_valor %in% names(resultado))) resultado[[col_valor]] <- NA
    
    # Llena esas columnas con los valores de la fila actual
    resultado[[col_titulo]][i] <- datos$titulo[i]
    resultado[[col_contrib]][i] <- datos$contribucionDirecta[i]
    resultado[[col_valor]][i] <- datos$valorObjetivo[i]
  }
  
  todosLosIndicadores <- resultado
  return(todosLosIndicadores)
  
}








#####                        :::::::::::::::                                ####
#####                           PROCESO                                     ####

importarCoFFEE <- function() {


## * Importar informes                                                      ####
  
 
  
  presupuestos = importarPresupuestosCoFFEE()
  listadoTotal = importarListadoTotal()
  relacionActuaciones = importarRelacionActuaciones()
  instrumentosJuridicos = importarInstrumentosJuridicos()
  indicadores = importarResumenIndicadores()
  todosLosIndicadores = importarTodosLosIndicadores()
  
 
  

  

## * Uno los informes                                                       ####
  
  # Uno la información de presupuestos al listado de iniciativas
  resultado <- full_join(presupuestos, listadoTotal, by = "codigoIniciativa")

  # Añado la información de la relación de actuaciones
  resultado <- full_join(resultado, relacionActuaciones, by = "codigoIniciativa")
  
  # # Añado la información de los instrumentos jurídicos
  resultado <- full_join(resultado, instrumentosJuridicos, by = "codigoIniciativa")
  
  # # Añado la información de los indicadores
  resultado <- full_join(resultado, indicadores, by = "codigoIniciativa")
  
  
  # # Añado la información con una columna para cada indicador
  resultado <- full_join(resultado, todosLosIndicadores, by = c("codigoIniciativa" = "iniciativaRelacionada"))
  
  # # Colapso la matriz diagonal de indicadores para unificarlo en un único registro
  # ── 1. Identificar columnas  ────────────────────────────────────────────
  indCols   <- names(resultado) %>% str_detect("^IND")  # TRUE/FALSE por columna
  colsInd   <- names(resultado)[indCols]                # columnas “IND…” Son las columnas de la matriz diagonal de indicadores
  colsClave <- names(resultado)[!indCols]               # resto (id, nombre, …) Son las columnas con valores duplicados
  
  # ── 2. Colapsar en data.table  ──────────────────────────────────────────
  dt <- as.data.table(resultado)
  
  resultadoColapsado <- dt[
    , lapply(.SD, \(x) x[which(!is.na(x))[1L]]),  # primer valor no-NA en cada IND*
    by      = colsClave,                         # agrupar por las columnas clave
    .SDcols = colsInd                            # solo las columnas dispersas
  ]
  
  # ── 3. Volver a data.frame  ─────────────────────────────────────────────
  resultado <- as.data.frame(resultadoColapsado)
  
  
  # Aplico el formato número a los valores objetivos
  resultado <- resultado %>%
    mutate(
      across(
        ends_with("valorObjetivo"),
        ~ round(as.numeric(.x), 10)             # numérico redondeado
      )
    )



  # Informo el campo claseActuacion con proyecto, subproyecto o subproyecto instrumental
  
  resultado <- resultado %>%
    mutate(claseActuacion = ifelse(
      is.na(claseActuacion),
      case_when(
        tipoIniciativa == "P" ~ "Proyecto",
        tipoIniciativa == "SP" ~ "Subproyecto",
        tipoIniciativa == "SI" ~ "Subproyecto Instrumental",
        TRUE ~ claseActuacion # Mantener valores existentes o no coincidencias
      ),
      claseActuacion # Mantener valores existentes si no están vacíos
    ))
  
  
## * Reordeno y renombro columnas                                                      ####
  
  

  resultado <- resultado %>%
    rename(
      pptN = presupuestoPrevistoSinIva,
      ivaPpt = ivaPresupuestoPrevisto,
      pptB = presupuestoPrevistoTotal,
      cefMrr = costeEstimadoAFinanciarPorElMrr
  )
  
  #ordenamos un poquillo las columnas
  resultado <- resultado %>% 
    select(codigoIniciativa, 
           codigoProvisionalIniciativa, 
           claseActuacion,
           denominacionIniciativa,
           ppn2020,
           ppn2021,
           ppn2022,
           ppn2023,
           ppn2024,
           ppn2025,
           ppn2026,
           pptN, 
           cefMrr, 
           ivaPpt, 
           pptB, 
           importeOperacionesSinIva,
           sumaImportesNetosIjs,
           importeTotalOperaciones,
           sumaImportesBrutosIjs,
           importeDestinatariosSinIva,
           importeTotalDestinatarios,
           
           everything())

  return (resultado)
  
}

