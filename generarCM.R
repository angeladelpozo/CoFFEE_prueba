# # #####                          :::::::::::::::                              #### 
# # #####                             PROCESO                                   ####
# 
source("configFondos.R")

####### Funcion principal
menu_interactivo <- function() {

    # Mostrar pasos
    cat("\n========== MENÚ PRINCIPAL ==========\n")
    cat("2. Importar datos de CoFFEE y exportar IJs como xlsx\n")
    cat("3. Imputación manual y depuración datos de CoFFEE (depende del Paso 2)\n")
    cat("4. Exportar informes CoFFEE generados (depende del Paso 3)\n")
    cat("=====================================\n")
    
    # Ejecutar acciones según la opción seleccionada
    
    tryCatch(
    {
      cat("\n==> Paso 2: Importación de datos de CoFFEE y exportación de IJs\n")
      datosCoFFEE <- importarCoFFEE()
      # Construir la ruta completa
      fullPath <- file.path(testDataPath, "datosCoFFEE.rds")
            
      # Guardar el archivo
      saveRDS(datosCoFFEE, file = fullPath)
            
      # Confirmar la operación
      cat("Archivo guardado en:", fullPath, "\n")
      
      cat("Importación desde CoFFEE completada.\n")

      cat("\n==> Paso 3: Depuración de datos de CoFFEE\n")
      
      coffeeImputado <- imputacionManual(datosCoFFEE)
      coffeeDepurado <- depurarCoffee(coffeeImputado)

      cat("Depuración de CoFFEE completada.\n")

      cat("\n==> Paso 4: Exportación de informes generados\n")

      coffeeConsolidado <- exportarCoffee(coffeeDepurado)

      # Verificar si la carpeta existe
      if (!dir.exists(testDataPath)) {
        dir.create(testDataPath, recursive = TRUE) # Crear la carpeta si no existe
        cat("Carpeta creada en:", testDataPath, "\n")
      }
    }
    , error = function(e)
    {
      cat("ERROR: ", e$message, "\n")
    })
}

# Funcion main
menu_interactivo()
