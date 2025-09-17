# # #####                          :::::::::::::::                              #### 
# # #####                             PROCESO                                   ####
# 
source("configFondos.R")
menu_interactivo <- function() {
  completado <- list(
    paso1 = FALSE,
    paso2 = FALSE,
    paso3 = FALSE,
    paso4 = FALSE,
    paso5 = FALSE,
    paso6 = FALSE,
    paso7 = FALSE
  )
  
  repeat {
    # Mostrar menú
    cat("\n========== MENÚ PRINCIPAL ==========\n")
    cat("1. Organizar estructura y HOs SIGEFE como plantilla CM\n")
    cat("2. Importar datos de CoFFEE y exportar IJs como xlsx\n")
    cat("3. Imputación manual y depuración datos de CoFFEE (depende del Paso 2)\n")
    cat("4. Exportar informes CoFFEE generados (depende del Paso 3)\n")
    cat("5. Vincular datos de SIGEFE y CoFFEE (depende del Paso 4)\n")
    cat("6. Generar Plantilla de CM basada en CoFFEE\n")
    cat("7. Salir\n")
    cat("=====================================\n")
    
    # Leer opción del usuario
    opcion <- as.numeric(readline(prompt = "Seleccione una opción: "))
    
    # Ejecutar acciones según la opción seleccionada
    if (opcion == 1) {
      cat("\n==> Paso 1: Importar SIGEFE\n")
      tryCatch({
        datosSIGEFE <- importarSIGEFE(scriptsPath, actuacionesP_path, actividades_path, subp_path, actSub_path)
        completado$paso1 <- TRUE
        cat("Importación desde SIGEFE completada.\n")
      }, error = function(e) {
        cat("ERROR: ", e$message, "\n")
      })
 
      ## Exportar como excel
      ## Cambio los nombres de las hojas según se ha indicado, pese a ser menos informativo
      names(datosSIGEFE) <- c("P","A","A2","SP","ASP","HOP", "HOA", "HOA2", "HOSP", "HOASP")


      exportarComoExcel("proyectosEnSigefe", datosSIGEFE$P,resultsPath)
      exportarComoExcel("actuacionesEnSigefe", datosSIGEFE$A,resultsPath)
      exportarComoExcel("actividadesEnSigefe", datosSIGEFE$A2,resultsPath)
      exportarComoExcel("subproyectosEnSigefe", datosSIGEFE$SP,resultsPath)
      exportarComoExcel("actuacionesDeSubproyectoEnSigefe", datosSIGEFE$ASP,resultsPath)
      
    } else if (opcion == 2) {
        cat("\n==> Paso 2: Importación de datos de CoFFEE y exportación de IJs\n")
        tryCatch({
          datosCoFFEE <- importarCoFFEE()
         
          
          
          #CASO EJEMPLO
          # if (codigoActual == "C12.R01.P01"){
          if (TRUE){
            # Construir la ruta completa
            fullPath <- file.path(testDataPath, "datosCoFFEE.rds")
            
            # Guardar el archivo
            saveRDS(datosCoFFEE, file = fullPath)
            
            # Confirmar la operación
            cat("Archivo guardado en:", fullPath, "\n")
            
            
          }
          
          
          
          
          
          
          
          
          
          
          
          
          completado$paso2 <- TRUE
          cat("Importación desde CoFFEE completada.\n")
        }, error = function(e) {
          cat("ERROR: ", e$message, "\n")
        })
    
    } else if (opcion == 3) {
      if (!completado$paso2) {
        cat("Debe completar el Paso 2 antes de ejecutar este paso.\n")
      } else {
        cat("\n==> Paso 3: Depuración de datos de CoFFEE\n")
        tryCatch({

          coffeeImputado <- imputacionManual(datosCoFFEE)
          
          
    
          coffeeDepurado <- depurarCoffee(coffeeImputado)

          completado$paso3 <- TRUE
          cat("Depuración de datos de CoFFEE completada.\n")
        }, error = function(e) {
          cat("ERROR: ", e$message, "\n")
        })
      }
      
    } else if (opcion == 4) {
      if (!completado$paso3) {
        cat("Debe completar el Paso 3 antes de ejecutar este paso.\n")
      } else {
        cat("\n==> Paso 4: Exportación de informes generados\n")
        tryCatch({
          coffeeConsolidado <- exportarCoffee(coffeeDepurado)
          
          # Verificar si la carpeta existe
          if (!dir.exists(testDataPath)) {
            dir.create(testDataPath, recursive = TRUE) # Crear la carpeta si no existe
            cat("Carpeta creada en:", testDataPath, "\n")
          }
          

        
          completado$paso4 <- TRUE
         
          
          
        }, error = function(e) {
          cat("ERROR: ", e$message, "\n")
        })
      }
      
    } else if (opcion == 5) {
      if (!completado$paso4) {
        cat("Debe completar el Paso 4 antes de ejecutar este paso.\n")
      } else {
        cat("\n==> Paso 5: Vinculación de datos de SIGEFE y CoFFEE\n")
        tryCatch({
          correspondenciaIniciativas <- vincularSigefeCoffee(coffeeConsolidado, datosSIGEFE)
          cat("Falta por desarrollar\n")
          
          completado$paso5 <- TRUE
          # cat("Vinculación completada.\n")
        }, error = function(e) {
          cat("ERROR: ", e$message, "\n")
        })
      }
      
    } else if (opcion == 6) {
      if (!completado$paso1 || !completado$paso2 || !completado$paso3 || !completado$paso4 || !completado$paso5) {
        cat("Debe completar los Pasos 1 al 5 antes de ejecutar este paso.\n")
      } else {
        cat("\n==> Paso 6: Generar Plantilla de CM basada en CoFFEE\n")
        tryCatch({
         
         
          generarPlantillaCargaMasivaBasadaEnCoffee(coffeeConsolidado)
          # cat("Plantilla de CM basada en CoFFEE generada con éxito.\n")
        }, error = function(e) {
          cat("ERROR: ", e$message, "\n")
        })
      }
      
    } else if (opcion == 7) {
      cat("\nSaliendo del programa. ¡Hasta luego!\n")
      break
      
    } else {
      cat("\nOpción no válida. Por favor, seleccione una opción entre 1 y 7.\n")
    }
  }
}

menu_interactivo()
