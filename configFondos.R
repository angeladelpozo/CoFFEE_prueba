#####                          :::::::::::::::                              ####
#####                             CONSTANTES                                ####


logActivado <- FALSE


# Define el orden de las columnas, organizadas por bloques
orden <- c(
  # Información de la iniciativa

  "codigoIniciativa", "codigoProvisionalIniciativa", "claseActuacion", "subtipoActuacion",
  "estructuraPrtr","listadoCids", "denominacionIniciativa", 
  
  
  # Indicadores
  "IND001180", "IND001180a", "IND001180b", "IND001180c", "IND001180d", "IND001180e", "IND001194", "IND001195", "IND001195a", "IND001195b", "IND001195c", "IND001195d", "IND001195e", "IND001199", "IND001200", "IND001200a", "IND001200b", "IND001201", "IND001201a", "IND001201v0", "IND001201v0a", "IND001201v0b", "IND001204", "IND001201b", "IND001209", "IND001209a", "IND001209b", "IND001209c", "IND001209d", "IND0010L35", "IND0010L36", "IND001L39", "IND001192", "IND001245", "IND001245a", "IND001245b", "IND001246", "IND001246a", "IND001246b", "IND001247", "IND001248", "IND001249", "IND001250", "IND001251", "IND001252", "IND001253", "IND001457", "IND001458", "IND001284", "IND001284a", "IND001284b", "IND001466Aa", "IND001286", "IND001288", "IND001292", "IND001293", "IND001294", "IND001285", "IND001313", "IND001313a", "IND001313b", "IND001313c", "IND001366", "IND001366a", "IND001366b", "IND001476", "IND001476a", "IND001476b", "IND001L75", "IND001L75a", "IND001L75b", "IND001L76", "IND0010L77", "IND0010L78", "IND0010L78a", "IND0010L78b", "IND0010L78c", "IND0010L79", "IND0010L79a", "IND0010L79b", "IND0010L79c", "IND0010L80",


  # Datos económicos
  "ppn2020", "ppn2021", "ppn2022", "ppn2023", "ppn2024", "ppn2025", "ppn2026",
  "pptN", "cefMrr", "ivaPpt", "pptB", "importeOperacionesSinIva", "sumaImportesNetosIjs",
  "importeTotalOperaciones", "sumaImportesBrutosIjs", "importeDestinatariosSinIva",
  "importeTotalDestinatarios",
  
  "estadoIniciativa",
  
  
  # Información jerárquica
  "codigoIniciativaPadre",  "estructura","tipoIniciativa", "profundidad",


  # Datos del responsable
  "nombreResponsable", "cargoResponsable", "nifResponsable", "telefonoResponsable",
  "correoElectronicoResponsable", "radixResponsable",

  # Información organizacional
  "dir3Unidad", "denominacionUnidad", "fechaInicio", "fechaFin", "dir3Ed", "entidadDecisora",
  "dir3Ee", "entidadEjecutora", "nifNormalizadoEe", "dir3Og", "organoGestor", "ambitoMayoritario",
  "ambito", "ccaa", "provincia",

  # Información del subproyecto y observaciones
  "destinoSubproyecto", "codigoMedida",  "desembolsos", "noOperaciones",
  "noOperacionesConDestinatario", "tipoIniciativaPadre", "noAuxDefinicion", "noAuxPlanificacion",
  "observacionesConcatenadas",

  # # Validaciones
  "validacion1a", "validacion1c", "validacion2c", "validacion3", "validacion6b",
  
  # # Generacion de hitos
  "subdireccionResponsable", "esBeneficiarioPublicoPrivado", "tipoInstrumentoSigefe", "tipoResponsableParaHitosAuxiliares",

  # # Otros
  "profundidadPrtr","generadaPorIniciativa", "codigoProvisionalGeneradaPorIniciativa",  "denominacionGeneradaPorIniciativa"
  
  )

# Define el orden de las columnas, organizadas por bloques
columnasAeliminar <- c(
  
  
  # Información jerárquica
  "codigoIniciativaPadre",  "estructura","tipoIniciativa", "profundidad",
  
  # Datos del responsable
  "nombreResponsable", "cargoResponsable", "nifResponsable", "telefonoResponsable",
  "correoElectronicoResponsable", "radixResponsable",
  
  # Información organizacional
  "dir3Unidad", "denominacionUnidad", "fechaInicio", "fechaFin", "dir3Ed", "entidadDecisora",
  "dir3Ee", "entidadEjecutora", "nifNormalizadoEe", "dir3Og", "organoGestor", "ambitoMayoritario",
  "ambito", "ccaa", "provincia",
  
  # Información del subproyecto y observaciones
  "destinoSubproyecto", "codigoMedida",  "desembolsos","tipoIniciativaPadre", "observacionesConcatenadas",
  
  # Generacion de hitos
  "subdireccionResponsable", "esBeneficiarioPublicoPrivado", "tipoInstrumentoSigefe", "tipoResponsableParaHitosAuxiliares",
    

  # # Indicadores
  # "IND001180", "IND001180a", "IND001180b", "IND001180c", "IND001180d", "IND001180e", "IND001194", "IND001195", "IND001195a", "IND001195b", "IND001195c", "IND001195d", "IND001195e", "IND001199", "IND001200", "IND001200a", "IND001200b", "IND001201", "IND001201a", "IND001201v0", "IND001201v0a", "IND001201v0b", "IND001204", "IND001201b", "IND001209", "IND001209a", "IND001209b", "IND001209c", "IND001209d", "IND0010L35", "IND0010L36", "IND001L39", "IND001192", "IND001245", "IND001245a", "IND001245b", "IND001246", "IND001246a", "IND001246b", "IND001247", "IND001248", "IND001249", "IND001250", "IND001251", "IND001252", "IND001253", "IND001457", "IND001458", "IND001284", "IND001284a", "IND001284b", "IND001466Aa", "IND001286", "IND001288", "IND001292", "IND001293", "IND001294", "IND001285", "IND001313", "IND001313a", "IND001313b", "IND001313c", "IND001366", "IND001366a", "IND001366b", "IND001476", "IND001476a", "IND001476b", "IND001L75", "IND001L75a", "IND001L75b", "IND001L76", "IND0010L77", "IND0010L78", "IND0010L78a", "IND0010L78b", "IND0010L78c", "IND0010L79", "IND0010L79a", "IND0010L79b", "IND0010L79c", "IND0010L80",

  # # Otros
  ##"profundidadPrtr","generadaPorIniciativa", 
  "codigoProvisionalGeneradaPorIniciativa", "denominacionGeneradaPorIniciativa"
  
  
  )

# Define los subtipos de actuacion

tipologiaSubtipoActuacion <- data.frame(
  tipoConvenio = 
    c(
      
      "Convenio suscrito con entidad privada.",
      
      "Convenio suscrito con entidad pública contemplada en el art. 3 de la Res. 1/2022 de 12 de abril de la SGFE: 
          a)  Ministerios identificados explícitamente en el PRTR ((27/04/2021).
          b)  Entidades del sector público institucional vinculadas o dependientes de Ministerios, si:
          - Aparecen expresamente en el PRTR.
          -Tienen competencias funcionales según RD de estructura o sus Estatutos de funcionamiento.
          c)  Consejerías autonómicas o divisiones departamentales análogas.
          d)  Entidades locales recogidas en el artículo 3 de la Ley 7/1985 sin importar el instrumento jurídico empleado.
          e)  Universidades públicas en los subproyectos que se les atribuyan.",
      
      "Convenio suscrito con la FEMP o con entidad pública distinta de las contempladas en el art. 3 de la Res. 1/2022 de 12 de abril de la SGFE. 
          Es decir, distinta de: 
          a)  Ministerios identificados explícitamente en el PRTR ((27/04/2021).
          b)  Entidades del sector público institucional vinculadas o dependientes de Ministerios, si:
          - Aparecen expresamente en el PRTR.
          - Tienen competencias funcionales según RD de estructura o sus Estatutos de funcionamiento.
          c)  Consejerías autonómicas o divisiones departamentales análogas.
          d)  Entidades locales recogidas en el artículo 3 de la Ley 7/1985 sin importar el instrumento jurídico empleado.
          e)  Universidades públicas en los subproyectos que se les atribuyan."
    ),
  
  tipoModificacionDeCredito = 
    c(
      
      "Modificación de créditos presupuestarios de entidades públicas contempladas en el art. 3 de la Res. 1/2022 de 12 de abril de la SGFE:\na)  Ministerios identificados explícitamente en el PRTR ((27/04/2021).\nb)  Entidades del sector público institucional vinculadas o dependientes de Ministerios, si:\n- Aparecen expresamente en el PRTR.\n-Tienen competencias funcionales según RD de estructura o sus Estatutos de funcionamiento.\nc)  Consejerías autonómicas o divisiones departamentales análogas.\nd)  Entidades locales recogidas en el artículo 3 de la Ley 7/1985 sin importar el instrumento jurídico empleado.\ne)  Universidades públicas en los subproyectos que se les atribuyan.",
      
      "Modificación de créditos presupuestarios de entidades públicas distintas de las contempladas en el art. 3 de la Res. 1/2022 de 12 de abril de la SGFE. Es decir, distinta de:\na)  Ministerios identificados explícitamente en el PRTR ((27/04/2021).\nb)  Entidades del sector público institucional vinculadas o dependientes de Ministerios, si:\n- Aparecen expresamente en el PRTR.\n- Tienen competencias funcionales según RD de estructura o sus Estatutos de funcionamiento.\nc)  Consejerías autonómicas o divisiones departamentales análogas.\nd)  Entidades locales recogidas en el artículo 3 de la Ley 7/1985 sin importar el instrumento jurídico empleado.\ne)  Universidades públicas en los subproyectos que se les atribuyan."
    ),
  
  tipoOtrosEspecificar = 
    c(
      
      "Aportación dineraria destinada a entidad pública contemplada en el art. 3 de la Res. 1/2022 de 12 de abril  de la SGFE: 
        a)  Ministerios identificados explícitamente en el PRTR ((27/04/2021).
        b)  Entidades del sector público institucional vinculadas o dependientes de Ministerios, si:
        - Aparecen expresamente en el PRTR.
        -Tienen competencias funcionales según RD de estructura o sus Estatutos de funcionamiento.
        b)  Consejerías autonómicas o divisiones departamentales análogas.
        c)  Entidades locales recogidas en el artículo 3 de la Ley 7/1985 sin importar el instrumento jurídico empleado.
        d)  Universidades públicas en los subproyectos que se les atribuyan",
      
      "Aportación dineraria destinada a entidad privada.",
      
      "Aportación dineraria destinada a la FEMP o entidades públicas distintas de las contempladas en el art. 3 de la Res. 1/2022 de 12 de abril  de la SGFE. Es decir, entidades distintas de:
        a)  Ministerios identificados explícitamente en el PRTR ((27/04/2021).
        b)  Entidades del sector público institucional vinculadas o dependientes de Ministerios, si:
        - Aparecen expresamente en el PRTR.
        -Tienen competencias funcionales según RD de estructura o sus Estatutos de funcionamiento
        b)  Consejerías autonómicas o divisiones departamentales análogas.
        c)  Entidades locales recogidas en el artículo 3 de la Ley 7/1985 sin importar el instrumento jurídico empleado.
        d)  Universidades públicas en los subproyectos que se les atribuyan",
      
      "Aportación patrimonial destinada a entidad pública contemplada en el art. 3 de la Res. 1/2022 de 12 de abril  de la SGFE: 
        a)  Ministerios identificados explícitamente en el PRTR ((27/04/2021).
        b)  Entidades del sector público institucional vinculadas o dependientes de Ministerios, si:
        - Aparecen expresamente en el PRTR.
        -Tienen competencias funcionales según RD de estructura o sus Estatutos de funcionamiento
        b)  Consejerías autonómicas o divisiones departamentales análogas.
        c)  Entidades locales recogidas en el artículo 3 de la Ley 7/1985 sin importar el instrumento jurídico empleado.
        d)  Universidades públicas en los subproyectos que se les atribuyan",
      
      "Aportación patrimonial destinada a entidad privada",
      
      "Aportación patrimonial destinada a la FEMP o entidades públicas distintas de las contempladas en el art. 3 de la Res. 1/2022 de 12 de abril  de la SGFE. Es decir, entidades distintas de:
        a)  Ministerios identificados explícitamente en el PRTR ((27/04/2021).
        b)  Entidades del sector público institucional vinculadas o dependientes de Ministerios, si:
        - Aparecen expresamente en el PRTR.
        -Tienen competencias funcionales según RD de estructura o sus Estatutos de funcionamiento
        b)  Consejerías autonómicas o divisiones departamentales análogas.
        c)  Entidades locales recogidas en el artículo 3 de la Ley 7/1985 sin importar el instrumento jurídico empleado.
        d)  Universidades públicas en los subproyectos que se les atribuyan",
      
      "Ley/ Plan/ Normativa",
      
      "Otros",
      
      "Préstamos participativos (conforme al art. 20 del Real Decreto-Ley 7/1996) a entidades pequeñas y medianas empresas (según la definición comtemplada en el Anexo I del Reglamento (UE) nº 651/2014 de la Comisión, de 17 de junio de 2014, por el que se declaran determinadas categorías de ayudas compatibles con el mercado interior en aplicación de los artículos 107 y 108 del Tratado).",
      
      "Ley/ Plan/ Normativa",
      
      "Acuerdo interdepartamental",
      
      "Pendiente de definir"),
  
  
  tipoSubvencion = 
    c(
      "Ayudas directas (subvenciones conforme al art. 22.2 de la Ley 38/2003, de 17 de noviembre, General de Subvenciones) otorgadas a entidad pública contemplada en el art. 3 de la Res. 1/2022 de 12 de abril de la SGFE:
          a) Ministerios identificados explícitamente en el PRTR ((27/04/2021).
          b) Entidades del sector público institucional vinculadas o dependientes de Ministerios, si:
            - Aparecen expresamente en el PRTR.
            - Tienen competencias funcionales según RD de estructura o sus Estatutos de funcionamiento.
          c) Consejerías autonómicas o divisiones departamentales análogas.
          d) Entidades locales recogidas en el artículo 3 de la Ley 7/1985 sin importar el instrumento jurídico empleado.
          e) Universidades públicas en los subproyectos que se les atribuyan.",
      
      "Ayudas directas (subvenciones conforme al art. 22.2 de la Ley 38/2003, de 17 de noviembre, General de Subvenciones) otorgadas a la FEMP o a entidad pública distinta de las contempladas en el art. 3 de la Res. 1/2022 de 12 de abril de la SGFE.Es decir, entidades distintas de:
          a) Ministerios identificados explícitamente en el PRTR ((27/04/2021).
          b) Entidades del sector público institucional vinculadas o dependientes de Ministerios, si:
            - Aparecen expresamente en el PRTR.
            - Tienen competencias funcionales según RD de estructura o sus Estatutos de funcionamiento.
          c) Consejerías autonómicas o divisiones departamentales análogas.
          d) Entidades locales recogidas en el artículo 3 de la Ley 7/1985 sin importar el instrumento jurídico empleado.
          e) Universidades públicas en los subproyectos que se les atribuyan.",
      
      "Ayudas directas (subvenciones conforme el art. 22.2 de la Ley 38/2003, de 17 de noviembre, General de Subvenciones) otorgadas a entidad privada.",
      
      "Convocatoria de ayudas en régimen de concurrencia competitiva dirigida a entidad pública contemplada en el art. 3 de la Res. 1/2022 de 12 de abril de la SGFE: 
          a)  Ministerios identificados explícitamente en el PRTR ((27/04/2021).
          b)  Entidades del sector público institucional vinculadas o dependientes de Ministerios, si:
          - Aparecen expresamente en el PRTR.
          -Tienen competencias funcionales según RD de estructura o sus Estatutos de funcionamiento
          b)  Consejerías autonómicas o divisiones departamentales análogas.
          c)  Entidades locales recogidas en el artículo 3 de la Ley 7/1985 sin importar el instrumento jurídico empleado.
          d)  Universidades públicas en los subproyectos que se les atribuyan",
      
      "Convocatoria de ayudas en régimen de concurrencia competitiva dirigida a la FEMP o entidades públicas distintas de las contempladas en el art. 3 de la Res. 1/2022 de 12 de abril de la SGFE.  Es decir, entidades distintas de:
          a)  Ministerios identificados explícitamente en el PRTR ((27/04/2021).
          b)  Entidades del sector público institucional vinculadas o dependientes de Ministerios, si:
          - Aparecen expresamente en el PRTR.
          -Tienen competencias funcionales según RD de estructura o sus Estatutos de funcionamiento
          b)  Consejerías autonómicas o divisiones departamentales análogas.
          c)  Entidades locales recogidas en el artículo 3 de la Ley 7/1985 sin importar el instrumento jurídico empleado.
          d)  Universidades públicas en los subproyectos que se les atribuyan",
      
      "Convocatoria de ayudas en régimen de concurrencia competitiva otorgadas a entidad privada."
    ))















#####                          :::::::::::::::                              ####
#####                             RUTAS                                     ####

#Obtenemos ruta del directorio de trabajo2
raizPath <- getwd()

## Ruta a las funciones útiles generales
utils_path <- file.path(raizPath, 'utils')

## Ruta scripts generales 
scriptsPath <- file.path(raizPath, 'fondos/code/scripts')

## Ruta scripts Coffee
scriptsCoffeePath <- file.path(raizPath, 'fondos/code/scripts/CoFFEE')

## Ruta a las funciones útiles
funcionesPersonal_path <- file.path(raizPath, 'fondos/code/funciones')

## Ruta de funciones Útiles para fondos
funcionesPath <- file.path(raizPath, 'fondos/code/funciones')

## Ruta datos input
data_path <- file.path(raizPath, 'fondos/data')

## Ruta actuacionesP SIGEFE
actuacionesP_path <- file.path(raizPath, 'fondos/data/cargaMasiva/SIGEFE/actuacionesProyecto.xlsx')

## Ruta actividades SIGEFE
actividades_path <- file.path(raizPath, 'fondos/data/cargaMasiva/SIGEFE/actividades.xlsx')


## Ruta subproyectos cargados en SIGEFE
subp_path <- file.path(raizPath, 'fondos/data/cargaMasiva/SIGEFE/subproyectos.xlsx')

## Ruta actuaciones de subproyecto cargadas en SIGEFE
actSub_path <- file.path(raizPath, 'fondos/data/cargaMasiva/SIGEFE/actuacionesSubproyecto.xlsx')

## Ruta presupuestos previstos anualizados cargados en CoFFEE
presupuestosPA_path <- file.path(raizPath, 'fondos/data/cargaMasiva/CoFFEE/Desembolsos - Histórico de modificaciones de presupuesto previsto anualizado.xlsx')

## Ruta histos estandarizados de actuacion
hitosEstandarRamaActuacionPath <- file.path(raizPath, 'fondos/data/cargaMasiva/CoFFEE/hosEstandarRamaActuacion.xlsx')


## Ruta resultados 
resultsPath <- file.path(raizPath, 'fondos/results')

## Ruta del archivo con las actuaciones que generan los subproyectos
dependenciasSubproyectosPath <- file.path(raizPath, 'fondos/data/cargaMasiva/CoFFEE/dependenciasSubproyectosCoffee.xlsx')



## Ruta de datos para tests 
testDataPath <- file.path(raizPath, 'fondos/data/test')
# #####                          :::::::::::::::                              #### 
# #####                             FUNCIONES                                 ####

source("utils/aCamelCase.R")
source("utils/deduplicarDataFrame.R")
source("utils/instalarSiFalta.R")
source("utils/exportarComoExcel.R")
source("utils/numeroAeuros.R")
source("utils/pasarColumnaAnumero.R")
source("utils/pasarColumnaAnumeroEntero.R")
source("utils/procesarFecha.R")


# Cargar módulos necesarios
source(file.path(scriptsPath, "imputacionManual.R"))
source(file.path(scriptsPath, "importarSIGEFE.R"))
source(file.path(scriptsPath, "importarCoFFEE.R"))
source(file.path(scriptsPath, "vincularSigefeCoffee.R"))
source(file.path(scriptsPath, "depurarCoffee.R"))
source(file.path(scriptsPath, "exportarCoffee.R"))
source(file.path(scriptsPath, "generarPlantillaCargaMasivaBasadaEnCoffee.R"))
source(file.path(scriptsPath, "CoFFEE/importarHosDeCoffee.R"))
source(file.path(scriptsPath, "CoFFEE/generarHosEstandarizados.R"))




# Cargar módulos necesarios para la depuración de Coffee
source(file.path(scriptsCoffeePath, "ordenarCoffee.R"))
source(file.path(scriptsCoffeePath, "obtenerDescendientesDeActuacion.R"))
source(file.path(scriptsCoffeePath, "validacion1a.R"))
source(file.path(scriptsCoffeePath, "validacion1c.R"))
source(file.path(scriptsCoffeePath, "validacion2c.R"))
source(file.path(scriptsCoffeePath, "validacion3.R"))
source(file.path(scriptsCoffeePath, "validacion6a.R"))

# Cargar módulos exportacion Coffee
source(file.path(scriptsCoffeePath, "exportarExcelProyectoConValidacion.R"))

# Cargar módulos necesarios
source(file.path(scriptsCoffeePath, "dibujarEstructura.R"))
source(file.path(scriptsCoffeePath, "dibujarEstructuraPrtr.R"))
source(file.path(scriptsCoffeePath, "rellenarProfundidadPrtr.R"))


#####                          :::::::::::::::                              ####
#####                             DESCARGAS y PAQUETES LOCAL                ####
# Instalar y cargar los paquetes necesarios
instalarSiFalta("data.table")
instalarSiFalta("data.tree")
instalarSiFalta("DiagrammeR")
instalarSiFalta("DT")
instalarSiFalta("lubridate")
instalarSiFalta("readxl")
instalarSiFalta("writexl")
instalarSiFalta("dplyr")
instalarSiFalta("fuzzyjoin")
instalarSiFalta("data.tree")
instalarSiFalta("openxlsx")
instalarSiFalta("tidyr")
instalarSiFalta("XLConnect")

#####                          :::::::::::::::                              ####
#####                             PAQUETES                                  ####
library(data.table)
library(data.tree)
library(lubridate)
library(readxl)
library(writexl)
library(dplyr)
library(DT)
library(openxlsx)
library(fuzzyjoin)
library(stringr)
library(tidyr)
library(DiagrammeR)
library(XLConnect)
