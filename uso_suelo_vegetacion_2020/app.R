# ...........................................................
# VEGETACIÓN Y USO DE SUELO - 2020
# Conjunto de datos: Capital Natural de Mexico ante el Cambio Climatico

# ...........................................................

# 1. PAQUETES


library(shiny)
library(shinythemes)
library(sf)
library(dplyr)
library(readxl)
library(leaflet)
library(htmltools)


# ...........................................................

# 2. CONFIGURACIÓN GLOBAL


options(shiny.maxRequestSize = 50 * 1024^2)


# ...........................................................

# 3. RUTAS DE DATOS


encontrar_archivo <- function(ruta_local, ruta_produccion = NULL) {

  if (file.exists(ruta_local)) {
    return(ruta_local)
  }

  if (!is.null(ruta_produccion) &&
      file.exists(ruta_produccion)) {
    return(ruta_produccion)
  }

  archivo_base <- basename(ruta_local)

  if (file.exists(archivo_base)) {
    return(archivo_base)
  }

  ruta_data <- file.path("data", archivo_base)

  if (file.exists(ruta_data)) {
    return(ruta_data)
  }

  warning(
    paste(
      "No se pudo encontrar el archivo:",
      archivo_base
    )
  )

  return(ruta_local)
}


ruta_malla <- encontrar_archivo(
  "malla_hex_10km.gpkg"
)

ruta_excel <- encontrar_archivo(
  "bau_2020_hadgem2_e5.xlsx"
)


# ...........................................................

# 4. CARGAR DATOS



cargar_datos <- function() {

  tryCatch({

    cat("Cargando datos geoespaciales...\n")

    malla <- st_read(
      ruta_malla,
      quiet = TRUE
    )


    cat("Cargando indicadores...\n")

    indicador <- read_excel(
      ruta_excel,
      sheet = "datos"
    )

    catalogo <- read_excel(
      ruta_excel,
      sheet = "catalogo_variable"
    )

    metadatos <- read_excel(
      ruta_excel,
      sheet = "metadatos"
    )


    # Validaciones


    if (!"hex_id" %in% names(malla)) {

      stop(
        "La malla no contiene la columna 'hex_id'."
      )

    }


    if (!all(c("id_hex", "valor") %in% names(indicador))) {

      stop(
        "El indicador no contiene las columnas ",
        "'id_hex' y/o 'valor'."
      )

    }


    list(
      malla = malla,
      indicador = indicador,
      catalogo = catalogo,
      metadatos = metadatos
    )


  }, error = function(e) {

    cat(
      "Error al cargar datos:",
      e$message,
      "\n"
    )

    return(NULL)

  })

}


# ...........................................................

# 5. CARGAR DATOS


datos_cargados <- cargar_datos()


if (is.null(datos_cargados)) {

  stop(
    "No se pudieron cargar los datos necesarios. ",
    "Verifica las rutas de los archivos."
  )

}


# ...........................................................

# 6. EXTRAER OBJETOS


malla <- datos_cargados$malla
indicador <- datos_cargados$indicador
catalogo <- datos_cargados$catalogo
metadatos <- datos_cargados$metadatos


# ...........................................................

# 7. PREPARACIÓN DE DATOS


# Seleccionar primera variable

variable <- catalogo |> slice(1)

nombre_variable <- variable$variable_nombre
tipo_dato <- variable$tipo_dato
unidades <- variable$unidades
descripcion <- variable$descripcion
fuente <- variable$fuente


# Convertir valores a texto

nombre_variable <- as.character(nombre_variable)
tipo_dato <- as.character(tipo_dato)
unidades <- as.character(unidades)
descripcion <- as.character(descripcion)
fuente <- as.character(fuente)



# Unir datos


datos <- malla |>
  left_join(
    indicador,
    by = c("hex_id" = "id_hex")
  )



# Validación del JOIN


n_sin_dato <- sum(
  is.na(datos$valor)
)


if (nrow(datos) != nrow(malla)) {

  warning(
    "El JOIN modificó el número de filas. ",
    "Verificar posibles duplicados."
  )

}



# Validación y limpieza de geometrías


cat(
  "Validando geometrías...\n"
)


# Eliminar geometrías vacías

datos <- datos[
  !st_is_empty(datos),
]


# Reparar geometrías inválidas

geometrias_invalidas <- sum(
  !st_is_valid(datos)
)

cat(
  "Geometrías inválidas:",
  geometrias_invalidas,
  "\n"
)


if (geometrias_invalidas > 0) {

  datos <- st_make_valid(datos)

}



# Transformar a WGS84 para Leaflet


if (!is.na(st_crs(datos))) {

  datos <- st_transform(
    datos,
    4326
  )

}



# Categorías

categorias <- sort(
  unique(
    na.omit(datos$valor)
  )
)


# ...........................................................

# 8. PALETA DE COLORES


paleta_colores_base <- c(
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7",
  "#000000"
)


if (
  length(categorias) <=
  length(paleta_colores_base)
) {

  colores <- paleta_colores_base[
    seq_along(categorias)
  ]

} else {

  colores <- colorRampPalette(
    paleta_colores_base
  )(
    length(categorias)
  )

}


paleta <- colorFactor(
  palette = colores,
  domain = categorias,
  na.color = "#D9D9D9"
)


# ...........................................................

# 9. MÉTODO DE EXTRACCIÓN


metodo_extraccion <- metadatos |>
  filter(
    clave == "metodo_extraccion"
  ) |>
  pull(valor)


if (length(metodo_extraccion) == 0) {

  metodo_extraccion <- "No especificado"

} else {

  metodo_extraccion <- as.character(
    metodo_extraccion[1]
  )

}


# ...........................................................

# 10. UI


ui <- fluidPage(

  theme = shinytheme("flatly"),



  # CSS


  tags$head(

    tags$style(

      HTML("

        /* GENERAL */

        body {
          background-color: #f4f6f8;
          font-family: 'Segoe UI', Arial, sans-serif;
        }


        /* ENCABEZADO */

        .header-box {
          background: linear-gradient(
            135deg,
            #2c3e50,
            #34495e
          );

          color: white;

          padding: 22px 25px;

          margin-bottom: 20px;

          border-radius: 8px;

          box-shadow:
            0 3px 8px rgba(0,0,0,0.12);
        }


        .header-box h1 {
          font-size: 28px;
          font-weight: 600;
        }


        .header-box p {
          font-size: 14px;
        }


        /* SIDEBAR */

        .well {
          background-color: #ffffff;

          border: 1px solid #dee2e6;

          border-radius: 8px;

          box-shadow:
            0 2px 6px rgba(0,0,0,0.06);
        }


        .sidebarPanel h4 {
          color: #2c3e50;
          font-weight: 600;
        }


        /* MAPA */

        .leaflet-container {

          border-radius: 8px;

          border: 1px solid #dee2e6;

          box-shadow:
            0 3px 10px rgba(0,0,0,0.10);
        }


        /* METADATOS */

        .metadata-panel {

          margin-top: 15px;

          margin-bottom: 25px;

          padding: 20px;

          background-color: #ffffff;

          border: 1px solid #dee2e6;

          border-radius: 8px;

          box-shadow:
            0 2px 7px rgba(0,0,0,0.07);
        }


        .metadata-title {

          margin-top: 0;

          margin-bottom: 20px;

          padding-bottom: 12px;

          border-bottom: 1px solid #e9ecef;

          color: #2c3e50;

          font-size: 18px;

          font-weight: 600;
        }


        .metadata-grid {

          display: grid;

          grid-template-columns:
            repeat(3, 1fr);

          gap: 14px;
        }


        .metadata-item {

          min-height: 80px;

          padding: 13px 15px;

          background-color: #f8f9fa;

          border-radius: 6px;

          border-left:
            4px solid #2c3e50;
        }


        .metadata-label {

          display: block;

          margin-bottom: 6px;

          color: #6c757d;

          font-size: 11px;

          font-weight: 700;

          text-transform: uppercase;

          letter-spacing: 0.4px;
        }


        .metadata-value {

          display: block;

          color: #2c3e50;

          font-size: 14px;

          font-weight: 500;

          line-height: 1.4;

          word-break: break-word;
        }


        /* BOTONES */

        .btn-block {

          border-radius: 5px;

          font-weight: 500;
        }


        /* RESPONSIVE */

        @media (max-width: 1000px) {

          .metadata-grid {

            grid-template-columns:
              repeat(2, 1fr);

          }

        }


        @media (max-width: 600px) {

          .metadata-grid {

            grid-template-columns:
              1fr;

          }


          .header-box h1 {

            font-size: 22px;

          }

        }

      ")

    )

  ),



  # ENCABEZADO


  div(

    class = "header-box",

    h1(

      nombre_variable,

      style = "margin: 0;"

    ),

    p(

      descripcion,

      style = "
        margin: 7px 0 0 0;
        opacity: 0.85;
      "

    )

  ),



  # ESTRUCTURA PRINCIPAL


  sidebarLayout(


   
    # SIDEBAR
   

    sidebarPanel(

      width = 3,


    
      # FILTROS


      div(

        class = "well",

        h4("Filtros"),


        selectInput(

          inputId = "categoria",

          label =
            "Seleccionar categoría:",

          choices =
            c(
              "Todas",
              categorias
            ),

          selected = "Todas"

        ),


        checkboxInput(

          inputId =
            "mostrar_sin_datos",

          label =
            "Mostrar hexágonos sin datos",

          value = TRUE

        )

      ),


      # DESCARGAS
   

      div(

        class = "well",

        h4("Descarga"),


        downloadButton(

          outputId =
            "descargar_csv",

          label =
            "Descargar CSV",

          class =
            "btn-primary btn-block"

        ),


        br(),
        br(),


        downloadButton(

          outputId =
            "descargar_geojson",

          label =
            "Descargar GeoJSON",

          class =
            "btn-success btn-block"

        )

      )

    ),


    
    # PANEL PRINCIPAL
  

    mainPanel(

      width = 9,



      # MAPA


      leafletOutput(

        outputId = "mapa",

        height = "550px"

      ),


      # RESUMEN DEL MAPA


      div(

        class = "info-box",

        p(

          style = "margin: 0;",

          "Hexágonos mostrados: ",

          strong(

            textOutput(

              "hex_count",

              inline = TRUE

            )

          ),

          " | Total: ",

          strong(

            format(

              nrow(datos),

              big.mark = ","

            )

          ),

          " | Sin datos: ",

          strong(

            format(

              n_sin_dato,

              big.mark = ","

            )

          )

        )

      ),



      # METADATOS

      div(

        class = "metadata-panel",


        h4(

          class = "metadata-title",

          "Metadatos del indicador"

        ),


        div(

          class = "metadata-grid",


          # VARIABLE

          div(

            class = "metadata-item",

            span(

              class = "metadata-label",

              "Variable"

            ),

            span(

              class = "metadata-value",

              nombre_variable

            )

          ),


          # TIPO

          div(

            class = "metadata-item",

            span(

              class = "metadata-label",

              "Tipo de dato"

            ),

            span(

              class = "metadata-value",

              tipo_dato

            )

          ),


          # FUENTE

          div(

            class = "metadata-item",

            span(

              class = "metadata-label",

              "Fuente"

            ),

            span(

              class = "metadata-value",

              ifelse(

                is.na(fuente) ||
                  fuente == "",

                "No especificada",

                fuente

              )

            )

          )

        )

      )

    )

  )

)


# ...........................................................

# 11. SERVER


server <- function(

  input,
  output,
  session

) {



  # DATOS FILTRADOS


  datos_filtrados <- reactive({

    req(datos)

    df <- datos



    # Filtro por categoría


    if (
      input$categoria != "Todas"
    ) {

      df <- df |>

        filter(
          valor == input$categoria
        )

    }



    # Mostrar / ocultar sin datos


    if (
      !input$mostrar_sin_datos
    ) {

      df <- df |>

        filter(
          !is.na(valor)
        )

    }


    df

  })



  # MAPA BASE


  output$mapa <- renderLeaflet({

    leaflet() |>

      addProviderTiles(

        providers$CartoDB.Positron,

        options =
          providerTileOptions(

            minZoom = 5,

            maxZoom = 18

          )

      ) |>

      setView(

        lng = -101.962332,

        lat = 22.541096,
        
        zoom = 5

      )

  })


  # ACTUALIZAR MAPA Y LEYENDA


  observe({

    datos_actuales <-
      datos_filtrados()


    req(
      nrow(datos_actuales) > 0
    )



    # Asegurar geometrías válidas


    datos_actuales <-
      datos_actuales[
        !st_is_empty(datos_actuales),
      ]


    req(
      nrow(datos_actuales) > 0
    )



    # CATEGORÍAS DE LA LEYENDA


    if (input$categoria == "Todas") {

      categorias_actuales <- categorias

      mostrar_sin_dato_leyenda <- TRUE

    } else {

      categorias_actuales <- input$categoria

      mostrar_sin_dato_leyenda <- FALSE

    }



    # Actualizar Leaflet

    mapa <- leafletProxy(

      "mapa",

      data = datos_actuales

    )



    # MAPA


    mapa |>

      clearShapes() |>

      clearControls() |>

      addPolygons(

        fillColor =
          ~paleta(valor),

        fillOpacity =
          0.75,

        color =
          "#666666",

        weight =
          0.3,

        smoothFactor =
          0.2,



        # Resaltado


        highlightOptions =
          highlightOptions(

            weight = 2,

            color =
              "#333333",

            bringToFront =
              TRUE,

            fillOpacity =
              0.9

          ),



        # Popup


        popup =
          ~paste0(

            "<div style='min-width: 180px;'>",

            "<b>ID:</b> ",

            htmlEscape(

              as.character(hex_id)

            ),

            "<br>",

            "<b>Categoría</b> ",

            ifelse(

              is.na(valor),

              "Sin dato",

              htmlEscape(

                as.character(valor)

              )

            ),

            "<hr style='margin: 6px 0;'>",

            "</div>"

          ),



        # Label


        label =
          ~htmlEscape(

            as.character(valor)

          ),

        labelOptions =
          labelOptions(

            noHide = FALSE

          )

      )



    # LEYENDA 

    if (input$categoria == "Todas") {

      mapa |>

        addLegend(

          position =
            "bottomleft",

          pal =
            paleta,

          values =
            categorias_actuales,

          title =
            htmlEscape(

              nombre_variable

            ),

          opacity =
            0.9,

          na.label =
            "Sin dato"

        )

    } else {

      mapa |>

        addLegend(

          position =
            "bottomleft",

          pal =
            paleta,

          values =
            categorias_actuales,

          title =
            htmlEscape(

              nombre_variable

            ),

          opacity =
            0.9

        )

    }

  })



  # CONTADOR


  output$hex_count <- renderText({

    format(

      nrow(

        datos_filtrados()

      ),

      big.mark = ","

    )

  })



  # DESCARGA CSV


  output$descargar_csv <-
    downloadHandler(

      filename = function() {

        paste0(

          gsub(

            " ",

            "_",

            tolower(

              nombre_variable

            )

          ),

          "_",

          Sys.Date(),

          ".csv"

        )

      },


      content = function(file) {

        datos_filtrados() |>

          st_drop_geometry() |>

          write.csv(

            file,

            row.names =
              FALSE,

            fileEncoding =
              "UTF-8"

          )

      }

    )



  # DESCARGA GEOJSON


  output$descargar_geojson <-
    downloadHandler(

      filename = function() {

        paste0(

          gsub(

            " ",

            "_",

            tolower(

              nombre_variable

            )

          ),

          "_",

          Sys.Date(),

          ".geojson"

        )

      },


      content = function(file) {

        datos_export <-
          datos_filtrados()


        if (

          inherits(

            datos_export,

            "sf"

          )

        ) {

          st_write(

            datos_export,

            file,

            driver =
              "GeoJSON",

            delete_dsn =
              TRUE,

            quiet =
              TRUE

          )

        } else {

          write.csv(

            datos_export,

            file,

            row.names =
              FALSE

          )

        }

      }

    )

}


# ...........................................................

# 12. EJECUTAR APLICACIÓN


shinyApp(

  ui = ui,

  server = server

)

