## Uso de suelo y vegetación 2020

### Objetivo

Se desarrolló una aplicación web interactiva en **R Shiny** para facilitar la consulta y exploración de información geoespacial de uso de suelo y vegetación correspondiente al año 2020. La aplicación busca presentar información espacial de manera clara y accesible mediante una interfaz web.

### Datos y procesamiento

La aplicación integra información geoespacial de uso de suelo y vegetación, previamente preparada para su visualización obtenida de Mendoza-Ponce et al. (2019) en Capital Natural de Mexico ante el Cambio Climatico (https://climaysociedad.atmosfera.unam.mx:8080/shiny/CapitalNaturalMexicoShiny/). Los datos son procesados en R y organizados para permitir su consulta dentro de la aplicación.

### Implementación

La aplicación fue desarrollada en **R Shiny**, utilizando componentes de visualización geoespacial para generar un mapa interactivo. La interfaz incorpora controles que permiten al usuario explorar la información y consultar diferentes categorías o elementos espaciales.

El código se encuentra organizado en el archivo `app.R` y utiliza las librerías necesarias para el procesamiento de datos y la generación de la interfaz y visualizaciones.

<img width="659" height="380" alt="image" src="https://github.com/user-attachments/assets/e517a5bb-082f-44d4-9f6e-8ad67b38adc2" />


### Funcionalidades principales

* Visualización interactiva de información geoespacial.
* Consulta de categorías de uso de suelo y vegetación.
* Filtros para facilitar la exploración de los datos.
* Interacción directa con los elementos del mapa.
* Presentación de información espacial mediante una interfaz web.

### Supuestos y limitaciones

La aplicación depende de la estructura y disponibilidad de los datos utilizados como entrada. El rendimiento puede verse afectado por el tamaño de los datos espaciales y los recursos disponibles en el servidor donde se ejecuta.

La aplicación está orientada principalmente a la **exploración y visualización de información**, por lo que los resultados deben interpretarse considerando las características y escala de los datos originales.

### Acceso

**Aplicación:** https://hbhrhehnh-aethermaps.shinyapps.io/uso_suelo_vegetacion_2020/

**Código fuente:** `app.R`

