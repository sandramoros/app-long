# Índice
# 1. Carga de librerías
# 2. Interfaz de usuario (UI)
#    2.1 Sidebar
#    2.2 Main Panel
# 3. Lógica del servidor (Server)
#    3.1 Reactivas
#    3.2 Selectores, eventos y outputs dinámicos
#    3.3 Outputs
# 4. shinyApp()


#### ===================================
### 1. Carga de librerías necesarias ###
#### ===================================

library(shiny) # Interfaz de usuario y servidor para aplicaciones web en R
library(shinyalert) # Mostrar alertas emergentes
library(readxl) # Leer archivos Excel
library(openxlsx) # Creación y manipulación de archivos Excel
library(tidyr) # Manipulación y transformación de datos
library(dplyr) # Operaciones con data frames
library(ggplot2) # Generación de gráficos
library(lmtest) # Pruebas Breusch-Pagan y Durbin-Watson
library(lme4) # Modelos de efectos mixtos lmer()
library(lmerTest) # p-valores para lmer()
library(nlme) # Modelos de efectos mixtos lme()
library(emmeans) # Comparaciones post hoc
library(car) # Para el test de Levene
library(base64enc) # Para convertir imágenes a base64
library(rmarkdown) # Generación de informes en distintos formatos
library(DT) # Visualización de tablas interactivas


#### ===============================================
### 2. Definición de la interfaz de usuario (UI) ###
#### ===============================================

# El bloque de la UI se define usando fluidPage() para permitir una estructura flexible (la interfaz se adapta al tamaño de la pantalla)
# Se organiza en dos partes principales: la barra lateral (sidebar) y el panel principal

ui <- fluidPage(

  # Centrar los pop-up de manera vertical aunque haya scroll
  tags$head(
    tags$style(HTML("
    .swal-modal {
      top: 50% !important;
      transform: translateY(-50%) !important;
      position: fixed !important;
      margin-top: 0 !important;
    }
  "))
  ),
  
  # Título de la app
  titlePanel("Análisis de datos preclínicos"), 

  ### -- BLOQUE: Layout barra lateral -- ###

  sidebarLayout( 
    sidebarPanel(
      # Botón para abrir un tutorial en HTML
      tags$a(href = "tutorial.html", target = "_blank",  
             class = "btn btn-primary", "Abrir tutorial (HTML)"),
      hr(),
      # Selección de plantilla de descarga (wide o long)
      radioButtons("template_choice", "Descargar plantilla", 
                   choices = c("Wide", "Long"), selected = "Wide"),
      downloadButton("downloadTemplate", "Descargar plantilla"),
      hr(),
      # Carga de datos desde un archivo Excel
      fileInput("file", "Cargar tus datos (archivo Excel)", accept = c(".xlsx")), # Solo se pueden subir archivos .xlsx
      # Control para definir el número de decimales
      numericInput("num_decimals", "Número de decimales:", value = 3, min = 0, max = 10),
      hr(),
      # Selector de timepoints
      selectizeInput("timepoints", "Selecciona timepoints", 
                     choices = NULL, multiple = TRUE),
      hr(),
      # Selector de la variable de interés
      uiOutput("var_selector"), # Selector generado dinámicamente desde el servidor (renderUI())
      hr(),
      # Selección del modo de comparación de grupos
      radioButtons("comp_mode", "Modo de comparación de grupos",
                   choices = c("Todos contra todos" = "all_vs_all", 
                               "Todos contra control" = "all_vs_control"),
                   selected = "all_vs_all"),
      # Selección de grupos a comparar
      selectizeInput("groups", "Selecciona grupos para comparar", 
                     choices = NULL, multiple = TRUE, selected = NULL),
      # Selector de grupo control --> solo si se elige "Todos contra control"
      conditionalPanel( # No es necesario renderUI() porque el condicional controla la visibilidad pero el input siempre existe en segundo plano
        condition = "input.comp_mode == 'all_vs_control'",
        selectInput("control_group", "Selecciona el grupo control", choices = NULL)
      ),
      # Análisis de homocedasticidad --> solo si hay 2 o + timepoints
      conditionalPanel(
        condition = "input.timepoints.length > 1",
        actionButton("check_homoscedasticity", "Realizar análisis de homocedasticidad"),
      ),
      hr(),
      # Selección del modelo a utilizar (lmer() o lme())
      conditionalPanel(
        condition = "input.timepoints.length > 1",
        uiOutput("model_selection_ui")
      ),
      # Selección de estructuras de correlación y varianza --> solo para lme()
      conditionalPanel(
        condition = "input.timepoints.length > 1 && input.model_type == 'lme'",
        uiOutput("correlation_selection_ui"),
        uiOutput("residual_correlation_ui"),
        uiOutput("variance_selection_ui")
      ),
      hr(),
      # Selección con o sin interacción --> solo para lmer() o lme()
      conditionalPanel(
        condition = "input.timepoints.length > 1",
        selectInput("interaction", "Incluir Interacción", choices = c("Sí", "No"), selected = "Sí"),
        hr()
      ),
      # Selector del tipo de corrección múltiple (pruebas post hoc)
      selectInput("p_adj_method", "Método de corrección de p-value (pruebas post hoc)",
                  choices = c("No hacer corrección" = "none",
                              "Tukey" = "tukey",
                              "Bonferroni" = "bonferroni",
                              "Holm" = "holm",
                              "FDR (Benjamini-Hochberg)" = "fdr"),
                  selected = "none"),
      hr(),
      # Botón para realizar el análisis final
      actionButton("analyze", "Realizar análisis"),
      hr(),
      # Botones para descargar informes
      downloadButton("downloadExcel", "Descargar informe Excel"),
      downloadButton("downloadReport", "Descargar informe HTML"),
      hr(),
      # Botón para reiniciar la aplicación
      actionButton("reset", "Reiniciar App")
    ),
    
    
    ### -- BLOQUE: Layout panel principal con pestañas para mostrar resultados -- ###

      mainPanel(
        tabsetPanel(
        # Visualización de datos cargados y procesados
        tabPanel("Datos cargados", DT::dataTableOutput("data_table")),  
        tabPanel("Datos procesados", DT::dataTableOutput("data_preview")),  
        # Gráfica de los datos
        tabPanel("Gráfica de los datos", plotOutput("model_plot")),
        # Modelo
        tabPanel("Modelo",
                 h5("Fórmula del modelo"),
                 verbatimTextOutput("model_formula"), # Mostrar la fórmula del modelo
                 h5("Resumen del modelo"),
                 verbatimTextOutput("model_summary"), # Mostrar el resumen del modelo
        ),
        # Comparaciones de modelos (con o sin interacción)
        tabPanel("Comparación de modelos (Interacción)",
                 fluidRow(
                   column(12, verbatimTextOutput("interaction_comparison_summary"))
                 )
        ),
        # Matriz de varianza-covarianza
        tabPanel("Matriz varianza-covarianza", verbatimTextOutput("var_cov_matrix")),
        # Comparaciones entre grupos en formato textual
        tabPanel("Comparaciones entre grupos", 
                 # Subapartado medias
                 h3("Medias estimadas"), 
                 h5("Medias globales por grupo"),
                 verbatimTextOutput("group_comparisons_emmeans"), # Para mostrar texto sin formato en la interfaz
                 h5("Medias por grupo dentro de cada timepoint"),
                 verbatimTextOutput("group_comparisons_emmeans_by_timepoint"),
                 h5("Medias por timepoint dentro de cada grupo"),
                 verbatimTextOutput("group_comparisons_emmeans_by_group"),
                 tags$hr(),
                 # Subapartado contrastes
                 h3("Comparaciones entre grupos (Contrastes)"), 
                 h5("Comparaciones globales"),
                 verbatimTextOutput("group_comparisons_contrasts"),
                 h5("Comparaciones entre grupos dentro de cada timepoint"),
                 verbatimTextOutput("group_comparisons_by_timepoint"),
                 h5("Comparaciones entre timepoints dentro de cada grupo"),
                 verbatimTextOutput("group_comparisons_by_group")
        ),
        # Comparaciones entre grupos en formato gráfico
        tabPanel("Gráfico de comparaciones",
                 # Comparación global entre grupos
                 tagList(
                   h4("Comparación global entre grupos"),
                   fluidRow(column(12, plotOutput("plot_global")))
                 ),
                 # Comparaciones por timepoint y por grupo
                 tagList(
                   hr(),
                   h4("Comparaciones entre grupos por timepoint"),
                   fluidRow(column(12, uiOutput("plot_by_timepoint"))),
                   
                   hr(),
                   h4("Comparaciones entre timepoints por grupo"),
                   fluidRow(column(12, uiOutput("plot_by_group")))
                 )
        ),
        # Diagnósticos del modelo en formato textual 
        tabPanel("Diagnósticos textuales", verbatimTextOutput("diagnostics")),
        # Diagnósticos del modelo en formato gráfico
        tabPanel("Diagnósticos gráficos",
                 # Histograma de residuos 
                 fluidRow(
                   column(4, plotOutput("hist_resid")), # Gráfico
                   column(4, uiOutput("hist_resid_text")) # Texto de interpretación
                 ),
                 # Q-Q plot de residuos
                 fluidRow(
                   column(4, plotOutput("qqplot")), # Gráfico
                   column(4, uiOutput("qqplot_text")) # Texto de interpretación
                 ),
                 # Q-Q plot de efectos aleatorios
                 fluidRow(
                   column(4, uiOutput("qqplot_re")), # Gráfico
                   column(4, uiOutput("qqplot_re_text"))  # Texto de interpretación
                 ),
                 # Residuos vs Valores ajustados
                 fluidRow(
                   column(4, plotOutput("resid_fitted")), # Gráfico
                   column(4, uiOutput("resid_fitted_text")) # Texto de interpretación
                 ),
                 # Autocorrelación de residuos
                 fluidRow(
                   column(4, plotOutput("autocorr_plot")), # Gráfico
                   column(4, uiOutput("autocorr_text")) # Texto de interpretación
                 )
        )
      )
    )
  )
)


#### ===========================================================================
### 3. Definición del servidor para gestionar la lógica y cálculos de la app ###
#### ===========================================================================

server <- function(input, output, session) {
  
  ### -- BLOQUE: Definición de variables reactivas -- ###
  
  ## Carga de datos ##
  
  data_raw <- reactive({
    req(input$file)
    
    # Intentar leer el archivo Excel
    df <- tryCatch({
      read_excel(input$file$datapath, na = "NA")
    }, error = function(e) {
      # Si falla la lectura notificar error
      showNotification("Error al cargar el archivo. Verifica el formato o la extensión.", type = "error")
      return(NULL)
    })
    # Si no se puede leer, salir
    req(!is.null(df))
    # Validar que el archivo no esté vacío
    validate(need(nrow(df) > 0, "Error: El archivo está vacío."))
    # Validar columnas requeridas
    required_columns <- c("ID_sujeto", "ID_grupo")
    missing_columns <- setdiff(required_columns, names(df))
    validate(
      need(length(missing_columns) == 0,
           paste0("Error: Faltan columnas requeridas en el archivo: ",
                  paste(missing_columns, collapse = ", ")))
    )
    # Si va bien, devolver el data frame
    df
  })
  
  
 
  ## Procesamiento de datos ##
  
  data_processed <- reactive({ # data_processed() se actualizará automáticamente cada vez que cambien input$file o input$data_format
    req(input$file) # Requiere un archivo cargado
    df <- data_raw() # Usa los datos del archivo. Se llama df, como en el apartado de carga de datos, pero no es problema porque sn diferentes reactive()
    wide_long <- ifelse(c("Timepoint") %in% names(df), "long", "wide")
    message("Los datos cargados tienen formato: ", wide_long)
    # Si el formato de los datos es "wide"--> se convierte a "long" #
    if (wide_long == "wide") {
      numeric_cols <- names(df)[sapply(df, is.numeric)] # Encuentra columnas numéricas porque solo se pueden transformar esas (no importa si se llaman Valor_timepoint1, T1... porque eligirá todas las numéricas que no sean variables identificadoras (ID_sujeto, ID_grupo))
      validate(need(length(numeric_cols) > 0, "Error: No se encontraron columnas numéricas en los datos Wide.")) # Si no hay columnas numéricas muestra error
      df_long <- df %>%
        pivot_longer(cols = all_of(numeric_cols), names_to = "Timepoint", values_to = "Valores_variable", names_repair = "unique") %>% # Las columnas se convierten en una nueva variable llamada "Timepoint". Los valores de esas columnas se almacenan en una nueva columna "Valores_variable" .
        mutate(Timepoint = factor(Timepoint)) # Convierte Timepoint en factor

      #  Si el formato de los datos es "long" #
    } else {
      if (!"Timepoint" %in% names(df)) # Verifica que la columna "Timepoint" está presente
        stop("Error: La columna Timepoint no está presente en los datos Long") # Si "Timepoint" no existe, muestra error
      variable_cols <- setdiff(names(df), c("ID_sujeto", "ID_grupo", "Nombre_grupo", "Timepoint")) # Identifica las columnas con valores (las que no son las especificadas en el vector)
      validate(need(length(variable_cols) > 0, "Error: No se encontraron columnas con datos en formato Long.")) # Si no hay columnas con datos muestra error y detiene la ejecución
      
      # Organizar los datos "long"
      df_long <- df %>%
        pivot_longer(cols = all_of(variable_cols), names_to = "Nombre_variable", values_to = "Valores_variable") %>% # Reestructura el dataset manteniendo las variables medidas en una única columna Nombre_variable
        filter(!is.na(Valores_variable) & Valores_variable != "") # Elimina valores vacíos o NA
    }
    # Ordena los datos por ID_sujeto
    df_long <- df_long %>% arrange(ID_sujeto)
    df_long # Contiene los datos listos para el análisis
  })
  
  
  ## Filtrado de datos (función auxiliar) ##
  
  getFilteredData <- function() { # No es reactiva --> solo se ejecuta cuando se llama
    df <- isolate(data_processed()) %>% # Obtiene los datos procesados y evita que se ejecute automáticamente cuando cambian los valores del input
      # Filtra los datos según las selecciones del usuario
      filter(ID_grupo %in% input$groups, # Mantiene solo los grupos seleccionados en la UI
             Timepoint %in% input$timepoints, # Mantiene solo los timepoints seleccionados en la UI
             Nombre_variable == input$variable) %>% # Mantiene solo la variable seleccionada en la UI
      drop_na(Valores_variable) # Elimina valores NA de la variable seleccionada
    df$ID_sujeto <- as.factor(df$ID_sujeto) # Convierte en factor
    df$ID_grupo  <- as.factor(df$ID_grupo) # Convierte en factor
    
    time_levels <- unique(df$Timepoint) # Extrae niveles únicos de Timepoint
    ordered_levels <- time_levels[order(as.numeric(gsub("\\D", "", time_levels)))] # Elimina letras (\\D = no dígitos) y extrae el número
    df$Timepoint <- factor(df$Timepoint, levels = ordered_levels, ordered = FALSE) # Ordena los niveles en orden creciente según el número y los vuelve a convertir en factor ordenado, manteniendo los nombres originales
    
    # Asegurar que la variable de respuesta es numérica
    df$Valores_variable <- as.numeric(df$Valores_variable)
    
    # Crear la columna grupo_time para modelar una varianza diferente por combinación grupo-timepoint
    if (!"grupo_time" %in% names(df)) {
      df$grupo_time <- as.factor(interaction(df$ID_grupo, df$Timepoint)) # Creamos como factor
    }
    
    # Eliminar cualquier fila con valores NA en las columnas críticas
    df <- df %>% drop_na(ID_sujeto, ID_grupo, Timepoint, Valores_variable, grupo_time)
    
    print(levels(df$Timepoint)) # Debbuging: imprime en la consola los niveles de Timepoint
    df # Devuelve los datos ya filtrados y listos para análisis
  }
  
  
  ## Generación de gráficos con los datos con los que se ajustan los modelos ##
  
  model_plot_result <- reactive({
    req(model_result())
    df <- model_result()$df
    mod_type <- model_result()$model_type
    
    # Verificar si df está vacío
    if (is.null(df) || nrow(df) == 0) {
      return(ggplot() + ggtitle("No hay datos disponibles para el gráfico"))
    }
    
    if (mod_type == "lm") {
      # Para modelos lm(): boxplot + puntos dispersos #
      ggplot(df, aes(x = as.factor(ID_grupo), y = Valores_variable)) +
        geom_boxplot(fill = "lightblue", alpha = 0.6) +
        geom_jitter(width = 0.2, alpha = 0.5) +
        labs(title = "Distribución por grupo", x = "Grupo", y = "Valores")
    } else {
      # Para modelos lmer()) y lme(): líneas por sujeto a lo largo del tiempo #
      ggplot(df, aes(x = Timepoint, y = Valores_variable, color = ID_grupo, group = ID_sujeto)) +
        geom_point(size = 2) + 
        geom_line(alpha = 0.5) +
        labs(title = paste("Evolución de la variable (", mod_type, ")", sep=""),
             x = "Timepoint", y = "Valores")
    }
  })
  
  
  ## Ajuste del modelo ##
  
  # Establecer codificación de factores (referencia)
  options(contrasts = c('contr.treatment', 'contr.poly')) # contr.treatment codifica los factores tomando una categoría de referencia; contr.poly se aplica a factores ordinales
  
  model_result <- eventReactive(input$analyze, { # El modelo se ajusta cuando el usuario presiona el botón "Analizar"
    req(input$file, input$groups, input$timepoints, input$variable) # Necesita que haya un archivo cargado y que el usuario haya seleccionado grupos, timepoints y variable
    df <- getFilteredData() # Filtra los datos usando getFilteredData() para asegurarse de que solo usa los datos seleccionados

    # Validaciones de seguridad antes de ajustar el modelo #
    
    # Calcular el número de timepoints con datos
    non_empty_timepoints <- df %>%
      group_by(Timepoint) %>%
      summarise(n = sum(!is.na(Valores_variable)), .groups = "drop") %>%
      filter(n > 0)
    
    if (nrow(non_empty_timepoints) == 1 && length(input$timepoints) > 1) {
      showNotification( 
        paste0("Solo hay datos disponibles en el timepoint: ", 
               paste(non_empty_timepoints$Timepoint, collapse = ", "), 
               ". Se requiere deseleccionar los timepoints vacíos."), # Muestra caja de error roja si no hay datos en algún timepoint seleccionado
        type = "warning",
        duration = NULL
      )
    }
    # Comprobar que hay datos después de filtrar
    validate(
      need(nrow(df) > 0, 
           "No hay datos disponibles para el timepoint y variable seleccionados.")
    )
    # Comprobar que hay >=2 grupos
    validate(
      need(length(unique(df$ID_grupo)) > 1, 
           "Se necesitan al menos 2 grupos con datos variados en el timepoint seleccionado.")
    )
    # Comprobar que al menos 2 grupos tienen más de un valor único en la variable respuesta
    group_variation <- df %>%
      group_by(ID_grupo) %>%
      summarise(n_valores = n_distinct(Valores_variable), .groups = "drop")
    
    validate(
      need(sum(group_variation$n_valores > 1) >= 2,
           "Se necesitan al menos 2 grupos con variabilidad en la variable de respuesta.")
    )
    
    # Calcular número de timepoints únicos
    num_selected_timepoints <- length(unique(df$Timepoint))
    
    # Darle el nombre de la variable original para que pueda aparecer en la fórmula
    df[[input$variable]] <- df$Valores_variable
    
    # Si solo hay un timepoint, aplicar modelo lm() #
    if (num_selected_timepoints == 1) {
    
      model_formula <- as.formula(paste(input$variable, "~ ID_grupo"))
      full_formula <- deparse(model_formula) # deparse() para texto legible
      
      model <- lm(model_formula, data = df) 
      
      # Medias estimadas para cada grupo
      lsm <- emmeans(model, specs = "ID_grupo")
      
      # Calcular comparaciones entre grupos con el método de ajuste seleccionado
      if(input$comp_mode == "all_vs_all") {
        cont <- contrast(lsm, method = "pairwise", adjust = input$p_adj_method)
      }else if(input$comp_mode =="all_vs_control"){
        cont <- contrast(lsm, method = "trt.vs.ctrl", adjust = input$p_adj_method, ref = input$control_group)
      }else{
        lsm <- "Error en medias estimadas"
        cont <- "Error en contrastes"
      } 
      
      group_comparisons <- list(
        emmeans_table = as.data.frame(summary(lsm)), 
        contrasts_table = as.data.frame(summary(cont, infer = c(TRUE, TRUE))) # p-valor y IC
      )
       
      return(list( # Devuelve una lista con el modelo ajustado y resultados asociados
          model = model, # Modelo ajustado
          df = df, # Datos filtrados usados
          model_type = "lm", # Tipo de modelo
          formula = full_formula, # Fórmula del modelo
          group_means_summary = group_comparisons$emmeans_table, # Medias estimadas
          group_comparisons_summary = group_comparisons$contrasts_table # Comparaciones entre grupos
  
        ))
      
    # Si hay más de un timepoint, aplicar LMM #
      
    }else if(num_selected_timepoints > 1){
      
      include_interaction <- isolate(input$interaction) == "Sí"
      model_formula <- as.formula(paste(input$variable, "~ ID_grupo", if (include_interaction) "* Timepoint" else "+ Timepoint")) # Si se selecciona interacción (TRUE), se hace * Timepoint
      
      
      # Modelo lmer()
      if (input$model_type == "lmer") {
        model_formula <- update(model_formula, . ~ . + (1 | ID_sujeto))
        
        model <- tryCatch({
          lmer(model_formula, data = df, control = lmerControl(optimizer = "bobyqa"))
        }, error = function(e) {
          showNotification(paste("Error ajustando el modelo lmer():", e$message), # Muestra caja de error roja si hay error de convergencia, etc.
                           type = "error", duration = NULL)
          return(NULL)
        })
        
        full_formula <- deparse(model_formula)
      
      
        
      # Modelo lme()
      } else if (input$model_type == "lme") {
        
        df <- df %>%
          dplyr::mutate(
            Timepoint = factor(Timepoint, levels = unique(Timepoint)[order(as.numeric(gsub("\\D", "", Timepoint)))]),
            TimeIndex = as.integer(Timepoint), # Para estructuras que necesitan orden (corSymm)
            TimeValue = as.numeric(stringr::str_extract(as.character(Timepoint), "\\d+")), # Se extraen los dígitos del Timepoint para usarlo en estructuras que necesitan distancias (corAR1, corCAR1)
            grupo_time = interaction(ID_grupo, Timepoint) # Se añade una columna nueva en el df
          )
        
        # Definir estructura de correlación residual para lme()
        correlation_residual <- switch(input$residual_corr_structure,
                                       "corAR1" = corAR1(form = ~ TimeValue | ID_sujeto),
                                       "corARMA" = corARMA(p = 1, q = 1, form = ~ TimeValue | ID_sujeto), # Autoregresivo (AR) p=1 --> el residuo actual depende del anterior. Media mñovil (MA) q=1 --> el residuo actual depende del error del anterior
                                       "corCompSymm" = corCompSymm(form = ~ Timepoint | ID_sujeto),
                                       "corSymm" = corSymm(form = ~ TimeIndex | ID_sujeto),
                                       NULL
        )
        
        # Estructura de la varianza de los residuos
        weights_formula <- switch(input$variance_structure,
                                  "varIdent_grupo" = varIdent(form = ~ 1 | ID_grupo),
                                  "varIdent_time" = varIdent(form = ~ 1 | Timepoint),
                                  "varIdent_grupo_time" = varIdent(form = ~ 1 | grupo_time),
                                  "varPower" = varPower(form = ~ Valores_variable),
                                  "varConstPower" = varConstPower(form = ~ Valores_variable),
                                  NULL # Por defecto: homocedasticidad
        )
        
        control_settings <- lmeControl(
          maxIter = 100, # Número máximo de iteraciones EM (Expectation-Maximization)
          niterEM = 50, # Número de iteraciones EM antes de empezar la maximización
          msMaxIter = 200, # Número máximo de iteraciones del optimizador
          msMaxEval = 500, # Número mxáimo de evaluaciones de log-likelihood
          tolerance = 1e-6,  # Tolerancia para declarar convergencia en el ciclo EM
          msTol = 1e-10, # Tolerancia para declarar convergencia en el ciclo maximizacióm
          opt = "nlminb", # Optimtzador predeterminado
        )
        
        model <- tryCatch({
          lme(
            fixed = model_formula,
            random = ~ 1 | ID_sujeto,
            correlation = correlation_residual,
            weights = weights_formula,
            data = df,
            control = control_settings
          )
        }, error = function(e) {
          showNotification(paste("Error ajustando el model lme():", e$message), # Muestra caja de error roja si hay error de convergencia, etc.
                           type = "error", duration = NULL)
          return(NULL)
        })
        
        # Fórmula para mostrar
        full_formula <- paste(deparse(model_formula),
                              "+ correlación residual:", input$residual_corr_structure,
                              "+ estructura de varianza residual:", input$variance_structure)
      }
      
      
      # Bloque común para LMM: comparaciones entre grupos
      if (is.null(model)) return(NULL)
      
      # Comparaciones globales entre grupos
      lsm_global <- emmeans(model, specs = "ID_grupo")
      if (input$comp_mode == "all_vs_all") {
        cont_global <- contrast(lsm_global, method = "pairwise", adjust = input$p_adj_method)
      } else if (input$comp_mode == "all_vs_control") {
        cont_global <- contrast(lsm_global, method = "trt.vs.ctrl", adjust = input$p_adj_method, ref = input$control_group)
      } else {
        lsm_global <- "Error en medias estimadas"
        cont_global <- "Error en contrastes"
      }
      
      # Medias estimadas por grupo globales
      lsm <- emmeans(model, specs = "ID_grupo")
      
      # Medias por grupo dentro de cada timepoint
      emmeans_by_timepoint <- tryCatch({
        emmeans(model, ~ ID_grupo | Timepoint)
      }, error = function(e) NULL)
      
      # Medias por timepoint dentro de cada grupo
      emmeans_by_group <- tryCatch({
        emmeans(model, ~ Timepoint | ID_grupo)
      }, error = function(e) NULL)
      
      
      # Comparaciones entre grupos dentro de cada timepoint
      cont_group_by_time <- contrast(emmeans_by_timepoint,
                                      method = if (input$comp_mode == "all_vs_all") "pairwise" else "trt.vs.ctrl",
                                      adjust = input$p_adj_method,
                                      ref = if (input$comp_mode == "all_vs_control") input$control_group else NULL)
      
      # Comparaciones entre timepoints dentro de cada grupo
      cont_time_by_group <- contrast(emmeans_by_group, method = "pairwise", adjust = input$p_adj_method)
      
      # Agrupar todos los resultados
      group_comparisons <- list(
        global_means = as.data.frame(summary(lsm_global)),
        by_timepoint_emmeans = as.data.frame(summary(emmeans_by_timepoint)),
        by_group_emmeans = as.data.frame(summary(emmeans_by_group)),
        global_contrasts = as.data.frame(summary(cont_global, infer = c(TRUE, TRUE))), # p-valor y IC
        by_timepoint_contrasts = as.data.frame(summary(cont_group_by_time, infer = c(TRUE, TRUE))), # p-valor y IC
        by_group_contrasts = as.data.frame(summary(cont_time_by_group, infer = c(TRUE, TRUE))) # p-valor y IC
      )
      
      # Devolver todo
      return(list(
        model = model,
        df = df,
        model_type = input$model_type,
        formula = full_formula,
        group_means_summary = group_comparisons$global_means,
        group_means_by_timepoint = if (!is.null(emmeans_by_timepoint)) summary(emmeans_by_timepoint) else NULL,
        group_means_by_group = if (!is.null(emmeans_by_group)) summary(emmeans_by_group) else NULL,
        group_comparisons_summary = group_comparisons$global_contrasts,
        comparisons_by_timepoint = group_comparisons$by_timepoint_contrasts,
        comparisons_by_group = group_comparisons$by_group_contrasts
      ))
      
    }
    
  })

  
  ## Activación del análisis al presionar el botón de analizar##
  
  # Inicialización de variables reactivas auxiliares #
  interaction_comparison_result <- reactiveVal(NULL) # Almacena el resultado de la comparación entre modelos +/- interacción
  var_cov_matrix_result <- reactiveVal(NULL) # Almacena la matriz de varianza-covarianza del modelo ajustado
  diagnostics_result <- reactiveVal(NULL) # Almacena los resultados de los tests de diagnóstico del modelo
  report_data <- reactiveVal(NULL) # Almacena los datos para generar el informe final
  
  # Acciones que se disparan al apretar "Realizar análisis" #
  observeEvent(input$analyze, {
    req(model_result())
    model_info <- model_result()
    mod <- model_info$model
    if (is.null(mod)) {
      cat("No se ha generado ningún modelo aún.\n")
      return()
    }
    
    # Ejecutar análisis y guardar resultados en reactives auxiliares
    interaction_comparison_result(getInteractionComparison()) # Guarda el resultado de comparar modelos con y sin interacción
    var_cov_matrix_result(tryCatch({ #tryCatch porque es una función base de lme4/nlme que puede fallar porque aun no está protegida como las otras
      as.data.frame(VarCorr(model_result()$model))
    }, error = function(e) {
      "No disponible" 
    })) # Guarda la matriz de varianzas-covarianzas
    diagnostics_result(getDiagnostics()) # Guarda los resultados de diagnóstico del modelo
    
    # Empaquetar los resultados con capture.output() para convertir la salida impresa en texto y usarla en el informe"
    report_data(list(
      model_formula = model_info$full_formula,
      interaction_comparison = if (!is.null(interaction_comparison_result()))
        capture.output(print(as.data.frame(interaction_comparison_result()))) else "No disponible",
      var_cov_matrix = if (!is.null(var_cov_matrix_result()))
        capture.output(print(var_cov_matrix_result())) else "No disponible",
      group_comparisons = if (!is.null(model_info$group_comparisons_summary)) {
        list(
          global_emmeans = capture.output(print(as.data.frame(model_info$group_means_summary))),
          by_timepoint_emmeans = if (!is.null(model_info$group_means_by_timepoint))
            capture.output(print(as.data.frame(model_info$group_means_by_timepoint))) else "No disponible",
          by_group_emmeans = if (!is.null(model_info$group_means_by_group))
            capture.output(print(as.data.frame(model_info$group_means_by_group))) else "No disponible",
          global_contrasts = capture.output(print(as.data.frame(model_info$group_comparisons_summary))),
          by_timepoint_contrasts = if (!is.null(model_info$comparisons_by_timepoint))
            capture.output(print(as.data.frame(model_info$comparisons_by_timepoint))) else "No disponible",
          by_group_contrasts = if (!is.null(model_info$comparisons_by_group))
            capture.output(print(as.data.frame(model_info$comparisons_by_group))) else "No disponible"
        )
      } else {
        NULL
      },
      diagnostics = if (!is.null(diagnostics_result()))
        capture.output(cat(diagnostics_result(), sep = "\n")) else "No disponible"
    ))
    
    message("`report_data()` actualizado correctamente.")
  })
  
  
  ## Comparaciones entre modelos ##
  
  # Comparación de modelos con y sin interacción --> lmer() y lme() #
  # Se ha de volver a calcular porque aquí se usa ML y no REML
  getInteractionComparison <- function() {
    req(input$model_type)
    df <- getFilteredData()
    
    # Verificar si hay múltiples observaciones por sujeto
    if (nrow(df) == length(unique(df$ID_sujeto))) {
      return("No se puede comparar modelos porque cada sujeto tiene solo un timepoint.")
    }
    
    # Definir fórmulas de efectos fijos
    formula_with_interaction <- as.formula("Valores_variable ~ ID_grupo * Timepoint")
    formula_no_interaction <- as.formula("Valores_variable ~ ID_grupo + Timepoint")
    
    if (input$model_type == "lmer") {
      random_part <- "(1 | ID_sujeto)"
      
      full_formula_with <- as.formula(
        paste("Valores_variable ~ ID_grupo * Timepoint +", random_part)
      )
      full_formula_without <- as.formula(
        paste("Valores_variable ~ ID_grupo + Timepoint +", random_part)
      )
      
      model_with <- lmer(full_formula_with, data = df, REML = FALSE)
      model_without <- lmer(full_formula_without, data = df, REML = FALSE)
    
    
    } else if (input$model_type == "lme") {
    
      df <- df %>%
        dplyr::mutate(
          Timepoint = factor(Timepoint, levels = unique(Timepoint)[order(as.numeric(gsub("\\D", "", Timepoint)))]),
          TimeIndex = as.integer(Timepoint), # Para estructuras que necesitan orden (corSymm)
          TimeValue = as.numeric(gsub("\\D", "", as.character(Timepoint))), # Para estructuras que necesitan distancias (corAR1, corCAR1)
          grupo_time = interaction(ID_grupo, Timepoint) # Se añade una columna nueva en el df
        )
      
      # Definir estructura de correlación residual para lme()
      correlation_residual <- switch(input$residual_corr_structure,
                                     "corAR1" = corAR1(form = ~ TimeValue | ID_sujeto),
                                     "corARMA" = corARMA(p = 1, q = 1, form = ~ TimeValue | ID_sujeto),
                                     "corCompSymm" = corCompSymm(form = ~ Timepoint | ID_sujeto),
                                     "corSymm" = corSymm(form = ~ TimeIndex | ID_sujeto),
                                     NULL
      )
      
      # Definir estructura de varianza residual para lme()
      weights_formula <- switch(input$variance_structure,
                                "varIdent_grupo" = varIdent(form = ~ 1 | ID_grupo),
                                "varIdent_time" = varIdent(form = ~ 1 | Timepoint),
                                "varIdent_grupo_time" = varIdent(form = ~ 1 | grupo_time),
                                "varPower" = varPower(form = ~ Valores_variable),
                                "varConstPower" = varConstPower(form = ~ Valores_variable),
                                NULL  # Por defecto: homocedasticidad
      )
      
      # Ajustar modelos con lme() (usando ML para comparación)
      
      control_settings <- lmeControl(
        maxIter = 100, # Número máximo de iteraciones EM (Expectation-Maximization)
        niterEM = 50, # Número de iteraciones EM antes de empezar la maximización
        msMaxIter = 200, # Número máximo de iteraciones del optimizador
        msMaxEval = 500, # Número mxáimo de evaluaciones de log-likelihood
        tolerance = 1e-6,  # Tolerancia para declarar convergencia en el ciclo EM
        msTol = 1e-10, # Tolerancia para declarar convergencia en el ciclo maximizacióm
        opt = "nlminb", # Optimtzador predeterminado
      )
      
      model_with <- tryCatch({
        lme(
          fixed = formula_with_interaction,
          random = ~ 1 | ID_sujeto,
          correlation = correlation_residual,
          weights = weights_formula,
          data = df,
          method = "ML",
          control = control_settings
        )
      }, error = function(e) {
        return(NULL)
      })
      
      model_without <- tryCatch({
        lme(
          fixed = formula_no_interaction,
          random = ~ 1 | ID_sujeto,
          correlation = correlation_residual,
          weights = weights_formula,
          data = df,
          method = "ML",
          control = control_settings
        )
      }, error = function(e) {
        return(NULL)
      })
    }
    
    # Comparar modelos con ANOVA
    comparison <- tryCatch({ # Por si falla si por algún motivo el ANOVA falla al comparar los modelos
      if (is.null(model_with)) stop("El ajuste del modelo CON interacción ha fallado.")
      if (is.null(model_without)) stop("El ajuste del modelo SIN interacción ha fallado.")
      anova(model_without, model_with)
    }, error = function(e) {
      paste("Error en la comparación de modelos:", e$message)
    })
    
    return(comparison)
  }
  
  
  ## Comparaciones entre grupos (texto) ## --> ya están incluidos en el bloque de ajuste del modelo
  
  
  ## Comparaciones entre grupos (gráficos) ##
  
  comparison_plots_result <- reactive({
    req(model_result())
    
    result <- list()
    
    # Gráfico global (siempre aplica)
    df_global <- model_result()$group_comparisons_summary
    result$global <- ggplot(df_global, aes(x = contrast, y = estimate, ymin = estimate - SE, ymax = estimate + SE)) +
      geom_pointrange() +
      coord_flip() +
      labs(x = "Comparación global entre grupos", y = "Diferencia estimada") +
      theme_minimal()
    
    # Gráfico por timepoint --> solo si no es lm()
    if (model_result()$model_type != "lm" && !is.null(model_result()$comparisons_by_timepoint)) {
      df_tp <- model_result()$comparisons_by_timepoint
      result$by_timepoint <- ggplot(df_tp, aes(x = contrast, y = estimate, ymin = estimate - SE, ymax = estimate + SE)) +
        geom_pointrange() +
        coord_flip() +
        facet_wrap(~ Timepoint) +
        labs(x = "Comparación entre grupos", y = "Diferencia estimada") +
        theme_minimal()
    } else {
      result$by_timepoint <- NULL
    }
    
    # Gráfico por grupo --> solo si no es lm()
    if (model_result()$model_type != "lm" && !is.null(model_result()$comparisons_by_group)) {
      df_grp <- model_result()$comparisons_by_group
      result$by_group <- ggplot(df_grp, aes(x = contrast, y = estimate, ymin = estimate - SE, ymax = estimate + SE)) +
        geom_pointrange() +
        coord_flip() +
        facet_wrap(~ ID_grupo) +
        labs(x = "Comparación entre timepoints", y = "Diferencia estimada") +
        theme_minimal()
    } else {
      result$by_group <- NULL
    }
    
    return(result)
  })
  
  
  ## Diagnósticos del modelo ##
  
  # Pruebas para el diagnóstico (diagóstico textual) #
  getDiagnostics <- function() {
    req(model_result())
    model_info <- model_result()
    mod <- model_info$model
    if (is.null(mod)) return("No se ha generado ningún modelo aún.")
    
    # Test de normalidad
    capture.output({
      cat("\n===== Test de Normalidad (Shapiro-Wilk) =====\n")
      shapiro_res <- shapiro.test(residuals(mod))
      print(shapiro_res)
      cat(ifelse(shapiro_res$p.value > 0.05,
                 "\n✅  Cumple con normalidad (p > 0,05)\n",
                 "\n⚠️ No cumple con normalidad (p < 0,05)\n"))
      
      cat("\n===== Test de Normalidad (Kolmogorov-Smirnov) =====\n")
      ks_res <- ks.test(residuals(mod), "pnorm", mean(residuals(mod)), sd(residuals(mod)))
      print(ks_res)
      cat(ifelse(ks_res$p.value > 0.05,
                 "\n✅  Cumple con normalidad (p > 0,05)\n",
                 "\n⚠️ No cumple con normalidad (p < 0,05)\n"))
      
      # Test de homocedasticidad y autocorrelación
      if (model_info$model_type == "lm") {
        cat("\n===== Test de homocedasticidad (Breusch-Pagan) =====\n")
        bp_test <- bptest(mod)
        print(bp_test)
        cat(ifelse(bp_test$p.value > 0.05,
                   "\n✅  No hay evidencia de heterocedasticidad (p > 0,05)\n",
                   "\n⚠️ Se detecta heterocedasticidad (p < 0,05)\n"))
        
        cat("\n===== Test de Independencia (Durbin-Watson) =====\n")
        dw_test <- dwtest(mod)
        print(dw_test)
        cat(ifelse(dw_test$p.value > 0.05,
                   "\n✅  No hay evidencia de autocorrelación (p > 0,05)\n",
                   "\n⚠️ Se detecta autocorrelación (p < 0,05)\n"))
      } else {
        cat("\n===== Test de Homocedasticidad (Levene) =====\n")
        tryCatch({ # Por si el modelo tiene problemas
          levene_test <- leveneTest(residuals(mod), group = model_info$df$ID_grupo)
          print(levene_test)
          cat(ifelse(levene_test$`Pr(>F)`[1] > 0.05,
                     "\n✅  No hay evidencia de heterocedasticidad (p > 0,05)\n",
                     "\n⚠️ Se detecta heterocedasticidad (p < 0,05)\n"))
        }, error = function(e) {
          cat("\n Error en Levene:", e$message, "\n")
        })
        cat("\n️ Durbin-Watson no es aplicable a modelos mixtos.\n")
      }
    })
  }
  
  # Gráficos de diagnóstico del modelo #
  diagnostics_plots_result <- reactive({
    req(model_result())
    mod <- model_result()$model
    mod_type <- model_result()$model_type 
    
    tryCatch({ # Por si hay errores en los gráficos
      # Extraer residuos según el modelo
      if (mod_type == "lm") {
        resid_vals <- residuals(mod) #  # En lm() residuos brutos (observado - ajustado)
        fitted_vals <- fitted(mod)
      } else if (mod_type == "lme") {
        resid_vals <- residuals(mod, type = "normalized") # En lme() residuos estandardizados ajustados por varianzas heterogéneas y la estructura de correlación residual
        fitted_vals <- fitted(mod)
      } else {
        resid_vals <- residuals(mod, type = "pearson") # En lmer() residuos Pearson (estandardizados por la SD estimada de cada observación)
        fitted_vals <- fitted(mod)
      }
      
      # Q-Q plot de efectos aleatorios --> solo lmer o lme
      qqplot_re <- if (mod_type %in% c("lmer", "lme")) {
        ranef_df <- ranef(mod)[[1]] # Obtenemos los efectos
        ranef_vals <- if (is.data.frame(ranef_df)) ranef_df[[1]] else as.numeric(ranef_df)
  
        if (length(ranef_vals) > 1) {
          qq_vals <- qqnorm(ranef_vals, plot.it = FALSE)  # Calcula teóricos y observados
          ggplot(data.frame(theoretical = qq_vals$x, observed = qq_vals$y), aes(x = theoretical, y = observed)) +
            geom_point() +
            geom_abline(slope = 1, intercept = 0, color = "black") +
            ggtitle("Q-Q plot de efectos aleatorios") +
            xlab("Cuantiles teóricos") +
            ylab("Cuantiles observados")
        } else {
          NULL
        }
      } else {
        NULL
      }
      
      # Generar lista de gráficos
      plots <- list(
        
        # Histograma de residuos con curva normal
        hist_resid = ggplot(data.frame(resid = resid_vals), aes(x = resid)) +
          geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightgray", color = "black") +
          stat_function(fun = dnorm, args = list(mean = mean(resid_vals), sd = sd(resid_vals)), col = "red") +
          ggtitle("Histograma de residuos") + 
          xlab("Residuos") +
          ylab("Densidad"),
        
        # Q-Q plot de residuos
        qqplot = ggplot(data.frame(resid = resid_vals), aes(sample = resid)) +
          stat_qq() + stat_qq_line() +
          ggtitle("Q-Q plot de residuos") +
          xlab("Cuantiles teóricos") +
          ylab("Cuantiles observados"),
        
        # Q-Q plot de efectos aleatorios
        qqplot_re = qqplot_re,
        
        # Residuos vs Valores ajustados
        resid_fitted = ggplot(data.frame(fitted = fitted_vals, resid = resid_vals), aes(x = fitted, y = resid)) +
          geom_point() +
          geom_hline(yintercept = 0, col = "red") +
          ggtitle("Residuos vs Valores ajustados") +
          xlab("Valores ajustados") +
          ylab("Residuos"),
        
        # Autocorrelación de residuos
        autocorr_plot = {
          acf_data <- acf(resid_vals, plot = FALSE)
          # Extraer valores y límites de confianza
          df_acf <- data.frame(lag = acf_data$lag, acf = acf_data$acf)
          conf_limit <- qnorm((1 + 0.95) / 2) / sqrt(length(resid_vals))
          ggplot(df_acf, aes(x = lag, y = acf)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            geom_hline(yintercept = conf_limit, linetype = "dashed", color = "blue") +
            geom_hline(yintercept = -conf_limit, linetype = "dashed", color = "blue") +
            geom_hline(yintercept = 0, color = "black") +
            ggtitle("Autocorrelación de residuos") +
            xlab("Retardo") +
            ylab("Autocorrelación") +
            theme_minimal()
        }
      )
      
      return(plots)
    }, error = function(e) {
      list(
        hist_resid = ggplot() + ggtitle("Error en Histograma de residuos"),
        qqplot = ggplot() + ggtitle("Error en Q-Q plot de residuos"),
        qqplot_re = ggplot() + ggtitle("Error en Q-Q plot de efectos aleatorios"),
        resid_fitted = ggplot() + ggtitle("Error en Residuos vs Valores ajustados"),
        autocorr_plot = ggplot() + ggtitle("Error en Autocorrelación")
      )
    })
  })
  
  
  ### -- BLOQUE: Actualización de UI dinámica (selectores, outputs) // Observadores de eventos del usuario (botones, acciones) -- ###
  
  ## Observador para actualizar los selectores cuando cambian los datos procesados ##
    observeEvent(data_processed(), {# Se ejecuta cuando data_processed() cambia
      df <- data_processed()
      
      # Grupos
      if ("ID_grupo" %in% names(df)) {
        unique_groups <- unique(na.omit(df$ID_grupo))
        updateSelectizeInput(session, "groups",
                             choices = unique_groups,
                             selected = if (length(unique_groups) > 0) unique_groups[1] else NULL,
                             server = TRUE)
        updateSelectInput(session, "control_group",
                          choices = unique_groups,
                          selected = if (length(unique_groups) > 0) unique_groups[1] else NULL)
      } else {
        updateSelectizeInput(session, "groups", choices = character(0), selected = NULL)
        updateSelectInput(session, "control_group", choices = character(0), selected = NULL)
      }
      
      # Timepoints
      if ("Timepoint" %in% names(df)) {
        unique_timepoints <- unique(na.omit(df$Timepoint))
        updateSelectizeInput(session, "timepoints",
                             choices = unique_timepoints,
                             selected = if (length(unique_timepoints) > 0) unique_timepoints[1] else NULL)
      } else {
        updateSelectizeInput(session, "timepoints", choices = character(0), selected = NULL)
      }
    })
    
  
  ## Selector dinámico de variable a analizar ##
  output$var_selector <- renderUI({ # var_selector es un uiOutput() --> necesita su renderUI() correspondiente
    req(input$file) # Requiere que haya un archivo cargado
    df <- data_processed()
    # Si existe la columna Nombre_variable, genera un selectInput() con sus valores únicos
    if ("Nombre_variable" %in% names(df))
      selectInput("variable", "Selecciona la variable de interés", choices = unique(df$Nombre_variable), selected = unique(df$Nombre_variable)[1])
    else
      selectInput("variable", "Selecciona la variable de interés", choices = list("No hay variables disponibles" = ""), selected = "")
  })
  
  # Inicialización de la variable reactiva pera la recomendación del modelo LMM (lmer por defecto hasta hacer test de homocedasticidad)
  recommended_model <- reactiveVal("lmer")
  
  ## Observador para verificar homocedasticidad cuando el usuario presiona el botón ##
  observeEvent(input$check_homoscedasticity, {
    req(input$groups, input$timepoints, input$variable)
    df <- data_processed() %>%
      # Filtra los datos según los inputs seleccionados
      filter(ID_grupo %in% input$groups,
             Timepoint %in% input$timepoints,
             Nombre_variable == input$variable) %>%
      drop_na(Valores_variable)
    validate(need(nrow(df) > 0, "No hay datos para realizar el test."))
    
    # Ajuste del modelo mixto con lmer() (con o sin interacción, en función de lo elegido)
    include_interaction <- isolate(input$interaction) == "Sí"
    
    model_formula <- if (include_interaction) {
      Valores_variable ~ ID_grupo * Timepoint
    } else {
      Valores_variable ~ ID_grupo + Timepoint
    }
    
    mod_lmer <- tryCatch({
      lmer(update(model_formula, . ~ . + (1 | ID_sujeto)), data = df)
    }, error = function(e) {
      showNotification(paste("Error en lmer():", e$message), type = "error")
      return(NULL)
    })
    
    # Test de Levene para homocedasticidad
    levene_test <- tryCatch({
      df$Residuals <- residuals(mod_lmer) # Calcular los residuales del modelo mod_lmer
      df$ID_grupo <- as.factor(df$ID_grupo) # Forzar como factor porque viene data_processed(), no de getFilteredData()
      leveneTest(df$Residuals ~ df$ID_grupo) # Aplicar el Test de Levene para evaluar si los residuales tienen varianza constante
    }, error = function(e) {
      showNotification(paste("Error en el test de Levene:", e$message), type = "error") # Muestra caja de error roja
      return(NULL)
    })
    if (is.null(levene_test)) return()
    
    # Evaluación de homocedasticidad con el p-valor y recomendación del modelo
    if (levene_test$`Pr(>F)`[1] > 0.05) {
      recommended_model("lmer")
      mensaje <- "<span style='color:green;'>No se detecta heterocedasticidad (p > 0,05). Se recomienda usar lmer().</span>"
    } else {
      recommended_model("lme")
      mensaje <- "<span style='color:red;'>Se detecta heterocedasticidad (p < 0,05). Se recomienda usar lme().</span>"
    }
    test_result <- paste0(
      "<b>Test de Levene para homocedasticidad:</b><br>",
      "Estadístico: ", round(levene_test$`F value`[1], 3), "<br>",
      "p-value: ", formatC(levene_test$`Pr(>F)`[1], digits = 5, format = "f"), "<br><br>",
      "<b>Conclusión:</b> ", mensaje
    )
    
    # Crear el gráfico de residuos por grupo
    residual_plot <- ggplot(df, aes(x = ID_grupo, y = Residuals, fill = ID_grupo)) +
      geom_boxplot() +
      facet_wrap(~ Timepoint) +
      theme_minimal() +
      labs(title = "Distribución de los residuos por grupo y timepoint",
           x = "Grupo", y = "Residuos")
    # Guardar el gráfico como imagen
    tmpfile <- tempfile(fileext = ".png")
    ggsave(tmpfile, plot = residual_plot, width = 6, height = 4, dpi = 150)
    # Convertir la imagen a base64
    encoded_img <- base64enc::dataURI(file = tmpfile, mime = "image/png")
    # Combina el texto con la imagen para el pop-up
    test_result <- paste0(
      "<b>Test de Levene para homocedasticidad:</b><br>",
      "Estadístico: ", round(levene_test$`F value`[1], 3), "<br>",
      "p-value: ", formatC(levene_test$`Pr(>F)`[1], digits = 5, format = "f"), "<br><br>",
      "<b>Conclusión:</b> ", mensaje, "<br><br>",
      "<img src='", encoded_img, "' style='max-width:75%; height:auto;'>"
    )
    
    # Mostrar los resultados en un pop-up
    shinyalert(
      title = "Resultados del Test de homocedasticidad",
      text = test_result,
      html = TRUE,
      size = "l",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      type = "info" # Para definir el estilo visual del pop-up
    )

  })
  
  
  ## Selección del modelo --> si el número de timepoints es >2 muestra lmer() y lme() ##
  output$model_selection_ui <- renderUI({
    req(input$timepoints) # Asegura que el input no es NULL
    req(length(input$timepoints) > 1) # Verifica que hay más de un timepoint seleccionado
    radioButtons("model_type", "Selecciona el modelo a utilizar:",
                 choices = c("Modelo mixto lmer()" = "lmer", "Modelo mixto lme()" = "lme"),
                 selected = recommended_model()) # Preselecciona el modelo recomendado
  })
  
  
  ## Selector de la estructura de correlación en los residuos --> solo para lme() ##
  output$residual_correlation_ui <- renderUI({
    req(input$model_type == "lme")
    
    selectInput("residual_corr_structure", "Estructura de correlación entre residuos:",
                choices = c(
                  "Ninguna" = "none",
                  "AR(1)" = "corAR1",
                  "ARMA(1,1)" = "corARMA",
                  "Simetria compuesta" = "corCompSymm",
                  "Correlación arbitraria" = "corSymm"
                ),
                selected = "none")
  })
  
  
  ## Selector de la estructura de la varianza de los residuos --> solo para lme() ##
  output$variance_selection_ui <- renderUI({
    req(input$model_type == "lme")
    
    selectInput("variance_structure", "Estructura de la varianza residual:",
                choices = c(
                  "Ninguna (homocedasticidad)" = "none",
                  "varIdent por grupo" = "varIdent_grupo",
                  "varIdent por timepoint" = "varIdent_time",
                  "varIdent por grupo y timepoint" = "varIdent_grupo_time",
                  "varPower" = "varPower",
                  "varConstPower" = "varConstPower"
                ),
                selected = "none"
    )
    
  })


  ## Observador para actualizar dinámicamente el selector del ajuste del p-valor ##
  observe({
    req(input$groups) # Asegurar que hay grupos seleccionados
    req(input$comp_mode)  # Hay que seleccionar el método de comparación
    df <- data_processed() # Obtener los datos procesados
    
    total_groups <- unique(df$ID_grupo) # Todos los grupos en el dataset
    selected_groups <- input$groups # Grupos que el usuario ha seleccionado
    
    # Debugging
    cat("Total grupos en dataset:", length(total_groups), "\n")
    cat("Grupos seleccionados:", length(selected_groups), "\n")
    cat("Modo de comparación:", input$comp_mode, "\n")
    
    if (length(total_groups) == 2) {
      # Caso 1: Solo 2 grupos en el dataset --> No mostrar corrección
      cat("Caso 1: Solo 2 grupos → NO mostrar corrección\n")
      updateSelectInput(session, "p_adj_method", choices = list("No aplica" = "none"), selected = "none")
      
    } else if (length(selected_groups) == 2) {
      # Caso 2: El usuario ha seleccionado solo 2 grupos pero hay más en el dataset
      cat("Caso 2: Solo 2 grupos seleccionados → Mostrar opciones de corrección\n")
      updateSelectInput(session, "p_adj_method",
                        choices = list("No hacer corrección" = "none",
                                       "Tukey" = "tukey",
                                       "Bonferroni" = "bonferroni",
                                       "Holm" = "holm",
                                       "FDR (Benjamini-Hochberg)" = "fdr"),
                        selected = "none")
      
    } else if (length(selected_groups) > 2 && input$comp_mode == "all_vs_all") {
      # Caso 3: Comparación "Todos contra todos" con más de 2 grupos
      cat("Caso 3: Todos contra todos → Mostrar opciones de corrección\n")
      updateSelectInput(session, "p_adj_method",
                        choices = list("No hacer corrección" = "none",
                                       "Tukey" = "tukey",
                                       "Bonferroni" = "bonferroni",
                                       "Holm" = "holm",
                                       "FDR (Benjamini-Hochberg)" = "fdr"),
                        selected = "none")
      
    } else if (length(selected_groups) > 2 && input$comp_mode == "all_vs_control") {
      # Caso 4: Comparación "Todos vs Control" --> Solo permitir Dunnett o sin corrección
      cat("Caso 4: Todos vs Control → Solo permitir Dunnett o ninguna corrección\n")
      updateSelectInput(session, "p_adj_method",
                        choices = list("No hacer corrección" = "none",
                                       "Dunnett" = "dunnett"),
                        selected = "dunnett")
    }
  })
  
  
  ## Reinicio de la aplicación cuando el usuario presiona "Reset" ##
  observeEvent(input$reset, {
    shinyalert(
      text = "¿Seguro que quieres reiniciar la aplicación?",
      type = "warning", # Para definir el estilo visual del pop-up
      showCancelButton = TRUE,
      confirmButtonText = "Sí, reiniciar",
      cancelButtonText = "No, cancelar",
      callbackR = function(x) {
        if (x) { # Si el usuario hace clic en "Sí"
          session$reload() # Reinicia la sesión
        }
      }
    )
  })
  
  
  ### -- BLOQUE: Output: salidas de la UI -- ###
  
  ## Tabla con los datos cargados ##
  output$data_table <- DT::renderDataTable({
    req(input$num_decimals) # Requiere un número de decimales seleccionado
    df <- data_raw()
    df <- df %>% mutate(across(where(is.numeric), ~ round(.x, input$num_decimals))) # Redondea según el número de decimales elegido por el usuario
    validate(need(nrow(df) > 0, "No hay datos en el archivo cargado.")) # Valida que haya datos antes de mostrarlos
    DT::datatable(df, options = list(pageLength = 20)) # Muestra la tabla
  })
  
  
  ## Tabla con los datos procesados ##
  output$data_preview <- DT::renderDataTable({
    req(input$num_decimals) # Requiere un número de decimales seleccionado
    df_dec <- data_processed() %>% mutate(across(where(is.numeric), ~ round(.x, input$num_decimals))) # Redondea según el número de decimales elegido por el usuario
    DT::datatable(df_dec, options = list(pageLength = 20)) # Muestra los datos ya procesados con visualización interactiva
  })
  
  
  ## Gráfica de los datos ##
  output$model_plot <- renderPlot({
    model_plot_result()
  })
  
  
  ## Información del modelo ## 
  
  # Muestra la fórmula del modelo #
  output$model_formula <- renderText({
    model_info <- model_result()
    if (!is.null(model_info$formula)) # full_formula está guardado como formula dentro de la lista en el bloque de ajuste del modelo
      paste("Fórmula del modelo:", model_info$formula)
    else
      "No se ha generado ninguna fórmula aún."
  })

  # Muestra el resumen del modelo #
  output$model_summary <- renderPrint({
    req(model_result())
    summary(model_result()$model)
  })
  
  
  ## Visualización del resultado de la comparación de interacción ##
  output$interaction_comparison_summary <- renderPrint({
    req(model_result())

    comparison <- getInteractionComparison()

    # Si es texto porque getInteractionComparison() de una frase de error mostrarla directamente
    if (is.character(comparison)) {
      cat(comparison, "\n")
      
      # Si és data.frame o anova.lme
    } else if (inherits(comparison, "data.frame") || inherits(comparison, "anova.lme") || inherits(comparison, "anova")) {
      print(comparison)
      
      # Extraer p-valor de la columna adecuada
      pval <- tryCatch({ # si la comparación ha falldo podría ser un texto y no un data frame
        if ("p-value" %in% names(comparison)) {
          comparison[["p-value"]][2]
        } else if ("Pr(>Chisq)" %in% names(comparison)) {
          comparison[["Pr(>Chisq)"]][2]
        } else {
          NULL
        }
      }, error = function(e) NULL)
      
      if (!is.null(pval) && is.numeric(pval) && !is.na(pval)) {
        if (pval < 0.05) {
          cat("\n El modelo con interacción mejora significativamente el ajuste (p =", signif(pval, 3), ").\n")
        } else {
          cat("\n El modelo con interacción no mejora significativamente el ajuste (p =", signif(pval, 3), ").\n")
        }
      } else {
        cat("\nNo se pudo extraer el valor p de la comparación.\n")
      }
      
    } else {
      cat("No se pudo calcular la comparación de interacción.\n")
    }
  })
  
  
  ## Matriz de varianza-covarianza del modelo--> solo para modelos mixtos ##
  output$var_cov_matrix <- renderPrint({
    req(model_result())
    model_info <- model_result()
    if (is.null(model_info$model)) {
      cat("No se ha generado ningún modelo aún.\n")
      return()
    }
    if (model_info$model_type == "lm") {
      cat("Los modelos lm() no incluyen efectos aleatorios, por lo tanto no tienen matriz de varianzas y covarianzas asociada.\n")
      return()
    }
    
    tryCatch({ # Por si el modelo es NULL o no está bien ajustado, etc.
      varcov <- VarCorr(model_info$model)
      var_cov_matrix_result(varcov)
      print(varcov)
      
      cat("\nInterpretación:\n")
      
      if (inherits(model_info$model, "lmerMod")) {
        cat("En este modelo solo se incluye un intercepto aleatorio, por lo que no aparecen covarianzas.\n")
        cat("La tabla muestra la desviación estándar estimada del intercepto aleatorio por sujeto y del residuo.\n")
        cat("La desviación estándar asociada a '(Intercept)' representa la variabilidad entre sujetos en su nivel basal. Si el valor es 0, indica que no hay variabilidad entre sujetos.\n")
        cat("La fila 'Residual' indica la variabilidad no explicada por el modelo.\n")
        
      } else if (inherits(model_info$model, "lme")) {
        cat("En este modelo solo se incluye un intercepto aleatorio, por lo que no aparecen covarianzas.\n")
        cat("La tabla muestra la varianza y la desviación estándar estimadas del intercepto aleatorio por sujeto y del residuo.\n")
        cat("La desviación estándar asociada a '(Intercept)' representa la variabilidad entre sujetos en su nivel basal. Si el valor es 0, indica que no hay variabilidad entre sujetos.\n")
        cat("La fila 'Residual' indica la variabilidad no explicada por el modelo.\n")
      }
      
    }, error = function(e) {
      cat("No se pudo calcular la matriz de varianza-covarianza.\n")
    })
  })
  

  ## Comparaciones entre grupos ##
  
  # Comparaciones entre grupos textual #
  # Mostrar medias estimadas por grupo
  # Global
  output$group_comparisons_emmeans <- renderPrint({
    req(model_result()) 
    model_result()$group_means_summary
  })
  # Por timepoint
  output$group_comparisons_emmeans_by_timepoint <- renderPrint({
    req(model_result()) 
    if (model_result()$model_type == "lm") {
      print("No aplica: el modelo lm() solo tiene un timepoint.")
    } else {
      print(model_result()$group_means_by_timepoint)
    }
  })
  # Por grupo
  output$group_comparisons_emmeans_by_group <- renderPrint({
    req(model_result()) 
    if (model_result()$model_type == "lm") {
      print("No aplica: el modelo lm() solo tiene un timepoint.")
    } else {
      print(model_result()$group_means_by_group)
    }
  })
  
  # Mostrar comparaciones de grupos (contrastes)
  # Global
  output$group_comparisons_contrasts <- renderPrint({
    req(model_result()) 
    model_result()$group_comparisons_summary
  })
  # Comparaciones entre grupos dentro de cada timepoint
  output$group_comparisons_by_timepoint <- renderPrint({
    req(model_result()) 
    if (model_result()$model_type == "lm") {
      print("No aplica: el modelo lm() solo tiene un timepoint.")
    } else {
      print(model_result()$comparisons_by_timepoint)
    }
  })
  # Comparaciones entre timepoints dentro de cada grupo
  output$group_comparisons_by_group <- renderPrint({
    req(model_result()) 
    if (model_result()$model_type == "lm") {
      print("No aplica: el modelo lm() solo tiene un timepoint.")
    } else {
      print(model_result()$comparisons_by_group)
    }
  })
  
  
  # Gráfico de comparaciones entre grupos #
  output$comparison_plots <- renderUI({
    req(model_result())
    
    tagList(
      # Gráfico global
      fluidRow(
        column(12, plotOutput("plot_global"))
      ),
      # Comparaciones adicionales si el modelo no es lm
      if (model_result()$model_type != "lm") {
        tagList(
          hr(),
          fluidRow(
            column(12, plotOutput("plot_by_timepoint"))
          ),
          hr(),
          fluidRow(
            column(12, plotOutput("plot_by_group"))
          )
        )
      }
    )
  })
  
  
  # Gráfico global
  output$plot_global <- renderPlot({
    req(comparison_plots_result()$global)
    comparison_plots_result()$global
  })
  # Gráfico de comparaciones por timepoint
  output$plot_by_timepoint <- renderUI({
    if (is.null(comparison_plots_result()$by_timepoint)) { # Primero se evalúa si hay gráfico y si lo hay se muestra el plotOutput()
      HTML("<em>No aplica: el modelo lm() solo tiene un timepoint.</em>")
    } else {
      plotOutput("plot_by_timepoint_real")
    }
  })
  # Gráfico por timepoint
  output$plot_by_timepoint_real <- renderPlot({
    req(comparison_plots_result()$by_timepoint)
    comparison_plots_result()$by_timepoint # Se dibuja el gráfico
  })

  # Gráfico de comparaciones por grupo
  output$plot_by_group <- renderUI({
    if (is.null(comparison_plots_result()$by_group)) { # Primero se evalúa si hay gráfico y si lo hay se muestra el plotOutput()
      HTML("<em>No aplica: el modelo lm() solo tiene un timepoint.</em>")
    } else {
      plotOutput("plot_by_group_real")
    }
  })
  # Gráfico por grupo
  output$plot_by_group_real <- renderPlot({
    req(comparison_plots_result()$by_group)
    comparison_plots_result()$by_group # Se dibuja el gráfico
  })
  
  
  ## Diagnósticos del modelo ##
  
  # Diagnósticos textuales #
  output$diagnostics <- renderPrint({
    req(model_result())
    diag_lines <- getDiagnostics()
    cat(diag_lines, sep="\n")
  })
  
  
  # Gráficos de diagnóstico #
  # Histograma
  output$hist_resid <- renderPlot({
    req(diagnostics_plots_result())
    diagnostics_plots_result()$hist_resid
  })
  output$hist_resid_text <- renderText({
    "Una distribución similar a la curva roja indica normalidad en los residuos."
  })
  
  # Q-Q plot residuos
   output$qqplot <- renderPlot({
    req(diagnostics_plots_result())
    diagnostics_plots_result()$qqplot
  })
  output$qqplot_text <- renderText({
    "Los puntos alineados en la línea indican normalidad en los residuos."
  })
  # Q-Q plot efectos aleatorios
  output$qqplot_re <- renderUI({
    if (is.null(diagnostics_plots_result()$qqplot_re)) { # Primero se evalúa si hay gráfico y si lo hay se muestra el plotOutput()
      HTML("Q-Q plot de efectos aleatorios:<br> <em>No aplica: el modelo lm() no tiene efectos aleatorios.</em>")
    } else {
      plotOutput("qqplot_re_real")
    }
  })
  output$qqplot_re_real <- renderPlot({
    req(diagnostics_plots_result()$qqplot_re)
    diagnostics_plots_result()$qqplot_re # Se dibuja el gráfico
  })
  output$qqplot_re_text <- renderText({
    "Los puntos alineados en la línea indican normalidad en los efectos aleatorios."
  })
  # Residuos vs. Valores ajustados
  output$resid_fitted <- renderPlot({
    req(diagnostics_plots_result())
    diagnostics_plots_result()$resid_fitted
  })
  output$resid_fitted_text <- renderText({
    "Una distribución aleatoria de los residuos indica homocedasticidad."
  })
  # Autocorrelación
  output$autocorr_plot <- renderPlot({
    req(diagnostics_plots_result())
    diagnostics_plots_result()$autocorr_plot
  })
  output$autocorr_text <- renderText({
    "Las barras dentro de la banda azul indican ausencia de autocorrelación."
  })
  
  
  ## Descarga de archivos ##
  
  # Descarga de plantillas (wide o long)
  output$downloadTemplate <- downloadHandler(
    # filename cambia según la opción elegida en input$template_choice
    filename = function() {
      if (input$template_choice == "Wide") "plantilla_wide.xlsx" else "plantilla_long.xlsx"
    },
    content = function(file) {
      if (input$template_choice == "Wide")
        file.copy("plantilla_wide.xlsx", file, overwrite = TRUE)
      else
        file.copy("plantilla_long.xlsx", file, overwrite = TRUE)
    }
  )
  
  # Descarga informe Excel #
  output$downloadExcel <- downloadHandler(
    filename = function() { paste0("informe_", Sys.Date(), ".xlsx") },
    content = function(file) {
      wb <- createWorkbook()
      
      # Crear hojas y guardar datos
      addWorksheet(wb, "Datos originales")
      writeDataTable(wb, "Datos originales", data_raw())
      
      addWorksheet(wb, "Datos procesados")
      writeDataTable(wb, "Datos procesados", data_processed())
      
      addWorksheet(wb, "Opciones seleccionadas")
        grupos_seleccionados <- ifelse(is.null(input$groups) || length(input$groups) == 0, "Ningún grupo seleccionado", paste(input$groups, collapse = ", "))
        modo_comparacion <- ifelse(is.null(input$comp_mode) || input$comp_mode == "", "No especificado", input$comp_mode)
        grupo_control <- ifelse(input$comp_mode == "all_vs_control" && !is.null(input$control_group), input$control_group, "No aplica")
        timepoints_seleccionados <- ifelse(is.null(input$timepoints) || length(input$timepoints) == 0, "Ningún timepoint seleccionado", paste(input$timepoints, collapse = ", "))
        variable_seleccionada <- ifelse(is.null(input$variable) || input$variable == "", "Ninguna variable seleccionada", input$variable)
        opciones_df <- data.frame( # Definir el data.frame 
          "Grupos seleccionados" = grupos_seleccionados,
          "Modo de comparación" = modo_comparacion,
          "Grupo control" = ifelse(input$comp_mode == "all_vs_control", grupo_control, "No aplica"),
          "Timepoints seleccionados" = timepoints_seleccionados,
          "Variable seleccionada" = variable_seleccionada
        )
      writeData(wb, "Opciones seleccionadas", opciones_df)
      
      addWorksheet(wb, "Modelo")
       model_formula = deparse(model_result()$formula)
      writeData(wb, "Modelo", x = "Fórmula del modelo:", startCol = 1, startRow = 1)
      writeData(wb, "Modelo", x = model_formula, startCol = 1, startRow = 2)
        model_txt <- capture.output(summary(model_result()$model))
      writeData(wb, "Modelo", model_txt, startCol = 1, startRow = 4)
      
      rd <- report_data()
      
      addWorksheet(wb, "Comparación interacción")
      writeData(wb, "Comparación interacción", rd$interaction_comparison)
      
      addWorksheet(wb, "Varianza-Covarianza")
      writeData(wb, "Varianza-Covarianza", rd$var_cov_matrix)
      
      addWorksheet(wb, "Comparación grupos")
      writeData(wb, "Comparación grupos", "Medias estimadas (globales)", startRow = 1)
      writeData(wb, "Comparación grupos", report_data()$group_comparisons$global_emmeans, startRow = 2)
        row <- length(report_data()$group_comparisons$global_emmeans) + 4
      writeData(wb, "Comparación grupos", "Medias por grupo dentro de cada timepoint", startRow = row)
      writeData(wb, "Comparación grupos", report_data()$group_comparisons$by_timepoint_emmeans, startRow = row + 1)
        row <- row + length(report_data()$group_comparisons$by_timepoint_emmeans) + 3
      writeData(wb, "Comparación grupos", "Medias por timepoint dentro de cada grupo", startRow = row)
      writeData(wb, "Comparación grupos", report_data()$group_comparisons$by_group_emmeans, startRow = row + 1)
        row <- row + length(report_data()$group_comparisons$by_group_emmeans) + 3
      writeData(wb, "Comparación grupos", "Contrastes globales", startRow = row)
      writeData(wb, "Comparación grupos", report_data()$group_comparisons$global_contrasts, startRow = row + 1)
        row <- row + length(report_data()$group_comparisons$global_contrasts) + 3
      writeData(wb, "Comparación grupos", "Contrastes entre grupos por timepoint", startRow = row)
      writeData(wb, "Comparación grupos", report_data()$group_comparisons$by_timepoint_contrasts, startRow = row + 1)
        row <- row + length(report_data()$group_comparisons$by_timepoint_contrasts) + 3
      writeData(wb, "Comparación grupos", "Contrastes entre timepoints por grupo", startRow = row)
      writeData(wb, "Comparación grupos", report_data()$group_comparisons$by_group_contrasts, startRow = row + 1)
      
      addWorksheet(wb, "Diagnósticos")
        diag_text <- rd$diagnostics
        diag_df <- data.frame(Text = diag_text, stringsAsFactors = FALSE)
      writeData(wb, "Diagnósticos", diag_df, startCol = 1, startRow = 1)
        style_wrap <- createStyle(wrapText = TRUE)
        addStyle(wb, sheet = "Diagnósticos", style = style_wrap, rows = 1, cols = 1, gridExpand = TRUE)
        setColWidths(wb, "Diagnósticos", cols = 1, widths = 80)
      
      # Guardar el archivo
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  # Descarga informe HTML #
  output$downloadReport <- downloadHandler(
    filename = function() { paste("informe_", Sys.Date(), ".html", sep = "") },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report/report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        selected_groups = input$groups, # Opciones seleccionadas en el menú de grupos
        selected_timepoints = input$timepoints, # Opciones seleccionadas en el menú de tiempo
        selected_variable = input$variable, # Opciones seleccionadas en el menú de variable
        comp_mode = input$comp_mode, # Opciones seleccionadas en el menú de comparativa de grupos
        control_group = ifelse(input$comp_mode == "all_vs_control" && !is.null(input$control_group), input$control_group, "No aplica"),
        data = data_raw(),
        data_processed = data_processed(),
        model_formula = deparse(model_result()$formula),
        model = model_result()$model,
        model_plot = model_plot_result(),
        interaction_comparison = interaction_comparison_result(),
        var_cov_matrix = var_cov_matrix_result(),
        group_comparisons = list(
          global_emmeans = model_result()$group_means_summary,
          by_timepoint_emmeans = if (model_result()$model_type == "lm" || is.null(model_result()$group_means_by_timepoint)) {
            "No aplica: el modelo lm solo tiene un timepoint."
          } else {
            model_result()$group_means_by_timepoint
          },
          by_group_emmeans = if (model_result()$model_type == "lm" || is.null(model_result()$group_means_by_group)) {
            "No aplica: el modelo lm solo tiene un timepoint."
          } else {
            model_result()$group_means_by_group
          },
          global_contrasts = model_result()$group_comparisons_summary,
          by_timepoint_contrasts = if (model_result()$model_type == "lm" || is.null(model_result()$comparisons_by_timepoint)) {
            "No aplica: el modelo lm solo tiene un timepoint."
          } else {
            model_result()$comparisons_by_timepoint
          },
          by_group_contrasts = if (model_result()$model_type == "lm" || is.null(model_result()$comparisons_by_group)) {
            "No aplica: el modelo lm solo tiene un timepoint."
          } else {
            model_result()$comparisons_by_group
          }
        ),
        group_plots = comparison_plots_result(),
        diagnostics = diagnostics_result(),
        diagnostics_plots = diagnostics_plots_result()
      )
      
      tryCatch({ # Por si hay algún error en la renderización
        rmarkdown::render(tempReport, output_format = "html_document", output_file = file,
                          params = params, envir = new.env(parent = globalenv()))
      }, error = function(e) {
        message("Error en la renderización: ", e$message)
        showNotification(paste("Error en la generación del informe:", e$message), type = "error")
      })
    }
  )
  
  
}


#### ============================
### 4. Se lanza la aplicación ###
#### ============================

shinyApp(ui, server)
