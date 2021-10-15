#' @title Predice la existencia de presencia en una estancia objetivo en los 2 registros anteriores relativos a la marca temporal
#'
#' @description Predice la existencia de presencia en una estancia objetivo en los 2 registros anteriores relativos a la marca temporal.
#' Recibe los datos de condiciones ambientales de la estancia objetivo del último mes.
#' Procesa, limpia y prepara los datos para la predicción.
#' Utiliza el algoritmo knn para predecir los últimos dos registro en la serie de tiempo.
#' Estos registros marcarán la existencia de presencia en la sala en el t+30' donde t es la marca temporal del último registro.
#' Para que exista presencia, el resultado de la predicción de los dos últimos dos registros debe ser de 1 (presencia).
#' tipo_estancia: aulas-pb, despachos-pb, empresas-pb, seminarios-pb, despachos-p1
#'
#' @param fecha_inicial, fecha_final, id_estancia, tipo_estancia
#'
#' @return json
#'
#' @examples  prediccion_presencia_temporal("2021-07-10 00:00:00", "2021-08-10 23:00:00", "03179970-24de-11eb-b605-01af9c6dd825", "aulas-pb")
#'
#' @import httr
#' jsonlite
#' rjson
#' RCurl
#' dplyr
#' prob
#' zoo
#' lubridate
#' timeDate
#' RWeka
#' caret
#' class
#' gmodels
#' rJava
#'
#' @export

prediccion_presencia_temporal <- function(fecha_inicial = as.character(Sys.time()-30*24*60*60), fecha_final = as.character(Sys.time()), id_estancia = "f4d0d070-24dd-11eb-b605-01af9c6dd825", tipo_estancia = "despachos-pb"){

  # Volcado parámetros
  id_dispositivo <- id_estancia
  fecha_1 <- fecha_inicial
  fecha_2 <- fecha_final
  tipo_estancia <- tolower(tipo_estancia)


  # ==============================================================================
  # PETICIÓN TOKEN THB
  # ==============================================================================

  cuerpo <- toJSON(list(username="kepa@techfriendly.es",password='kepa_tech'))
  post <- httr::POST(url = "http://116.202.100.157:8080/api/auth/login",
                     add_headers("Content-Type"="application/json","Accept"="application/json"),
                     body = cuerpo,
                     encode = "json",verbose()
  )

  resultado_peticion_token <- httr::content(post)
  auth_thb <- paste("Bearer",resultado_peticion_token$token)


  #=================================================================================================
  #=================================================================================================

  # 1) PETICIÓN DATOS ENTRE FECHAS Y TRATAMIENTO DF

  #=================================================================================================
  #=================================================================================================

  # Paso a timestamp
  fecha_1 <- format(as.numeric(as.POSIXct(fecha_1))*1000,scientific = F)
  fecha_2 <- format(as.numeric(as.POSIXct(fecha_2))*1000,scientific = F)

  keys <- URLencode(c("temperatura,humedad_relativa,co2,luminosidad,pm1_0,pm2_5,pm10,p0_3,p0_5,p1,p2_5,p3,p10,estado,intensidad,puerta,presencia"))
  url_thb_fechas <- paste("http://116.202.100.157:8080/api/plugins/telemetry/DEVICE/",id_dispositivo,"/values/timeseries?limit=10000&keys=",keys,"&startTs=",fecha_1,"&endTs=",fecha_2,sep = "")
  peticion <- GET(url_thb_fechas, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  # Tratamiento datos. De raw a dataframe
  df <- jsonlite::fromJSON(rawToChar(peticion$content))
  # TENGO QUE COGER EL QUE MENOS DATOS TENGA Y TIRAR DE ESE TIMESTAMP
  ts_dif <- prob::setdiff(df$temperatura$ts,df$presencia$ts) # Búsqueda de datos a remover de temperatura y humedad relativa
  if(!identical(ts_dif,numeric(0))){
    df$temperatura <- df$temperatura[!(df$temperatura$ts %in% c(ts_dif)),]
    df$humedad_relativa <- df$humedad_relativa[!(df$humedad_relativa$ts %in% c(ts_dif)),]
  }
  estado <- df$estado
  df <- df[-grep("estado",names(df))]
  df_datos <- data.frame(format(df$temperatura$ts,scientific=FALSE),df$temperatura$value,df$humedad_relativa$value,df$co2$value,df$luminosidad$value,
                         df$pm1_0$value,df$pm2_5$value,df$pm10$value,df$p0_3$value,df$p0_5$value,df$p1$value,df$p2_5$value,df$p3$value,df$p10$value,
                         df$intensidad$value,df$puerta$value,df$presencia$value,stringsAsFactors = FALSE)
  colnames(df_datos) <- c("ts",names(df))
  df_datos$fecha_time <- as.POSIXct(as.numeric(df_datos$ts)/1000, origin = "1970-01-01")
  df_datos <- df_datos[,c(18,1:17)]



  #=================================================================================================
  #=================================================================================================

  # 2) ETL + ESTADÍSTICA DESCRIPTIVA

  #=================================================================================================
  #=================================================================================================

  # 2.1) ETL DF

  # Paso a numérico
  cols <- colnames(df_datos)[3:ncol(df_datos)]
  df_datos[cols] <- sapply(df_datos[cols],as.numeric)

  # Errores a NA
  fecha_columna <- df_datos[,1]
  df_datos <- df_datos[,-1]
  df_datos[df_datos > 5000] <- NA
  df_datos$puerta[df_datos$puerta == 3] <- NA
  df_datos$Fecha <- fecha_columna
  df_datos <- df_datos[,c(ncol(df_datos),(1:(ncol(df_datos)-1)))]  # Orden columnas

  # Paso de NA temperatura, humedad, particulas y co2 = PROMEDIO
  df_datos <- df_datos[order(df_datos$ts),]  # Orden cronológico
  df_datos$temperatura <- round(zoo::na.approx(df_datos$temperatura,na.rm = FALSE),1)
  df_datos$humedad_relativa <- round(zoo::na.approx(df_datos$humedad_relativa,na.rm = FALSE),1)
  df_datos$co2 <- round(zoo::na.approx(df_datos$co2,na.rm = FALSE),1)
  df_datos$p0_3[is.na(df_datos$p0_3)] <- median(df_datos$p0_3, na.rm = TRUE)
  df_datos$p0_5[is.na(df_datos$p0_5)] <- median(df_datos$p0_5, na.rm = TRUE)
  df_datos$pm1_0[is.na(df_datos$pm1_0)] <- median(df_datos$pm1_0, na.rm = TRUE)
  df_datos$pm2_5[is.na(df_datos$pm2_5)] <- median(df_datos$pm2_5, na.rm = TRUE)
  df_datos$pm10[is.na(df_datos$pm10)] <- median(df_datos$pm10, na.rm = TRUE)
  df_datos$p1[is.na(df_datos$p1)] <- median(df_datos$p1, na.rm = TRUE)
  df_datos$p3[is.na(df_datos$p3)] <- median(df_datos$p3, na.rm = TRUE)
  df_datos$p10[is.na(df_datos$p10)] <- median(df_datos$p10, na.rm = TRUE)
  df_datos$p2_5[is.na(df_datos$p2_5)] <- median(df_datos$p2_5, na.rm = TRUE)
  df_datos$luminosidad[is.na(df_datos$luminosidad)] <- median(df_datos$luminosidad, na.rm = TRUE)
  df_datos$puerta[is.na(df_datos$puerta)] <- median(df_datos$puerta, na.rm = TRUE)
  df_datos <- df_datos[!is.na(df_datos$temperatura),] # Eliminación últimos NA


  # Definición por tipo de estancia: aulas-pb, despachos-pb, empresas-pb, seminarios-pb, despachos-p1
  switch(tipo_estancia,
         "aulas-pb"={
           df_datos$luminosidad_binaria <- df_datos$luminosidad
           for(i in 1:nrow(df_datos)){
             if(df_datos$luminosidad_binaria[i] >= 3500 & df_datos$intensidad[i] == 0){
               df_datos$luminosidad_binaria[i] <- 0
             }else if(df_datos$luminosidad_binaria[i] < 3500 & df_datos$luminosidad_binaria[i] >= 3000){
               df_datos$luminosidad_binaria[i] <- rbinom(1, size = 1, prob=0.9)
             }else{
               df_datos$luminosidad_binaria[i] <- 1
             }
           }
         },
         "despachos-pb"={
           df_datos$luminosidad_binaria <- df_datos$luminosidad
           for(i in 1:nrow(df_datos)){
             if(df_datos$luminosidad_binaria[i] >= 3500 & df_datos$intensidad[i] == 0){
               df_datos$luminosidad_binaria[i] <- 0
             }else if(df_datos$luminosidad_binaria[i] < 3500 & df_datos$luminosidad_binaria[i] >= 3000){
               df_datos$luminosidad_binaria[i] <- rbinom(1, size = 1, prob=0.9)
             }else{
               df_datos$luminosidad_binaria[i] <- 1
             }
           }
         },
         "empresas-pb"={
           df_datos$luminosidad_binaria <- df_datos$luminosidad
           for(i in 1:nrow(df_datos)){
             if(df_datos$luminosidad_binaria[i] >= 2500 & df_datos$intensidad[i] == 0){
               df_datos$luminosidad_binaria[i] <- 0
             }else if(df_datos$luminosidad_binaria[i] < 2500 & df_datos$luminosidad_binaria[i] >= 2000){
               df_datos$luminosidad_binaria[i] <- rbinom(1, size = 1, prob=0.9)
             }else{
               df_datos$luminosidad_binaria[i] <- 1
             }
           }
         },
         "seminarios-pb"={
           df_datos$luminosidad_binaria <- df_datos$luminosidad
           for(i in 1:nrow(df_datos)){
             if(df_datos$luminosidad_binaria[i] >= 2500 & df_datos$intensidad[i] == 0){
               df_datos$luminosidad_binaria[i] <- 0
             }else if(df_datos$luminosidad_binaria[i] < 2500 & df_datos$luminosidad_binaria[i] >= 2000){
               df_datos$luminosidad_binaria[i] <- rbinom(1, size = 1, prob=0.9)
             }else{
               df_datos$luminosidad_binaria[i] <- 1
             }
           }
         },
         "despachos-p1"={
           df_datos$luminosidad_binaria <- df_datos$luminosidad
           for(i in 1:nrow(df_datos)){
             if(df_datos$luminosidad_binaria[i] >= 2500 & df_datos$intensidad[i] == 0){
               df_datos$luminosidad_binaria[i] <- 0
             }else if(df_datos$luminosidad_binaria[i] < 2500 & df_datos$luminosidad_binaria[i] >= 2000){
               df_datos$luminosidad_binaria[i] <- rbinom(1, size = 1, prob=0.9)
             }else{
               df_datos$luminosidad_binaria[i] <- 1
             }
           }
         }
  )



  # Normalización (unsupervised attribute normalize filter) - Facilita la implementaciónd e algoritmos
  df_datos$Fecha <- as.character(df_datos$Fecha)  # Se evita normalizar la fecha
  df_datos$luminosidad <- as.character(df_datos$luminosidad)
  df_datos$luminosidad_binaria <- as.character(df_datos$luminosidad_binaria)
  df_datos$puerta <- as.character(df_datos$puerta)
  df_datos$intensidad <- as.character(df_datos$intensidad)
  df_datos$presencia <- as.character(df_datos$presencia)
  datos_normalizados <- RWeka::Normalize(~., data = df_datos)

  datos_normalizados$luminosidad <- as.numeric(datos_normalizados$luminosidad)
  datos_normalizados$luminosidad_binaria <- as.numeric(datos_normalizados$luminosidad_binaria)
  datos_normalizados$puerta <- as.numeric(datos_normalizados$puerta)
  datos_normalizados$intensidad <- as.numeric(datos_normalizados$intensidad)
  datos_normalizados$presencia <- as.numeric(datos_normalizados$presencia)


  #======
  # 3) INGENIERÍA DE CARÁCTERISTICAS

  # 3.1) FECHA, Y HORA
  # Separación variable fecha_time en 2 variables: 1) fecha y 2) hora
  datos_normalizados$Fecha <- as.POSIXct(datos_normalizados$Fecha, format="%Y-%m-%d %H:%M:%S")  # Se evita normalizar la fecha
  datos_normalizados$fecha <- format(datos_normalizados$Fecha, format = "%Y-%m-%d")
  datos_normalizados$fecha <- as.Date(datos_normalizados$fecha)
  datos_normalizados$hora <- format(datos_normalizados$Fecha, format = "%H:%M:%S")
  datos_normalizados$hora <- hms(datos_normalizados$hora)  # Paquete lubridate para compara fechas

  # 3.2 HORA CATEGORIZADA (Mañana 1 (7:00 – 11:00) = 1; Mañana 2 (>11:00 –15:00) = 2; Tarde 1 (>15:00 - 19:00) = 3; Tarde 2 (>19:00 - 23:00) = 4; Noche (>23:00 – 7:00) = 5)
  datos_normalizados$hora_categorizada <- rep(0, nrow(datos_normalizados))



  for(i in 1:nrow(datos_normalizados)){
    hora_cat <- gsub("H.*","",datos_normalizados$hora[i])
    if(grepl("M",hora_cat)){
      hora_cat <- 0
    }else{
      hora_cat <- as.numeric(hora_cat)
    }
    datos_normalizados$hora_categorizada[i] <- hora_cat
  }

  # 3.3 FIMES DE SEMANA & FESTIVOS (1) VS LABORABLES (0)
  # Extracción calendario laboral Logroño
  peticion_dias_festivos <- GET("https://holydayapi.herokuapp.com/holidays/city_code/26089/year/2021", add_headers("Content-Type"="application/json","Accept"="application/json"))
  df_festivos <- jsonlite::fromJSON(rawToChar(peticion_dias_festivos$content))
  df_festivos$day <- as.Date(df_festivos$day,"%d/%m/%Y")
  df_festivos$day <- as.Date(df_festivos$day,"%Y-%m-%d")

  # Desagregación fines de semana
  datos_normalizados$fin_semana_festivo <- weekdays(datos_normalizados$fecha)
  datos_normalizados$fin_semana_festivo <- ifelse(datos_normalizados$fin_semana_festivo == "sábado" | datos_normalizados$fin_semana_festivo == "domingo",1,0)
  datos_normalizados$fin_semana_festivo[which(datos_normalizados$fecha %in% df_festivos$day)] <- 1  # A los festivos también les asigno un 1

  # 3.4 PRESENCIA INFERIDA DE VARIABLES PREDICTORAS
  #datos_normalizados$presencia_inferida <- rep(0,nrow(datos_normalizados))
  for(i in 1:nrow(datos_normalizados)){
    if(datos_normalizados$luminosidad_binaria[i] == 1){
      datos_normalizados$presencia[i] <- 1
      hora_ref <- datos_normalizados$hora_categorizada[i] - 1
      dia_ref <- datos_normalizados$fecha[i]
      datos_normalizados$presencia[datos_normalizados$fecha == dia_ref & datos_normalizados$hora_categorizada == hora_ref] <- 1
    }
    if(datos_normalizados$presencia[i] == 1 & datos_normalizados$luminosidad_binaria[i] == 1){
      datos_normalizados$presencia[i] <- 1
      hora_ref <- datos_normalizados$hora_categorizada[i] - 1
      dia_ref <- datos_normalizados$fecha[i]
      datos_normalizados$presencia[datos_normalizados$fecha == dia_ref & datos_normalizados$hora_categorizada == hora_ref] <- 1
    }
  }

  # Orden de datos normalziados por fecha y volcado de 2 últimos registros
  datos_normalizados <- datos_normalizados[order(datos_normalizados$Fecha),]
  datos_normalizados_2_ultimos_registros <- datos_normalizados[(nrow(datos_normalizados)-1):nrow(datos_normalizados),]
  df_datos <- df_datos[order(df_datos$Fecha),]
  df_datos_2_ultimos_registros <- df_datos[(nrow(df_datos)-1):nrow(df_datos),]
  # Al menos debe de haber 2 datos en la misma franga horaria para realizar la predicción
  if(datos_normalizados_2_ultimos_registros$hora_categorizada[1] != datos_normalizados_2_ultimos_registros$hora_categorizada[2]){
    return(FALSE)
  }





  #========================================================================================================================
  #========================================================================================================================

  # MODELO DE PREDICCIÓN: KNN

  #========================================================================================================================
  #========================================================================================================================


  datos_normalizados$presencia <- as.numeric(datos_normalizados$presencia)
  #datos_normalizados <- datos_normalizados[,c(3:15,22,23,18)]
  datos_normalizados <- datos_normalizados[,c(3:5,7:15,22,23,18)]
  datos_normalizados <- na.omit(datos_normalizados)

  set.seed(123)

  default_idx = sample(nrow(datos_normalizados), 0.7*nrow(datos_normalizados))  # Separación 70-30
  default_trn = datos_normalizados[default_idx, ]
  default_tst = datos_normalizados[-default_idx, ]

  # Datos entrenamiento
  pos_presencia <- grep("presencia", colnames(default_trn))
  X_default_trn = default_trn[, -pos_presencia]
  #X_default_trn = default_trn
  y_default_trn = default_trn$presencia

  # Datos test
  pos_presencia <- grep("presencia", colnames(default_tst))
  X_default_tst = default_tst[, -pos_presencia]
  #X_default_tst = default_tst
  y_default_tst = default_tst$presencia


  #============================
  # Selección de k óptimo
  set.seed(123)
  k_to_try = 1:100
  err_k = rep(x = 0, times = length(k_to_try))

  calc_class_err = function(actual, predicted) {
    mean(actual != predicted)
  }

  for (i in seq_along(k_to_try)) {
    pred = knn(train = X_default_trn,
               test  = X_default_tst,
               cl    = y_default_trn,
               k     = k_to_try[i])
    err_k[i] = calc_class_err(y_default_tst, pred)
  }


  # Grafico del error por número de K
  #plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20,
  #     xlab = "k, número de vecinos", ylab = "Error clasificación",
  #     main = "(Test) Error vs Nº Vecinos")
  # add line for min error seen
  #abline(h = min(err_k), col = "darkorange", lty = 3)
  # add line for minority prevalence in test set
  #abline(h = mean(y_default_tst == "Yes"), col = "grey", lty = 2)

  min(err_k[err_k != 0])
  which(err_k == min(err_k[err_k != 0]))
  k_optimo <- max(which(err_k ==min(err_k[err_k != 0])))

  table(y_default_tst)
  #============================



  # Predicción
  #head(class::knn(train = X_default_trn, test = X_default_tst, cl = y_default_trn, k= k_optimo))
  #prediccion <- class::knn(train = X_default_trn, test = X_default_tst, cl = y_default_trn, k= k_optimo)

  # Evaluación modelo/attributes
  #X_default_tst <- data.frame(X_default_tst)
  #clases <- data.frame(prediccion, X_default_tst$presencia)
  #names(clases) <- c("Predicción", "Observados")
  #head(clases)

  #CrossTable(x = clases$Observados, y = clases$Predicción, prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)
  #precision <- 100 * sum(y_default_trn == prediccion)/NROW(y_default_trn)

  #confusionMatrix(table(prediccion ,y_default_tst))




  # Predicción de los 2 últimos registros

  datos_normalizados_2_ultimos_registros$presencia <- as.numeric(datos_normalizados_2_ultimos_registros$presencia)
  #datos_normalizados <- datos_normalizados[,c(3:15,22,23,18)]
  datos_normalizados_2_ultimos_registros <- datos_normalizados_2_ultimos_registros[,c(3:5,7:15,22,23,18)]
  datos_normalizados_2_ultimos_registros <- na.omit(datos_normalizados_2_ultimos_registros)

  # Datos test
  default_tst = datos_normalizados_2_ultimos_registros
  pos_presencia <- grep("presencia", colnames(default_tst))
  X_default_tst = default_tst[, -pos_presencia]
  #X_default_tst = default_tst
  y_default_tst = default_tst$presencia


  prediccion <- class::knn(train = X_default_trn, test = X_default_tst, cl = y_default_trn, k= k_optimo)
  prediccion <- as.numeric(as.character(prediccion))
  resultado_ambas_predicciones <- prediccion[1] + prediccion[2]
  if(resultado_ambas_predicciones > 1){
    fecha_presencia <- min(df_datos_2_ultimos_registros$Fecha)
    fecha_presencia <- as.character(as.timeDate(fecha_presencia) + 10*60)
    fecha_presencia <- format(as.numeric(as.POSIXct(fecha_presencia))*1000,scientific = F)
    presencia_prediccion <- TRUE
  }else{
    fecha_presencia <- min(df_datos_2_ultimos_registros$Fecha)
    fecha_presencia <- as.character(as.timeDate(fecha_presencia) + 10*60)
    fecha_presencia <- format(as.numeric(as.POSIXct(fecha_presencia))*1000,scientific = F)
    presencia_prediccion <- FALSE
  }



  # GET token del dispositivo
  url <- paste("http://116.202.100.157:8080/api/device/",id_dispositivo,"/credentials",sep = "")
  get_token <- httr::GET(url = url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  token <- jsonlite::fromJSON(rawToChar(get_token$content))
  token <- token$credentialsId

  # Actualizacióna atributo presencia-prediccion plataforma
  url <- paste("http://116.202.100.157:8080/api/v1/",token,"/attributes",sep = "")
  json_envio_plataforma <- paste('{"presencia_prediccion":', presencia_prediccion,'}',sep = "")
  post <- httr::POST(url = url,
                     add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                     body = json_envio_plataforma,
                     verify= FALSE,
                     encode = "json",verbose()
  )

  # Actualizacióna atributo hora_presencia plataforma
  json_envio_plataforma <- paste('{"hora_presencia":"', fecha_presencia,'"}',sep = "")
  post <- httr::POST(url = url,
                     add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                     body = json_envio_plataforma,
                     verify= FALSE,
                     encode = "json",verbose()
  )

  return(1)



}

