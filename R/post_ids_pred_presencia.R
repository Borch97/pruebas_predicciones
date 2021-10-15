#' @title Llama N veces a la funcion de prediccion presencia donde N es el numero de dispositivos relacionados con el activo recbido
#'
#' @description Llama N veces a la funcion de prediccion presencia donde N es el numero de dispositivos relacionados con el activo recbido
#'
#' @param fecha_inicial, fecha_final, tipo_estancia
#'
#' @return json
#'
#' @examples  llamada_prediccion_presencia_temporal("2021-07-10 00:00:00", "2021-08-10 23:00:00", "aulas-pb")
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

llamada_prediccion_presencia_temporal <- function(fecha_inicial, fecha_final, tipo_estancia){

  # Volcado parámetros
  fecha_1 <- fecha_inicial
  fecha_2 <- fecha_final
  tipo_estancia <- tolower(tipo_estancia)


  #df <- read.csv("inst/extdata/dispositivos.csv", sep = ",")

  #id_aulas <- unlist(str_split(df$dev_id[c(grep("Aula ", df$Nombre),grep("Taller ", df$Nombre))],","))
  #id_empresas <- unlist(str_split(df$dev_id[1:16],","))
  #id_despachos_pb <- unlist(str_split(df$dev_id[17:27],","))
  #id_seminarios <- unlist(str_split(df$dev_id[28:34],","))
  #id_despachos_p1 <- unlist(str_split(df$dev_id[42:54],","))

  id_aulas <- c("f4d0d070-24dd-11eb-b605-01af9c6dd825","fa1cb5d0-24dd-11eb-b605-01af9c6dd825","feccf770-24dd-11eb-b605-01af9c6dd825","03179970-24de-11eb-b605-01af9c6dd825","083542e0-24de-11eb-b605-01af9c6dd825","0e3d9a70-24de-11eb-b605-01af9c6dd825","0e3d9a70-24de-11eb-b605-01af9c6dd825","f4d371c0-2d9f-11eb-b605-01af9c6dd825","02d28360-2da0-11eb-b605-01af9c6dd825","0b615ec0-2da0-11eb-b605-01af9c6dd825")
  id_empresas <- c("aa6cf240-24d6-11eb-b605-01af9c6dd825","af8e9350-24d6-11eb-b605-01af9c6dd825","7f2c2bf0-276a-11eb-b605-01af9c6dd825","b5fc6370-24d6-11eb-b605-01af9c6dd825","caaf8810-276a-11eb-b605-01af9c6dd825","c38cfbd0-24d6-11eb-b605-01af9c6dd825","cba93f40-24d6-11eb-b605-01af9c6dd825","cf2ac4e0-24d6-11eb-b605-01af9c6dd825","e7d0e380-24d6-11eb-b605-01af9c6dd825","ef5309d0-24d6-11eb-b605-01af9c6dd825","79a16980-2da4-11eb-b605-01af9c6dd825","f9bf09a0-24d6-11eb-b605-01af9c6dd825","00f6b970-24d7-11eb-b605-01af9c6dd825","04c79790-24d7-11eb-b605-01af9c6dd825","08af5910-24d7-11eb-b605-01af9c6dd825","575a6340-2da4-11eb-b605-01af9c6dd825")
  id_despachos_pb <- c("1e6fea50-24df-11eb-b605-01af9c6dd825","229e9fe0-24df-11eb-b605-01af9c6dd825","27a84c20-24df-11eb-b605-01af9c6dd825","2c6781e0-24df-11eb-b605-01af9c6dd825","30d7fb60-24df-11eb-b605-01af9c6dd825","350fffc0-24df-11eb-b605-01af9c6dd825","39802b20-24df-11eb-b605-01af9c6dd825","43d0b3b0-24df-11eb-b605-01af9c6dd825","47b05ee0-24df-11eb-b605-01af9c6dd825","4c4e7810-24df-11eb-b605-01af9c6dd825","92a2d790-24dd-11eb-b605-01af9c6dd825")
  id_seminarios <- c("a14e2ec0-24dd-11eb-b605-01af9c6dd825","b97ea830-24dd-11eb-b605-01af9c6dd825","b4f30590-24dd-11eb-b605-01af9c6dd825","a14e2ec0-24dd-11eb-b605-01af9c6dd825","cabeb400-24dd-11eb-b605-01af9c6dd825","d89f8f40-24dd-11eb-b605-01af9c6dd825","e2ca4050-24dd-11eb-b605-01af9c6dd825")
  id_despachos_p1 <- c("5321b630-24de-11eb-b605-01af9c6dd825","59d9d5c0-24de-11eb-b605-01af9c6dd825","5eaeb660-24de-11eb-b605-01af9c6dd825","6311c260-24de-11eb-b605-01af9c6dd825","67f54900-24de-11eb-b605-01af9c6dd825","6d17ad60-24de-11eb-b605-01af9c6dd825","72dda4c0-24de-11eb-b605-01af9c6dd825","7d24b770-24de-11eb-b605-01af9c6dd825","1dda2880-309c-11eb-b605-01af9c6dd825","817d8a40-24de-11eb-b605-01af9c6dd825","860200f0-24de-11eb-b605-01af9c6dd825","8a7fe7f0-24de-11eb-b605-01af9c6dd825","900c7fd0-24de-11eb-b605-01af9c6dd825","9fef93b0-24de-11eb-b605-01af9c6dd825")



  switch(tipo_estancia,
         "aulas-pb"={
           vector_ids <- id_aulas
         },
         "despachos-pb"={
           vector_ids <- id_despachos_pb
         },
         "empresas-pb"={
           vector_ids <- id_empresas
         },
         "seminarios-pb"={
           vector_ids <- id_seminarios
         },
         "despachos-p1"={
           vector_ids <- id_despachos_p1
         }
  )

  for(i in 1:length(vector_ids)){
    tryCatch({
      print(vector_ids[i])
      prediccion_presencia_temporal(fecha_1, fecha_2, vector_ids[i], tipo_estancia)
    }, error=function(e){
      cat("ERROR :",conditionMessage(e), "\n")
    })
  }

}
