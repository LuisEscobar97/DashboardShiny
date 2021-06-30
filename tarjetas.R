library(httr)
library(dplyr)

#get access token
get_token <- function(){
  #built de body request
  bodyy = list(
    grant_type = "client_credentials",
    client_id = cliente,
    client_secret = secret,
    resource = recurso
  )  
  #make de post request to get the list of parameters which has de acces token
  tokn <-
    POST(
      access_token,
      body = bodyy,
      encode = "multipart"
    )
  listaParams <- content(tokn, "parsed")
}



#function to get the data that we need
get_tags <- function(apiPath,tokenRespose){
  
  response <-
    GET(apiPath,
        add_headers(Authorization = paste("Bearer", 
                                          tokenRespose$access_token, 
                                          sep = " ")),
        accept_json(),
        content_type_json()
    )
}
#function to get the contente from a GET response
get_content <- function(response){
  content_response <- content(response)
}

#Forma real de como pasar el response de JSOn primero a un date frame sin estructura y luego de ese mismo
#dataFrame pasar a un data frame con las estructura correcta para poder graficar datos

#get data for open tags from the content response
df_open_tags <- function(contentTags){
  dataFrameAux2 <- as.data.frame(t(sapply(contentTags$value, c)))
  dFT <-
    data.frame(
      Linea = unlist(lapply(dataFrameAux2$L_x00ed_nea, c)),
      Area = unlist(lapply(dataFrameAux2$Are_x00e1_, c)),
      Tipo = unlist(lapply(dataFrameAux2$TipoTarjet, c)),
      Prioridad = unlist(lapply(dataFrameAux2$Prioridad, c)),
      Autor = unlist(lapply(dataFrameAux2$DetectadaPor, c)),
      FechaLev = unlist(lapply(dataFrameAux2$FechaLevantamiento, c)),
      estado = unlist(lapply(dataFrameAux2$Status, c)),
      TipoAnomalia = unlist(lapply(dataFrameAux2$TipoAnomalia, c)),
      Maquina = unlist(lapply(dataFrameAux2$Maquina_x002f_Equipo, c)),
      Parte = unlist(lapply(dataFrameAux2$Parte_x002f_Accesorio, c)),
      Anomalía = unlist(lapply(dataFrameAux2$Anomalia, c)),
      Id = unlist(lapply(dataFrameAux2$id0, c)),
      FechaSol = unlist(lapply(
        dataFrameAux2$Fecha_x0020_Solucion[is.null(dataFrameAux2$Fecha_x0020_Solucion)] <-
          "", c
      )),
      Paso = unlist(lapply(dataFrameAux2$Paso_x0020_AM, c)),
      SolucionadaPor = unlist(lapply(
        dataFrameAux2$Solucionada_x0020_Por[is.null(dataFrameAux2$Solucionada_x0020_Por)] <-
          "", c
      )),valor=1
    )
}

#get data for closed tags from the content response
df_closed_tags <- function(contentTags){
  dataFrameAux3 <- as.data.frame(t(sapply(tarjetasCe$value, c)))
  dataFrameTarjetasCe <-
    data.frame(
      Linea = unlist(lapply(dataFrameAux3$L_x00ed_nea, c)),
      Area = unlist(lapply(dataFrameAux3$Are_x00e1_, c)),
      Tipo = unlist(lapply(dataFrameAux3$TipoTarjet, c)),
      Prioridad = unlist(lapply(dataFrameAux3$Prioridad, c)),
      Autor = unlist(lapply(dataFrameAux3$DetectadaPor, c)),
      FechaLev = unlist(lapply(dataFrameAux3$FechaLevantamiento, c)),
      estado = unlist(lapply(dataFrameAux3$Status, c)),
      TipoAnomalia = unlist(lapply(dataFrameAux3$TipoAnomalia, c)),
      Maquina = unlist(lapply(dataFrameAux3$Maquina_x002f_Equipo, c)),
      Parte = unlist(lapply(dataFrameAux3$Parte_x002f_Accesorio, c)),
      Anomalía = unlist(lapply(dataFrameAux3$Anomalia, c)),
      Id = unlist(lapply(dataFrameAux3$id0, c)),
      FechaSol = unlist(lapply(
        dataFrameAux3$Fecha_x0020_Solucion[is.null(dataFrameAux3$Fecha_x0020_Solucion)] <-
          "", c
      )),
      Paso = unlist(lapply(dataFrameAux3$Paso_x0020_AM, c)),
      SolucionadaPor = unlist(lapply(
        dataFrameAux3$Solucionada_x0020_Por[is.null(dataFrameAux3$Solucionada_x0020_Por)] <-
          "", c
      )),valor=1
    )
}


get_tags_query <- function(case,dataframe){
  #case 1 for to get tags grouped by tipo anomalia and tipo filtered by tipo equals 
  #azul and ist count by tipo anomalia
  if(case==1){
    tag<- dataframe %>% 
      group_by(TipoAnomalia,Tipo)%>%
      filter(Tipo=="Azul") %>%
      count(TipoAnomalia)
  } #case 2 for get tags grouped by tipo anomalia and tipo filtered by tipo equals 
  #roja and its count by tipo anomalia
  else if(case==2){
    tag<- dataframe %>% 
      group_by(TipoAnomalia,Tipo)%>%
      filter(Tipo=="Roja") %>%
      count(TipoAnomalia)
  }#case 3 for get tags grouped by tipo anomalia and tipo
  #and its count by tipo anomalia
  else if(case==3){
    tag<-dataframe %>% 
      group_by(TipoAnomalia,Tipo) %>%
      count(TipoAnomalia)
  }#case 4 for get tags grouped by maquina and tipo filtered by tipo equals azul
  #and its count by maquina
  else if(case==4){
    tag <- dataframe %>% 
      group_by(Maquina,Tipo)%>%
      filter(Tipo=="Azul")%>%
      count(Maquina)
  }#case 4 for get tags grouped by maquina and tipo filtered by tipo equals roja
  #and its count by maquina
  else if(case==5){
    tag <- dataframe%>% 
      group_by(Maquina,Tipo)%>%
      filter(Tipo=="Roja")%>%
      count(Maquina)
  }
}

#call the get_token function
listaParametros <- get_token()

#call function to get the data for open tags
tarjetasJson <- get_tags(ruta_tags,listaParametros)

#call function to get the data for closed tags
tarjetasJsonCerradas <- get_tags(ruta_tags_c,listaParametros)


#way to get the important data froma a GET response
tarjetas <- get_content(tarjetasJson)
tarjetasCe <- get_content(tarjetasJsonCerradas)

dataFrameTarjetas <- df_open_tags(tarjetas)
dataFrameTarjetasCe <- df_closed_tags(tarjetasCe)
#union of the both tags queries
dataFrameTarjetas<- rbind(dataFrameTarjetas,dataFrameTarjetasCe)

tagsT <- get_tags_query(1,dataFrameTarjetas)

