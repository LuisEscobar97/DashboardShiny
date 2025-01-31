library(httr)
library(ggplot2)

get_token_t <- function(){
  #get the env varaibles
  source('env.R')
  
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
get_tCard <- function(apiPath,tokenRespose){
  
  response <-
    GET(apiPath,
        add_headers(Authorization = paste("Bearer", 
                                          tokenRespose$access_token, 
                                          sep = " ")),
        accept_json(),
        content_type_json()
    )
}


#function to get the content from a GET response
get_content_t <- function(response){
  content_response <- content(response)
}

#function to extrac and tranform data from get content response to data freme
get_tCard_df <- function(content){
      #Forma real de como pasar el response de JSOn primero a un date frame sin estructura y luego de ese mismo
    dataFrameAux5 <- as.data.frame(t(sapply(content$value, c)))
      #dataFrame pasar a un data frame con las estructura correcta para poder graficar datos
    dataFrameTCARDS <-
      data.frame(
        id = unlist(lapply(dataFrameAux5$ID, c)),
        TCard = unlist(lapply(dataFrameAux5$Tcard, c)),
        colaborador= unlist(lapply(dataFrameAux5$colaborador, c)),
        correo_colaborador=unlist(lapply(dataFrameAux5$CorreoCol, c)),
        no_SAP_col=unlist(lapply(dataFrameAux5$N_x002e_SAPColaborador, c)),
        coach = unlist(lapply(dataFrameAux5$Evaluador, c)),
        correo_coach=unlist(lapply(dataFrameAux5$CorreoCoach, c)),
        no_SAP_coach=unlist(lapply(dataFrameAux5$N_x002e_SAPEvaluador, c)),
        Linea = unlist(lapply(dataFrameAux5$Linea, c)),
        Area = unlist(lapply(dataFrameAux5$Area, c)),
        fecha = unlist(lapply(dataFrameAux5$FechaLevantamiento, c)),
        esperado = unlist(lapply(dataFrameAux5$esperado, c)),
        obtenido = unlist(lapply(dataFrameAux5$obtenido, c)),
        calificacion=unlist(lapply(dataFrameAux5$calificacion, c)),
        comentarios = unlist(lapply(
          dataFrameAux5$Extra[is.null(dataFrameAux5$Extra)] <-
            "", c
        ))
      )
}

#call the get_token function
listaParametros_t <- get_token_t()

#get the main data
datos <-get_tCard(ruta_tcard,listaParametros_t)

#manera de como obtener la lista con unicamnete los valores de la consulta
datos <- get_content_t(datos)

data_frame_tcards <- get_tCard_df(datos)


