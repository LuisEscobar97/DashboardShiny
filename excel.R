library(httr)
library(dplyr)

#get access token
get_token_e <- function(){
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
get_exs <- function(apiPath,tokenRespose){
  
  response <-
    GET(apiPath,
        add_headers(Authorization = paste("Bearer", 
                                          tokenRespose$access_token, 
                                          sep = " ")),
        accept_json()
    )
}
#function to get the contente from a GET response
get_content_e <- function(response){
  content_response <- content(response)
}
#
listaParametros_e <- get_token_e()

#call function to get the data for open tags
ex_p <- get_exs(ruta_ex,listaParametros_e)

#way to get the important data froma a GET response
ex_p_content <- get_content_e(ex_p)

