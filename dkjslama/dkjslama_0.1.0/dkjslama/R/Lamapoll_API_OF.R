### Original Lamapoll API Functions, all credit to Lamapoll

#' Lamapoll Individual Data Download
#'
#' This function Let's you download Data of one questionnaire from Lamapoll through their API. The result of the function is an object of class dataframe.
#' @param email The mailadress that you are registered with in Lamapoll.
#' @param poll_name The name of the questionnaire in Lamapoll, from which you want to download the data.
#' @param apikey The personal API key, that Lamapoll created for you.
#' @return An object of class dataframe.
#' @examples
#' downloaded_data <- get_lamapoll_data <-function("duck@pond.com", "duckpoll", "xyz1234")
#' @import openxlsx
#' @import httr
#' @import jsonlite
#' @import curl
#' @import openssl
#' @export

get_lamapoll_data <-function(email, poll_name, apikey){

  # request authorization
  authorization_query <-paste0("https://api.lamapoll.de/api.php?task=requestAuth&user=", email)
  authorization <- curl::curl_fetch_memory(authorization_query)
  authorization_result <- jsonlite::fromJSON(rawToChar(authorization$content))
  if (authorization_result$code !="200") stop(paste0("Error in authorization request: ", authorization_result$code))
  auth_token <- authorization_result$authToken
  user_token <- openssl::sha256(paste0(auth_token, apikey))

  # authorize
  authorize_query <-paste0("https://api.lamapoll.de/api.php", "?", "task=authorize&user=", email, "&userToken=", user_token)
  final_auth <- curl::curl_fetch_memory(authorize_query)
  final_result <- jsonlite::fromJSON(rawToChar(final_auth$content))
  if (final_result$code !="200") stop(paste0("Error in authorization: ", final_result$code))

  requestToken <- final_result$requestToken
  api_token <- openssl::sha256(paste0(requestToken, apikey))

  # get data
  get_data_query <-paste0("https://api.lamapoll.de/api.php", "?", "task=getResults&type=csv&pollName=", poll_name, "&apiToken=", api_token, "&user=", email)
  fetch_data <- curl::curl_fetch_memory(get_data_query)
  data_result <- jsonlite::fromJSON(rawToChar(fetch_data$content))
  if (data_result$code !="200") stop(paste0("Error in data request: ", data_result$code))
  data <- read.csv(text = data_result$data)

  return(data)
}

#' Lamapoll Questionnaire List Download
#'
#' This function Let's you download a list of questionnaire names from Lamapoll through their API. The result of the function is an object of class dataframe.
#' @param email The mailadress that you are registered with in Lamapoll.
#' @param fb_status The status of the questionnaires of which you want all names. Can be set to "online" or "offline" or "inwork".
#' @param apikey The personal API key, that Lamapoll created for you.
#' @return An object of class dataframe.
#' @examples
#' downloaded_names <- get_lamapoll_list <-function("duck@pond.com", "duckpoll", "xyz1234")
#' @import openxlsx
#' @import httr
#' @import jsonlite
#' @import curl
#' @import openssl
#' @export

get_lamapoll_list <-function(email, fb_status, apikey){

# request authorization
authorization_query <-paste0("https://api.lamapoll.de/api.php?task=requestAuth&user=", "lamapoll@dkjs.de")
authorization <- curl::curl_fetch_memory(authorization_query)
authorization_result <- jsonlite::fromJSON(rawToChar(authorization$content))
if (authorization_result$code !="200") stop(paste0("Error in authorization request: ", authorization_result$code))
auth_token <- authorization_result$authToken
user_token <- openssl::sha256(paste0(auth_token, apikey))

# authorize
authorize_query <-paste0("https://api.lamapoll.de/api.php", "?", "task=authorize&user=", "lamapoll@dkjs.de", "&userToken=", user_token)
final_auth <- curl::curl_fetch_memory(authorize_query)
final_result <- jsonlite::fromJSON(rawToChar(final_auth$content))
if (final_result$code !="200") stop(paste0("Error in authorization: ", final_result$code))

requestToken <- final_result$requestToken
api_token <- openssl::sha256(paste0(requestToken, apikey))
# get data
get_data_query <-paste0("https://api.lamapoll.de/api.php", "?", "task=listPolls&status=", fb_status, "&apiToken=", api_token, "&user=", "lamapoll@dkjs.de")
fetch_data <- curl::curl_fetch_memory(get_data_query)
data_result <- jsonlite::fromJSON(rawToChar(fetch_data$content))
df_fb_names <- data_result$items
return(df_fb_names)
}
