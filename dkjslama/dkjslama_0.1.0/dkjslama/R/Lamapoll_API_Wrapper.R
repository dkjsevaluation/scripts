#' Lamapoll Data Download
#'
#' This function Let's you download Data from Lamapoll through their API. Upon using, the package will create a folder in the path entered in the function (second parameter: "xdir"). The data will be stored in individual excel files for each questionnaire in this folder.
#' @param xtatus The status of the Questionnaire in Lamapoll. You can enter three types of status. 1., "selection". This will download the data of selected Questionnaires. The names of the questionnaires have to be entered in an excel file. Please ask Team W&E for further instructions. 2., "online". This will download all data of all questionnaires, that are currently online. 3., "offline". This will download all data of all questionnaires, that are currently offline. The package will create a subfolder for each type of status and save the data in the appropriate subfolder. The function will add one column to each datafile called "p_name", stating the name of the Lamapoll questionnaire.
#' @param xdir The directory into which the function will download the data, i.e. your backup folder.
#' @param ydir The directory that holds an excel file with one column, in which questionnaire names from Lamapoll are being pasted. You can use the function to download a selection of Lamapoll questionnaire data by specifying "selection" as status (xstatus) and creating an excel file with the dedicated questionnaire names in one column.
#' @param apikey The personal API key, that Lamapoll created for the DKJS User Account.
#' @return A folder that holds one excel file for each Lamapoll questionnaires with the specified status (xstatus).
#' @examples
#' backup.lamapoll <- function("online", "...Data/Backup/", "...Data/Programmliste.xslx", "xyz123")
#' @import openxlsx
#' @import httr
#' @import fs
#' @import jsonlite
#' @import curl
#' @import openssl
#' @export

backup.lamapoll <- function(xstatus, xdir, ydir, apikey) {

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

  programmliste <- openxlsx::read.xlsx(paste(ydir), sheet = 1, colNames = TRUE)
  p_list <- as.list(programmliste$FB_name)

  if (xstatus == "online" || xstatus == "offline") {

    fb_names <- as.data.frame(get_lamapoll_list("lamapoll@dkjs.de", xstatus, apikey))
    names(fb_names)[1] <- "fb_names"
    list_status <- as.list(fb_names$fb_names)

    p_list_df_status <- lapply(list_status, function (x) get_lamapoll_data("lamapoll@dkjs.de", x, apikey))
    names(p_list_df_status) <- list_status
    names(list_status) <- list_status
    p_list_df_status <- p_list_df_status[sapply(p_list_df_status, nrow)>0]
    p_list_df2_status <- mapply(cbind, p_list_df_status, "p_name"=names(p_list_df_status), SIMPLIFY=F)

    file_sysdate <- Sys.Date()
    file_sysdate <- format(file_sysdate, format="_%d%m%y")
    backup_dir <- paste0(xdir, "backup", file_sysdate)
    fs::dir_create(backup_dir)

    dir_status <- paste0(backup_dir, "/", "backup", file_sysdate, "_", xstatus)
    fs::dir_create(dir_status)

    file_ending_status <- paste0("_", xstatus, file_sysdate, ".xlsx")
    lapply(p_list_df2_status, function (x) openxslx::write.xlsx(x, paste0(dir_status, "/", names(p_list_df2_status[]), file_ending_status)))
  }

  else if (xstatus == "selection") {

    p_list_df <- lapply(p_list, function (x) get_lamapoll_data("lamapoll@dkjs.de", x, apikey))
    names(p_list_df) <- p_list
    names(p_list) <- p_list
    p_list_df <- p_list_df[sapply(p_list_df, nrow)>0]
    p_list_df2 <- mapply(cbind, p_list_df, "p_name"=names(p_list_df), SIMPLIFY=F)

    file_sysdate <- Sys.Date()
    file_sysdate <- format(file_sysdate, format="_%d%m%y")
    backup_dir <- paste0(xdir, "backup", file_sysdate)
    fs::dir_create(backup_dir)

    dir_selection <- paste0(backup_dir, "/", "backup", file_sysdate, "_selection")
    fs::dir_create(dir_selection)
    file_ending <- paste0("_selection", file_sysdate, ".xlsx")
    lapply(p_list_df2, function (x) openxlsx::write.xlsx(x, paste0(dir_selection, "/", names(p_list_df2[]), file_ending)))
  }

  else if (xstatus == "inwork") {
    return("If a questionnaire is still being set up, there should be no data to download. Please make sure, that the questionnaire is either online or offline in Lamapoll.")
  }

  else {
    return("Oh no! You entered something wrong. Call for help!", "online")
  }
}
