backup.lamapoll <- function(xstatus, xdir, ydir, email, apikey) {

  get.lamapoll.list <-function(email, fb_status, apikey){

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
    api_token <<- openssl::sha256(paste0(requestToken, apikey))
    # get data
    get_data_query <-paste0("https://api.lamapoll.de/api.php", "?", "task=listPolls&status=", fb_status, "&apiToken=", api_token, "&user=", email)
    fetch_data <- curl::curl_fetch_memory(get_data_query)
    data_result <- jsonlite::fromJSON(rawToChar(fetch_data$content))
    df_fb_names <- data_result$items
    return(df_fb_names)
    return(api_token)
  }

  get.lamapoll.data <-function(poll_name, email, api_token){

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

    fb_names <- as.data.frame(get.lamapoll.list(email, xstatus, api_token))
    names(fb_names)[1] <- "fb_names"
    list_status <- as.list(fb_names$fb_names)

    p_list_df_status <- lapply(list_status, function (x) get.lamapoll.data(email, x, apikey))
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
    for (i in seq_along(p_list_df2_status)) {
      openxlsx::write.xlsx(p_list_df2_status[[i]], paste0(dir_status, "/", names(p_list_df2_status)[i], file_ending_status))
    }
  }

  else if (xstatus == "selection") {

    p_list_df <- lapply(p_list, function (x) get.lamapoll.data(email, x, apikey))
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
    for (i in seq_along(p_list_df2_status)) {
      openxlsx::write.xlsx(p_list_df2_status[[i]], paste0(dir_status, "/", names(p_list_df2_status)[i], file_ending_status))
    }
  }

  else if (xstatus == "inwork") {
    return("If a questionnaire is still being set up, there should be no data to download. Please make sure, that the questionnaire is either online or offline in Lamapoll.")
  }

  else {
    return("Oh no! You entered something wrong. Call for help!")
  }
}


###

backup.lamapoll(xstatus = "online", xdir = "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Backup/", ydir = "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Programmliste.xlsx", email = "we@dkjs.de", apikey = "wGl5v4fz")



##############################################################
xdir <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Backup/"
ydir <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Programmliste.xlsx"
email <- "we@dkjs.de"
apikey <- "wGl5v4fz"
xstatus <- "online"


####

get.lamapoll.list <-function(email, fb_status, apikey){

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
  api_token <<- openssl::sha256(paste0(requestToken, apikey))
  # get data
  get_data_query <-paste0("https://api.lamapoll.de/api.php", "?", "task=listPolls&status=", fb_status, "&apiToken=", api_token, "&user=", email)
  fetch_data <- curl::curl_fetch_memory(get_data_query)
  data_result <- jsonlite::fromJSON(rawToChar(fetch_data$content))
  df_fb_names <- data_result$items
  return(df_fb_names)
  return(api_token)
}

###
get.lamapoll.list(email = "we@dkjs.de", fb_status = "online", apikey = "wGl5v4fz")

####
get.lamapoll.data <-function(poll_name, email, api_token){

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

  fb_names <- as.data.frame(get.lamapoll.list(email, xstatus, apikey))
  names(fb_names)[1] <- "fb_names"
  list_status <- as.list(fb_names$fb_names)

  p_list_df_status <- lapply(list_status, function (x) get.lamapoll.data(x, email = "we@dkjs.de", api_token = api_token))
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
  for (i in seq_along(p_list_df2_status)) {
    openxlsx::write.xlsx(p_list_df2_status[[i]], paste0(dir_status, "/", names(p_list_df2_status)[i], file_ending_status))
  }
}


##
for (i in seq_along(p_list_df2_status)) {
  openxlsx::write.xlsx(p_list_df2_status[[i]], paste0(dir_status, "/", names(p_list_df2_status)[i], file_ending_status))
}

##

else if (xstatus == "selection") {

  p_list_df <- lapply(p_list, function (x) get.lamapoll.data(email, x, apikey))
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
  return("Oh no! You entered something wrong. Call for help!")
}



####################################################################################
########## This works ##############################################################
####################################################################################

backup.lamapoll <- function(xstatus, xdir, ydir, email, apikey) {

  get.lamapoll.list <-function(email, xstatus, apikey){

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
    api_token <<- openssl::sha256(paste0(requestToken, apikey))
    # get data
    get_data_query <-paste0("https://api.lamapoll.de/api.php", "?", "task=listPolls&status=", xstatus, "&apiToken=", api_token, "&user=", email)
    fetch_data <- curl::curl_fetch_memory(get_data_query)
    data_result <- jsonlite::fromJSON(rawToChar(fetch_data$content))
    df_fb_names <- data_result$items
    return(df_fb_names)
  }

  get.lamapoll.data <-function(poll_name, email, api_token){

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

    fb_names <- as.data.frame(get.lamapoll.list(email, xstatus, apikey))
    names(fb_names)[1] <- "fb_names"
    list_status <- as.list(fb_names$fb_names)

    p_list_df_status <- lapply(list_status, function (x) get.lamapoll.data(x, email, api_token))
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
    for (i in seq_along(p_list_df2_status)) {
      openxlsx::write.xlsx(p_list_df2_status[[i]], paste0(dir_status, "/", names(p_list_df2_status)[i], file_ending_status))
    }
  }

  else if (xstatus == "selection") {

    p_list_df <- lapply(p_list, function (x) get.lamapoll.data(x, email, apitoken))
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
    for (i in seq_along(p_list_df2_status)) {
      openxlsx::write.xlsx(p_list_df2_status[[i]], paste0(dir_status, "/", names(p_list_df2_status)[i], file_ending_status))
    }
  }

  else if (xstatus == "inwork") {
    return("If a questionnaire is still being set up, there should be no data to download. Please make sure, that the questionnaire is either online or offline in Lamapoll.")
  }

  else {
    return("Oh no! You entered something wrong. Call for help!")
  }
}
