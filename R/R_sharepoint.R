library(devtools)
load_all(".")

library(dkjslama)
library(roxygen2)
roxygenise()


devtools::document()
library(Rd2roxygen)
rab("C:/Users/dkjslama/", build = TRUE)

rab("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Alex/R/dkjslama/", build = TRUE)

install.packages("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Alex/R/dkjslama/dkjslama_0.1.0.tar.gz", repos = NULL, type = "source")

library(dkjslama)
backup.lamapoll("selection",
                "C:/Users/Alexander Wedel//OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung/Daten/Backup/",
                "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Alex/Programmliste.xlsx",
                "wGl5v4fz")

a <- openxlsx::read.xlsx("C:/testo/Programmliste.xlsx", sheet = 1, colNames = T)

backup.lamapoll("selection",
                "C:/Users/Alexander Wedel//OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung/Daten/Backup/",
                "C:/testo/Programmliste.xlsx",
                "wGl5v4fz")

library(devtools)
devtools::install_github("dkjsevaluation/dkjslama")


library(curl)
library(httr)
system("curl --ntlm --user Alexander Wedel:s0ng0ku1!! --upload-file C:/testo/UploadRtest.txt https://dkjs.sharepoint.com/sites/abtee/ee/E&E (Sharepoint)/Forms/AllItems.aspx?viewpath=/sites/abtee/ee/E&E (Sharepoint)/Forms/AllItems.aspx&id=/sites/abtee/ee/E&E (Sharepoint)/6_Team_E&E/Alex&viewid=fef6008b-b892-46ad-8683-d2e096bf8068/UploadRtest.txt")
system("curl --ntlm --user Alexander Wedel:s0ng0ku1!! --upload-file C:/testo/UploadRtest.txt https://dkjs.sharepoint.com/sites/abtee/ee/E&E (Sharepoint)/6_Team_E&E/Alex/UploadRtest.csv")


require(httr)
require(getPass)

user <- getPass(msg="Enter User ID: ")
pw <- getPass(msg="Enter password: ")

url <- "https://dkjs.sharepoint.com/sites/abtee/ee/E&E (Sharepoint)/6_Team_E&E/Alex/rdownloadtest.csv"
r <- GET(url, authenticate(user,pw,type="ntlm"))


cmd <- paste("curl --max-time 7200 --connect-timeout 7200 --ntlm --user", "user:pw", "--upload-file C:/testo/UploadRtest.csv", "https://dkjs.sharepoint.com/sites/abtee/ee/E&E (Sharepoint)/6_Team_E&E/Alex/UploadRtest.csv", sep = " ")
system(cmd)


a <- "https://dkjs.sharepoint.com/sites/abtee/ee/E%26E%20(Sharepoint)/Forms/AllItems.aspx?viewpath=%2Fsites%2Fabtee%2Fee%2FE%26E%20%28Sharepoint%29%2FForms%2FAllItems%2Easpx&id=%2Fsites%2Fabtee%2Fee%2FE%26E%20%28Sharepoint%29%2F6%5FTeam%5FE%26E%2FAlex&viewid=fef6008b%2Db892%2D46ad%2D8683%2Dd2e096bf8068/UploadRtest.csv"

cmd <- paste("curl --max-time 7200 --connect-timeout 7200 --ntlm --user", "Alexander Wedel:s0ng0ku1!!",  "--upload-file C:/testo/UploadRtest.csv","dkjs.sharepoint.com/sites/abtee/ee/E&E (Sharepoint)/6_Team_E&E/Alex/UploadRtest.csv", sep = " ")
system(cmd)

cmd <- paste("curl --max-time 7200 --connect-timeout 7200 --ntlm --user", "alexander.wedel@dkjs.de:s0ng0ku1!!",  "--upload-file C:/testo/UploadRtest.csv","dkjs.sharepoint.com/sites/abtee/ee/E&E (Sharepoint)/6_Team_E&E/Alex/UploadRtest.csv", sep = " ")
system(cmd)

# Install
install.packages("devtools")
devtools::install_github("esbeneickhardt/sharepointr")

library(sharepointr)
# Parameters
client_id <- "insert_from_first_step"
client_secret <- "insert_from_first_step"
tenant_id <- "insert_from_fourth_step"
resource_id <- "insert_from_fourth_step"
site_domain <- "yourorganisation.sharepoint.com"
sharepoint_url <- "https://yourorganisation.sharepoint.com/sites/MyTestSite"

# Get Token
sharepoint_token <- get_sharepoint_token(client_id, client_secret, tenant_id, resource_id, site_domain)

# Get digest value
sharepoint_digest_value <- get_sharepoint_digest_value(sharepoint_token, sharepoint_url)

# List folders
sharepoint_path <- "Shared Documents/test"
get_sharepoint_folder_names(sharepoint_token, sharepoint_url, sharepoint_digest_value, sharepoint_path)

a <- read.csv("C:/testo/UploadRtest.csv")

write.csv(a, "\\\\dkjs.sharepoint.coms@SSL\\sites\\abtee\\ee\\E&E (Sharepoint)\\6_Team_E&E\\Alex\\UploadRtest.csv")

library(readxl)
b <- read_excel('\\\\dkjs.sharepoint.coms@SSL\\sites\\abtee\\ee\\E&E (Sharepoint)\\6_Team_E&E\\Alex\\Programmliste.xlsx', 'Sheet1', skip=1)

##################################################################################
##################################################################################

install.packages("AzureGraph")
install.packages("Microsoft365R")
library(AzureGraph)
library(Microsoft365R)
login <- create_graph_login()
get_graph_login()

df.test <- get_sharepoint_site(site_url="https://dkjs.sharepoint.com/sites/abtee/ee/E&E (Sharepoint)/6_Team_E&E/Alex/rdownloadtest.csv")

mydrv$upload_file("C:/Users/Alexander Wedel/Downloads/DKJS_LiGa_Ergebnisse-t0_deskriptiv_Schulaufsicht.xlsx")



######

tenant <-  Sys.getenv("tenant")
secret <- Sys.getenv("secret")
app <- Sys.getenv("app")

# authenticate with AAD
# - on first login, call create_graph_login()
# - on subsequent logins, call get_graph_login()
gr <- create_graph_login(tenant=tenant, app=app, password=secret)
df.test <- get_sharepoint_site(site_url="https://dkjs.sharepoint.com/sites/abtee/ee/E&E (Sharepoint)/6_Team_E&E/Alex/", auth_type="device_code")


######## Eigener One-Drive Ordner #################################################

## https://cran.r-project.org/web/packages/Microsoft365R/vignettes/auth.html

### Authentifizierung geht
library(Microsoft365R)
options(microsoft365r_use_cli_app_id=TRUE)
mydrv <- get_business_onedrive()
mydrv$list_files()

## Datei im Browser öffnen geht
docs <- mydrv$get_item("Dokumente")
docs$list_files()
myfile <- docs$get_item("applicationdata_2022-01-01.xls")
myfile$open()

## Datei herunterladen geht
setwd(tempdir())
mydrv$download_file("Dokumenteresultate_post_wide_2022-06-10.xlsx")

a <- openxlsx::read.xlsx(paste0(tempdir(),"/Dokumenteresultate_post_wide_2022-06-10.xlsx"))
openxlsx::write.xlsx(tempdir(), "testfile.xlsx")

## Datei umbenennen geht
item <- mydrv$get_item("Mappe1.xlsx")
item$update(name="RenameMappe1.xlsx")

## Datei hochladen geht
docs$upload("testfile.xlsx")

###### shared files geht
mysh <- mydrv$list_shared_files()
shfiles <- mydrv$list_shared_items(info = c("all"))

# mydrv$download_file("PEG-KOPE_6Monate_Alex.docx")

#### sharepoint site: meine geht
myst <- get_sharepoint_site(site_url = "https://dkjs-my.sharepoint.com/personal/alexander_wedel_dkjs_de/")
mystdr <- myst$get_drive()
mystdr$list_files()
mystdr$list_shared_files()
mysh$remoteItem[[71]]$download()


### Geleganz geht
myst2 <- get_sharepoint_site(site_url = "https://dkjs.sharepoint.com/teams/GeLeGanz-SP")
mystdr2 <- myst2$get_drive()
mystdr2$list_files()

#######################################################
#################################################################################################
### W&E 2 geht!!!!
setwd(tempdir())
myst2 <- get_sharepoint_site(site_url = "https://dkjs.sharepoint.com/teams/WE-SP")
mystdr2 <- myst2$get_drive()
mystdr2$list_files()
mystdr2$download_file("test.xlsx")
a <- openxlsx::read.xlsx(paste0(tempdir(),"/test.xlsx"))

openxlsx::write.xlsx(a, paste0(tempdir(),"/testfile.xlsx"))
b <- openxlsx::read.xlsx(paste0(tempdir(),"/testfile.xlsx"))

docs2 <- mystdr2$get_item("test")
docs2$upload("testfile.xlsx")

mystdr2$upload_file(src = "test.xlsx", dest = "test.xlsx")

unlink(tempdir(), recursive = T)
#################################################################################################
#######################################################

# geht auch
list_sharepoint_sites()
myst <- get_sharepoint_site("Intranet")
drvdoc <- myst$get_drive("Dokumente")
drvdoc$list_files()

site <- get_sharepoint_site("https://dkjs.sharepoint.com/sites/abtee/ee/E&E (Sharepoint)/6_Team_E&E/Alex/", app = "31359c7f-bd7e-475c-86db-fdb8c937548e")


#######
library(Microsoft365R)
outl <-get_business_outlook()
