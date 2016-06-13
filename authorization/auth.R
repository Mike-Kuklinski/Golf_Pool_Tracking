# Code to create authorization token for app

#setwd('Boomer Pool/test/')
library(googlesheets)

# Get key for google sheet
#extract_key_from_url('https://docs.google.com/spreadsheets/d/1Yp7DkCWlp4M21bFz6o4h7Cb0J-5BMvT2lieql3ZvYiU/edit')
key_lu <- "1Yp7DkCWlp4M21bFz6o4h7Cb0J-5BMvT2lieql3ZvYiU"

#shiny_token <- gs_auth()
#saveRDS(shiny_token, 'boomer_app_token.rds')
#gs_auth()
#gs_auth()
#auth_code <- gs_webapp_auth_url()
#redirect_uri()
#gs_we
#gs_webapp_get_token(auth_code)

# Submit token
#gs_auth(token = '~/R Scripts/Boomer Pool/authorization/boomer_app_token.rds')
gs_auth(token = 'authorization/boomer_app_token.rds')
