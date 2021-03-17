af_set_api_token <- function(api_token) {

  options(apps_flyer_api_key = api_token)
  lg$info("Your token set successful")

}
