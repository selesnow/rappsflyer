.onAttach <- function(lib, pkg,...){
  packageStartupMessage(rappsflyerWelcomeMessage())
}

#
#

rappsflyerWelcomeMessage <- function(){
  # library(utils)

  paste0("\n",
         "---------------------\n",
         "Welcome to rappsflyer version ", utils::packageDescription("rappsflyer")$Version, "\n",
         "\n",
         "Author:           Alexey Seleznev (Head of analytics dept at Netpeak).\n",
         "Telegram channel: https://t.me/R4marketing \n",
         "Email:            selesnow@gmail.com\n",
         "Blog:             https://alexeyseleznev.wordpress.com \n",
         "Facebook:         https://facebook.com/selesnown \n",
         "Linkedin:         https://www.linkedin.com/in/selesnow \n",
         "\n",
         "Type ?rappsflyer for the main documentation.\n",
         "The github page is: https://github.com/selesnow/rappsflyer/\n",
         "\n",
         "Suggestions and bug-reports can be submitted at: https://github.com/selesnow/rappsflyer/issues\n",
         "Or contact: <selesnow@gmail.com>\n",
         "\n",
         "\tTo suppress this message use:  ", "suppressPackageStartupMessages(library(rappsflyer))\n",
         "---------------------\n"
  )
}


.onLoad <- function(libname, pkgname) {

  # where function
  utils::globalVariables("where")

  # lgr
  assign(
    "lg",
    lgr::get_logger(name = "rappsflyer"),
    envir = parent.env(environment())
  )

  ## access token
  if ( Sys.getenv("APPSFLYER_API_TOKEN") != "" ) {

    af_api_token <- Sys.getenv("APPSFLYER_API_TOKEN")

    lg$info("Set api_token from system variable APPSFLYER_API_TOKEN")

  } else {

    af_api_token <- NULL

  }

  ## default app id
  if ( Sys.getenv("APPSFLYER_APP_ID") != "" ) {

    af_app_id <- Sys.getenv("APPSFLYER_APP_ID")

    lg$info("Set app_id from system variable APPSFLYER_APP_ID")

  } else {

    af_app_id <- NULL

  }

  op <- options()
  op.rappsflyer <- list(apps_flyer_app_id  = af_app_id,
                        apps_flyer_api_key = af_api_token)

  toset <- !(names(op.rappsflyer) %in% names(op))
  if (any(toset)) options(op.rappsflyer[toset])

  invisible()
}
