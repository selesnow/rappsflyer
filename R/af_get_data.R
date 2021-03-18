af_get_data <- function(
  date_from  = Sys.Date() - 8,
  date_to    = Sys.Date() - 1,
  dimensions = c("app_id", "pid", "af_channel", "c", "af_c_id", "geo"),
  metrics    = c("impressions", "clicks", "installs", "sessions", "loyal_users", "cost", "revenue", "uninstalls"),
  filters    = NULL,
  currency   = NULL,
  timezone  = "Europe/Moscow",
  app_id     = getOption("apps_flyer_app_id"),
  api_token  = getOption("apps_flyer_api_key")
) {

  # https://support.appsflyer.com/hc/en-us/articles/213223166-Master-API-user-acquisition-metrics-via-API

  # send api call
  retry(
    {
      answer <- GET(url = "https://hq.appsflyer.com/export/master_report/v4",
                    query = list(
                      api_token = api_token,
                      app_id    = app_id,
                      from      = date_from,
                      to        = date_to,
                      groupings = str_c(dimensions, collapse = ","),
                      kpis      = str_c(metrics, collapse = ","),
                      filters   = filters,
                      currency  = currency
                    )
                  )

      # check limit error
      if ( status_code(answer) != 200 ) {

        msg <- ifelse( str_detect(string = content(answer, "text", encoding = "UTF-8"), "^Limit reached.*"), "Limit reached", content(answer, "text", encoding = "UTF-8") )
        lg$error(content(answer, "text", encoding = "UTF-8"))
        stop(msg)

      }

    },
  when      = "Limit reached",
  interval  = 60,
  max_tries = 5

  )

  data <- content(answer, "parsed", "text/csv", encoding = "UTF-8")

  return(data)
}

