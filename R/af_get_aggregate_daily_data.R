af_get_aggregate_data <- function(
  date_from              = Sys.Date() - 8,
  date_to                = Sys.Date() - 1,
  report_type            = c("daily_report", "partners_report", "partners_by_date_report", "geo_report", "geo_by_date_report"),
  additional_fields      = c('keyword_id', 'store_reinstall', 'deeplink_url', 'oaid', 'install_app_store', 'contributor1_match_type', 'contributor2_match_type', 'contributor3_match_type', 'match_type'),
  media_source           = NULL,
  attribution_touch_type = NULL,
  currency               = NULL,
  timezone               = "Europe/Moscow",
  retargeting            = NULL,
  app_id                 = getOption("apps_flyer_app_id"),
  api_token              = getOption("apps_flyer_api_key")
) {

  # docs https://support.appsflyer.com/hc/ru/articles/207034346-Pull-APIs-Pulling-AppsFlyer-Reports-by-APIs
  if (length(additional_fields) > 1) {
    additional_fields <- str_c(additional_fields, collapse=",")
  }


  # check report type
  report_type <- match.arg(report_type)

  retry(
    {
      # compose query
      answer <- GET(
        str_glue('https://hq.appsflyer.com/export/{app_id}/{report_type}/v5'),
          query = list(
            api_token              = api_token,
            from                   = date_from,
            to                     = date_to,
            timezone               = timezone,
            media_source           = media_source,
            attribution_touch_type = attribution_touch_type,
            additional_fields      = additional_fields,
            currency               = currency,
            reattr                 = retargeting
          )
      )

      # check limit error
      if ( status_code(answer) != 200 ) {

        msg <- ifelse( str_detect(string = content(answer, encoding = "UTF-8"), "^Limit reached.*"), "Limit reached", content(answer, encoding = "UTF-8") )
        stop(msg)

      }

    },
    when      = "Limit reached",
    interval  = 60,
    max_tries = 5
  )

  # parse result
  data <- content(answer, "parsed", "text/csv", encoding = "UTF-8", na = c("", "NA", "N/A"))

  # replace lose values
  data <-
  data %>%
    mutate(across(where(is.numeric), replace_na, 0))

  # return result
  return(data)

}



