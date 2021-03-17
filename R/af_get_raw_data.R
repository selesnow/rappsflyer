af_get_raw_data <- function(
  date_from              = Sys.Date() - 8,
  date_to                = Sys.Date() - 1,
  report_type            = c("installs_report", "in_app_events_report", "uninstall_events_report"),
  media_source           = NULL,
  category               = NULL,
  event_name             = NULL,
  currency               = NULL,
  is_organic             = FALSE,
  additional_fields      = c("device_model", "keyword_id", "store_reinstall", "deeplink_url", "oaid", "install_app_store", "contributor1_match_type", "contributor2_match_type", "contributor3_match_type", "match_type", "device_category", "gp_referrer", "gp_click_time", "gp_install_begin", "amazon_aid", "keyword_match_type", "att", "conversion_type", "campaign_type", "is_lat"),
  timezone               = "Europe/Moscow",
  retargeting            = NULL,
  maximum_rows           = 1000000,
  app_id                 = getOption("apps_flyer_app_id"),
  api_token              = getOption("apps_flyer_api_key")
) {

  # docs https://support.appsflyer.com/hc/en-us/articles/360007530258-Using-Pull-API-raw-data
  if (length(additional_fields) > 1) {
    additional_fields <- str_c(additional_fields, collapse=",")
  }

  if (length(event_name) > 1) {
    event_name <- str_c(event_name, collapse=",")
  }

  # check report type
  report_type <- match.arg(report_type)

  # check organic or not
  if ( is_organic ) {
    report_type <- str_c("organic", report_type, sep = "_")
  }


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
          additional_fields      = additional_fields,
          media_source           = media_source,
          category               = category,
          event_name             = event_name,
          maximum_rows           = format(maximum_rows, scientific = FALSE),
          reattr                 = retargeting
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

  # parse result
  data <- content(answer, "parsed", "text/csv", encoding = "UTF-8", na = c("", "NA", "N/A"))

  # replace lose values
  data <-
    data %>%
    mutate(across(where(is.numeric), replace_na, 0))

  # return result
  return(data)

}
