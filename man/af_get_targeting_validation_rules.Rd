\name{af_get_targeting_validation_rules}
\alias{af_get_targeting_validation_rules}
\title{
Get targeting validation rules
}
\description{
Get targeting validation rules from AppsFlyer
}
\usage{
af_get_targeting_validation_rules(
  date_from              = Sys.Date() - 8,
  date_to                = Sys.Date() - 1,
  report_type            = c("invalid_installs_report",
                             "invalid_in_app_events_report"),
  additional_fields      = c("device_model",
                             "keyword_id",
                             "store_reinstall",
                             "deeplink_url",
                             "oaid",
                             "rejected_reason",
                             "rejected_reason_value",
                             "contributor1_match_type",
                             "contributor2_match_type",
                             "contributor3_match_type",
                             "match_type",
                             "device_category",
                             "gp_referrer",
                             "gp_click_time",
                             "gp_install_begin",
                             "amazon_aid",
                             "keyword_match_type",
                             "att",
                             "conversion_type",
                             "campaign_type",
                             "is_lat"),
  timezone               = "Europe/Moscow",
  maximum_rows           = 1000000,
  app_id                 = getOption("apps_flyer_app_id"),
  api_token              = getOption("apps_flyer_api_key")
)
}
\arguments{
  \item{date_from}{
Reporting start date.
}
  \item{date_to}{
Reporting finish date.
}
  \item{report_type}{
Report type. One of: invalid_installs_report, invalid_in_app_events_report. For more details go to Integration > API access in AppsFlyer Web UI.
}
  \item{additional_fields}{
Character vector of report's additional fields.
}
  \item{timezone}{
Your timezone, for example Europe/Moscow.
}
  \item{maximum_rows}{
Report rows limit.
}
  \item{app_id}{
Your app id from apps flyer.
}
  \item{api_token}{
Your AppsFlyer API token V1.0 for more details go \href{https://support.appsflyer.com/hc/en-us/articles/360004562377}{link}
}
}
\author{
Alexey Seleznev
}
\seealso{
\href{https://support.appsflyer.com/hc/en-us/articles/207034346-Pull-APIs-Pulling-AppsFlyer-Reports-by-APIs}{AppsFlyer Pull API documentation}
}
\examples{
\dontrun{
af_set_api_token("Your API token")

inv_data <- af_get_targeting_validation_rules(
    date_from   = "2021-03-01",
    date_to     = "2021-03-15",
    report_type = "invalid_installs_report",
    app_id      = "example.app.com"
)
}
}
