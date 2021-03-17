\name{af_get_raw_data}
\alias{af_get_raw_data}
\title{
Get AppsFlyer raw data reports
}
\description{
Get AppsFlyer organic and non-organic raw data reports
}
\usage{
af_get_raw_data(
    date_from = Sys.Date() - 8,
    date_to = Sys.Date() - 1,
    report_type = c("installs_report",
                    "in_app_events_report",
                    "uninstall_events_report"),
    media_source           = NULL,
    category               = NULL,
    event_name             = NULL,
    currency               = NULL,
    is_organic = FALSE,
    additional_fields = c("device_model",
                          "keyword_id",
                          "store_reinstall",
                          "deeplink_url",
                          "oaid",
                          "install_app_store",
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
    timezone = "Europe/Moscow",
    retargeting = NULL,
    maximum_rows = 1000000,
    app_id = getOption("apps_flyer_app_id"),
    api_token = getOption("apps_flyer_api_key"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date_from}{
Reporting start date.
}
  \item{date_to}{
Reporting finish date.
}
  \item{report_type}{
Report type. One of: installs_report, in_app_events_report, uninstall_events_report. For more details go to Integration > API access in AppsFlyer Web UI.
}
  \item{media_source}{
Use to limit (filter) to a specific media source.for example, if you need inly facebook data in your report use media_source="facebook".
}
  \item{category}{
Traffic source category filter, for examplr category="standard".
}
  \item{event_name}{
Filter in-app events by specified events. Select multiple events by using a character vector. For example: event_name = c( "af_purchase", "ftd" ).
}
  \item{currency}{
Currency of the revenue and cost.
}
  \item{is_organic}{
If TRUE you get only organic data.
}
  \item{additional_fields}{
Character vector of report's additional fields.
}
  \item{timezone}{
Your timezone, for example Europe/Moscow.
}
  \item{retargeting}{
If TRUE you get retargeting data.
}
  \item{maximum_rows}{
Report rows limit.
}
  \item{app_id}{
Your app id from apps flyer.
}
  \item{api_token}{
Your AppsFlyer API token.
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

raw_data <- af_get_raw_data(
    date_from   = "2021-03-01",
    date_to     = "2021-03-15",
    report_type = "installs_report",
    app_id      = "id0001111"
)
}
}
