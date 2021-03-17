\name{af_get_aggregate_data}
\alias{af_get_aggregate_data}
\title{
Get AppsFlyer aggregate data
}
\description{
Get AppsFlyer Aggregate (user acquisition and retargeting) data
}
\usage{
af_get_aggregate_data(
    date_from = Sys.Date() - 8,
    date_to = Sys.Date() - 1,
    report_type = c("daily_report",
                    "partners_report",
                    "partners_by_date_report",
                    "geo_report",
                    "geo_by_date_report"),
    additional_fields = c("keyword_id",
                          "store_reinstall",
                          "deeplink_url",
                          "oaid",
                          "install_app_store",
                          "contributor1_match_type",
                          "contributor2_match_type",
                          "contributor3_match_type",
                          "match_type"),
    media_source = NULL,
    attribution_touch_type = NULL,
    currency = NULL,
    timezone = "Europe/Moscow",
    retargeting = NULL,
    app_id = getOption("apps_flyer_app_id"),
    api_token = getOption("apps_flyer_api_key"))
}
\arguments{
  \item{date_from}{
Reporting start date.
}
  \item{date_to}{
Reporting finish date.
}
  \item{report_type}{
Report type. One of: daily_report, partners_report, partners_by_date_report, geo_report, geo_by_date_report. For more details go to Integration > API access in AppsFlyer Web UI.
}
  \item{additional_fields}{
Character vector of report's additional fields.
}
  \item{media_source}{
Use to limit (filter) to a specific media source.for example, if you need inly facebook data in your report use media_source="facebook".
}
  \item{attribution_touch_type}{
Set this parameter as shown in the example to get \href{https://support.appsflyer.com/hc/en-us/articles/207034346#viewthrough-attribution-vta-kpis}{view-through attribution} (VTA) KPIs. For example attribution_touch_type="impression".
}
  \item{currency}{
Currency of revenue and cost. Aggregate Pull API reports always use the app-specific currency.
}
  \item{timezone}{
Your timezone, for example Europe/Moscow.
}
  \item{retargeting}{
If TRUE you get retargeting data.
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

geo_data <- af_get_aggregate_data(
    date_from   = "2021-03-01",
    date_to     = "2021-03-15",
    report_type = "geo_by_date_report",
    app_id      = "id0001111"
)
}
}
