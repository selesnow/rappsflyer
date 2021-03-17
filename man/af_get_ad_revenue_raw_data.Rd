\name{af_get_ad_revenue_raw_data}
\alias{af_get_ad_revenue_raw_data}

\title{
Get ad revenue raw data
}
\description{
Get ad revenue raw data from AppsFlyer Pull API
}
\usage{
af_get_ad_revenue_raw_data(
    date_from = Sys.Date() - 8,
    date_to = Sys.Date() - 1,
    report_type = c("ad_revenue_raw"),
    is_organic = FALSE,
    additional_fields = c("device_model",
                          "keyword_id",
                          "store_reinstall",
                          "deeplink_url",
                          "oaid",
                          "ad_unit",
                          "segment",
                          "placement",
                          "monetization_network",
                          "impressions",
                          "mediation_network",
                          "is_lat"),
    timezone = "Europe/Moscow",
    retargeting = NULL,
    maximum_rows = 1000000,
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
Report type. now it's only ad_revenue_raw.
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

ad_data <- af_get_ad_revenue_raw_data(
    date_from = "2021-03-01",
    date_to   = "2021-03-15",
    app_id    = "id0001111"
)
}
}
