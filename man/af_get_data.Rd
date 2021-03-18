\name{af_get_data}
\alias{af_get_data}
\title{
Get data from AppsFlyer Master API
}
\description{
Get statistic data from AppsFlyer Master API
}
\usage{
af_get_data(
  date_from  = Sys.Date() - 8,
  date_to    = Sys.Date() - 1,
  dimensions = c("app_id",
                 "pid",
                 "af_channel",
                 "c",
                 "af_c_id",
                 "geo"),
  metrics    = c("impressions",
                 "clicks",
                 "installs",
                 "sessions",
                 "loyal_users",
                 "cost",
                 "revenue",
                 "uninstalls"),
  filters    = NULL,
  currency   = NULL,
  timezone  = "Europe/Moscow",
  app_id     = getOption("apps_flyer_app_id"),
  api_token  = getOption("apps_flyer_api_key")
)
}
\arguments{
  \item{date_from}{
Reporting start date.
}
  \item{date_to}{
Reporting finish date.
}
  \item{dimensions}{
Character vector. These dimensions are used for collecting the data into groups to allow easier and more accurate examination of the information.. For more details see \href{https://support.appsflyer.com/hc/en-us/articles/213223166-Master-API-user-acquisition-metrics-via-API#groupings}{docs}.
}
  \item{metrics}{
KPIs are the metrics used for gaining an insight into the behavior of your app. For more details see \href{https://support.appsflyer.com/hc/en-us/articles/213223166-Master-API-user-acquisition-metrics-via-API#kpis}{docs}.
}
  \item{filters}{
Data filter expression, for example \code{filters=="geo=US,DE"}.
}
  \item{currency}{
Monetary values are in the app-specific currency. Example: \code{currency="preferred"}
}
  \item{timezone}{
Time zone used is according to the app-specific timezone. Example: \code{timezone="Europe/Moscow"}
}
  \item{app_id}{
Your app id from apps flyer.
}
  \item{api_token}{
Your AppsFlyer API token V1.0 for more details go \href{https://support.appsflyer.com/hc/en-us/articles/360004562377}{link}.
}
}
\author{
Alexey Seleznev
}
\seealso{
\href{https://support.appsflyer.com/hc/en-us/articles/213223166-Master-API-user-acquisition-metrics-via-API}{Master API documentation}
\href{https://support.appsflyer.com/hc/ru/articles/208387843}{Field dictionary}
}
\examples{
\dontrun{
af_set_api_token("Your API token")

ad_data <- af_get_data(
    date_from = "2021-03-01",
    date_to   = "2021-03-15",
    app_id    = "id0001111"
)
}
}
