# rappsflyer - R Пакет для работы с API AppsFlyer <a href='https:/selesnow/github.io/rappsflyer'><img src='https://raw.githubusercontent.com/selesnow/rappsflyer/master/inst/rappsflyer.png' align="right" height="139" /></a>

На данный момент пакет поддерживает загрузку агрегированных и сырых данных из Pull API AppsFlyer.

# Установка пакета

`rappsflyer` можно установить как с GitHub, так и с CRAN:

CRAN: install.packages('rappsflyer')
GitHub: devtools::install_github('selesnow/rappsflyer')

# Функции пакета

* `af_set_api_token()` - Установить API токен
* `af_get_aggregate_data()` - Получить агрегированные данные
* `af_get_raw_data()` - Получить сырые данные
* `af_get_ad_revenue_raw_data()` - Получить отчёты о доходе с рекламы
* `af_get_targeting_validation_rules()` - Получить отчёт о ошибочных установках и событиях
*  `af_get_data()` - Получить данные из Master API (функция на стадии тестирования)

# Подробная справка

Более подробную справку о работе с пакетом можно получить из [виньетки](https://cran.r-project.org/web/packages/rappsflyer/vignettes/rappsflyer-intro.html): `vignette('rappsflyer-intro', rappsflyer)`

### Автор пакета
Алексей Селезнёв, Head of analytics dept. at [Netpeak](https://netpeak.net)
<Br>telegram channel: [R4marketing](https://t.me/R4marketing)
<Br>email: selesnow@gmail.com
<Br>skype: selesnow
<Br>facebook: [facebook.com/selesnow](https://facebook.com/selesnow)
<Br>linkedin: [linkedin.com/in/selesnow](https://linkedin.com/in/selesnow)
<Br>blog: [alexeyseleznev.wordpress.com](https://alexeyseleznev.wordpress.com/)
