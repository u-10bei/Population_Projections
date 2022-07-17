#ライブラリインストール(必要に応じて)
#install.packages( "tidyverse" )
#install.packages( "fable" )
#install.packages( "feasts" )
#install.packages( "urca" )
#install.packages( "reshape2" )

# 該当リポジトリを変数に格納
repo = 
  c( "https://raw.githubusercontent.com/u-10bei/Population_Projections/" )

# 人口推計に使うデータの格納場所を変数に格納
popURL = 
  c( "main/data/population_jp_year.csv" )

# ライブラリの読み込み
library( readr )

repo |>
  paste0( popURL ) |>                     # 読み込むアドレスの編集
  read_csv( show_col_types = FALSE ) ->   # ネット上のファイル読み込み
pop_df

# ライブラリの読み込み
library( ggplot2 )

# 総人口のグラフ
pop_df |>
  ggplot( aes( x = Year,
               y = Total )) +
  geom_line()

# 自己相関のグラフ
pop_df$Total |>
  acf()

# 偏自己相関のグラフ
pop_df$Total |>
  pacf() 

# 学習データと予測データ
prow_test = 5

pop_df |>
  tail( n = prow_test ) ->
pop_test

# ライブラリの読み込み
library( fable )

pop_df |>
  head( n = { nrow( pop_df ) - prow_test }) |>
  as_tsibble( index = Year ) -> 
pop_train

# ライブラリの読み込み
library( dplyr )

# ＡＩＣが自動推定＋２までのモデルを候補として列挙
capture.output({
  pop_train |>
    model(
      arima = ARIMA( Total,
                     ic = "aic",
                     trace = TRUE,
                     stepwise = FALSE ))
  }) |>
  read_table( col_names = c( "symbol", "AIC" ),
              col_types = "cn",
              comment = "<" ) |>
  na.omit() ->
trace_arima

max_AIC = min( trace_arima$AIC ) + 2
trace_arima |>
  filter( AIC < max_AIC ) 

# 予測
pop_train |>
  model(
    arima120 = ARIMA( Total ~ 0 + pdq( 1, 2, 0 )),
    arima220 = ARIMA( Total ~ 0 + pdq( 2, 2, 0 )), 
    arima021 = ARIMA( Total ~ 0 + pdq( 0, 2, 1 )),
    arima121 = ARIMA( Total ~ 0 + pdq( 1, 2, 1 )),
    arima221 = ARIMA( Total ~ 0 + pdq( 2, 2, 1 )),
    arima022 = ARIMA( Total ~ 0 + pdq( 0, 2, 2 ))) |>
  forecast( h = "5 years" ) ->
pop_arima_f
pop_arima_f |>
  filter( Year == 2020 ) 
pop_test |>
  select( Year, Total ) |>
  tail( 1 )

# 社人研予測との比較
# 該当ＵＲＬを変数に格納
ipssURL = 
  c( "main/data/forecast_ipss.csv" )

repo |>
  paste0( ipssURL ) |>                    # 読み込むアドレスの編集
  read_csv( show_col_types = FALSE ) ->   # ネット上のファイル読み込み
ipss_test

pop_arima_f |>
  as.data.frame() |>
  filter( .model == "arima221" ) |>
  select( Year, "forecast" = .mean ) |>
  inner_join( pop_test, by = "Year" ) |>
  inner_join( ipss_test, by = "Year" )|>
  select( Year,
          Total,
          forecast,
          DMBM,
          DMBH,
          DLBM,
          DLBH ) ->
join_test
join_test

# ライブラリの読み込み
library( reshape2 )

# 描画
join_test |> 
  melt(id = "Year",
       measure = c( "Total",
                    "forecast",
                    "DMBM",
                    "DMBH",
                    "DLBM",
                    "DLBH")) |>
  ggplot( aes( x = Year,
               y = value,
               shape = variable,
               colour = variable,
               group = variable )) +
  geom_line() +
  geom_point()