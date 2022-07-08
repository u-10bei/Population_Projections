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
library( fable )
library( dplyr )

# ネット上のファイル読み込み
repo |>
  paste0( popURL ) |>                     # 読み込むアドレスの編集
  read_csv( show_col_types = FALSE ) |>   # ネット上のファイル読み込み
  as_tsibble( index = Year ) |>           # ＴＳＩＢＢＬＥライブラリに変換
  mutate( Dr = Death / Total ) ->         # 死亡率の計算
pop_tsibble3

# ライブラリの読み込み
library( ggplot2 )

# 出生数、死亡率のグラフ
pop_tsibble3 |>
  autoplot( Birth )
pop_tsibble3 |>
  autoplot( Dr )

# ライブラリの読み込み
library( feasts )

# 自己相関のグラフ
pop_tsibble3 |>
  ACF( Birth ) |>
  autoplot()
pop_tsibble3 |>
  ACF( log( Dr )) |>
  autoplot()

# 偏自己相関のグラフ
pop_tsibble3 |>
  PACF( Birth ) |>
  autoplot()
pop_tsibble3 |>
  PACF( log( Dr )) |>
  autoplot()

pop_tsibble3 |>
  model(STL( Birth ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble3 |>
  model(STL( Dr ~ season( window = Inf ))) |>
  components() |>
  autoplot()

# 学習データと予測データ
prow_test2 = 6
prow_train2 = nrow( pop_tsibble3 ) - prow_test2

pop_tsibble3 |>
  tail( n = prow_test2 ) ->
pop_test3

pop_tsibble3 |>
  head( n = prow_train2 ) ->
pop_train3

# ライブラリの読み込み
library( dplyr )

# ＡＩＣが自動推定＋２までのモデルを候補として列挙
capture.output({
  pop_train3 |>
    model(
      arima = ARIMA( Birth,
                     ic = "aic",
                     trace = TRUE,
                     stepwise = FALSE ))
}) |>
  read_table( col_names = c( "symbol", "AIC" ),
              col_types = "cn",
              comment = "<" ) |>
  na.omit() ->
trace_arimaB

max_AICB = min( trace_arimaB$AIC ) + 2
trace_arimaB |>
  filter( AIC < max_AICB ) 

pop_train3 |>
  model(
    arimaB110 = ARIMA( Birth ~ 0 + pdq( 1, 1, 0 )),
    arimaB310 = ARIMA( Birth ~ 0 + pdq( 3, 1, 0 )),
    arimaB410 = ARIMA( Birth ~ 0 + pdq( 4, 1, 0 )),
    arimaB510 = ARIMA( Birth ~ 0 + pdq( 5, 1, 0 )),
    arimaB211 = ARIMA( Birth ~ 0 + pdq( 2, 1, 1 )), 
    arimaB311 = ARIMA( Birth ~ 0 + pdq( 3, 1, 1 )),
    arimaB012 = ARIMA( Birth ~ 0 + pdq( 0, 1, 2 )),
    arimaB411 = ARIMA( Birth ~ 0 + pdq( 4, 1, 1 ))) ->
pop_arimaB

# ＡＲＩＭＡによる予測
pop_arimaB |>
  forecast( h = "6 years") ->
pop_arimaB_f
pop_arimaB_f |>
  filter( Year == 2020 ) 
pop_test3 |>
  select( Year, Birth ) |>
  tail( 1 )

# ＡＩＣが自動推定＋２までのモデルを候補として列挙
capture.output({
  pop_train3 |>
    model(
      arima = ARIMA( log( Dr ),
                     ic = "aic",
                     trace = TRUE,
                     stepwise = FALSE ))
}) |>
  read_table( col_names = c( "symbol", "AIC" ),
              col_types = "cn",
              comment = "<" ) |>
  na.omit() ->
trace_arimaDr

max_AICDr = min( trace_arimaDr$AIC ) + 2
trace_arimaDr |>
  filter( AIC < max_AICDr ) 

pop_train3 |>
  model(
    arimaDr420 = ARIMA( log( Dr ) ~ 0 + pdq( 4, 2, 0 )),
    arimaDr221 = ARIMA( log( Dr ) ~ 0 + pdq( 2, 2, 1 )),
    arimaDr321 = ARIMA( log( Dr ) ~ 0 + pdq( 3, 2, 1 )),
    arimaDr022 = ARIMA( log( Dr ) ~ 0 + pdq( 0, 2, 2 )),
    arimaDr222 = ARIMA( log( Dr ) ~ 0 + pdq( 2, 2, 2 )),    
    arimaDr023 = ARIMA( log( Dr ) ~ 0 + pdq( 0, 2, 3 )),
    arimaDr123 = ARIMA( log( Dr ) ~ 0 + pdq( 1, 2, 3 )),
    arimaDr024 = ARIMA( log( Dr ) ~ 0 + pdq( 0, 2, 4 ))) ->
pop_arimaDr

# ＡＲＩＭＡによる予測
pop_arimaDr |>
  forecast( h = "6 years") ->
pop_arimaDr_f
pop_arimaDr_f |>
  filter( Year == 2020 )
pop_test3 |>
  select( Year, Dr ) |>
  tail( 1 )

# 出生数、死亡数の合算
pop_test3 |>
  rename( "forecast_BD" = Total ) ->
pop_arima_f3

pop_arimaB_f |>
  as.data.frame() |>
  filter( .model == "arimaB510" ) |>
    select( .mean ) ->
pop_arima_f3[, 3 ]

pop_arimaDr_f |>
  as.data.frame() |>
  filter( .model == "arimaDr024" ) |>
  select( "Dr" = .mean ) ->
pop_arima_f3[, 11 ]

pop_arima_f3 |>
  mutate( Death = forecast_BD * Dr,
          forecast_BD = lag( forecast_BD + Birth - Death )) ->
pop_arima_f3

# 社人研予測との比較
# 該当ＵＲＬを変数に格納
ipssURL = 
  c( "main/data/forecast_ipss.csv" )

repo |>
  paste0( ipssURL ) |>                    # 読み込むアドレスの編集
  read_csv( show_col_types = FALSE ) |>   # ネット上のファイル読み込み
  as_tsibble( index = Year ) ->           # ＴＳＩＢＢＬＥライブラリに変換
ipss_test

pop_arima_f3[ ,1:2 ] |>
  inner_join( pop_test3, by = "Year") |>
  inner_join( ipss_test, by = "Year") |>
  select( Year,
          Total,
          forecast_BD,
          DMBM,
          DMBH,
          DLBM,
          DLBH ) ->
join_test3
join_test3

# ライブラリの読み込み
library( reshape2 )

# 描画
join_test3 |> 
  melt(id = "Year",
       measure = c( "Total",
                    "forecast_BD",
                    "DMBM",
                    "DMBH",
                    "DLBM",
                    "DLBH")) |>
  ggplot( aes(x = Year,
              y = value,
              shape = variable,
              colour = variable,
              group = variable )) +
  geom_line() +
  geom_point()

pop_test3 |>
  select( Year, Birth ) ->
  pop_testB

pop_arimaB_f |>
  filter( .model == "arimaB510" ) |>
  autoplot() +
  autolayer( pop_testB )

pop_test3 |>
  select( Year, Dr ) ->
  pop_testDr

pop_arimaDr_f |>
  filter( .model == "arimaDr024" ) |>
  autoplot() +
  autolayer( pop_testDr )
