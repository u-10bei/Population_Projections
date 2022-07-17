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
  mutate( Dr = Death / Total,             # 年代別の死亡率計算
          Dru14 = Du14 / Tu14,
          Drm = Dm / Tm,
          Dro65 = Do65 / To65 ) ->
pop_tsibble4

# ライブラリの読み込み
library( ggplot2 )

# 出生数、死亡率のグラフ
pop_tsibble4 |>
  autoplot( Birth )
pop_tsibble4 |>
  autoplot( Dr )
pop_tsibble4 |>
  autoplot( Dru14 )
pop_tsibble4 |>
  autoplot( Drm )
pop_tsibble4 |>
  autoplot( Dro65 )

# ライブラリの読み込み
library( feasts )

# 自己相関のグラフ
pop_tsibble4 |>
  ACF( Birth ) |>
  autoplot()
pop_tsibble4 |>
  ACF( Dr ) |>
  autoplot()
pop_tsibble4 |>
  ACF( Dru14 ) |>
  autoplot()
pop_tsibble4 |>
  ACF( Drm ) |>
  autoplot()
pop_tsibble4 |>
  ACF( Dro65 ) |>
  autoplot()

# 偏自己相関のグラフ
pop_tsibble4 |>
  PACF( Birth ) |>
  autoplot()
pop_tsibble4 |>
  PACF( Dr ) |>
  autoplot()
pop_tsibble4 |>
  PACF( Dru14 ) |>
  autoplot()
pop_tsibble4 |>
  PACF( Drm ) |>
  autoplot()
pop_tsibble4 |>
  PACF( Dro65 ) |>
  autoplot()

pop_tsibble4 |>
  model(STL( Birth ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble4 |>
  model(STL( Dr ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble4 |>
  model(STL( Dru14 ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble4 |>
  model(STL( Drm ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble4 |>
  model(STL( Dro65 ~ season( window = Inf ))) |>
  components() |>
  autoplot()

# 学習データと予測データ
prow_test2 = 6
prow_train2 = nrow( pop_tsibble4 ) - prow_test2

pop_tsibble4 |>
  tail( n = prow_test2 ) ->
pop_test4

pop_tsibble4 |>
  head( n = prow_train2 ) ->
pop_train4

# ＡＩＣが自動推定＋２までのモデルを候補として列挙
capture.output({
  pop_train4 |>
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

pop_train4 |>
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
pop_test4 |>
  select( Year, Birth ) |>
  tail( 1 )

# ＡＩＣが自動推定＋２までのモデルを候補として列挙
capture.output({
  pop_train4 |>
    model(
      arima = ARIMA( log( Dru14 ),
                     ic = "aic",
                     trace = TRUE,
                     stepwise = FALSE ))
}) |>
  read_table( col_names = c( "symbol", "AIC" ),
              col_types = "cn",
              comment = "<" ) |>
  na.omit() ->
trace_arimaDru

max_AICDru = min( trace_arimaDru$AIC ) + 2
trace_arimaDru |>
  filter( AIC < max_AICDru ) 

pop_train4 |>
  model(
    arimaDru220 = ARIMA( log( Dru14 ) ~ 0 + pdq( 2, 2, 0 )),
    arimaDru320 = ARIMA( log( Dru14 ) ~ 0 + pdq( 3, 2, 0 )),
    arimaDru420 = ARIMA( log( Dru14 ) ~ 0 + pdq( 4, 2, 0 )),
    arimaDru221 = ARIMA( log( Dru14 ) ~ 0 + pdq( 2, 2, 1 )),
    arimaDru223 = ARIMA( log( Dru14 ) ~ 0 + pdq( 2, 2, 3 ))) ->
pop_arimaDru

# ＡＲＩＭＡによる予測
pop_arimaDru |>
  forecast( h = "6 years") ->
pop_arimaDru_f
pop_arimaDru_f |>
  filter( Year == 2020 ) 
pop_test4 |>
  select( Year, Dru14 ) |>
  tail( 1 )

# ＡＩＣが自動推定＋２までのモデルを候補として列挙
capture.output({
  pop_train4 |>
    model(
      arima = ARIMA( log( Drm ),
                     ic = "aic",
                     trace = TRUE,
                     stepwise = FALSE ))
}) |>
  read_table( col_names = c( "symbol", "AIC" ),
              col_types = "cn",
              comment = "<" ) |>
  na.omit() ->
trace_arimaDrm

max_AICDrm = min( trace_arimaDrm$AIC ) + 2
trace_arimaDrm |>
  filter( AIC < max_AICDrm ) 

pop_train4 |>
  model(
    arimaDrm220 = ARIMA( log( Drm ) ~ 0 + pdq( 2, 2, 0 )),
    arimaDrm320 = ARIMA( log( Drm ) ~ 0 + pdq( 3, 2, 0 )),
    arimaDrm021 = ARIMA( log( Drm ) ~ 0 + pdq( 0, 2, 1 )),
    arimaDrm121 = ARIMA( log( Drm ) ~ 0 + pdq( 1, 2, 1 )),
    arimaDrm221 = ARIMA( log( Drm ) ~ 0 + pdq( 2, 2, 1 )),
    arimaDrm022 = ARIMA( log( Drm ) ~ 0 + pdq( 0, 2, 2 )),
    arimaDrm023 = ARIMA( log( Drm ) ~ 0 + pdq( 0, 2, 3 ))) ->
pop_arimaDrm

# ＡＲＩＭＡによる予測
pop_arimaDrm |>
  forecast( h = "6 years") ->
pop_arimaDrm_f
pop_arimaDrm_f |>
  filter( Year == 2020 ) 
pop_test4 |>
  select( Year, Drm ) |>
  tail( 1 )

# ＡＩＣが自動推定＋２までのモデルを候補として列挙
capture.output({
  pop_train4 |>
    model(
      arima = ARIMA( log( Dro65 ),
                     ic = "aic",
                     trace = TRUE,
                     stepwise = FALSE ))
}) |>
  read_table( col_names = c( "symbol", "AIC" ),
              col_types = "cn",
              comment = "<" ) |>
  na.omit() ->
trace_arimaDro

max_AICDro = min( trace_arimaDro$AIC ) + 2
trace_arimaDro |>
  filter( AIC < max_AICDro ) 

pop_train4 |>
  model(
    arimaDro212 = ARIMA( log( Dro65 ) ~ 0 + pdq( 2, 1, 2 )),
    arimaDro013 = ARIMA( log( Dro65 ) ~ 0 + pdq( 0, 1, 3 )),
    arimaDro113 = ARIMA( log( Dro65 ) ~ 0 + pdq( 1, 1, 3 )),
    arimaDro014 = ARIMA( log( Dro65 ) ~ 0 + pdq( 0, 1, 4 ))) ->
pop_arimaDro

# ＡＲＩＭＡによる予測
pop_arimaDro |>
  forecast( h = "6 years") ->
  pop_arimaDro_f
pop_arimaDro_f |>
  filter( Year == 2020 ) 
pop_test4 |>
  select( Year, Dro65 ) |>
  tail( 1 )

# 出生数、死亡数の合算
pop_test4 |>
  rename( "forecast_BD" = Total ) ->
pop_arima_f4

pop_arimaB_f |>
  as.data.frame() |>
  filter( .model == "arimaB510" ) |>
  select( .mean ) ->
pop_arima_f4[, 3 ]

pop_arimaDru_f |>
  as.data.frame() |>
  filter( .model == "arimaDru223" ) |>
  select( "Dru" = .mean ) ->
pop_arima_f4[, 12 ]

pop_arimaDrm_f |>
  as.data.frame() |>
  filter( .model == "arimaDrm023" ) |>
  select( "Drm" = .mean ) ->
pop_arima_f4[, 13 ]

pop_arimaDro_f |>
  as.data.frame() |>
  filter( .model == "arimaDro014" ) |>
  select( "Dro" = .mean ) ->
pop_arima_f4[, 14 ]

pop_arima_f4 |>
  mutate( Du14 = Tu14  * Dru14,
          Dm = Tm * Drm,
          Do65 = To65 * Dro65,
          forecast_BD = lag( forecast_BD +
                               Birth -
                               Du14 -
                               Dm - 
                               Do65 )) ->
pop_arima_f4

# 社人研予測との比較
# 該当ＵＲＬを変数に格納
ipssURL = 
  c( "main/data/forecast_ipss.csv" )

repo |>
  paste0( ipssURL ) |>                    # 読み込むアドレスの編集
  read_csv( show_col_types = FALSE ) |>   # ネット上のファイル読み込み
  as_tsibble( index = Year ) ->           # ＴＳＩＢＢＬＥライブラリに変換
ipss_test

pop_arima_f4[ ,1:2 ] |>
  inner_join( pop_test4, by = "Year") |>
  inner_join( ipss_test, by = "Year") |>
  select( Year,
          Total,
          forecast_BD,
          DMBM,
          DMBH,
          DLBM,
          DLBH ) ->
join_test4
join_test4

# ライブラリの読み込み
library( reshape2 )

# 描画
join_test4 |> 
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

pop_test4 |>
  select( Year, Birth ) ->
pop_testB

pop_arimaB_f |>
  filter( .model == "arimaB510" ) |>
  autoplot() +
  autolayer( pop_testB )

pop_test4 |>
  select( Year, Dru14 ) ->
pop_testDru

pop_arimaDru_f |>
  filter( .model == "arimaDru223" ) |>
  autoplot() +
  autolayer( pop_testDru )

pop_test4 |>
  select( Year, Drm ) ->
  pop_testDrm

pop_arimaDrm_f |>
  filter( .model == "arimaDrm023" ) |>
  autoplot() +
  autolayer( pop_testDrm )

pop_test4 |>
  select( Year, Dro65 ) ->
  pop_testDro

pop_arimaDro_f |>
  filter( .model == "arimaDro014" ) |>
  autoplot() +
  autolayer( pop_testDro )
