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

repo |>
  paste0( popURL ) |>                     # 読み込むアドレスの編集
  read_csv( show_col_types = FALSE ) |>   # ネット上のファイル読み込み
  as_tsibble( index = Year ) ->           # ＴＳＩＢＢＬＥライブラリに変換
pop_tsibble

# ライブラリの読み込み
library( ggplot2 )

# 出生数、死亡数のグラフ
pop_tsibble |>
  autoplot( Birth )
pop_tsibble |>
  autoplot( Death )

# ライブラリの読み込み
library( feasts )

# 自己相関のグラフ
pop_tsibble |>
  ACF( Birth ) |>
  autoplot()
pop_tsibble |>
  ACF( Death ) |>
  autoplot()

# 偏自己相関のグラフ
pop_tsibble |>
  PACF( Birth ) |>
  autoplot()
pop_tsibble |>
  PACF( Death ) |>
  autoplot()

pop_tsibble |>
  model(STL( Birth ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble |>
  model(STL( Death ~ season( window = Inf ))) |>
  components() |>
  autoplot()

# 学習データと予測データ
prow_test2 = 6
prow_train2 = nrow( pop_tsibble ) - prow_test2

pop_tsibble |>
  tail( n = prow_test2 ) ->
pop_test2

pop_tsibble |>
  head( n = prow_train2 ) ->
pop_train2

# ライブラリの読み込み
library( dplyr )

# ＡＩＣが自動推定＋２までのモデルを候補として列挙
capture.output({
  pop_train2 |>
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

pop_train2 |>
  model(
    arimaB012 = ARIMA( Birth ~ 0 + pdq( 0, 1, 2 )),    
    arimaB110 = ARIMA( Birth ~ 0 + pdq( 1, 1, 0 )),
    arimaB310 = ARIMA( Birth ~ 0 + pdq( 3, 1, 0 )),
    arimaB410 = ARIMA( Birth ~ 0 + pdq( 4, 1, 0 )),
    arimaB510 = ARIMA( Birth ~ 0 + pdq( 5, 1, 0 )),
    arimaB211 = ARIMA( Birth ~ 0 + pdq( 2, 1, 1 )), 
    arimaB311 = ARIMA( Birth ~ 0 + pdq( 3, 1, 1 )),
    arimaB411 = ARIMA( Birth ~ 0 + pdq( 4, 1, 1 ))) ->
pop_arimaB

# ＡＲＩＭＡによる予測
pop_arimaB |>
  forecast( h = "6 years") ->
pop_arimaB_f
pop_arimaB_f |>
  filter( Year == 2020 ) 
pop_test2 |>
  select( Year, Birth ) |>
  tail( 1 )

# ＡＩＣが自動推定＋２までのモデルを候補として列挙
capture.output({
  pop_train2 |>
    model(
      arima = ARIMA( Death,
                     ic = "aic",
                     trace = TRUE,
                     stepwise = FALSE ))
}) |>
  read_table( col_names = c( "symbol", "AIC" ),
              col_types = "cn",
              comment = "<" ) |>
  na.omit() ->
trace_arimaD

max_AICD = min( trace_arimaD$AIC ) + 2
trace_arimaD |>
  filter( AIC < max_AICD ) 

pop_train2 |>
  model(
    arima = ARIMA( Death,
                   ic = "aic",
                   trace = TRUE,
                   stepwise = FALSE )) ->
pop_arimaD_t

pop_train2 |>
  model(
    arimaD221 = ARIMA( Death ~ 0 + pdq( 2, 2, 1 )),    
    arimaD321 = ARIMA( Death ~ 0 + pdq( 3, 2, 1 )),
    arimaD022 = ARIMA( Death ~ 0 + pdq( 0, 2, 2 )),
    arimaD122 = ARIMA( Death ~ 0 + pdq( 1, 2, 2 )),
    arimaD222 = ARIMA( Death ~ 0 + pdq( 2, 2, 2 )),
    arimaD023 = ARIMA( Death ~ 0 + pdq( 0, 2, 3 )),
    arimaD123 = ARIMA( Death ~ 0 + pdq( 1, 2, 3 )),
    arimaD024 = ARIMA( Death ~ 0 + pdq( 0, 2, 4 ))) ->
pop_arimaD

# ＡＲＩＭＡによる予測
pop_arimaD |>
  forecast( h = "6 years") ->
pop_arimaD_f
pop_arimaD_f |>
  filter( Year == 2020 ) 
pop_test2 |>
  select( Year, Death ) |>
  tail( 1 )

# 出生数、死亡数の合算
pop_test2 |>
  rename( "forecast_BD" = Total ) ->
pop_arima_f2

pop_arimaB_f |>
  as.data.frame() |>
  filter( .model == "arimaB510" ) |>
  select( .mean ) ->
pop_arima_f2[, 3 ]

pop_arimaD_f |>
  as.data.frame() |>
  filter( .model == "arimaD221" ) |>
  select( .mean ) ->
pop_arima_f2[, 4 ]

pop_arima_f2 |>
  mutate( forecast_BD = lag( forecast_BD + Birth - Death )) ->
pop_arima_f2

# 社人研予測との比較
# 該当ＵＲＬを変数に格納
ipssURL = 
  c( "main/data/forecast_ipss.csv" )

repo |>
  paste0( ipssURL ) |>                    # 読み込むアドレスの編集
  read_csv( show_col_types = FALSE ) |>   # ネット上のファイル読み込み
  as_tsibble( index = Year ) ->           # ＴＳＩＢＢＬＥライブラリに変換
  ipss_test

pop_arima_f2[ 2:6, 1:2 ] |>
  inner_join( pop_test2, by = "Year") |>
  inner_join( ipss_test, by = "Year") |>
  select( Year,
          Total,
          forecast_BD,
          DMBM,
          DMBH,
          DLBM,
          DLBH ) ->
join_test2
join_test2

# ライブラリの読み込み
library( reshape2 )

# 描画
join_test2 |> 
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

pop_test2 |>
  select( Year, Birth ) ->
pop_testB

pop_arimaB_f |>
  filter( .model == "arimaB510" ) |>
  autoplot() +
  autolayer( pop_testB )

pop_test2 |>
  select( Year, Death ) ->
pop_testD

pop_arimaD_f |>
  filter( .model == "arimaD221" ) |>
  autoplot() +
  autolayer( pop_testD )
