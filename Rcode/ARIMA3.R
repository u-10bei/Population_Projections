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
  ACF( Dr ) |>
  autoplot()

# 偏自己相関のグラフ
pop_tsibble3 |>
  PACF( Birth ) |>
  autoplot()
pop_tsibble3 |>
  PACF( Dr ) |>
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

# ＡＲＩＭＡモデルの推定
pop_train3 |>
  model(
    arimaB = ARIMA( Birth,
                    ic = "aic",
                    stepwise = FALSE )) ->
pop_arimaB

pop_train3 |>
  model(
    arimaB = ARIMA( Dr,
                    ic = "aic",
                    stepwise = FALSE )) ->
pop_arimaDr

# ＡＲＩＭＡによる予測
pop_arimaB |>
  forecast( h = "6 years") ->
pop_arimaB_f

pop_arimaDr |>
  forecast( h = "6 years") ->
pop_arimaDr_f

# 出生数、死亡数の合算
pop_test3 |>
  rename( "forecast_BD" = Total ) ->
pop_arima_f3

pop_arimaB_f |>
  as.data.frame() |>
  select( .mean ) ->
pop_arima_f3[, 3 ]

pop_arimaDr_f |>
  as.data.frame() |>
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
  autoplot() +
  autolayer( pop_testB )

pop_test3 |>
  select( Year, Dr ) ->
  pop_testDr

pop_arimaDr_f |>
  autoplot() +
  autolayer( pop_testDr )