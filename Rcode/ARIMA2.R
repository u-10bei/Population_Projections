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

# ＡＲＩＭＡモデルの推定
pop_train2 |>
  model(
    arimaB = ARIMA( Birth,
                   ic = "aic",
                   stepwise = FALSE )) ->
pop_arimaB

pop_train2 |>
  model(
    arimaD = ARIMA( Death,
                    ic = "aic",
                    stepwise = FALSE )) ->
pop_arimaD

# ＡＲＩＭＡによる予測
pop_arimaB |>
  forecast( h = "6 years") ->
pop_arimaB_f

pop_arimaD |>
  forecast( h = "6 years") ->
pop_arimaD_f

# 出生数、死亡数の合算
# ライブラリの読み込み
library( dplyr )

pop_test2 |>
  rename( "forecast_BD" = Total ) ->
pop_arima_f2

pop_arimaB_f |>
  as.data.frame() |>
  select( .mean ) ->
pop_arima_f2[,3]

pop_arimaD_f |>
  as.data.frame() |>
  select( .mean ) ->
pop_arima_f2[,4]

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
  autoplot() +
  autolayer( pop_testB )

pop_test2 |>
  select( Year, Death ) ->
pop_testD

pop_arimaD_f |>
  autoplot() +
  autolayer( pop_testD )
