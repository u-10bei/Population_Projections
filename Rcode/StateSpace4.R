# パッケージのインストール（必要に応じて）
# install.packages( "readr" )
# install.packages( "dplyr" )
# install.packages( "KFAS" )
# install.packages( "ggplot2" )
# install.packages( "reshape2" )

# 該当リポジトリを変数に格納
repo = 
  c( "https://raw.githubusercontent.com/u-10bei/Population_Projections/" )

# 人口推計に使うデータの格納場所を変数に格納
popURL = 
  c( "main/data/population_jp_year.csv" )

# ライブラリの読み込み
library( readr )
library( dplyr )

repo |>
  paste0( popURL ) |>                     # 読み込むアドレスの編集
  read_csv( show_col_types = FALSE ) |>   # ネット上のファイル読み込み
  mutate( Year = {
    paste0( Year, "-10-01" ) |>           # １０月１日現在の日付型にする
      as.Date()},
          Dr = Death / Total,             # 年代別の死亡率計算
          Dru14 = Du14 / Tu14,
          Drm = Dm / Tm,
          Dro65 = Do65 / To65 ) ->
pop_df4 
    
# ライブラリの読み込み
library( ggplot2 )

# 出生数、死亡率のグラフ
pop_df4 |>
  ggplot( aes( x = Year,
               y = Birth )) +
  geom_line() 
pop_df4 |>
  ggplot( aes( x = Year,
               y = Dr )) +
  geom_line()
pop_df4 |>
  ggplot( aes( x = Year,
               y = Dru14 )) +
  geom_line()
pop_df4 |>
  ggplot( aes( x = Year,
               y = Drm )) +
  geom_line()
pop_df4 |>
  ggplot( aes( x = Year,
               y = Dro65 )) +
  geom_line()

# 自己相関のグラフ
pop_df4$Birth |>
  acf()
pop_df4$Dr |>
  acf()
pop_df4$Dru14 |>
  acf()
pop_df4$Drm |>
  acf()
pop_df4$Dro65 |>
  acf()

# 偏自己相関のグラフ
pop_df4$Birth |>
  pacf()
pop_df4$Dr |>
  pacf()
pop_df4$Dru14 |>
  pacf()
pop_df4$Drm |>
  pacf()
pop_df4$Dro65 |>
  pacf()

# 学習データと予測データ
prow_test2 = 6
prow_train2 = nrow( pop_df4 ) - prow_test2

pop_df4 |>
  tail( n = prow_test2 ) ->
pop_test4

pop_df4 |>
  head( n = prow_train2 ) ->
pop_train4

# ライブラリの読み込み
library( KFAS )

# モデル構造の決定
SSModel( H = NA,
         pop_train4$Birth ~ SSMtrend( degree = 2,
                                     Q = list( NA, NA ))) |>
  fitSSM( inits = c( 1, 1, 1 )) ->        # パラメタ推定
fit_trend_B

SSModel( H = NA,
         pop_train4$Dru14 ~ SSMtrend( degree = 2,
                                   Q = list( NA, NA ))) |>
  fitSSM( inits = c( 1, 1, 1 )) ->        # パラメタ推定
fit_trend_Dru

SSModel( H = NA,
         pop_train4$Drm ~ SSMtrend( degree = 2,
                                      Q = list( NA, NA ))) |>
  fitSSM( inits = c( 1, 1, 1 )) ->        # パラメタ推定
  fit_trend_Drm

SSModel( H = NA,
         pop_train4$Dro65 ~ SSMtrend( degree = 2,
                                      Q = list( NA, NA ))) |>
  fitSSM( inits = c( 1, 1, 1 )) ->        # パラメタ推定
  fit_trend_Dro

# 将来予測の結果と予測区間
fit_trend_B$model |>
  predict( interval = "prediction",
           n.ahead = 6 ) |>
  as.data.frame() ->
forecast_trend_B

fit_trend_Dru$model |>
  predict( interval = "prediction",
           n.ahead = 6 ) |>
  as.data.frame() ->
forecast_trend_Dru

fit_trend_Drm$model |>
  predict( interval = "prediction",
           n.ahead = 6 ) |>
  as.data.frame() ->
  forecast_trend_Drm

fit_trend_Dro$model |>
  predict( interval = "prediction",
           n.ahead = 6 ) |>
  as.data.frame() ->
  forecast_trend_Dro

# 出生数、死亡数の合算
pop_test4 |>
  rename( "forecast_BD" = Total ) ->
pop_SS_f4

forecast_trend_B |>
  select( fit ) ->
pop_SS_f4[, 3 ]

forecast_trend_Dru |>
  select( fit ) ->
pop_SS_f4[, 12 ]

forecast_trend_Drm |>
  select( fit ) ->
pop_SS_f4[, 13 ]

forecast_trend_Dro |>
  select( fit ) ->
pop_SS_f4[, 14 ]
pop_SS_f4 |>
  mutate( Du14 = Tu14  * Dru14,
          Dm = Tm * Drm,
          Do65 = To65 * Dro65,
          forecast_BD = lag( forecast_BD +
                               Birth -
                               Du14 -
                               Dm - 
                               Do65 )) ->
pop_SS_f4
pop_SS_f4

# 社人研予測との比較
# 該当ＵＲＬを変数に格納
ipssURL = 
  c( "main/data/forecast_ipss.csv" )

repo |>
  paste0( ipssURL ) |>                    # 読み込むアドレスの編集
  read_csv( show_col_types = FALSE ) |>   # ネット上のファイル読み込み
  mutate( Year = {
    paste0( Year, "-10-01" ) |>           # １０月１日現在の日付型にする
      as.Date()}
  ) ->
ipss_test

pop_SS_f4[ , 1:2 ] |>
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
  melt( id = "Year",
        measure = c( "Total",
                     "forecast_BD",
                     "DMBM",
                     "DMBH",
                     "DLBM",
                     "DLBH" )) |>
  ggplot( aes( x = Year,
               y = value,
               shape = variable,
               colour = variable,
               group = variable )) +
  geom_line() +
  geom_point()
