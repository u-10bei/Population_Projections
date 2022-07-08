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
  select( Year,                           # 総人口と出生、死亡のデータ
          Total,
          Birth,
          Death ) |>
  mutate( Year = {
    paste0( Year, "-10-01" ) |>           # １０月１日現在の日付型にする
      as.Date()},
          Dr = Death / Total ) ->
pop_df3 
    
# ライブラリの読み込み
library( ggplot2 )

# 出生数、死亡率のグラフ
pop_df3 |>
  ggplot( aes( x = Year,
               y = Birth )) +
  geom_line() 

pop_df3 |>
  ggplot( aes( x = Year,
               y = Dr )) +
  geom_line()

# 自己相関のグラフ
pop_df3$Birth |>
  acf()
pop_df3$Dr |>
  acf()

# 偏自己相関のグラフ
pop_df3$Birth |>
  pacf()
pop_df3$Dr |>
  pacf()

# 学習データと予測データ
prow_test2 = 6
prow_train2 = nrow( pop_df3 ) - prow_test2

pop_df3 |>
  tail( n = prow_test2 ) ->
pop_test3

pop_df3 |>
  head( n = prow_train2 ) ->
pop_train3

# ライブラリの読み込み
library( KFAS )

# モデル構造の決定
SSModel( H = NA,
         pop_train3$Birth ~ SSMtrend( degree = 2,
                                     Q = list( NA, NA ))) |>
  fitSSM( inits = c( 1, 1, 1 )) ->        # パラメタ推定
fit_trend_B

SSModel( H = NA,
         pop_train3$Dr ~ SSMtrend( degree = 2,
                                   Q = list( NA, NA ))) |>
  fitSSM( inits = c( 1, 1, 1 )) ->        # パラメタ推定
fit_trend_Dr

# 将来予測の結果と予測区間
fit_trend_B$model |>
  predict( interval = "prediction",
           n.ahead = 6 ) |>
  as.data.frame() ->
forecast_trend_B

fit_trend_Dr$model |>
  predict( interval = "prediction",
           n.ahead = 6 ) |>
  as.data.frame() ->
forecast_trend_Dr

# 出生数、死亡数の合算
pop_test3 |>
  rename( "forecast_BD" = Total ) ->
pop_SS_f3

forecast_trend_B |>
  select( fit ) ->
pop_SS_f3[, 3 ]

forecast_trend_Dr |>
  select( fit ) ->
pop_SS_f3[, 5 ]

pop_SS_f3 |>
  mutate( Death = forecast_BD * Dr,
          forecast_BD = lag( forecast_BD + Birth - Death )) ->
pop_SS_f3
pop_SS_f3

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

pop_SS_f3[ 2:6, 1:2 ] |>
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
