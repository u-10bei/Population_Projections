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
      as.Date()}
  ) ->
pop_df 
    
# ライブラリの読み込み
library( ggplot2 )

# 出生数、死亡数のグラフ
pop_df |>
  ggplot( aes( x = Year,
               y = Birth )) +
  geom_line() 

pop_df |>
  ggplot( aes( x = Year,
               y = Death )) +
  geom_line()

# 自己相関のグラフ
pop_df$Birth |>
  acf()
pop_df$Death |>
  acf()

# 偏自己相関のグラフ
pop_df$Birth |>
  pacf()
pop_df$Death |>
  pacf()

# 学習データと予測データ
prow_test2 = 6
prow_train2 = nrow( pop_df ) - prow_test2

pop_df |>
  tail( n = prow_test2 ) ->
pop_test2

pop_df |>
  head( n = prow_train2 ) ->
pop_train2

# ライブラリの読み込み
library( KFAS )

# モデル構造の決定
SSModel( H = NA,
         pop_train2$Birth ~ SSMtrend( degree = 2,
                                     Q = list( NA, NA ))) |>
  fitSSM( inits = c( 1, 1, 1 )) ->        # パラメタ推定
fit_trend_B

SSModel( H = NA,
         pop_train2$Death ~ SSMtrend( degree = 2,
                                     Q = list( NA, NA ))) |>
  fitSSM( inits = c( 1, 1, 1 )) ->        # パラメタ推定
fit_trend_D

# 将来予測の結果と予測区間
fit_trend_B$model |>
  predict( interval = "prediction",
           n.ahead = 6 ) |>
  as.data.frame() ->
forecast_trend_B

fit_trend_D$model |>
  predict( interval = "prediction",
           n.ahead = 6 ) |>
  as.data.frame() ->
  forecast_trend_D

# 出生数、死亡数の合算
pop_test2 |>
  rename( "forecast_BD" = Total ) ->
pop_SS_f2

forecast_trend_B |>
  select( fit ) ->
  pop_SS_f2[,3]

forecast_trend_D |>
  select( fit ) ->
  pop_SS_f2[,4]

pop_SS_f2 |>
  mutate( forecast_BD = lag( forecast_BD + Birth - Death )) ->
pop_SS_f2
pop_SS_f2

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

pop_SS_f2[ 2:6, 1:2 ] |>
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
