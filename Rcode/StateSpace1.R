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
  select( Year, Total ) |>                # 総人口のみのデータにする
  mutate( Year = {
    paste0( Year, "-10-01" ) |>           # １０月１日現在の日付型にする
      as.Date()}
  ) ->
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
prow_train = nrow( pop_df ) - prow_test

pop_df |>
  tail( n = prow_test ) ->
pop_test

pop_df |>
  head( n = prow_train ) ->
pop_train

# ライブラリの読み込み
library( KFAS )

# モデル構造の決定
SSModel( H = NA,
         pop_train$Total ~ SSMtrend( degree = 2,
                                     Q = list( NA, NA ))) |>
  fitSSM( inits = c( 1, 1, 1 )) ->        # パラメタ推定
fit_trend

# 将来予測の結果と予測区間
fit_trend$model |>
  predict( interval = "prediction",
           n.ahead = 5 ) |>
  as.data.frame() ->
forecast_trend

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

pop_test |>
  inner_join( ipss_test, by = "Year" ) |>
  cbind( forecast_trend$fit ) |>
  rename( forecast = "forecast_trend$fit" ) |>
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
  melt( id = "Year",
        measure = c( "Total",
                     "forecast",
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
