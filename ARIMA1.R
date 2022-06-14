#ライブラリインストール(必要に応じて)
#install.packages( "readr" )
#install.packages( "fable" )
#install.packages( "feasts" )
#install.packages("urca")

# 該当ＵＲＬを変数に格納
c( "https://raw.githubusercontent.com/u-10bei/Population_Projections/main/population_jp_year.csv" ) -> popURL

# ライブラリの読み込み
library( readr )

# ネット上のファイル読み込み
popURL |> read_csv( show_col_types = FALSE ) -> pop_csv

# ライブラリの読み込み
library( fable )

# ＴＳＩＢＢＬＥライブラリに変換
pop_csv |> 
  as_tsibble( index = Year ) -> pop_tsibble

# ライブラリの読み込み
library( feasts )
library( ggplot2 )

# 総人口のグラフ
pop_tsibble |>
  autoplot( Total )

# 自己相関のグラフ
pop_tsibble |>
  ACF( Total ) |>
  autoplot()

# 偏自己相関のグラフ
pop_tsibble |>
  PACF( Total ) |>
  autoplot()

# ライブラリの読み込み
library( tidyr )

tsibble::fill_gaps( pop_tsibble ) |>
  fill( Total , .direction = "down" ) |>
  fill( Birth , .direction = "down" ) |>
  fill( Death , .direction = "down" ) -> pop_filled

pop_filled |>
  model(STL( Total ~ season( window = Inf ))) |>
  components() |>
  autoplot()

# 予測データと訓練データ
5 -> prow_test
pop_tsibble |> nrow() - prow_test -> prow_train
pop_tsibble |> tail( n = prow_test ) -> pop_test
pop_tsibble |> head( n = prow_train ) -> pop_train

#説明変数
pop_train[ , c( "Birth", "Death" )] -> pt_BD

# ＡＲＩＭＡモデルの推定
pop_train |>
  model(arima = ARIMA( Total,ic = "aic" )) -> pop_arima

# ＡＲＩＭＡによる予測
forecast( pop_arima,
          xreg = pop_test$Total,
          h = "5 years",
          level = c( 95, 70 )) -> pop_arima_f

#描画
autoplot( pop_arima_f ) + autolayer( pop_test )
