#ライブラリインストール(必要に応じて)
#install.packages( "tidyverse" )
#install.packages( "fable" )
#install.packages( "feasts" )
#install.packages( "urca" )
#install.packages( "reshape2" )

# 該当リポジトリを変数に格納
c( "https://raw.githubusercontent.com/u-10bei/Population_Projections/" ) -> repo
# 該当ＵＲＬを変数に格納
repo |> paste0( c( "main/data/population_jp_year.csv" )) -> popURL

# ライブラリの読み込み
library( readr )
library( fable )

# ネット上のファイル読み込み
popURL |>
  read_csv( show_col_types = FALSE ) |>
  # ＴＳＩＢＢＬＥライブラリに変換
  as_tsibble( index = Year ) -> pop_tsibble

# ライブラリの読み込み
library( ggplot2 )

# 総人口のグラフ
pop_tsibble |>
  autoplot( Total )

# ライブラリの読み込み
library( feasts )

# 自己相関のグラフ
pop_tsibble |>
  ACF( Total ) |>
  autoplot()

# 偏自己相関のグラフ
pop_tsibble |>
  PACF( Total ) |>
  autoplot()

pop_tsibble |>
  model(STL( Total ~ season( window = Inf ))) |>
  components() |>
  autoplot()

# 学習データと予測データ
5 -> prow_test
pop_tsibble |> nrow() - prow_test -> prow_train
pop_tsibble |> tail( n = prow_test ) -> pop_test
pop_tsibble |> head( n = prow_train ) -> pop_train

# ＡＲＩＭＡモデルの推定
pop_train |>
  model( arima = ARIMA( Total, ic = "aic" )) -> pop_arima

# ＡＲＩＭＡによる予測
pop_arima |>
forecast( xreg = pop_test$Total,
          h = "5 years") -> pop_arima_f

# 社人研予測との比較
# 該当ＵＲＬを変数に格納
repo |> paste0( c( "main/data/forecast_ipss.csv" )) -> ipssURL
                
# ネット上のファイル読み込み
ipssURL |>
  read_csv( show_col_types = FALSE ) |>
  # ＴＳＩＢＢＬＥライブラリに変換
  as_tsibble( index = Year ) -> ipss_test

# ライブラリの読み込み
library( dplyr )

pop_arima_f |>
  as.data.frame() |>
  select( Year, "forecast" = .mean ) |> 
  inner_join( pop_test, by = "Year" ) |>
  inner_join( ipss_test, by = "Year" ) |>
  select( Year,
          Total,
          forecast,
          DMBM,
          DMBH,
          DLBM,
          DLBH ) -> join_test

# ライブラリの読み込み
library( reshape2 )

join_test |> 
  melt(id="Year",measure=c( "Total",
                            "forecast",
                            "DMBM",
                            "DMBH",
                            "DLBM",
                            "DLBH")) -> join_plot

#描画
ggplot( join_plot,
        aes(x = Year,
            y = value,
            shape = variable,
            colour = variable,
            group = variable )) +
  geom_line() +
  geom_point()

pop_arima_f |> autoplot() +
  autolayer( pop_test )