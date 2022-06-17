#ライブラリインストール(必要に応じて)
#install.packages( "tidyverse" )
#install.packages( "fable" )
#install.packages( "feasts" )
#install.packages( "urca" )
#install.packages( "reshape2" )

# 該当ＵＲＬを変数に格納
c( "https://raw.githubusercontent.com/u-10bei/Population_Projections/main/population_jp_year.csv" ) -> popURL

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

# 予測データと訓練データ
5 -> prow_test
pop_tsibble |> nrow() - prow_test -> prow_train
pop_tsibble |> tail( n = prow_test ) -> pop_test
pop_tsibble |> head( n = prow_train ) -> pop_train

# ＡＲＩＭＡモデルの推定
pop_train |>
  model( arima = ARIMA( Birth, ic = "aic" )) -> pop_arimaB
pop_train |>
  model( arima = ARIMA( Death, ic = "aic" )) -> pop_arimaD

# ＡＲＩＭＡによる予測
pop_arimaB |>
forecast( xreg = pop_test$Birth,
          h = "5 years") -> pop_arimaB_f
pop_arimaD |>
  forecast( xreg = pop_test$Death,
            h = "5 years") -> pop_arimaD_f

# ライブラリの読み込み
library( dplyr )

pop_test |> rename( "forecast_BD" = Total ) -> pop_arima_f2

# 社人研予測との比較
# 該当ＵＲＬを変数に格納
c( "https://raw.githubusercontent.com/u-10bei/Population_Projections/main/forecast_ipss.csv" ) -> ipssURL

# ネット上のファイル読み込み
ipssURL |>
  read_csv( show_col_types = FALSE ) |>
  # ＴＳＩＢＢＬＥライブラリに変換
  as_tsibble( index = Year ) -> ipss_test

pop_arimaB_f |>
  as.data.frame() |>
  select( .mean ) -> pop_arima_f2[ ,3 ]
pop_arimaD_f |>
  as.data.frame() |>
  select( .mean ) -> pop_arima_f2[ ,4 ]

pop_arima_f2 |>
  mutate( Total_BD = lag( forecast_BD + Birth - Death )) -> pop_arima_f2
pop_train[ 66,2 ] + pop_train[ 66,3 ] - pop_train[ 66,4 ] -> pop_arima_f2[ 1,2 ]

pop_arima_f2[,1:2] |>
  inner_join( pop_test, by = "Year") |>
  inner_join( ipss_test, by = "Year") -> join_test2

# ライブラリの読み込み
library( reshape2 )

join_test2 |> 
  melt(id="Year",measure=c( "Total",
                            "forecast_BD",
                            "DMBM",
                            "DMBH",
                            "DLBM",
                            "DLBH")) -> join_plot2

#描画
ggplot( join_plot2,
        aes(x = Year,
            y = value,
            shape = variable,
            colour = variable,
            group = variable )) +
  geom_line() +
  geom_point()

pop_test |>
  select(Year,Birth) -> pop_testB
pop_arimaB_f |>
  autoplot() +
  autolayer( pop_testB )
pop_test |>
  select(Year,Death) -> pop_testD
pop_arimaD_f |>
  autoplot() +
  autolayer( pop_testD )
