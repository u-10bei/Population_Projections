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
library( dplyr )

# ネット上のファイル読み込み
popURL |>
  read_csv( show_col_types = FALSE ) |>
  # ＴＳＩＢＢＬＥライブラリに変換
  as_tsibble( index = Year ) |>
  mutate( Dr = Death / Total,
          Dru14 = Du14 / Tu14,
          Drm = Dm / Tm,
          Dro65 = Do65 / To65 ) -> pop_tsibble

# ライブラリの読み込み
library( ggplot2 )

# 出生数、死亡率のグラフ
pop_tsibble |>
  autoplot( Birth )
pop_tsibble |>
  autoplot( Dr )
pop_tsibble |>
  autoplot( Dru14 )
pop_tsibble |>
  autoplot( Drm )
pop_tsibble |>
  autoplot( Dro65 )

# ライブラリの読み込み
library( feasts )

# 自己相関のグラフ
pop_tsibble |>
  ACF( Birth ) |>
  autoplot()
pop_tsibble |>
  ACF( Dr ) |>
  autoplot()
pop_tsibble |>
  ACF( Dru14 ) |>
  autoplot()
pop_tsibble |>
  ACF( Drm ) |>
  autoplot()
pop_tsibble |>
  ACF( Dro65 ) |>
  autoplot()

# 偏自己相関のグラフ
pop_tsibble |>
  PACF( Birth ) |>
  autoplot()
pop_tsibble |>
  PACF( Dr ) |>
  autoplot()
pop_tsibble |>
  PACF( Dru14 ) |>
  autoplot()
pop_tsibble |>
  PACF( Drm ) |>
  autoplot()
pop_tsibble |>
  PACF( Dro65 ) |>
  autoplot()

pop_tsibble |>
  model(STL( Birth ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble |>
  model(STL( Dr ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble |>
  model(STL( Dru14 ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble |>
  model(STL( Drm ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble |>
  model(STL( Dro65 ~ season( window = Inf ))) |>
  components() |>
  autoplot()

# 学習データと予測データ
6 -> prow_test2
pop_tsibble |> nrow() - prow_test2 -> prow_train2
pop_tsibble |> tail( n = prow_test2 ) -> pop_test2
pop_tsibble |> head( n = prow_train2 ) -> pop_train2

# ＡＲＩＭＡモデルの推定
pop_train2 |>
  model( arima = ARIMA( Birth,
                        ic = "aic",
                        stepwise = FALSE )) -> pop_arimaB
pop_train2 |>
  model( arima = ARIMA( Dru14,
                        ic = "aic",
                        stepwise = FALSE )) -> pop_arimaDru
pop_train2 |>
  model( arima = ARIMA( Drm,
                        ic = "aic",
                        stepwise = FALSE )) -> pop_arimaDrm
pop_train2 |>
  model( arima = ARIMA( Dro65,
                        ic = "aic",
                        stepwise = FALSE )) -> pop_arimaDro

# ＡＲＩＭＡによる予測
pop_arimaB |>
forecast( xreg = pop_test2$Birth,
          h = "6 years") -> pop_arimaB_f
pop_arimaDru |>
  forecast( xreg = pop_test2$Dru14,
            h = "6 years") -> pop_arimaDru_f
pop_arimaDrm |>
  forecast( xreg = pop_test2$Drm,
            h = "6 years") -> pop_arimaDrm_f
pop_arimaDro |>
  forecast( xreg = pop_test2$Dro65,
            h = "6 years") -> pop_arimaDro_f

# 社人研予測との比較
# 該当ＵＲＬを変数に格納
repo |> paste0( c( "main/data/forecast_ipss.csv" )) -> ipssURL

# ネット上のファイル読み込み
ipssURL |>
  read_csv( show_col_types = FALSE ) |>
  # ＴＳＩＢＢＬＥライブラリに変換
  as_tsibble( index = Year ) -> ipss_test

# 出生数、死亡数の合算
pop_test2 |> rename( "forecast_BD" = Total ) -> pop_arima_f4

pop_arimaB_f |>
  as.data.frame() |>
  select( .mean ) -> pop_arima_f4[ ,3 ]
pop_arimaDru_f |>
  as.data.frame() |>
  select( .mean ) -> pop_arima_f4[ ,12 ]
pop_arimaDrm_f |>
  as.data.frame() |>
  select( .mean ) -> pop_arima_f4[ ,13 ]
pop_arimaDro_f |>
  as.data.frame() |>
  select( .mean ) -> pop_arima_f4[ ,14 ]
pop_arima_f4 |>
  mutate( Du14 = Tu14  * Dru14,
          Dm = Tm * Drm,
          Do65 = To65 * Dro65,
          forecast_BD = lag( forecast_BD +
                               Birth -
                               Du14 -
                               Dm - 
                               Do65 )) -> pop_arima_f4

pop_arima_f4[ 2:6, 1:2 ] |>
  inner_join( pop_test, by = "Year") |>
  inner_join( ipss_test, by = "Year") |>
  select( Year,
          Total,
          forecast_BD,
          DMBM,
          DMBH,
          DLBM,
          DLBH ) -> join_test4

# ライブラリの読み込み
library( reshape2 )

join_test4 |> 
  melt(id="Year",measure=c( "Total",
                            "forecast_BD",
                            "DMBM",
                            "DMBH",
                            "DLBM",
                            "DLBH")) -> join_plot4

#描画
ggplot( join_plot4,
        aes(x = Year,
            y = value,
            shape = variable,
            colour = variable,
            group = variable )) +
  geom_line() +
  geom_point()

pop_test2 |>
  select( Year, Birth ) -> pop_testB
pop_arimaB_f |>
  autoplot() +
  autolayer( pop_testB )
pop_test2 |>
  select( Year, Dru14 ) -> pop_testDru
pop_arimaDru_f |>
  autoplot() +
  autolayer( pop_testDru )
pop_test2 |>
  select( Year, Drm ) -> pop_testDrm
pop_arimaDrm_f |>
  autoplot() +
  autolayer( pop_testDrm )
pop_test2 |>
  select( Year, Dro65 ) -> pop_testDro
pop_arimaDro_f |>
  autoplot() +
  autolayer( pop_testDro )
