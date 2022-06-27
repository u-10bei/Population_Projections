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
          Dro65 = Do65 / To65 ) -> pop_tsibble4

# ライブラリの読み込み
library( ggplot2 )

# 出生数、死亡率のグラフ
pop_tsibble4 |>
  autoplot( Birth )
pop_tsibble4 |>
  autoplot( Dr )
pop_tsibble4 |>
  autoplot( Dru14 )
pop_tsibble4 |>
  autoplot( Drm )
pop_tsibble4 |>
  autoplot( Dro65 )

# ライブラリの読み込み
library( feasts )

# 自己相関のグラフ
pop_tsibble4 |>
  ACF( Birth ) |>
  autoplot()
pop_tsibble4 |>
  ACF( Dr ) |>
  autoplot()
pop_tsibble4 |>
  ACF( Dru14 ) |>
  autoplot()
pop_tsibble4 |>
  ACF( Drm ) |>
  autoplot()
pop_tsibble4 |>
  ACF( Dro65 ) |>
  autoplot()

# 偏自己相関のグラフ
pop_tsibble4 |>
  PACF( Birth ) |>
  autoplot()
pop_tsibble4 |>
  PACF( Dr ) |>
  autoplot()
pop_tsibble4 |>
  PACF( Dru14 ) |>
  autoplot()
pop_tsibble4 |>
  PACF( Drm ) |>
  autoplot()
pop_tsibble4 |>
  PACF( Dro65 ) |>
  autoplot()

pop_tsibble4 |>
  model(STL( Birth ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble4 |>
  model(STL( Dr ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble4 |>
  model(STL( Dru14 ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble4 |>
  model(STL( Drm ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble4 |>
  model(STL( Dro65 ~ season( window = Inf ))) |>
  components() |>
  autoplot()

# 学習データと予測データ
6 -> prow_test2
pop_tsibble4 |> nrow() - prow_test2 -> prow_train4
pop_tsibble4 |> tail( n = prow_test2 ) -> pop_test4
pop_tsibble4 |> head( n = prow_train4 ) -> pop_train4

# ＶＡＲモデルの推定
pop_train4 |>
  model( var = VAR( Birth,
                    lag.max = 10,                    
                    ic = "aic",
                    stepwise = FALSE )) -> pop_varB
pop_train4 |>
  model( var = VAR( Dru14,
                    lag.max = 10,                    
                    ic = "aic",
                    stepwise = FALSE )) -> pop_varDru
pop_train4 |>
  model( var = VAR( Drm,
                    lag.max = 10,                    
                    ic = "aic",
                    stepwise = FALSE )) -> pop_varDrm
pop_train4 |>
  model( var = VAR( Dro65,
                    lag.max = 10,                    
                    ic = "aic",
                    stepwise = FALSE )) -> pop_varDro

# ＶＡＲによる予測
pop_varB |>
  forecast( h = "6 years") -> pop_varB_f
pop_varDru |>
  forecast( h = "6 years") -> pop_varDru_f
pop_varDrm |>
  forecast( h = "6 years") -> pop_varDrm_f
pop_varDro |>
  forecast( h = "6 years") -> pop_varDro_f

# 出生数、死亡数の合算
pop_test4 |> rename( "forecast_BD" = Total ) -> pop_var_f4

pop_varB_f |>
  as.data.frame() |>
  select( .mean ) -> pop_var_f4[ ,3 ]
pop_varDru_f |>
  as.data.frame() |>
  select( .mean ) -> pop_var_f4[ ,12 ]
pop_varDrm_f |>
  as.data.frame() |>
  select( .mean ) -> pop_var_f4[ ,13 ]
pop_varDro_f |>
  as.data.frame() |>
  select( .mean ) -> pop_var_f4[ ,14 ]

pop_var_f4 |>
  mutate( Du14 = Tu14  * Dru14,
          Dm = Tm * Drm,
          Do65 = To65 * Dro65,
          forecast_BD = lag( forecast_BD +
                               Birth -
                               Du14 -
                               Dm - 
                               Do65 )) -> pop_var_f4

# 社人研予測との比較
# 該当ＵＲＬを変数に格納
repo |> paste0( c( "main/data/forecast_ipss.csv" )) -> ipssURL

# ネット上のファイル読み込み
ipssURL |>
  read_csv( show_col_types = FALSE ) |>
  # ＴＳＩＢＢＬＥライブラリに変換
  as_tsibble( index = Year ) -> ipss_test

pop_var_f4[ 2:6, 1:2 ] |>
  inner_join( pop_test4, by = "Year") |>
  inner_join( ipss_test, by = "Year") |>
  select( Year,
          Total,
          forecast_BD,
          DMBM,
          DMBH,
          DLBM,
          DLBH ) -> join_test4
join_test4

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

pop_test4 |>
  select( Year, Birth ) -> pop_testB
pop_varB_f |>
  autoplot() +
  autolayer( pop_testB )
pop_test4 |>
  select( Year, Dru14 ) -> pop_testDu
pop_varDru_f |> autoplot() +
  autolayer( pop_testDu )
pop_test4 |>
  select( Year, Drm ) -> pop_testDm
pop_varDrm_f |> autoplot() +
  autolayer( pop_testDm )
pop_test4 |>
  select( Year, Dro65 ) -> pop_testDo
pop_varDro_f |> autoplot() +
  autolayer( pop_testDo )
