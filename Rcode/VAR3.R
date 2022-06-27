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
  mutate( Dr = Death / Total ) -> pop_tsibble3

# ライブラリの読み込み
library( ggplot2 )

# 出生数、死亡率のグラフ
pop_tsibble3 |>
  autoplot( Birth )
pop_tsibble3 |>
  autoplot( Dr )

# ライブラリの読み込み
library( feasts )

# 自己相関のグラフ
pop_tsibble3 |>
  ACF( Birth ) |>
  autoplot()
pop_tsibble3 |>
  ACF( Dr ) |>
  autoplot()

# 偏自己相関のグラフ
pop_tsibble3 |>
  PACF( Birth ) |>
  autoplot()
pop_tsibble3 |>
  PACF( Dr ) |>
  autoplot()

pop_tsibble3 |>
  model(STL( Birth ~ season( window = Inf ))) |>
  components() |>
  autoplot()
pop_tsibble3 |>
  model(STL( Dr ~ season( window = Inf ))) |>
  components() |>
  autoplot()

# 学習データと予測データ
6 -> prow_test2
pop_tsibble3 |> nrow() - prow_test2 -> prow_train3
pop_tsibble3 |> tail( n = prow_test2 ) -> pop_test3
pop_tsibble3 |> head( n = prow_train3 ) -> pop_train3

# ＶＡＲモデルの推定
pop_train3 |>
  model( var = VAR( Birth,
                    lag.max = 10,                    
                    ic = "aic",
                    stepwise = FALSE )) -> pop_varB
pop_train3 |>
  model( var = VAR( Dr,
                    lag.max = 10,                    
                    ic = "aic",
                    stepwise = FALSE )) -> pop_varDr

# ＶＡＲによる予測
pop_varB |>
  forecast( h = "6 years") -> pop_varB_f
pop_varDr |>
  forecast( h = "6 years") -> pop_varDr_f

# 出生数、死亡数の合算
pop_test3 |> rename( "forecast_BD" = Total ) -> pop_var_f3

pop_varB_f |>
  as.data.frame() |>
  select( .mean ) -> pop_var_f3[ ,3 ]
pop_varDr_f |>
  as.data.frame() |>
  select( "Dr" = .mean ) -> pop_var_f3[ ,11 ]
pop_var_f3 |>
  mutate( Death = forecast_BD * Dr,
          forecast_BD = lag( forecast_BD + Birth - Death )) -> pop_var_f3

# 社人研予測との比較
# 該当ＵＲＬを変数に格納
repo |> paste0( c( "main/data/forecast_ipss.csv" )) -> ipssURL

# ネット上のファイル読み込み
ipssURL |>
  read_csv( show_col_types = FALSE ) |>
  # ＴＳＩＢＢＬＥライブラリに変換
  as_tsibble( index = Year ) -> ipss_test

pop_var_f3[ 2:6, 1:2 ] |>
  inner_join( pop_test3, by = "Year") |>
  inner_join( ipss_test, by = "Year") |>
  select( Year,
          Total,
          forecast_BD,
          DMBM,
          DMBH,
          DLBM,
          DLBH ) -> join_test3
join_test3

# ライブラリの読み込み
library( reshape2 )

join_test3 |> 
  melt(id="Year",measure=c( "Total",
                            "forecast_BD",
                            "DMBM",
                            "DMBH",
                            "DLBM",
                            "DLBH")) -> join_plot3

#描画
ggplot( join_plot3,
        aes(x = Year,
            y = value,
            shape = variable,
            colour = variable,
            group = variable )) +
  geom_line() +
  geom_point()

pop_test3 |>
  select( Year, Birth ) -> pop_testB
pop_varB_f |>
  autoplot() +
  autolayer( pop_testB )
pop_test3 |>
  select( Year, Dr ) -> pop_testDr
pop_varDr_f |>
  autoplot() +
  autolayer( pop_testDr )