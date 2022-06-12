#ライブラリインストール(必要に応じて)
#install.packages( "readr" )
#install.packages( "xts" )
#install.packages( "forecast" )
#install.packages( "tseries" )

# ライブラリの読み込み
library( readr )
library( xts )
library( forecast )
library( tseries )

# 該当ＵＲＬを変数に格納
c("https://raw.githubusercontent.com/u-10bei/Population_Projections/main/population_jp.csv") -> popURL

# ネット上のファイル読み込み
popURL |> read_csv( show_col_types = FALSE ) -> pop_csv

# ＸＴＳライブラリに変換
pop_csv |>
  read.zoo() |>
  as.xts() -> pop_xts

# 総人口のグラフとコレログラム
pop_xts$Total_p |> 
  ggtsdisplay( main="日本の人口" )

# 予測データと訓練データ
pop_xts |> tail( ,5 ) -> pop_test
pop_xts |> window( end = as.Date( "2015-10-01" )) -> pop_train

# ＡＲＩＭＡモデルの推定
auto.arima( y = pop_train$Total_p,
            ic = "aic",
            max.order = 7,
            stepwise = F,
            approximation = F,
            parallel = T,
            num.cores = 2
            ) -> pop_arima

# 残差のチェック
checkresiduals( pop_arima )

# 残差の正規性の検定
jarque.bera.test( resid( pop_arima ))
# 正規分布と有意に異なっているので失敗。

# ＡＲＩＭＡによる予測
forecast( pop_arima,
          xreg = pop_test$Total_p ,
          h = 5,
          level = c(95,70)) -> pop_arima_f

# 結果の描画
autoplot( pop_arima_f,
          predict.colour = 1,
          main = "forecast_arima1")
