# 該当ＵＲＬを変数に格納
repo = "https://raw.githubusercontent.com/u-10bei/Population_Projections/"
popURL = repo + "main/data/population_jp_year.csv"

# ライブラリの読み込み
import pandas as pd
from dateutil.relativedelta import relativedelta

# ネット上のファイル読み込み
pop_csv = pd.read_csv( popURL,
                      parse_dates = [ 'Year' ])
pop_csv.Year = pop_csv.Year.apply( lambda x: x + relativedelta( months = 9 ))
pop_df = pop_csv[[ 'Year', 'Total' ]]\
          .astype({ 'Total': float }).set_index( 'Year' )

# ライブラリの読み込み
import matplotlib.pyplot as plt
plt.style.use( 'ggplot' )

# グラフのサイズ変更
plt.rcParams[ 'figure.figsize' ] = [ 10, 5 ]

# 総人口のグラフ
plt.plot( pop_df )

# ライブラリの読み込み
import pmdarima as pm

# 自己相関のグラフ
pm.utils.plot_acf( pop_df,
                   lags = 20,
                   alpha = .05 )

# 偏自己相関のグラフ
pm.utils.plot_pacf( pop_df,
                    lags = 20,
                    alpha = .05 )

# 学習データと予測データ
prow_test = 5
pop_test = pop_df.tail()
pop_train = pop_df.head( prow_test * -1 )

# ＡＲＩＭＡモデルの推定
pop_arima = pm.auto_arima( pop_train,
                           information_criterion = 'aic' )
pop_arima

# モデリング
from statsmodels.tsa.arima.model import ARIMA
pop_arima = ARIMA( pop_train, order = ( 0, 2, 0 )).fit()

# ＡＲＩＭＡによる予測
pop_arima_f = pop_arima.predict( '2016', '2020' )
pop_arima_f

# 社人研予測との比較
# 該当ＵＲＬを変数に格納
ipssURL = repo + "main/data/forecast_ipss.csv"

# ネット上のファイル読み込み
ipss_csv = pd.read_csv( ipssURL,
                        parse_dates = [ 'Year' ])
ipss_csv.Year = ipss_csv.Year.apply( lambda x: x + relativedelta( months = 9 ))
ipss_test = ipss_csv.set_index( 'Year' )

# ライブラリの読み込み
import numpy as np

# 予測と実測の比較（グラフ）
x_axis = np.arange(pop_arima_f.shape[0])
plt.plot( x_axis,
          pop_test,
          label = "Total",
          color = 'black' )
plt.plot( x_axis,
          pop_arima_f,
          label="forecast",
          color='blue')
plt.plot( x_axis,
          ipss_test.DMBM,
          label="DMBM",
          color='green')
plt.plot( x_axis,
          ipss_test.DMBH,
          label="DMBH",
          color='green')
plt.plot( x_axis,
          ipss_test.DLBH,
          label="DLBH",
          color='green')
plt.plot( x_axis,
          ipss_test.DLBM,
          label="DLBM",
          color='red')
plt.legend()
plt.show()