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
         .set_index( 'Year' )
pop_dfB = pop_csv[[ 'Year', 'Birth' ]]\
          .astype({ 'Birth': float }).set_index( 'Year' )
pop_dfDr = pop_csv[[ 'Year', 'Death' ]]\
          .astype({ 'Death': float }).set_index( 'Year' )\
          .rename(columns={ 'Death': 'Dr' })
pop_dfDr.Dr = pop_dfDr.Dr / pop_df.Total

# ライブラリの読み込み
import matplotlib.pyplot as plt
plt.style.use( 'ggplot' )

fig = plt.figure( figsize = [ 16, 5 ])

ax1 = fig.add_subplot( 1, 2, 1 )   #１行２列の１番目
ax1.plot( pop_dfB )

ax2 = fig.add_subplot( 1, 2, 2 )   #１行２列の２番目
ax2.plot( pop_dfDr )

fig.tight_layout()
plt.show()

# ライブラリの読み込み
import statsmodels.api as sm

fig = plt.figure( figsize = [ 16, 5 ])

ax1 = fig.add_subplot( 1, 2, 1 )   #１行２列の１番目
# 自己相関のグラフ
sm.graphics.tsa.plot_acf( pop_dfB,
                          lags = 20,
                          alpha = .05,
                          ax = ax1,
                          title = "Birth" )

ax2 = fig.add_subplot( 1, 2, 2 )   #１行２列の２番目
sm.graphics.tsa.plot_acf( pop_dfDr,
                          lags = 20,
                          alpha = .05,
                          ax = ax2,
                          title = "Death" )

fig.tight_layout()
plt.show()

fig = plt.figure( figsize = [ 16, 5 ])

ax1 = fig.add_subplot( 1, 2, 1 )   #１行２列の１番目
# 偏自己相関のグラフ
sm.graphics.tsa.plot_pacf( pop_dfB,
                           lags = 20,
                           ax = ax1,
                           title = "Birth" )

ax2 = fig.add_subplot( 1, 2, 2 )   #１行２列の２番目
sm.graphics.tsa.plot_pacf( pop_dfDr,
                           lags = 20,
                           ax = ax2,
                           title = "Death" )

fig.tight_layout()
plt.show()

# 学習データと予測データ
prow_test = 6
pop_test2 = pop_df.tail( prow_test )
pop_testB = pop_dfB.tail( prow_test )
pop_trainB = pop_dfB.head( prow_test * -1 )
pop_testDr = pop_dfDr.tail( prow_test )
pop_trainDr = pop_dfDr.head( prow_test * -1 )

# ライブラリの読み込み
import pmdarima as pm

# ＡＲＩＭＡモデルの推定
pop_arimaB = pm.auto_arima( pop_trainB,
                           information_criterion = 'aic' )
pop_arimaDr = pm.auto_arima( pop_trainDr,
                           information_criterion = 'aic' )
pop_arimaB,pop_arimaDr

# モデリング
from statsmodels.tsa.arima.model import ARIMA
pop_arimaB = ARIMA( pop_trainB, order = ( 1, 1, 1 )).fit()
pop_arimaDr = ARIMA( pop_trainDr, order = ( 0, 2, 1 )).fit()

# ＡＲＩＭＡによる予測
pop_arima_fB = pop_arimaB.predict( '2015', '2020' )
pop_arima_fDr = pop_arimaDr.predict( '2015', '2020' )
pop_arima_fB,pop_arima_fDr

pop_Total = pop_test2.Total +\
            pop_arima_fB -\
            ( pop_test2.Total *\
             pop_arima_fDr ) 
pop_arima_f3 = pop_Total.shift( 1 ).tail()

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

plot_test2 = pop_test2.tail()
# 予測と実測の比較（グラフ）
x_axis = np.arange(pop_arima_f3.shape[0])
plt.plot( x_axis,
          plot_test2,
          label = "Total",
          color = 'black' )
plt.plot( x_axis,
          pop_arima_f3,
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