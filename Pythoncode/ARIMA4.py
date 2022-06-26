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
          .astype({ 'Birth': float })\
          .set_index( 'Year' )
pop_dfD = pop_csv[[ 'Year', 'Tu14', 'Du14', 'Tm', 'Dm', 'To65', 'Do65' ]]\
          .set_index( 'Year' )
pop_dfDru = pop_dfD[[ 'Du14' ]]\
          .astype({ 'Du14': float })\
          .rename(columns={ 'Du14': 'Dru' })
pop_dfDrm = pop_dfD[[ 'Dm' ]]\
          .astype({ 'Dm': float })\
          .rename(columns={ 'Dm': 'Drm' })
pop_dfDro = pop_dfD[[ 'Do65' ]]\
          .astype({ 'Do65': float })\
          .rename(columns={ 'Do65': 'Dro' })
pop_dfDru.Dru = pop_dfD.Du14 / pop_dfD.Tu14
pop_dfDrm.Drm = pop_dfD.Dm / pop_dfD.Tm
pop_dfDro.Dro = pop_dfD.Do65 / pop_dfD.To65

# ライブラリの読み込み
import matplotlib.pyplot as plt
plt.style.use( 'ggplot' )
plt.plot( pop_dfB )

fig = plt.figure( figsize = [ 16, 5 ])

ax1 = fig.add_subplot( 1, 3, 1 )   #１行３列の１番目
ax1.plot( pop_dfDru )

ax2 = fig.add_subplot( 1, 3, 2 )   #１行３列の２番目
ax2.plot( pop_dfDrm )

ax3 = fig.add_subplot( 1, 3, 3 )   #１行３列の３番目
ax3.plot( pop_dfDro )

fig.tight_layout()
plt.show()

# ライブラリの読み込み
import statsmodels.api as sm
sm.graphics.tsa.plot_acf( pop_dfB,
                          lags = 20,
                          alpha = .05,
                          title = "Birth" )
plt.show()

fig = plt.figure( figsize = [ 16, 5 ])

ax1 = fig.add_subplot( 1, 3, 1 )   #１行３列の１番目
# 自己相関のグラフ
sm.graphics.tsa.plot_acf( pop_dfDru,
                          lags = 20,
                          alpha = .05,
                          ax = ax1,
                          title = "Death under14" )

ax2 = fig.add_subplot( 1, 3, 2 )   #１行３列の２番目
sm.graphics.tsa.plot_acf( pop_dfDrm,
                          lags = 20,
                          alpha = .05,
                          ax = ax2,
                          title = "Death 15 to 64" )

ax3 = fig.add_subplot( 1, 3, 3 )   #１行３列の３番目
sm.graphics.tsa.plot_acf( pop_dfDro,
                          lags = 20,
                          alpha = .05,
                          ax = ax3,
                          title = "Death over65" )

fig.tight_layout()
plt.show()

sm.graphics.tsa.plot_pacf( pop_dfB,
                          lags = 20,
                          alpha = .05,
                          title = "Birth" )
plt.show()

fig = plt.figure( figsize = [ 16, 5 ])

ax1 = fig.add_subplot( 1, 3, 1 )   #１行３列の１番目
# 偏自己相関のグラフ
sm.graphics.tsa.plot_pacf( pop_dfDru,
                          lags = 20,
                          alpha = .05,
                          ax = ax1,
                          title = "Death under14" )

ax2 = fig.add_subplot( 1, 3, 2 )   #１行３列の２番目
sm.graphics.tsa.plot_pacf( pop_dfDrm,
                          lags = 20,
                          alpha = .05,
                          ax = ax2,
                          title = "Death 15 to 64" )

ax3 = fig.add_subplot( 1, 3, 3 )   #１行３列の３番目
sm.graphics.tsa.plot_pacf( pop_dfDro,
                          lags = 20,
                          alpha = .05,
                          ax = ax3,
                          title = "Death over65" )

fig.tight_layout()
plt.show()

# 学習データと予測データ
prow_test = 6
pop_test2 = pop_df.tail( prow_test )
pop_testD = pop_dfD.tail( prow_test )
pop_testB = pop_dfB.tail( prow_test )
pop_trainB = pop_dfB.head( prow_test * -1 )
pop_testDru = pop_dfDru.tail( prow_test )
pop_trainDru = pop_dfDru.head( prow_test * -1 )
pop_testDrm = pop_dfDrm.tail( prow_test )
pop_trainDrm = pop_dfDrm.head( prow_test * -1 )
pop_testDro = pop_dfDro.tail( prow_test )
pop_trainDro = pop_dfDro.head( prow_test * -1 )

# ライブラリの読み込み
import pmdarima as pm

# ＡＲＩＭＡモデルの推定
pop_arimaB = pm.auto_arima( pop_trainB,
                           information_criterion = 'aic' )
pop_arimaDru = pm.auto_arima( pop_trainDru,
                           information_criterion = 'aic' )
pop_arimaDrm = pm.auto_arima( pop_trainDrm,
                           information_criterion = 'aic' )
pop_arimaDro = pm.auto_arima( pop_trainDro,
                           information_criterion = 'aic' )
pop_arimaB,pop_arimaDru,pop_arimaDrm,pop_arimaDro

# モデリング
from statsmodels.tsa.arima.model import ARIMA
pop_arimaB = ARIMA( pop_trainB, order = ( 1, 1, 1 )).fit()
pop_arimaDru = ARIMA( pop_trainDru, order = ( 4, 2, 0 )).fit()
pop_arimaDrm = ARIMA( pop_trainDrm, order = ( 0, 2, 1 )).fit()
pop_arimaDro = ARIMA( pop_trainDro, order = ( 0, 1, 1 )).fit()

# ＡＲＩＭＡによる予測
pop_arima_fB = pop_arimaB.predict( '2015', '2020' )
pop_arima_fDru = pop_arimaDru.predict( '2015', '2020' )
pop_arima_fDrm = pop_arimaDrm.predict( '2015', '2020' )
pop_arima_fDro = pop_arimaDro.predict( '2015', '2020' )
pop_arima_fB,pop_arima_fDru,pop_arima_fDrm,pop_arima_fDro

pop_Total = pop_test2.Total +\
            pop_arima_fB -\
            ( pop_testD.Tu14 *\
             pop_arima_fDru ) -\
            ( pop_testD.Tm *\
             pop_arima_fDrm ) -\
            ( pop_testD.To65 *\
             pop_arima_fDro )
pop_arima_f4 = pop_Total.shift( 1 ).tail()

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
x_axis = np.arange(pop_arima_f4.shape[0])
plt.figure( figsize = [ 16, 5 ])
plt.plot( x_axis,
          plot_test2,
          label = "Total",
          color = 'black' )
plt.plot( x_axis,
          pop_arima_f4,
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