set style fill empty


set linetype 1 lc rgb 'red'
set linetype 2 lc rgb 'blue'

set xdata time            # x軸に時間データを指定
set timefmt "%H:%M:%S"    # 時間データのフォーマットを指定
set format x "%H:%m:%d"   # x軸の目盛りのフォーマットを指定

set xtics rotate by 45 offset -2,-1.5


plot filename using 1:2:3:4:5:($5 < $2?1:2) with candlesticks linetype -1 linecolor variable title filename

pause -1


