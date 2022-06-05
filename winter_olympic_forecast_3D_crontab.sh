. /etc/profile
. /home/kan-dai/.bashrc
export DISPLAY=:1
export LANG='en_US.UTF-8'
R --vanilla CMD BATCH --no-save --no-restore /media/kan-dai/workspace/workcode/personal/dk_winter_olympic_3D/winter_olympic_forecast_3D_small_crontab.R
