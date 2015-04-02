#!/bin/bash

for name in pna epo wp ea nao soi nina3 tna tsa whwp oni mei nina4 nina34 pdo np ao amon.us qbo; do
  wget -nc "http://www.esrl.noaa.gov/psd/data/correlation/$name.data"
done

wget -nc http://www.esrl.noaa.gov/psd/data/timeseries/monthly/AMM/ammsst.data
