#!/usr/bin/env bash

# Get the CAS Data
CAS_FILE="datasets/Crash_Analysis_System_(CAS)_data.csv"
TMS_FILE="datasets/Crash_Analysis_System_(CAS)_data.csv"
TMS_SITES_FILE="datasets/State_highway_traffic_monitoring_sites.csv"
REGION_FILE="datasets/statsnzregional-council-2019-clipped-generalised-CSV.zip"

if [ ! -f "$CAS_FILE" ]; then
    echo "
CAS File not found:
Navigate here: https://opendata-nzta.opendata.arcgis.com/datasets/8d684f1841fa4dbea6afaefc8a1ba0fc/explore
Click Download
Select CSV Download
Move the file to the datasets directory
e.g. mkdir datasets
mv ~/Downloads/Crash_Analysis_System_(CAS)_data.csv datasets/Crash_Analysis_System_(CAS)_data.csv
Run ./bin/datasets.sh again"
fi

if [ ! -f "$TMS_FILE" ]; then
    echo "
TMS File not found:
Navigate here: https://opendata-nzta.opendata.arcgis.com/datasets/NZTA::tms-daily-traffic-counts-api/about
Click Download
Select CSV Download
Move the file to the datasets directory
mv ~/Downloads/TMS_daily_traffic_counts_API.csv datasets/TMS_daily_traffic_counts_API.csv
Run ./bin/datasets.sh again"
fi

if [ ! -f "$TMS_SITES_FILE" ]; then
    echo "
TMS File not found:
Navigate here: https://opendata-nzta.opendata.arcgis.com/datasets/NZTA::state-highway-traffic-monitoring-sites/about
Click Download
Select CSV Download
Move the file to the datasets directory
mv ~/Downloads/State_highway_traffic_monitoring_sites.csv datasets/State_highway_traffic_monitoring_sites.csv
Run ./bin/datasets.sh again"
fi



if [ ! -f "$REGION_FILE" ]; then
    echo "
Zip file not found: $REGION_FILE => Please download the data first:
    - Create account at datafinder.stats.govt.nz if necessary.
    - Navigate to the following URL for regional council definition, export to CSV with map projection EPSG:2193 and click 'Accept terms and create download':
        - https://datafinder.stats.govt.nz/layer/98765-regional-council-2019-clipped-generalised/
    - Place the zip files in directory datasets in the current working tree.
    - mv ~/Downloads/statsnzregional-council-2019-clipped-generalised-CSV.zip datasets/statsnzregional-council-2019-clipped-generalised-CSV.zip
    - Run ./bin/datasets.sh again
    "
fi

unzip $REGION_FILE -d datasets








# Get the Regional Council Spatial Data
# wget "https://datafinder.stats.govt.nz/services;key=05d93eccefa44ba6a8d2e9b27ac08856/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=layer-106666&outputFormat=application/json" -O datasets/statsnz_regional_council.json
# Get the TMS Data
# wget "https://services.arcgis.com/CXBb7LAjgIIdcsPt/arcgis/rest/services/TMS_Telemetry_Sites/FeatureServer/0/query?where=startDate>='2019-01-01' AND startDate<='2022-12-31' AND OBJECTID>=1 AND OBJECTID<=32000&outFields=*&resultType=standard&outSR=4326&f=json" -O datasets/tms_counts_1.json




