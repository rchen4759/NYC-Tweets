FROM rocker/r-ver

ENV TIKTOK_UA="Mozilla%252F5.0%2B%28iPhone%253B%2BCPU%2BiPhone%2BOS%2B12_2%2Blike%2BMac%2BOS%2BX%29%2BAppleWebKit%252F605.1.15%2B%28KHTML%2C%2Blike%2BGecko%29%2BVersion%252F13.0%2BMobile%252F15E148%2BSafari%252F604.1"

COPY api.R /usr/api.R
COPY run_api.R /usr/run_api.R

RUN apt-get update && \
  apt-get install -y libcurl4-openssl-dev libssl-dev libssh2-1-dev libxml2-dev zlib1g-dev gconf-service libasound2 libatk1.0-0 libc6 libcairo2 libcups2 libdbus-1-3 libexpat1 libfontconfig1 libgcc1 libgconf-2-4 libgdk-pixbuf2.0-0 libglib2.0-0 libgtk-3-0 libnspr4 libpango-1.0-0 libpangocairo-1.0-0 libstdc++6 libx11-6 libx11-xcb1 libxcb1 libxcomposite1 libxcursor1 libxdamage1 libxext6 libxfixes3 libxi6 libxrandr2 libxrender1 libxss1 libxtst6 ca-certificates fonts-liberation libappindicator1 libnss3 lsb-release xdg-utils wget && \
  R -e "install.packages(c('devtools'))" \
  R -e "devtools::install_github('benjaminguinaudeau/tiktokr')" \
  R -e "install.packages('plumber')" \
  R -e "reticulate::install_miniconda()" \
  R -e "tiktokr::tk_install()" \
  R -e "tiktokr::tk_init() ; tiktokr::get_signature('test')"

EXPOSE 6543

CMD Rscript /usr/run_api.R
