FROM mcr.microsoft.com/playwright:focal

RUN apt-get update && apt-get install -y python3-pip
COPY . .
COPY sign.py /usr/app/sign.py
RUN pip3 install TikTokApi
