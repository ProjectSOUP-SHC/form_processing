FROM virtualstaticvoid/heroku-docker-r:shiny
ENV PORT=8080
RUN echo $GOOGLESHEETS_SERVICEACCOUNT_TOKEN|base64 -d > /app/service-account.json
RUN cat  /app/service-account.json
CMD ["/usr/bin/R", "--no-save", "--gui-none", "-f", "/app/run.R"]
