FROM virtualstaticvoid/heroku-docker-r:shiny
ENV PORT=8080
RUN echo $GOOGLESHEETS_SERVICEACCOUNT_TOKEN|base64 -d > /app/service-account.json
ADD 
ADD ./.profile.d /app/.profile.d
RUN cat  /app/service-account.json
RUN rm /bin/sh && ln -s /bin/bash /bin/sh
CMD ["/usr/bin/R", "--no-save", "--gui-none", "-f", "/app/run.R"]
