FROM virtualstaticvoid/heroku-docker-r:shiny
ENV PORT=8080
RUN echo $GOOGLESHEETS_SERVICEACCOUNT_TOKEN|base64 -d > /app/service-account.json
ADD ./.profile.d /app/.profile.d
RUN chmod 755 /app/.profile.d/heroku-exec.sh
RUN cat  /app/service-account.json
RUN rm /bin/sh && ln -s /bin/bash /bin/sh
CMD bash /app/.profile.d/heroku-exec.sh && /usr/bin/R --no-save --gui-none -f /app/run.R
