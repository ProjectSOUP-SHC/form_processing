FROM virtualstaticvoid/heroku-docker-r:shiny
ENV PORT=8080
RUN apt install libsodium-dev
CMD ["/usr/bin/R", "--no-save", "--gui-none", "-f", "/app/run.R"]
