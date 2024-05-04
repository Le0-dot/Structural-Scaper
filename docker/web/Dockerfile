FROM python:3.12.3-alpine3.19

COPY ./src/common /app/src/common
COPY ./src/web /app/src/web

WORKDIR /app/src/web/structural_scraper_web/static/js
RUN wget https://unpkg.com/htmx.org@1.9.12/dist/htmx.min.js
RUN wget https://unpkg.com/htmx.org@1.9.12/dist/ext/json-enc.js

WORKDIR /app/src/web
RUN wget https://github.com/tailwindlabs/tailwindcss/releases/download/v3.4.3/tailwindcss-linux-x64 -O tailwindcss
RUN chmod +x tailwindcss
RUN ./tailwindcss -i ./structural_scraper_web/static/css/input.css -o ./structural_scraper_web/static/css/tailwind.css --minify
RUN rm tailwindcss ./structural_scraper_web/static/css/input.css

WORKDIR /app/src/common
RUN pip install .

WORKDIR /app/src/web
RUN pip install .

WORKDIR /app/src/web/structural_scraper_web
CMD ["python", "-m", "structural_scraper_web"]
