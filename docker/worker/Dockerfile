FROM python:3.12.3-alpine3.19

COPY ./src/common /app/src/common
COPY ./src/worker /app/src/worker

WORKDIR /app/src/common
RUN pip install .

WORKDIR /app/src/worker
RUN pip install .

WORKDIR /app/src/web/structural_scraper_worker
CMD ["python", "-m", "structural_scraper_worker"]
