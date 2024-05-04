import asyncio

from fastapi import FastAPI, status

from structural_scraper_common import webdriver, StartWorker

from .worker import worker


app = FastAPI()


@app.post("/start", status_code=status.HTTP_200_OK)
async def start_work(schema: StartWorker):
    asyncio.get_running_loop().create_task(worker(schema.url, webdriver))
