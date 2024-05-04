import asyncio
from urllib.parse import unquote

from fastapi import FastAPI, status

from structural_scraper_common import webdriver

from .worker import worker


app = FastAPI()


@app.post("/start", status_code=status.HTTP_200_OK)
async def start_work(url: str):
    url = unquote(url)
    asyncio.get_running_loop().create_task(worker(url, webdriver))
