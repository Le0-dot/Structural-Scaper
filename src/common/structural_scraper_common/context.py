from os import getenv
from typing import Iterator
from contextlib import contextmanager

from pymongo import MongoClient
from pymongo.database import Database
from selenium.webdriver import Remote, FirefoxOptions
from selenium.webdriver.common.options import BaseOptions
from selenium.webdriver.remote.webdriver import WebDriver


@contextmanager
def mongo(url: str | None = getenv("MONGO_URL"), db: str = "db") -> Iterator[Database]:
    with MongoClient(url) as client:
        yield client[db]


def webdriver(
    url: str | None = getenv("SELENIUM_URL"), options: BaseOptions = FirefoxOptions()
) -> WebDriver:
    if url:
        return Remote(command_executor=url, options=options)
    return Remote(options=options)
