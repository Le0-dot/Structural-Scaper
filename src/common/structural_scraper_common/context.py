from typing import Iterator
from contextlib import contextmanager

from pymongo import MongoClient
from pymongo.database import Database
from selenium.webdriver import Remote, FirefoxOptions
from selenium.webdriver.common.options import BaseOptions
from selenium.webdriver.remote.webdriver import WebDriver


@contextmanager
def mongo(url: str = "mongodb://127.0.0.1", db: str = "db") -> Iterator[Database]:
    with MongoClient(url) as client:
        yield client[db]


def webdriver(
    url: str = "http://127.0.0.1:4444", options: BaseOptions = FirefoxOptions()
) -> WebDriver:
    return Remote(command_executor=url, options=options)
