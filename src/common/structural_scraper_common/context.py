from typing import Iterator
from contextlib import contextmanager

from pymongo import MongoClient
from pymongo.database import Database


@contextmanager
def mongo(url: str = "mongodb://127.0.0.1", db: str = "db") -> Iterator[Database]:
    with MongoClient(url) as client:
        yield client[db]
