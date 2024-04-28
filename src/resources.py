from typing import Any, Callable, Iterator
from fastapi.templating import Jinja2Templates
from selenium import webdriver


def resource(factory: Callable[[], Any]):
    class Resource:
        __value = None

        @classmethod
        def get(cls) -> Iterator[Any]:
            if cls.__value is None:
                cls.__value = factory()
            yield cls.__value

    return Resource


Templates = resource(lambda: Jinja2Templates("templates", autoescape=False, auto_reload=True))
Driver = resource(lambda: webdriver.Firefox())
