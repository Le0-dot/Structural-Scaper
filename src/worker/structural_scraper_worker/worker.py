import os
import re
from pathlib import Path
from typing import Any, Callable, ContextManager, Iterable
from urllib.parse import urlparse
from functools import partial
from operator import is_not

from pymongo.database import Database
from selenium.webdriver.common.by import By
from selenium.webdriver.remote.webdriver import WebDriver
from selenium.webdriver.remote.webelement import WebElement
from jinja2 import Environment, BaseLoader

from structural_scraper_common import mongo, Document, Extractor, ValueType


def get_value(value: ValueType, tag: WebElement) -> str | None:
    if value == ValueType.text:
        return tag.text
    return tag.get_attribute(value)


# def map_non_none[T, U](func: Callable[[T], U | None], iterable: Iterable[T]) -> Iterable[U]:
def map_non_none(func: Callable, iterable: Iterable) -> Iterable:
    return filter(partial(is_not, None), map(func, iterable))


def extract_one(extractor: Extractor, driver: WebDriver) -> str | list[str] | None:
    match driver.find_elements(By.CSS_SELECTOR, extractor.selector):
        case []:
            return None
        case [element]:
            return get_value(extractor.value, element)
        case elements:
            return list(map_non_none(partial(get_value, extractor.value), elements))


def extract(extractors: list[Extractor], driver: WebDriver) -> dict[str, Any]:
    return {extractor.name: extract_one(extractor, driver) for extractor in extractors}


def get_model(netloc: str, db_context: Callable[[], ContextManager[Database]] = lambda: mongo()) -> Document:
    with db_context() as db:
        model = db["model"].find_one({"host": netloc})
        return Document.model_validate(model)


async def worker(url: str, driver_context: Callable[[], ContextManager[WebDriver]]):
    model = get_model(urlparse(url).netloc)

    env = Environment(loader=BaseLoader())
    filename_template = env.from_string(model.template.filename)
    content_template = env.from_string(model.template.content)
    next_template = env.from_string(model.template.next)

    save_dir = Path(os.getenv("SAVE_DIR", "."))

    with driver_context() as driver:
        while url:
            driver.get(url)
            context = extract(model.extractors, driver)
            context.update({"url": url, "search": re.search})

            filename = filename_template.render(context)
            content = content_template.render(context)
            url = next_template.render(context)

            (save_dir / filename).write_text(content)
