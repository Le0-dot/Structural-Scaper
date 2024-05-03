# Using BeautifulSoup instead of native selenium search,
# since need to mutate DOM in memory

from typing import Any, ContextManager, Callable
from urllib.parse import urlparse
from functools import partial
from time import sleep

from selenium.webdriver.remote.webdriver import WebDriver
from bs4 import BeautifulSoup as bs, Tag
from cachetools import cached, TTLCache
from cachetools.keys import hashkey


def get_host(url: str) -> str:
    parsed = urlparse(url)
    return f"{parsed.scheme}://{parsed.netloc}"


def complete_link(link: Tag, host: str) -> Tag:
    if link.has_attr("href"):
        attr = "href"
    elif link.has_attr("data-href"):
        attr = "data-href"
    else:
        return link
    href = str(link[attr])

    if href.startswith("http"):
        return link
    if not href.startswith("/"):
        link["href"] = "/" + href
    link[attr] = host + href
    return link


def find_css(soup: bs, host: str) -> str:
    links = soup.find_all("link", rel="stylesheet")
    normalized = map(partial(complete_link, host=host), links)
    return "\n".join(map(str, normalized))


def remove_js(soup: bs) -> None:
    for tag in soup.find_all("script"):
        tag.decompose()


def get_value(tag: Tag, value: str) -> str:
    match value:
        case "text":
            return tag.text
        case "href" if tag.has_attr("href"):
            return str(tag["href"])
        case "href":
            return tag.text
        case "innerHTML":
            return tag.decode_contents()
        case "outerHTML":
            return str(tag)
    raise Exception("Invalid value parameter")


@cached(
    cache=TTLCache(maxsize=8, ttl=300),
    key=lambda url, driver_context, wait_seconds: hashkey(url),
)
def get_clean(
    url: str, driver_context: Callable[[], ContextManager[WebDriver]], wait_seconds: int
) -> bs:
    with driver_context() as driver:
        driver.get(url)
        sleep(wait_seconds)

        soup = bs(driver.page_source, "html.parser")
    remove_js(soup)

    return soup


@cached(
    cache=TTLCache(maxsize=8, ttl=300),
    key=lambda url, driver_context, wait_seconds: hashkey(url),
)
def get_context(
    url: str, driver_context: Callable[[], ContextManager[WebDriver]], wait_seconds: int
) -> dict[str, Any]:
    soup = get_clean(url, driver_context, wait_seconds)
    return {
        "style": soup.find("head style") or "",
        "links": find_css(soup, get_host(url)),
        "body": soup.find("body"),
    }
