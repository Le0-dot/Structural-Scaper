# Using BeautifulSoup instead of native selenium search,
# since need to mutate DOM in memory

from typing import Any
from urllib.parse import urlparse
from functools import partial
from time import sleep

from selenium.webdriver.remote.webdriver import WebDriver
from selenium.webdriver.common.by import By
from bs4 import BeautifulSoup as bs, Tag
from cachetools.func import ttl_cache


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


@ttl_cache(ttl=300)
def get_clean(url: str, driver: WebDriver, wait_seconds: int = 3) -> bs:
    driver.get(url)
    sleep(wait_seconds)

    soup = bs(driver.page_source, "html.parser")
    remove_js(soup)

    return soup


@ttl_cache(ttl=300)
def get_context(url: str, driver: WebDriver) -> dict[str, Any]:
    soup = get_clean(url, driver)
    return {
        "style": soup.find("head style") or "",
        "links": find_css(soup, get_host(url)),
        "body": soup.find("body"),
    }
