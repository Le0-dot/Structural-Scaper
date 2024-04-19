from urllib.parse import urlparse
from functools import partial

# import requests
from selenium.webdriver.remote.webdriver import WebDriver
from selenium.webdriver.common.by import By
from bs4 import BeautifulSoup as bs, Tag
from jinja2.environment import Template
from cachetools.func import ttl_cache


def get_url_base(url: str) -> str:
    parsed = urlparse(url)
    return f"{parsed.scheme}://{parsed.netloc}"


def normalize_link(link: Tag, base: str) -> Tag:
    href = link["href"]
    assert isinstance(href, str)

    if href.startswith("http"):
        return link
    if not href.startswith("/"):
        link["href"] = "/" + href
    link["href"] = base + href
    return link


def find_css(soup: bs, base: str) -> str:
    links = soup.find_all("link", rel="stylesheet")
    normalized = map(partial(normalize_link, base=base), links)
    return "\n".join(map(str, normalized))


def remove_js(soup: bs) -> None:
    body = soup.find("body")
    assert isinstance(body, Tag)

    for tag in body.find_all("script"):
        tag.decompose()


@ttl_cache(ttl=300)
def get_and_clean(url: str, driver: WebDriver) -> bs:
    driver.get(url)
    html = driver.find_element(By.TAG_NAME, 'html').get_attribute('outerHTML')
    assert html is not None
    soup = bs(html, "html.parser")
    remove_js(soup)
    return soup


@ttl_cache(ttl=300)
def get_and_render(url: str, template: Template, driver: WebDriver) -> str:
    soup = get_and_clean(url, driver)

    bindings = {
        "style": soup.find("head style") or "",
        "links": find_css(soup, get_url_base(url)),
        "body": soup.find("body"),
    }

    return template.render(bindings)
