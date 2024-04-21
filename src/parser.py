from typing import Any, Callable
from functools import partial

from bs4 import BeautifulSoup as bs


# def dict_map[T, U](func: Callable[[T], U], dictionary: dict[Any, T]) -> dict[Any, U]:
def dict_map(func: Callable, dictionary: dict) -> dict:
    return {k: func(v) for k, v in dictionary.items()}


def select(soup: bs, selector: str) -> str:
    return '\n'.join(map(str, soup.select(selector)))
    # result = soup.select_one(selector)
    # if result is None:
    #     raise Exception("Selector is not valid for current page")
    # return result.text


def select_all(soup: bs, selectors: dict[Any, str]) -> dict[Any, str]:
    return dict_map(partial(select, soup), selectors)
