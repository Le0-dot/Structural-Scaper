from re import search
from typing import Any
from bs4 import Tag

from fastapi import APIRouter, Request, Body
from fastapi.responses import HTMLResponse

from ..cleaner import get_clean
from ..resources import state_context
from ..jinja_validation import validate
from ..state import State

from structural_scraper_common import webdriver, ValueType


router = APIRouter(
    default_response_class=HTMLResponse,
)


def get_value(value: ValueType, tag: Tag) -> str:
    match value:
        case ValueType.href:
            return str(tag["href"])
        case ValueType.text:
            return str(tag.text)
        case ValueType.innerHTML:
            return str(tag.innerHTML)
        case ValueType.outerHTML:
            return str(tag.outerHTML)
    raise ValueError


def value_or_list(values: list[Any]) -> Any | list[Any]:
    if len(values) == 1:
        return values[0]
    return values


def find_variables(state: State) -> dict[str, Any]:
    soup = get_clean(state.url, webdriver, state.delay)
    variables: dict = {
        extractor.name: value_or_list([get_value(extractor.value, v) for v in soup.select(extractor.selector)])
        for extractor in state.extractors.values()
        if extractor.name is not None and extractor.selector is not None
    }
    variables.update({"url": state.url, "search": search})
    return variables


@router.put("/filename")
async def put_filename(request: Request, data: dict[str, str] = Body()):
    with state_context(request) as state:
        state.template.filename = data["filename"]
        return validate(state.template.filename, find_variables(state))


@router.put("/next")
async def put_next(request: Request, data: dict[str, str] = Body()):
    with state_context(request) as state:
        state.template.next = data["next"]
        return validate(state.template.next, find_variables(state))


@router.put("/content")
async def put_content(request: Request, data: dict[str, str] = Body()):
    with state_context(request) as state:
        state.template.content = data["content"]
        return validate(state.template.content, find_variables(state))
