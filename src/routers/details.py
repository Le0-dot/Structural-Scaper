from fastapi import APIRouter, Body, Depends, Request
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from state import State
from selector import parse_selector, build_selector
from cleaner import get_clean, get_value
from resources import templates, driver


router = APIRouter(default_response_class=HTMLResponse)


@router.get("/")
def details(request: Request, templates: Jinja2Templates = Depends(templates)):
    state = State(request)
    assert state.current_extractor is not None
    selectors = parse_selector(state.current_extractor.selector)
    return templates.TemplateResponse(
        name="details.html",
        context={"selectors": selectors},
        request=request,
    )


@router.post("/preview")
def preview(
    request: Request,
    body: dict[str, str | list[str]] = Body(),
):
    selector = build_selector(body)
    state = State(request)
    assert state.current_extractor is not None
    state.current_extractor.selector = selector

    extractor = state.current_extractor
    if not extractor.valid_value():
        extractor.value = extractor.guess_value()

    bs = get_clean(state.url, driver, state.delay)
    return "\n".join(
        get_value(tag, extractor.value) for tag in bs.select(selector)
    )
