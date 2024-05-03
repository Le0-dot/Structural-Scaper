from fastapi import APIRouter, Body, Depends, HTTPException, Request, status
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from selector import parse_selector, build_selector
from cleaner import get_clean, get_value
from resources import state_context, templates

from structural_scraper_common import driver, ValueType


router = APIRouter(default_response_class=HTMLResponse)


@router.get("/")
async def get_details(
    request: Request, templates: Jinja2Templates = Depends(templates)
):
    with state_context(request) as state:
        if state.current_extractor is None:
            raise HTTPException(
                status.HTTP_400_BAD_REQUEST, "current extractor is None"
            )
        if state.current_extractor.selector is None:
            raise HTTPException(status.HTTP_400_BAD_REQUEST, "current selector is None")
        selectors = parse_selector(state.current_extractor.selector)
    return templates.TemplateResponse(
        name="details.html",
        context={"selectors": selectors},
        request=request,
    )


@router.post("/preview")
async def post_preview(
    request: Request,
    body: dict[str, str | list[str]] = Body(),
):
    selector = build_selector(body)
    with state_context(request) as state:
        assert state.current_extractor is not None
        state.current_extractor.selector = selector

        extractor = state.current_extractor
        if extractor.value is None or extractor.value != ValueType.href:
            extractor.value = extractor.guess_value()

        bs = get_clean(state.url, driver, state.delay)

    return "\n".join(get_value(tag, extractor.value) for tag in bs.select(selector))
