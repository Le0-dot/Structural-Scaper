from fastapi import APIRouter, Depends, Request, Body
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from cleaner import get_context
from resources import state_context, templates

from structural_scraper_common import webdriver


router = APIRouter(default_response_class=HTMLResponse)


@router.get("/")
async def get_select(
    request: Request,
    id: int,
    templates: Jinja2Templates = Depends(templates),
):
    with state_context(request) as state:
        state.current_extractor_id = id
        return templates.TemplateResponse(
            name="selector.html",
            context=get_context(state.url, webdriver, state.delay),
            request=request,
        )


@router.put("/selector")
async def put_selector(request: Request, data: str = Body()):
    with state_context(request) as state:
        assert state.current_extractor is not None
        state.current_extractor.selector = data
