from fastapi import APIRouter, Depends, Request
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from cleaner import get_context
from resources import state_context, templates, driver


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
            context=get_context(state.url, driver, state.delay),
            request=request,
        )
