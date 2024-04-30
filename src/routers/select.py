from fastapi import APIRouter, Body, Depends, Request, status
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from state import State
from cleaner import get_context
from resources import templates, driver


router = APIRouter(default_response_class=HTMLResponse)


@router.get("/")
def select(
    request: Request,
    id: int,
    templates: Jinja2Templates = Depends(templates),
):
    state = State(request)
    state.current_extractor_id = id
    return templates.TemplateResponse(
        name="selector.html",
        context=get_context(state.url, driver, state.delay),
        request=request,
    )


@router.put("/selector", status_code=status.HTTP_200_OK)
def put_selector(request: Request, data: str = Body()):
    state = State(request)
    assert state.current_extractor is not None
    state.current_extractor.selector = data
