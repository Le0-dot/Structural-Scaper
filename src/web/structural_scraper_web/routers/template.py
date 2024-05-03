from fastapi import APIRouter, Request, Body
from fastapi.responses import HTMLResponse

from resources import state_context
from jinja_validation import validate


router = APIRouter(
    default_response_class=HTMLResponse,
)


@router.put("/filename")
async def put_filename(request: Request, data: dict[str, str] = Body()):
    with state_context(request) as state:
        state.template.filename = data["filename"]
        return validate(state.template.filename, state.names)


@router.put("/next")
async def put_next(request: Request, data: dict[str, str] = Body()):
    with state_context(request) as state:
        state.template.next = data["next"]
        return validate(state.template.next, state.names)


@router.put("/content")
async def put_content(request: Request, data: dict[str, str] = Body()):
    with state_context(request) as state:
        state.template.content = data["content"]
        return validate(state.template.content, state.names)
