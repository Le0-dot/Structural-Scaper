from fastapi import APIRouter, Request, Body, status
from fastapi.responses import Response

from resources import state_context


router = APIRouter(
    default_response_class=Response,
)


@router.put("/filename", status_code=status.HTTP_204_NO_CONTENT)
def put_filename(request: Request, data: dict[str, str] = Body()):
    with state_context(request) as state:
        state.template.filename = data["filename"]


@router.put("/content", status_code=status.HTTP_204_NO_CONTENT)
def put_content(request: Request, data: dict[str, str] = Body()):
    with state_context(request) as state:
        state.template.content = data["content"]
