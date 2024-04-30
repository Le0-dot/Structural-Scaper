from fastapi import APIRouter, Request, Body, status
from fastapi.responses import Response

from state import State


router = APIRouter(
    default_response_class=Response,
)


@router.put("/filename", status_code=status.HTTP_204_NO_CONTENT)
def put_filename(request: Request, data: dict[str, str] = Body()):
    state = State(request)
    state.template.filename = data["filename"]


@router.put("/content", status_code=status.HTTP_204_NO_CONTENT)
def put_content(request: Request, data: dict[str, str] = Body()):
    state = State(request)
    state.template.content = data["content"]
