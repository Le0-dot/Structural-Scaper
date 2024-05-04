from typing import Iterator
from contextlib import contextmanager

from fastapi import Request
from fastapi.templating import Jinja2Templates

from .state import State


def templates(path: str = "templates") -> Jinja2Templates:
    return Jinja2Templates(
        path, autoescape=False, auto_reload=True, trim_blocks=True, lstrip_blocks=True
    )


@contextmanager
def state_context(request: Request) -> Iterator[State]:
    state = State.model_validate(request.session["state"])
    try:
        yield state
    finally:
        request.session["state"] = state.model_dump()


def init_state(request: Request, url: str, delay: int) -> None:
    state = State(url=url, delay=delay)
    request.session["state"] = state.model_dump()
