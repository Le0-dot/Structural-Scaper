import os
from urllib.parse import unquote, urljoin

from requests import post
from fastapi import APIRouter, Depends, HTTPException, Request, Response, status
from fastapi.responses import HTMLResponse, RedirectResponse
from fastapi.templating import Jinja2Templates
from pydantic import ValidationError

from ..resources import init_state, state_context, templates

from structural_scraper_common import mongo, StartWorker


router = APIRouter()


@router.get("/", response_class=HTMLResponse)
def get_index(request: Request, templates: Jinja2Templates = Depends(templates)):
    return templates.TemplateResponse(name="index.html", request=request)


@router.get("/init", response_class=RedirectResponse)
def get_init(request: Request, url: str, delay: int):
    init_state(request, unquote(url), delay)
    return str(request.url_for("get_recipe"))


@router.get("/save", response_class=HTMLResponse)
def get_save(request: Request):
    with state_context(request) as state, mongo() as db:
        try:
            model_repr = state.to_model().dump()
            db["model"].insert_one(model_repr)
            return Response(status_code=status.HTTP_204_NO_CONTENT)
        except ValidationError:
            return "Found errors"


@router.get("/success", response_class=HTMLResponse)
def get_success(request: Request):
    with state_context(request) as state:
        schema = StartWorker(url=state.url)
    if (worker := os.getenv("WORKER_URL")) is None:
        raise HTTPException(status.HTTP_503_SERVICE_UNAVAILABLE)
    post(urljoin(worker, "start"), json=schema.model_dump())
    return "<h1>Success</h1>"
