from urllib.parse import unquote

from fastapi import APIRouter, Depends, Request
from fastapi.responses import HTMLResponse, RedirectResponse
from fastapi.templating import Jinja2Templates

from state import State
from resources import templates


router = APIRouter()


@router.get("/", response_class=HTMLResponse)
def index(request: Request, templates: Jinja2Templates = Depends(templates)):
    return templates.TemplateResponse(name="index.html", request=request)


@router.get("/init", response_class=RedirectResponse)
def init(request: Request, url: str, delay: int):
    State(request).init(unquote(url), delay)
    return str(request.url_for("recipe"))
