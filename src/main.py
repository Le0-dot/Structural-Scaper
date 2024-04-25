from random import randint
from urllib.parse import unquote

from fastapi import Depends, FastAPI, Request
from fastapi.responses import HTMLResponse, RedirectResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from jinja2 import Environment, FileSystemLoader
from selenium.webdriver.ie.webdriver import WebDriver
from starlette.middleware.sessions import SessionMiddleware

import state
import parser
from selector import parse_selector
from cleaner import get_clean, get_context
from resources import Templates, Driver


def gen_secret(length: int) -> str:
    return "".join([chr(randint(0, 128)) for _ in range(length)])


app = FastAPI()
app.add_middleware(SessionMiddleware, secret_key="sdfadsfa")  # gen_secret(128))

app.mount("/js", StaticFiles(directory="static/js"), name="js")


@app.get("/", response_class=HTMLResponse)
def index(request: Request, templates: Jinja2Templates = Depends(Templates.get)):
    state.init(request.session)
    return templates.TemplateResponse(name="index.html", request=request)


@app.get("/select", response_class=HTMLResponse)
def select(
    request: Request,
    url: str,
    templates: Jinja2Templates = Depends(Templates.get),
    driver: WebDriver = Depends(Driver.get),
):
    url = unquote(url)
    state.select(request.session, url)
    return templates.TemplateResponse(
        name="selector.html",
        context=get_context(url, driver),
        request=request,
    )


@app.get("/select/details", response_class=HTMLResponse)
def details(request: Request, selector: str, templates: Jinja2Templates = Depends(Templates.get)):
    selectors = parse_selector(selector)
    return templates.TemplateResponse(
        name="details.html",
        context={"selectors": selectors},
        request=request,
    )


@app.get("/select/next", response_class=RedirectResponse)
def next(request: Request, selector: str):
    if state.push(request.session, selector):
        return f"/select?url={state.get_url(request.session)}"
    return "/select/confirm"


@app.get("/select/confirm", response_class=HTMLResponse)
def confirm(
    request: Request,
    templates: Jinja2Templates = Depends(Templates.get),
    driver: WebDriver = Depends(Driver.get),
):
    soup = get_clean(state.get_url(request.session), driver)
    selectors = state.get_selectors(request.session)
    selectors = dict(enumerate(selectors))
    result = parser.select_all(soup, selectors)
    return templates.TemplateResponse(
        name="confirm.html",
        context={"result": result},
        request=request,
    )
