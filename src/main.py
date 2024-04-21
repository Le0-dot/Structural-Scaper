from random import randint
from urllib.parse import unquote

from fastapi import Depends, FastAPI, Request
from fastapi.responses import FileResponse, HTMLResponse, RedirectResponse
from fastapi.staticfiles import StaticFiles
from jinja2 import Environment
from selenium.webdriver.ie.webdriver import WebDriver
from starlette.middleware.sessions import SessionMiddleware

import state
import parser
from selector import parse_selector
from cleaner import get_and_clean, get_and_render
from resources import Env, Driver


def gen_secret(length: int) -> str:
    return "".join([chr(randint(0, 128)) for _ in range(length)])


app = FastAPI()
app.add_middleware(SessionMiddleware, secret_key="sdfadsfa")  # gen_secret(128))

app.mount("/js", StaticFiles(directory="static/js"), name="js")


@app.get("/", response_class=FileResponse)
def index(request: Request):
    state.init(request.session)
    return "./static/html/index.html"


@app.get("/select", response_class=HTMLResponse)
def select(
    request: Request,
    url: str,
    env: Environment = Depends(Env.get),
    driver: WebDriver = Depends(Driver.get),
):
    url = unquote(url)
    state.select(request.session, url)
    return get_and_render(url, env.get_template("selector.html"), driver)


@app.get("/select/details", response_class=HTMLResponse)
def details(selector: str, env: Environment = Depends(Env.get)):
    selectors = parse_selector(selector)
    template = env.get_template("details.html")
    return template.render({"selectors": selectors})


@app.get("/select/next", response_class=RedirectResponse)
def next(request: Request, selector: str):
    print(selector)
    if state.push(request.session, selector):
        return f"/select?url={state.get_url(request.session)}"
    return "/select/confirm"


@app.get("/select/confirm", response_class=HTMLResponse)
def confirm(
    request: Request,
    env: Environment = Depends(Env.get),
    driver: WebDriver = Depends(Driver.get),
):
    soup = get_and_clean(state.get_url(request.session), driver)
    selectors = state.get_selectors(request.session)
    selectors = dict(enumerate(selectors))
    result = parser.select_all(soup, selectors)
    template = env.get_template("confirm.html")
    return template.render({"result": result})
