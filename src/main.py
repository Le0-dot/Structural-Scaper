from typing import Any
from random import randint
from urllib.parse import unquote

from fastapi import Depends, FastAPI, Request, Response, status
from fastapi.responses import HTMLResponse, RedirectResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from pydantic import BaseModel
from selenium.webdriver.ie.webdriver import WebDriver
from starlette.middleware.sessions import SessionMiddleware

import parser
from state import State, Extractor
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
    return templates.TemplateResponse(name="index.html", request=request)


@app.get("/init", response_class=RedirectResponse)
def init(request: Request, url: str, delay: int):
    State(request).init(unquote(url), delay)
    return str(request.url_for("recipe"))


@app.get("/recipe", response_class=HTMLResponse)
def recipe(request: Request, templates: Jinja2Templates = Depends(Templates.get)):
    extractors = map(Extractor, State(request).extractors)
    contexts = (e.to_dict(request=request) for e in extractors)

    extractor_tempalte = templates.get_template("extractor.html")
    rendered = map(extractor_tempalte.render, contexts)

    return templates.TemplateResponse(
        name="recipe.html",
        context={"extractors": rendered},
        request=request,
    )


@app.get("/extractor", response_class=HTMLResponse)
def extractor(request: Request, templates: Jinja2Templates = Depends(Templates.get)):
    extractors = State(request).extractors
    extractor = Extractor()
    extractors.append(extractor.data)
    return templates.TemplateResponse(
        name="extractor.html",
        context={"id": extractor.id},
        request=request,
    )


@app.put("/recipe/current", status_code=status.HTTP_204_NO_CONTENT)
def current(request: Request, idx: int):
    State(request).current_extractor_id = idx


@app.put("/recipe/extractor/{id}/name", status_code=status.HTTP_204_NO_CONTENT)
def put_name(request: Request, id: int, data: dict[str, str] = Body()):
    extractor = State(request).extractor_by_id(id)
    if extractor is None:
        return Response(status_code=status.HTTP_404_NOT_FOUND)
    extractor.name = data["name"]


@app.delete("/recipe/extractor/{id}", status_code=status.HTTP_200_OK)
def remove(request: Request, id: int):
    state = State(request)
    idx = state.extractor_idx_by_id(id)
    if idx is None:
        return Response(status_code=status.HTTP_404_NOT_FOUND)
    del state.extractors[idx]


@app.get("/select", response_class=HTMLResponse)
def select(
    request: Request,
    id: int,
    templates: Jinja2Templates = Depends(Templates.get),
    driver: WebDriver = Depends(Driver.get),
):
    state = State(request)
    state.current_extractor_id = id
    url = state.url
    return templates.TemplateResponse(
        name="selector.html",
        context=get_context(url, driver),
        request=request,
    )


@app.get("/select/details", response_class=HTMLResponse)
def details(
    request: Request, selector: str, templates: Jinja2Templates = Depends(Templates.get)
):
    selectors = parse_selector(selector)
    return templates.TemplateResponse(
        name="details.html",
        context={"selectors": selectors},
        request=request,
    )


@app.get("/select/next", response_class=RedirectResponse)
def next(request: Request, selector: str):
    state = State(request)

    assert state.current_extractor is not None
    state.current_extractor.selector = selector
    del state.current_extractor

    return str(request.url_for("recipe"))


# @app.get("/select/confirm", response_class=HTMLResponse)
# def confirm(
#     request: Request,
#     templates: Jinja2Templates = Depends(Templates.get),
#     driver: WebDriver = Depends(Driver.get),
# ):
#     soup = get_clean(state.get_url(request.session), driver)
#     selectors = state.get_selectors(request.session)
#     selectors = dict(enumerate(selectors))
#     result = parser.select_all(soup, selectors)
#     return templates.TemplateResponse(
#         name="confirm.html",
#         context={"result": result},
#         request=request,
#     )
