from random import randint
from urllib.parse import unquote

from fastapi import Body, Depends, FastAPI, Request, Response, status
from fastapi.responses import HTMLResponse, RedirectResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from starlette.middleware.sessions import SessionMiddleware

from state import State, Extractor
from selector import parse_selector, build_selector
from cleaner import get_clean, get_context
from resources import templates, driver


def gen_secret(length: int) -> str:
    return "".join([chr(randint(0, 128)) for _ in range(length)])


app = FastAPI()
app.add_middleware(SessionMiddleware, secret_key="sdfadsfa")  # gen_secret(128))

app.mount("/js", StaticFiles(directory="static/js"), name="js")
app.mount("/css", StaticFiles(directory="static/css"), name="css")


@app.get("/", response_class=HTMLResponse)
def index(request: Request, templates: Jinja2Templates = Depends(templates)):
    return templates.TemplateResponse(name="index.html", request=request)


@app.get("/init", response_class=RedirectResponse)
def init(request: Request, url: str, delay: int):
    State(request).init(unquote(url), delay)
    return str(request.url_for("recipe"))


@app.get("/recipe", response_class=HTMLResponse)
def recipe(request: Request, templates: Jinja2Templates = Depends(templates)):
    state = State(request)
    del state.current_extractor

    extractors = map(Extractor, state.extractors)
    contexts = (e.to_dict(request=request) for e in extractors)

    extractor_tempalte = templates.get_template("extractor.html")
    rendered = map(extractor_tempalte.render, contexts)

    return templates.TemplateResponse(
        name="recipe.html",
        context={"extractors": rendered, "template": state.template},
        request=request,
    )


@app.get("/extractor", response_class=HTMLResponse)
def extractor(request: Request, templates: Jinja2Templates = Depends(templates)):
    extractors = State(request).extractors
    extractor = Extractor()
    extractors.append(extractor.data)
    return templates.TemplateResponse(
        name="extractor.html",
        context=extractor.to_dict(),
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


@app.put("/recipe/extractor/{id}/value", status_code=status.HTTP_204_NO_CONTENT)
def put_value(request: Request, id: int, data: dict[str, str] = Body()):
    extractor = State(request).extractor_by_id(id)
    if extractor is None:
        return Response(status_code=status.HTTP_404_NOT_FOUND)
    extractor.value = data["value"]


@app.delete("/recipe/extractor/{id}", status_code=status.HTTP_200_OK)
def remove(request: Request, id: int):
    state = State(request)
    idx = state.extractor_idx_by_id(id)
    if idx is None:
        return Response(status_code=status.HTTP_404_NOT_FOUND)
    del state.extractors[idx]


@app.put("/recipe/template/filename", status_code=status.HTTP_204_NO_CONTENT)
def put_filename(request: Request, data: dict[str, str] = Body()):
    state = State(request)
    state.template.filename = data["filename"]


@app.put("/recipe/template/content", status_code=status.HTTP_204_NO_CONTENT)
def put_content(request: Request, data: dict[str, str] = Body()):
    state = State(request)
    state.template.content = data["content"]


@app.get("/select", response_class=HTMLResponse)
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


@app.put("/select/selector", status_code=status.HTTP_200_OK)
def put_selector(request: Request, data: str = Body()):
    state = State(request)
    assert state.current_extractor is not None
    state.current_extractor.selector = data


@app.get("/select/details", response_class=HTMLResponse)
def details(
    request: Request, templates: Jinja2Templates = Depends(templates)
):
    state = State(request)
    assert state.current_extractor is not None
    selectors = parse_selector(state.current_extractor.selector)
    return templates.TemplateResponse(
        name="details.html",
        context={"selectors": selectors},
        request=request,
    )


@app.post("/select/details/preview", response_class=HTMLResponse)
def preview(
    request: Request,
    body: dict[str, str | list[str]] = Body(),
):
    selector = build_selector(body)
    state = State(request)
    assert state.current_extractor is not None
    state.current_extractor.selector = selector

    bs = get_clean(state.url, driver, state.delay)
    return "\n".join(tag.text for tag in bs.select(selector))
