from fastapi import APIRouter, Request, Depends, status
from fastapi.responses import HTMLResponse, Response
from fastapi.templating import Jinja2Templates

from state import State, Extractor
from resources import templates


router = APIRouter(
    default_response_class=Response,
)


@router.get("/", response_class=HTMLResponse)
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


@router.put("/current", status_code=status.HTTP_204_NO_CONTENT)
def current(request: Request, idx: int):
    State(request).current_extractor_id = idx
