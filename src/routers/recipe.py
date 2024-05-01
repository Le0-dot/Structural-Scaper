from fastapi import APIRouter, Request, Depends, status
from fastapi.responses import HTMLResponse, Response
from fastapi.templating import Jinja2Templates

from resources import state_context, templates


router = APIRouter(
    default_response_class=Response,
)


@router.get("/", response_class=HTMLResponse)
def recipe(request: Request, templates: Jinja2Templates = Depends(templates)):
    with state_context(request) as state:
        del state.current_extractor

        contexts = [e.model_dump() for e in state.extractors.values()]
        for context in contexts:
            context.update(request=request)

        extractor_tempalte = templates.get_template("extractor.html")
        rendered = map(extractor_tempalte.render, contexts)

        return templates.TemplateResponse(
            name="recipe.html",
            context={"extractors": rendered, "template": state.template},
            request=request,
        )


@router.put("/current", status_code=status.HTTP_204_NO_CONTENT)
def current(request: Request, id: int):
    with state_context(request) as state:
        state.current_extractor_id = id
