from fastapi import APIRouter, Request, Depends
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from resources import state_context, templates


router = APIRouter(
    default_response_class=HTMLResponse,
)


@router.get("/")
async def get_recipe(request: Request, templates: Jinja2Templates = Depends(templates)):
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
