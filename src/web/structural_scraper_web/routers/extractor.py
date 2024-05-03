from fastapi import APIRouter, Request, Body, HTTPException, Depends, Response, status
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from state import Extractor, ValueType
from resources import state_context, templates


router = APIRouter(
    default_response_class=Response,
)


@router.get("/", response_class=HTMLResponse)
async def get_extractor(request: Request, templates: Jinja2Templates = Depends(templates)):
    with state_context(request) as state:
        extractor = Extractor()
        state.append_extractor(extractor)
        return templates.TemplateResponse(
            name="extractor.html",
            context=extractor.model_dump(),
            request=request,
        )


@router.put("/{id}/name", status_code=status.HTTP_200_OK)
async def put_extractor_name(request: Request, id: int, data: dict[str, str] = Body()):
    with state_context(request) as state:
        extractor = state.extractors[id]
        if extractor is None:
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND)
        extractor.name = data["name"]


@router.put("/{id}/value", status_code=status.HTTP_204_NO_CONTENT)
async def put_extractor_value(request: Request, id: int, data: dict[str, str] = Body()):
    with state_context(request) as state:
        extractor = state.extractors[id]
        if extractor is None:
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND)
        extractor.value = ValueType(data["value"])


@router.delete("/{id}", status_code=status.HTTP_200_OK)
async def delete_extractor(request: Request, id: int):
    with state_context(request) as state:
        if id not in state.extractors.keys():
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND)
        del state.extractors[id]
