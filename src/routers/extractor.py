from fastapi import APIRouter, Request, Body, HTTPException, Depends, status
from fastapi.responses import HTMLResponse, Response
from fastapi.templating import Jinja2Templates

from state import State, Extractor
from resources import templates


router = APIRouter(
    default_response_class=Response,
)


@router.get("/", response_class=HTMLResponse)
def extractor(request: Request, templates: Jinja2Templates = Depends(templates)):
    extractors = State(request).extractors
    extractor = Extractor()
    extractors.append(extractor.to_dict())
    return templates.TemplateResponse(
        name="extractor.html",
        context=extractor.to_dict(),
        request=request,
    )


@router.put("/{id}/name", status_code=status.HTTP_204_NO_CONTENT)
def put_name(request: Request, id: int, data: dict[str, str] = Body()):
    extractor = State(request).extractor_by_id(id)
    if extractor is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND)
    extractor.name = data["name"]


@router.put("/{id}/value", status_code=status.HTTP_204_NO_CONTENT)
def put_value(request: Request, id: int, data: dict[str, str] = Body()):
    extractor = State(request).extractor_by_id(id)
    if extractor is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND)
    extractor.value = data["value"]


@router.delete("/{id}", status_code=status.HTTP_200_OK)
def remove(request: Request, id: int):
    state = State(request)
    idx = state.extractor_idx_by_id(id)
    if idx is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND)
    del state.extractors[idx]
