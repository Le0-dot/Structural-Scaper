from pydantic import BaseModel


class StartWorker(BaseModel):
    url: str
