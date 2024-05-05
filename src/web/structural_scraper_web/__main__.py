import os

import uvicorn

from .app import app


if __name__ == "__main__":
    uvicorn.run(
        app,
        host="0.0.0.0",
        port=int(os.getenv("WEB_PORT", 80)),
    )
