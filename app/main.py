import os
import logging
import logging.config
import uvicorn
import yaml
import tensorflow as tf
from fastapi import FastAPI, Request
from typing import Dict, Union
from .utils import lang_mappings, Prediction, Features


os.environ["TF_FORCE_GPU_ALLOW_GROWTH"] = "true"

# Load logging config
with open("app/logging.yaml", "r") as f:
    config = yaml.safe_load(f.read())
    logging.config.dictConfig(config)

logger = logging.getLogger(__name__)

# Load Code Classifier Model
model_name = "code_classifier"
loaded_model = tf.keras.models.load_model(f"app/models/{model_name}")


app = FastAPI(
    title="FastKube",
    description="A simple demo API for serving a trained ML model using Kubernetes",
    version="0.1.0",
)


@app.get("/")
@app.get("/home")
async def read_root(request: Request) -> Dict[str, str]:
    logger.info(f"Home page requested by user: {request.client.host}")
    return {"Message": "API is running"}


@app.post("/predict", response_model=Prediction)
async def predict(payload: Features, request: Request) -> Dict[str, Union[str, int, float]]:
    predicted_class = loaded_model.predict([payload.code_snippet])
    logger.info(f"Successfully Generated predicttion for user: {request.client.host}")
    return Prediction(
        predicted_class=predicted_class.argmax(),
        predicted_language=lang_mappings[predicted_class.argmax()],
        predicted_value=predicted_class[0][predicted_class.argmax()],
    )


if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000, log_level="info")
