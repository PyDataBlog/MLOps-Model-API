import pytest
from fastapi.testclient import TestClient
from app.main import app


@pytest.fixture
def client():
    return TestClient(app)


def test_root(client):
    response = client.get("/")
    assert response.status_code == 200
    assert response.json() == {"Message": "API is running"}


def test_predict(client):
    response = client.post("/predict", json={"code_snippet": "'#include <string>'"})
    assert response.status_code == 200
    results = response.json()
    assert results["predicted_class"] == 3
    assert results["predicted_language"] == "C++"
    assert round(results["predicted_value"], 4) == 0.3762
