from locust import HttpUser, task


class SwarmUsers(HttpUser):
    """Simulate users swarmming the application."""

    @task
    def index_page(self):
        self.client.get("/")

    @task
    def predict_page(self):
        self.client.post("/predict", json={"code_snippet": "'#include <string>'"})
