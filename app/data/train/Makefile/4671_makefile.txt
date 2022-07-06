docker:
	docker build -t mihxil/npo-pyapi-flask .

docker-run:
	docker run --env secret=${secret} -p 5000:5000  mihxil/npo-pyapi-flask

run:
	FLASK_ENV=development configdir=$(pwd) flask run
